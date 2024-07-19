// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.DataTruncation;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.Locale;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import nexj.core.meta.Primitive;
import nexj.core.meta.persistence.sql.Column;
import nexj.core.meta.persistence.sql.Index;
import nexj.core.meta.persistence.sql.IndexColumn;
import nexj.core.meta.persistence.sql.RelationalDatabase;
import nexj.core.meta.persistence.sql.RelationalDatabaseFragment;
import nexj.core.meta.persistence.sql.RelationalSchema;
import nexj.core.meta.persistence.sql.Table;
import nexj.core.persistence.Query;
import nexj.core.persistence.operator.FunctionOperator;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.Binary;
import nexj.core.util.HashTab;
import nexj.core.util.KeywordTab;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.SoftHashTab;
import nexj.core.util.TZ;

public class MySQLAdapter extends SQLAdapter
{
   // constants

   /**
    * SQL keywords that have to be quoted.
    */
   protected final static KeywordTab KEYWORDS = new KeywordTab(new String[]
   {
      "accessible", "add", "all", "alter", "analyze", "and", "as", "asc",
      "asensitive", "before", "between", "bigint", "binary", "blob", "both", "by",
      "call", "cascade", "case", "change", "char", "character", "check", "collate",
      "column", "condition", "constraint", "continue", "convert", "create", "cross",
      "current_date", "current_time", "current_timestamp", "current_user", "cursor",
      "database", "databases", "day_hour", "day_microsecond", "day_minute",
      "day_second", "dec", "decimal", "declare", "default", "delayed", "delete",
      "desc", "describe", "deterministic", "distinct", "distinctrow", "div",
      "double", "drop", "dual", "each", "else", "elseif", "enclosed", "escaped",
      "exists", "exit", "explain", "false", "fetch", "float", "float4", "float8",
      "for", "force", "foreign", "from", "fulltext", "general", "grant", "group",
      "having", "high_priority", "hour_microsecond", "hour_minute", "hour_second",
      "if", "ignore", "ignore_server_ids", "in", "index", "infile", "inner", "inout",
      "insensitive", "insert", "int", "int1", "int2", "int3", "int4", "int8",
      "integer", "interval", "into", "is", "iterate", "join", "key", "keys", "kill",
      "leading", "leave", "left", "like", "limit", "linear", "lines", "load",
      "localtime", "localtimestamp", "lock", "long", "longblob", "longtext", "loop",
      "low_priority", "master_heartbeat_period", "master_ssl_verify_server_cert",
      "match", "maxvalue", "maxvalue", "mediumblob", "mediumint", "mediumtext",
      "middleint", "minute_microsecond", "minute_second", "mod", "modifies",
      "natural", "no_write_to_binlog", "not", "null", "numeric", "on", "optimize",
      "option", "optionally", "or", "order", "out", "outer", "outfile", "precision",
      "primary", "procedure", "purge", "range", "read", "read_write", "reads",
      "real", "references", "regexp", "release", "rename", "repeat", "replace",
      "require", "resignal", "resignal", "restrict", "return", "revoke", "right",
      "rlike", "schema", "schemas", "second_microsecond", "select", "sensitive",
      "separator", "set", "show", "signal", "signal", "slow", "smallint", "spatial",
      "specific", "sql", "sql_big_result", "sql_calc_found_rows", "sql_small_result",
      "sqlexception", "sqlstate", "sqlwarning", "ssl", "starting", "straight_join",
      "table", "terminated", "then", "tinyblob", "tinyint", "tinytext", "to",
      "trailing", "trigger", "true", "undo", "union", "unique", "unlock", "unsigned",
      "update", "usage", "use", "using", "utc_date", "utc_time", "utc_timestamp",
      "values", "varbinary", "varchar", "varcharacter", "varying", "when", "where",
      "while", "with", "write", "xor", "year_month", "zerofill"
   });
   
   /**
    * Default fetch size to use (default for MySQL is all rows)
    * Note: this only works with MySQLServer > 5.0.2 and MySQL Connector/J >= 3.2.1-alpha
    *       the following must be set for fetch size to take effect: useCursorFetch=true && useServerPrepStmts=true 
    */
   protected final static int DEFAULT_FETCH_SIZE = 256;

   // These are valid for MySQL 5.0.3+.
   // Older versions have max varchar and binary column precision of 255.

   /**
    * Maximum char column precision.
    */
   public final static int MAX_CHAR_PRECISION = 255;

   /**
    * Maximum varchar column precision. (the actual limit is 65535B split between all variable columns,
    * but after doing benchmark testing 8k characters seems to be a good value).
    */
   public final static int MAX_VARCHAR_PRECISION  = 8192;

   /**
    * Maximum binary column precision.
    */
   public final static int MAX_BINARY_PRECISION = 255;

   /**
    * Maximum varbinary column precision. (the actual limit is 65535B split between all variable columns,
    * but after doing benchmark testing 8k characters seems to be a good value).
    */
   public final static int MAX_VARBINARY_PRECISION = 8192;

   /**
    * Maximum float column precision.
    */
   public final static int MAX_FLOAT_PRECISION = 53;

   /**
    * Pattern used to extract the key name from a duplicate key error message. e.g. ("^Duplicate entry '(.*)' for key (.*)$")
    */
   protected final static Pattern DUPLICATE_KEY_NAME_PATTERN = Pattern.compile("^.*'(.*)' .* ([0-9]+)[^0-9]*$");

   /**
    * The Timestamp string format using MySQL identifiers
    */
   protected final static String TIMESTAMP_FORMAT_SQL = "%Y-%m-%d %H:%i:%s.%f";
   
   /**
    * The Timestamp format identical to TIMESTAMP_FORMAT_SQL as a Java SimpleDateFormat.
    * This object needs to be cloned before use since it stores state while formatting.
    */
   protected final static SimpleDateFormat TIMESTAMP_FORMAT = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS", Locale.ENGLISH);

   static
   {
      TIMESTAMP_FORMAT.setTimeZone(TZ.UTC);
   }

   // associations
   
   /**
    *  Map of table metadata object to index ordinal number map: String[Table, Integer].
    */
   protected final static Lookup s_indexSchemaNameMap = new SoftHashTab();

   /**
    * The class logger.
    */
   private final static Logger s_logger = Logger.getLogger(MySQLAdapter.class);

   /**
    * The Timestamp format used by MySQL when representing and expecting timestamps.
    * Local instance, lazy init.
    */
   protected SimpleDateFormat m_timestampFormat;

   // operations

   /**
    * @return The keyword table.
    */
   protected Set getKeywords()
   {
      return KEYWORDS;
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendIdentityColumn(java.lang.StringBuffer, nexj.core.persistence.sql.SQLInsert)
    */
   public boolean appendIdentityColumn(StringBuffer buf, SQLInsert work)
   {
      return false;
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendIdentityPrefix(java.lang.StringBuffer, nexj.core.persistence.sql.SQLInsert)
    */
   public void appendIdentityPrefix(StringBuffer buf, SQLInsert work)
   {
   }

   /**
    * should use java.sql.Statement.getGeneratedKeys() after statement execution (since it's hard to make LAST_INSERT_ID() part of the SQL statement),
    * see http://dev.mysql.com/doc/refman/5.0/en/connector-j-usagenotes-basic.html#connector-j-usagenotes-last-insert-id
    *
    * @see nexj.core.persistence.sql.SQLAdapter#appendIdentitySuffix(java.lang.StringBuffer, nexj.core.persistence.sql.SQLInsert)
    */
   public boolean appendIdentitySuffix(StringBuffer buf, SQLInsert work)
   {
      buf.append(";select last_insert_id()"); // <-- this is only possible by setting "allowMultiQueries" to true in mysql.connection 
      return true;
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendIdentityValue(java.lang.StringBuffer, nexj.core.persistence.sql.SQLInsert)
    */
   public boolean appendIdentityValue(StringBuffer buf, SQLInsert work)
   {
      return false;
   }

   /**
    * see http://www.petefreitag.com/item/613.cfm for examples
    *
    * @see nexj.core.persistence.sql.SQLAdapter#appendInfixHint(java.lang.StringBuffer, nexj.core.persistence.Query)
    */
   public void appendInfixHint(StringBuffer buf, Query query)
   {
   }
 
   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendLikeEscape(java.lang.StringBuffer)
    */
   public void appendLikeEscape(StringBuffer buf)
   {
      buf.append(" escape '\\\\'");
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendLiteral(java.lang.StringBuffer, java.lang.String)
    */
   protected void appendLiteral(StringBuffer buf, String sValue)
   {
      buf.append('\'');

      // escape single-quotes and backslashes
      for (int i = 0; i < sValue.length(); ++i)
      {
         char ch = sValue.charAt(i);

         switch (ch)
         {
            case '\'':
               buf.append('\'');

               break;

            case '\\':
               buf.append('\\');

               break;
         }

         buf.append(ch);
      }

      buf.append('\'');
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendLiteral(java.lang.StringBuffer, nexj.core.meta.Primitive, java.lang.Object)
    */
   public void appendLiteral(StringBuffer buf, Primitive type, Object value)
   {
      int i;

      if (value == null)
      {
         buf.append("null");
         return;
      }

      switch (type.getOrdinal())
      {
         case Primitive.STRING_ORDINAL:
            if (m_bUnicode)
            {
               buf.append('N');
            }

            appendLiteral(buf, value.toString());

            break;

         case Primitive.BINARY_ORDINAL:
            buf.append("0x");
            Binary.append(buf, ((Binary)value).getData(), -1);
            break;

         case Primitive.INTEGER_ORDINAL:
            buf.append(((Integer)value).intValue());
            break;

         case Primitive.LONG_ORDINAL:
            buf.append(((Long)value).longValue());
            break;

         case Primitive.DECIMAL_ORDINAL:
            buf.append(((BigDecimal)value).toString());
            break;

         case Primitive.FLOAT_ORDINAL:
            i = buf.length();
            buf.append(((Float)value).floatValue());

            if (buf.indexOf("E", i) < 0)
            {
               buf.append("E0");
            }

            break;

         case Primitive.DOUBLE_ORDINAL:
            i = buf.length();
            buf.append(((Double)value).doubleValue());

            if (buf.indexOf("E", i) < 0)
            {
               buf.append("E0");
            }

            break;

         case Primitive.TIMESTAMP_ORDINAL:
            if (m_timestampFormat == null)
            {
               m_timestampFormat = (SimpleDateFormat)TIMESTAMP_FORMAT.clone();
            }

            buf.append("date_format('").append(m_timestampFormat.format((Timestamp)value)).append("','" + TIMESTAMP_FORMAT_SQL + "')");

            break;

         case Primitive.BOOLEAN_ORDINAL:
            buf.append((((Boolean)value).booleanValue()) ? "true" : "false");
            break;

         default:
            throw new IllegalArgumentException("Invalid literal type");
      }
   }

   /**
    * Append MySQL specific match expression syntax.
    * The symbols used in this function have to map 1-to-1 to MatchNode.SYMBOL
    * @param buf The buffer to append to.
    * @param expression Validated expression using MatchOperator symbols.
    *                   Has to be Object because String is passed as is due to the requirement of
    *                   not modifying the original expression during validation.
    * @param bMax Append expression for maximum possible score
    */
   protected void appendMatchExpression(StringBuffer buf, Object expression, boolean bMax)
   {
      if (expression instanceof String)
      {
         buf.append('"');
         appendMatchLiteral(buf, expression.toString()); // sanitize
         buf.append('"');
      }
      else if (expression instanceof Pair)
      {
         if (Symbol.AND.equals(((Pair)expression).getHead())) // MatchNode.AND.SYMBOL
         {
            buf.append('(');

            for (Pair next = ((Pair)expression).getNext(); next != null; next = next.getNext())
            {
               buf.append((bMax) ? " (" : " +("); // use OR for max score gathering
               appendMatchExpression(buf, next.getHead(), bMax);
               buf.append(')');
            }

            buf.append(')');
         }
         else if (Symbol.LIKE_P.equals(((Pair)expression).getHead())) // MatchNode.FUZZY.SYMBOL
         {
            buf.append("(*"); // MySQL doesn't have fuzzy search, '*' applies to unquoted term only
            appendMatchLiteral(buf, ((Pair)expression).getNext().getHead().toString()); // sanitize
            buf.append("*)");
         }
         else if (Symbol.NOT.equals(((Pair)expression).getHead())) // MatchNode.NOT.SYMBOL
         {
            // in MySQL a '+' and a '-' are mutually exclusive (e.g. "and not" is just '-')
            if (buf.length() > 1 && buf.charAt(buf.length() - 2) == '+') // i.e. +(...)
            {
               buf.setCharAt(buf.length() - 2, '-');
               appendMatchExpression(buf, ((Pair)expression).getNext(), bMax); // already has () by +(...)
            }
            else
            {
               buf.append(" -(");
               appendMatchExpression(buf, ((Pair)expression).getNext(), bMax);
               buf.append(')');
            }
         }
         else if (Symbol.OR.equals(((Pair)expression).getHead())) // MatchNode.OR.SYMBOL
         {
            buf.append('(');

            for (Pair next = ((Pair)expression).getNext(); next != null; next = next.getNext())
            {
               buf.append(" ("); // in MySQL OR is a space
               appendMatchExpression(buf, next.getHead(), bMax);
               buf.append(')');
            }

            buf.append(')');
         }
         else if (((Pair)expression).getHead() instanceof String) // MatchNode.VALUE.SYMBOL
         {
            buf.append('"');
            appendMatchLiteral(buf, ((Pair)expression).getHead().toString()); // sanitize
            buf.append('"');
         }
         else if (Symbol.MUL.equals(((Pair)expression).getHead())) // MatchNode.WEIGHT.SYMBOL
         {
            buf.append("(");

            for (Pair next = ((Pair)expression).getNext(); next != null; next = next.getNext())
            {
               double dWeight = ((Number)((Pair)next.getHead()).getHead()).doubleValue();
               int nWeight = (int)Math.round(dWeight * 10); // set "arbitrarily" range -10 <= x <= 10

               for (int i = 0, nCount = Math.abs(nWeight); i < nCount; ++i)
               {
                  buf.append((nWeight > 0) ? '>' : '<');
               }

               appendMatchExpression(buf, ((Pair)next.getHead()).getNext(), bMax);
               buf.append(' ');
            }

            buf.append(')');
         }
         else
         {
            throw new IllegalStateException(); // for the case if a new operator gets added
         }
      }
      else
      {
         throw new IllegalStateException(); // for the case if a new operator gets added
      }
   }

   /**
    * Append a literal SQL sanitized string to the buffer that can be used as a Full-Text literal.
    * MySQL does not provide any escape characters for Full-Text literals.
    * MySQL does not index punctuation therefore punctuation is equivalent to whitespace.
    * @param buf The buffer to append to.
    * @param sValue The value to sanitize and append.
    */
   protected void appendMatchLiteral(StringBuffer buf, String sValue)
   {
      for (int i = 0, nLength = sValue.length(); i < nLength; ++i)
      {
         char ch = sValue.charAt(i);

         switch (ch)
         {
            case '"':
               buf.append(' '); // replace with a space (in MySQL 5.0.3+ whitespace is ignored)

               break;

            case '\'': // also need escaped for outer quotes
               buf.append('\'');
            default: // fall through
               buf.append(ch);
         }
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendMatchStatement(java.lang.StringBuffer, java.lang.String, nexj.core.meta.persistence.sql.Column, nexj.core.persistence.sql.SQLJoin, nexj.core.scripting.Pair)
    */
   public void appendMatchStatement(StringBuffer buf, String sAlias, Column column, SQLJoin join,
                                    Pair expression)
   {
      buf.append("nullif(match(").append(join.alias).append('.');
      appendColumn(buf, column);
      buf.append(") against ('"); // range 0.0 <= x
      appendMatchExpression(buf, expression, false);
      buf.append("' in boolean mode)");
      buf.append("/match(").append(join.alias).append('.');
      appendColumn(buf, column);
      buf.append(") against ('"); // range 0.0 <= x
      appendMatchExpression(buf, expression, true);
      buf.append("' in boolean mode), 0)"); // normalize value to 0 < x <= 1.0 by dividing by max
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendNoRowsBlock(java.lang.StringBuffer)
    */
   public boolean appendNoRowsBlock(StringBuffer buf)
   {
      return false; // I know not how to wrap an update statement in an if block as MySQL only supports if blocks inside stored procedures
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendNoRowsEnd(java.lang.StringBuffer)
    */
   public void appendNoRowsEnd(StringBuffer buf)
   {
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendNoRowsStart(java.lang.StringBuffer)
    */
   public void appendNoRowsStart(StringBuffer buf)
   {
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendPrefixHint(java.lang.StringBuffer, nexj.core.persistence.Query)
    */
   public void appendPrefixHint(StringBuffer buf, Query query)
   {
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendSuffixHint(java.lang.StringBuffer, nexj.core.persistence.Query)
    */
   public void appendSuffixHint(StringBuffer buf, Query query)
   {
      if (query.getMaxCount() >= 0 && (!query.isCursor() || !query.isPlural() || query.isLimited()))
      {
         query.setLimited(true);

         buf.append(" limit ");
         buf.append(roundUpMaxCount(query.getOffset() + query.getMaxCount() + ((query.isPlural()) ? 1 : 0)));
      }

      // Note: The following will lock all tables of the query.
      if (query.isLocking())
      {
         buf.append(" for update");
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendTableHint(java.lang.StringBuffer, nexj.core.persistence.sql.SQLJoin, nexj.core.persistence.Query)
    */
   public void appendTableHint(StringBuffer buf, SQLJoin join, Query query)
   {
      // If the join is on the primary key of the destination then force use of the primary key
      // (in some instances [e.g. user list scroll] MySQL doesn't use primary key (or any key)
      // for NJEntityType join [most likely because the table contains only 6 rows],
      // however this increases query execution time to >5s)
      // It is OK to force a primary key join even if other columns of table are used in where
      // clause because there will be at most one row to check.
      // See http://dev.mysql.com/doc/refman/5.0/en/join.html for "index_hint"
      // if (join.destinationKey != null && join.destinationKey.isObjectKey())
      // {
      //    // preferred workaround is to add more records to NJEntityType table
      //    buf.append(" force index (PRIMARY) "); 
      // }
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendTypeConversion(java.lang.StringBuffer, java.lang.Object, nexj.core.meta.Primitive, nexj.core.meta.Primitive, nexj.core.persistence.sql.SQLGenerator)
    */
   public void appendTypeConversion(StringBuffer buf, Object op, Primitive fromType, Primitive type, SQLGenerator gen)
   {
      String sSuffix = "";
      if (type != fromType)
      {
         String sPrefix = null;
   
         switch (type.getOrdinal())
         {
            case Primitive.BINARY_ORDINAL:
   
               switch(fromType.getOrdinal())
               {
                  case Primitive.STRING_ORDINAL: // from hex
                     sPrefix = "unhex(";
                     sSuffix = ")";

                     break;
               }
   
               break;
   
            case Primitive.BOOLEAN_ORDINAL:
               switch(fromType.getOrdinal())
               {
                  case Primitive.DECIMAL_ORDINAL: // fall through
                  case Primitive.DOUBLE_ORDINAL:  // fall through
                  case Primitive.FLOAT_ORDINAL:   // fall through
                  case Primitive.INTEGER_ORDINAL: // fall through
                  case Primitive.LONG_ORDINAL:    // !0
                     sPrefix = "((";
                     sSuffix = ") != 0)";
   
                     break;
   
                  case Primitive.STRING_ORDINAL:  // false == 0 , true == anything else
                     sPrefix = "((";
                     sSuffix = ") != '0')";

                     break;
               }
   
               break;
   
            case Primitive.DECIMAL_ORDINAL:
            case Primitive.DOUBLE_ORDINAL:
            case Primitive.FLOAT_ORDINAL:
            case Primitive.INTEGER_ORDINAL:
            case Primitive.LONG_ORDINAL:
               switch(fromType.getOrdinal())
               {
                  case Primitive.BOOLEAN_ORDINAL:   // 0 or 1
                  case Primitive.DECIMAL_ORDINAL:   // fall through
                  case Primitive.DOUBLE_ORDINAL:    // fall through
                  case Primitive.FLOAT_ORDINAL:     // doubleValue
                  case Primitive.INTEGER_ORDINAL:   // fall through
                  case Primitive.LONG_ORDINAL:      // longValue
                  case Primitive.STRING_ORDINAL:    // bigDecimal
                     sPrefix = ""; // MySQL auto-converts better then with convert(), even for strings
   
                     break;
   
                  case Primitive.TIMESTAMP_ORDINAL: // milliseconds.remainder
                     sPrefix = "(timestampdiff(frac_second, timestamp('1970-01-01'), "; // unix_timestamp() no usec
                     sSuffix = ") / 1000)"; // 1000 <= millisec -> usec

                     break;
               }
   
               break;
   
            case Primitive.STRING_ORDINAL:
               switch(fromType.getOrdinal())
               {
                  case Primitive.BINARY_ORDINAL:    // hex
                     sPrefix = "hex(";
                     sSuffix = ")";
   
                     break;
   
                  case Primitive.BOOLEAN_ORDINAL:   // fall through
                  case Primitive.DECIMAL_ORDINAL:   // fall through
                  case Primitive.DOUBLE_ORDINAL:    // fall through
                  case Primitive.FLOAT_ORDINAL:     // fall through
                  case Primitive.INTEGER_ORDINAL:   // fall through
                  case Primitive.LONG_ORDINAL:      // toString
                     sPrefix = ""; // MySQL auto-converts better then with convert()
   
                     break;
   
                  case Primitive.TIMESTAMP_ORDINAL: // "Gyyyy-MM-dd HH:mm:ss" UTC (no G same as Oracle)
                     sPrefix = "date_format("; // see Primitive.s_timestampInFormat
                     sSuffix = ", '%Y-%m-%d %H:%i:%s.%f')"; // MySQL cannot do negative years

                     break;
               }
   
               break;
   
            case Primitive.TIMESTAMP_ORDINAL:
               switch(fromType.getOrdinal())
               {
                  case Primitive.DECIMAL_ORDINAL:   // fall through
                  case Primitive.DOUBLE_ORDINAL:    // fall through
                  case Primitive.FLOAT_ORDINAL:     // from milliseconds.remainder
                  case Primitive.INTEGER_ORDINAL:   // fall through
                  case Primitive.LONG_ORDINAL:      // from milliseconds
                     sPrefix = "timestampadd(frac_second, ("; // from_unixtime() does not consider usec
                     sSuffix = ") * 1000, timestamp('1970-01-01'))"; // 1000 <= usec -> milisec
   
                     break;
   
                  case Primitive.STRING_ORDINAL:    // from "Gyyyy-MM-dd HH:mm:ss" (no G same as Oracle)
                     sPrefix = "str_to_date("; // see StringUtil.s_timestampOutFormat
                     sSuffix = ", '%Y-%m-%d %H:%i:%s.%f')"; // MySQL TS year >= 1000

                     break;
               }
   
               break;
         }
         
         if (sPrefix == null)
         {
            throw new IllegalArgumentException("Invalid type in SQLAdapter.appendTypeConversion()");
         }
   
         buf.append(sPrefix);
      }

      gen.appendOperand(buf, op);
      buf.append(sSuffix);
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#bindIdentity(java.sql.PreparedStatement, nexj.core.persistence.sql.SQLInsert)
    */
   public void bindIdentity(PreparedStatement stmt, SQLInsert work) throws SQLException
   {
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#close(java.sql.Connection)
    */
   public void close(Connection connection)
   {
      try
      {
         super.close(connection);
      }
      catch (IllegalStateException e)
      {
         s_logger.debug("Unexpected error while closing connection, probably caused by closing " +
            "a dedicated streaming result set connection without an associated transaction", e);
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#createSchemaManager()
    */
   public SQLSchemaManager createSchemaManager()
   {
      return new MySQLSchemaManager(this);
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#getDuplicateKeyName(java.sql.SQLException)
    */
   protected String getDuplicateKeyName(SQLException e)
   {
      if (!isDuplicateKeyException(e))
      {
         return null;
      }

      Matcher matcher = DUPLICATE_KEY_NAME_PATTERN.matcher(e.getMessage());

      // second group of the regex, the first being the value
      // can't look-up actual name of index because don't know table name
      // and MySQL indexes are unique per table but the exception is in the form of:
      // "Duplicate entry '' for key 1"
      return (matcher.find()) ? matcher.group(2) : null;
   }

   /**
    * should use stmt.getGeneratedKeys() but potential for multiple RPC calls, so don't use it to keep consistent with other Adapters
    *
    * @see nexj.core.persistence.sql.SQLAdapter#getIdentityValue(java.sql.PreparedStatement, nexj.core.meta.persistence.sql.Column, nexj.core.persistence.sql.SQLInsert)
    */
   public Object getIdentityValue(PreparedStatement stmt, Column column, SQLInsert work) throws SQLException
   {
      stmt.getMoreResults();

      ResultSet rs = stmt.getResultSet();
      rs.next();
      
      Object value = getBind(column).getValue(rs, 0, this);

      stmt.getMoreResults();
      
      return value;
   }

   /**
    * Gets SQL statement to execute when a connection is first established.
    * @return The initial SQL string; null to issue no initial statement.
    */
   public String getInitialSQL()
   {
      StringBuffer buf = new StringBuffer();

      buf.append("set sql_mode = concat(@@sql_mode, ',ANSI_QUOTES')"); // allow using doublequote when quoting column names in "CREATE TABLE" statements
      buf.append(";set optimizer_search_depth = 0"); // let DB automatically decide on how long it takes to examine plans, improves long planning sessions
      buf.append(";set max_sort_length = ").append(Math.max(MAX_VARCHAR_PRECISION, MAX_VARBINARY_PRECISION)); // set TEXT/BLOB minimum sorting length to be same as cutoff between varchar/text
      return buf.toString();
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#getMatchJoin(nexj.core.meta.persistence.sql.Column, nexj.core.scripting.Pair)
    */
   public Table getMatchJoin(Column column, Pair expression)
   {
      Table table = new Table(null);
      Index primary = new Index(table);
      Table srcTable = column.getTable();
      Index srcPK = srcTable.getPrimaryKey(); // it better exist or there's no way to join

      table.setType(Table.EXTERNAL); // has to be set EXTERNAL or setQuotedName() fails
      table.setAlias(getTableName(srcTable, null, "$", true));
      table.setPrimaryKey(primary);

      // reuse PrimaryKey columns since they are not modified
      for (int i = 0, nCount = srcPK.getIndexColumnCount(); i < nCount; ++i)
      {
         primary.addIndexColumn(new IndexColumn(srcPK.getIndexColumn(i).getColumn(), true));
      }

      return table;
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#getMaxTime()
    */
   protected long getMaxTime()
   {
      return 253402300799999L;
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#getMinTime()
    */
   protected long getMinTime()
   {
      return -30609792000000L;
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#getTestSQL()
    */
   public String getTestSQL()
   {
      return "select 1";
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#indexNameMatches(nexj.core.meta.persistence.sql.Index, java.lang.String)
    */
   protected boolean indexNameMatches(Index index, String sPhysicalName)
   {
      int indexOrdinal;

      if (indexNameMatches(index.getName(), 0, sPhysicalName, null, true)) // try name match
      {
         return true;
      }
      
      try // try converting string to a numeric value
      {
         indexOrdinal = Integer.parseInt(sPhysicalName);
      }
      catch (NumberFormatException e)
      {
         return false; // not something that can be used for lookup
      }
      
      sPhysicalName = getIndexName(index.getTable(), indexOrdinal);

      return (sPhysicalName == null) ? false : indexNameMatches(index.getName(), 0, sPhysicalName, null, true);
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#isBatchSupported()
    */
   public boolean isBatchSupported()
   {
      return true;
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#isBatchUpdateCountSupported()
    */
   public boolean isBatchUpdateCountSupported()
   {
      return true;
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#isBatchable(nexj.core.persistence.sql.SQLWork)
    */
   public boolean isBatchable(SQLWork work)
   {
      return true;
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#isDateRangeException(java.sql.SQLException)
    */
   protected boolean isDateRangeException(SQLException e)
   {
      if (e instanceof DataTruncation && "01004".equals(e.getSQLState()))
      {
         String sMsg = e.getMessage();

         return sMsg != null && sMsg.indexOf(" datetime ") > 0;
      }

      return false;
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#isDuplicateKeyException(java.sql.SQLException)
    */
   protected boolean isDuplicateKeyException(SQLException e)
   {
      return e.getErrorCode() == 1062; // MySQL Duplicate entry Error
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#isDeadlockException(java.sql.SQLException)
    */
   protected boolean isDeadlockException(SQLException e)
   {
      return e.getErrorCode() == 1213;
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#isLockTimeoutException(java.sql.SQLException)
    */
   protected boolean isLockTimeoutException(SQLException e)
   {
      return e.getErrorCode() == 1205; // MySQL Lock wait timeout exceeded
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#isSharedConnection(nexj.core.persistence.Query)
    */
   protected boolean isSharedConnection(Query query)
   {
      // MySQL cannot have two simultaneous ResultSets on the same connection, hence cursor queries
      // should never share their connections with other queries
      return query == null || !query.isCursor();
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendStringLengthPrefix(java.lang.StringBuffer, nexj.core.persistence.operator.FunctionOperator)
    */
   public String appendStringLengthPrefix(StringBuffer buf, FunctionOperator op)
   {
      buf.append("char_length(");

      return ")";
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendSubstringPrefix(java.lang.StringBuffer, nexj.core.persistence.operator.FunctionOperator)
    */
   public String appendSubstringPrefix(StringBuffer buf, FunctionOperator op)
   {
      buf.append("substr(");

      return ")";
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#isLiteral(nexj.core.meta.Primitive, java.lang.Object)
    */
   public boolean isLiteral(Primitive type, Object value)
   {
      return true;
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#isQueryTimeoutException(java.sql.SQLException)
    */
   protected boolean isQueryTimeoutException(SQLException e)
   {
      return e.getErrorCode() == 1317 || // Query execution was interrupted
             e.getErrorCode() == 0 && // some query timeouts do not have an error code
              "Statement cancelled due to timeout or client request".equals(e.getMessage());
   }

   /**
    * do not use MySQL streaming cursors, they block the connection for other statements/resultsets in the transaction
    * instead always use a fixed fetch size (the data will be materialized in a temporary table on the server)
    * @see nexj.core.persistence.sql.SQLAdapter#setFetchSize(java.sql.PreparedStatement, nexj.core.persistence.Query)
    */
   public void setFetchSize(PreparedStatement stmt, Query query) throws SQLException
   {
      stmt.setFetchSize(DEFAULT_FETCH_SIZE);
   }

   /**
    * Try to retrieve name of index that maps to a given id
    * @param table The table name of index
    * @param nId The ordinal id of index
    * @return Name of index or null if not found
    */
   protected String getIndexName(Table table, int nId)
   {
      Lookup indexMap;

      synchronized(s_indexSchemaNameMap)
      {
         // do a lookup to map from a number to a name (e.g. exceptions return an index number in schema instead of name)
         indexMap = (Lookup)s_indexSchemaNameMap.get(table);  
      }

      Integer indexOrdinal = Primitive.createInteger(nId);
      String sIndexName = null; // reset for use in lookup stage

      if (indexMap != null)
      {
         sIndexName = (String)indexMap.get(indexOrdinal);

         if (sIndexName != null || indexMap.contains(indexOrdinal))
         {
            return sIndexName;
         }
      }

      // do a refresh from DB (this might cause more hits to DB then necessary
      // if indexOrdinal is actually invalid, but would keep the cache up to date if
      // indexOrdinal is valid and DB schema changed)
      SQLConnection con = null;
      ResultSet rs = null;

      try
      {
         con = getConnection();
         rs = con.getConnection().getMetaData().getIndexInfo(table.getOwnerName(), null, table.getTableName(), false, true);
         indexMap = new HashTab();

         for (int i = 0; rs.next();)
         {
            if (rs.getShort("ORDINAL_POSITION") == 1) // this is first column of index (every index has first column)
            {
               sIndexName = rs.getString("INDEX_NAME");

               if ("PRIMARY".equals(sIndexName)) // MySQL doesn't store index name for Primary Key, it's just called "PRIMARY"
               {
                  sIndexName = table.getPrimaryKey().getName();
               }

               indexMap.put(Primitive.createInteger(++i),
                            table.getTableName() + '.' + sIndexName);
            }
         }

         sIndexName = (String)indexMap.get(indexOrdinal); // find the correct name in cache

         if (sIndexName == null)
         {
            indexMap.put(indexOrdinal, null);
         }
      }
      catch (SQLException e)
      {
         return null; // no conclusive answer
      }
      finally
      {
         if (rs != null)
         {
            try
            {
               rs.close();
            }
            catch (SQLException e)
            {
            }
         }

         if (con != null)
         {
            con.decRef();
         }
      }

      synchronized(s_indexSchemaNameMap)
      {
         s_indexSchemaNameMap.put(table, indexMap); // add new version of HashMap for this table
      }

      return sIndexName;
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#isUnicode(nexj.core.meta.persistence.sql.RelationalSchema, ResultSet, int)
    */
   protected Boolean isUnicode(RelationalSchema schema, ResultSet rs, int nColumn)
      throws SQLException
   {
      ResultSetMetaData rsMeta = rs.getMetaData();
      RelationalDatabase ds = (RelationalDatabase)schema.getDataSource();
      StringBuffer buf = new StringBuffer(128);

      // the table_schema column actually contains the database name
      // rsMeta.getSchemaName(nColumn) returns empty string
      buf.append("select character_set_name from information_schema.columns where table_schema=");
      appendLiteral(buf, ((RelationalDatabaseFragment)ds.getDefaultFragment()).getDatabase());
      buf.append(" and table_name=");
      appendLiteral(buf, rsMeta.getTableName(nColumn));
      buf.append(" and column_name=");
      appendLiteral(buf, rsMeta.getColumnName(nColumn));

      String sSQL = buf.toString();
      PreparedStatement stmt = null;
      ResultSet rsColumn = null;
      Boolean unicode = null;

      log(sSQL);

      try
      {
         stmt = rs.getStatement().getConnection().prepareStatement(sSQL);
         rsColumn = executeQuery(stmt);

         if (rsColumn.next())
         {
            String sCharSet = rsColumn.getString(1); // the "character_set_name" column

            if ("utf8".equalsIgnoreCase(sCharSet))
            {
               unicode = Boolean.TRUE;
            }
            else if (sCharSet != null)
            {
               unicode = Boolean.FALSE;
            }
         }
      }
      finally
      {
         try
         {
            if (rsColumn != null)
            {
               rsColumn.close();
            }
         }
         finally
         {
            close(stmt);
         }
      }

      return unicode;
   }
}