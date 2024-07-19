// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.sql.Types;
import java.util.Set;
import java.util.TimerTask;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import nexj.core.meta.Primitive;
import nexj.core.meta.persistence.sql.Column;
import nexj.core.meta.persistence.sql.Index;
import nexj.core.meta.persistence.sql.RelationalDatabase;
import nexj.core.meta.persistence.sql.RelationalDatabaseFragment;
import nexj.core.meta.persistence.sql.RelationalSchema;
import nexj.core.persistence.Operator;
import nexj.core.persistence.Query;
import nexj.core.persistence.operator.AggregateOperator;
import nexj.core.persistence.operator.FunctionOperator;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.Binary;
import nexj.core.util.IOUtil;
import nexj.core.util.KeywordTab;
import nexj.core.util.StringUtil;
import nexj.core.util.sql.ConnectionWrapper;
import nexj.core.util.sql.StatementCancelationTask;

/**
 * Persistence adapter for PostgreSQL 8.4+.
 * NOTE:
 * - In postgresql.conf set max_prepared_transactions to >= max_connections.
 * - PostgreSQL only throws a LockTimeoutException for select ... for update nowait.
 */
public class PostgreSQLAdapter extends CaseInsensitiveSQLAdapter
{
   // constants

   /**
    * SQL keywords that have to be quoted.
    */
   protected final static KeywordTab KEYWORDS = new KeywordTab(new String[]
   {
      "all", "analyse", "analyze", "and", "any", "array", "as", "asc", "asymmetric",
      "authorization", "binary", "both", "case", "cast", "check", "collate",
      "column", "concurrently", "constraint", "create", "cross", "current_catalog",
      "current_date", "current_role", "current_schema", "current_time",
      "current_timestamp", "current_user", "default", "deferrable", "desc",
      "distinct", "do", "else", "end", "except", "false", "fetch", "for", "foreign",
      "freeze", "from", "full", "grant", "group", "having", "ilike", "in",
      "initially", "inner", "intersect", "into", "is", "isnull", "join", "leading",
      "left", "like", "limit", "localtime", "localtimestamp", "natural", "not",
      "notnull", "null", "offset", "on", "only", "or", "order", "outer", "over",
      "overlaps", "placing", "primary", "references", "returning", "right", "select",
      "session_user", "similar", "some", "symmetric", "table", "then", "to",
      "trailing", "true", "union", "unique", "user", "using", "variadic", "verbose",
      "when", "where", "window", "with",
   });

   /**
    * Maximum character column precision.
    */
   protected final static int MAX_VARCHAR_PRECISION = 10485760; // will throw error for length greater than this.

   /**
    * Maximum bytea column size.
    */
   protected final static int MAX_BYTEA_PRECISION = 1073741824; // 1GB http://www.postgresql.org/about/

   /**
    * Pattern used to extract the constraint name from a duplicate key error message
    * Depending on locale, messages will have different quotation marks i.e. "", >><<, <<>>, <<  >>
    */
   protected final static Pattern DUPLICATE_KEY_NAME_PATTERN = Pattern.compile("^.* (\\\"|>>|<<|<< )(.*?)(\\\"|<<|>>| >>)");

   // attributes

   /**
    * True if the DB product is a restricted version for a cloud.
    */
   protected boolean m_bClouded;

   // associations

   /**
    * The schema manager.
    */
   protected PostgreSQLSchemaManager m_schemaManager;

   // variables used for PGXAConnection reflections
   protected static Class s_PGXAConnection;
   protected static java.lang.reflect.Field s_AbstractJdbc23PooledConnection$ConnectionHandler_Con;
   protected static java.lang.reflect.Field s_PGXAConnection$ConnectionHandler_Con;
   protected static java.lang.reflect.Field s_READ;
   protected static Method s_close;
   protected static Method s_getBaseColumnName;
   protected static Method s_getBaseSchemaName;
   protected static Method s_getBaseTableName;
   protected static Method s_getInputStream;
   protected static Method s_getLargeObjectAPI;
   protected static Method s_getQueryExecutor;
   protected static Method s_open;

   static
   {
      try
      {
         s_PGXAConnection = Class.forName("org.postgresql.xa.PGXAConnection");

         s_PGXAConnection$ConnectionHandler_Con = Class.forName("org.postgresql.xa.PGXAConnection$ConnectionHandler").getDeclaredField("con");
         s_PGXAConnection$ConnectionHandler_Con.setAccessible(true);

         s_AbstractJdbc23PooledConnection$ConnectionHandler_Con = Class.forName("org.postgresql.ds.jdbc23.AbstractJdbc23PooledConnection$ConnectionHandler").getDeclaredField("con");
         s_AbstractJdbc23PooledConnection$ConnectionHandler_Con.setAccessible(true);

         s_getQueryExecutor = Class.forName("org.postgresql.core.BaseConnection").getMethod("getQueryExecutor", null);

         s_getLargeObjectAPI =  Class.forName("org.postgresql.PGConnection").getMethod("getLargeObjectAPI", null);

         Class clazz = Class.forName("org.postgresql.largeobject.LargeObjectManager");

         s_READ = clazz.getField("READ");
         s_open = clazz.getMethod("open", new Class[]{long.class, int.class});

         clazz = Class.forName("org.postgresql.largeobject.LargeObject");

         s_getInputStream = clazz.getMethod("getInputStream", null);
         s_close = clazz.getMethod("close", null);

         clazz = Class.forName("org.postgresql.PGResultSetMetaData");

         s_getBaseColumnName = clazz.getMethod("getBaseColumnName", new Class[]{int.class});
         s_getBaseTableName = clazz.getMethod("getBaseTableName", new Class[]{int.class});
         s_getBaseSchemaName = clazz.getMethod("getBaseSchemaName", new Class[]{int.class});
      }
      catch (Throwable t)
      {
         s_PGXAConnection = null;
      }
   }

   /**
    * Bind for BLOB column.
    */
   protected final static Bind BIND_BLOB = new Bind()
   {
      public void setValue(PreparedStatement stmt, int nOrdinal, Object value, SQLAdapter adapter) throws SQLException
      {
         ((((PostgreSQLAdapter)adapter).isClouded()) ? SQLAdapter.BIND_BINARY : SQLAdapter.BIND_BLOB)
            .setValue(stmt, nOrdinal, value, adapter);
      }

      public Object getValue(ResultSet rs, int nOrdinal, SQLAdapter adapter) throws SQLException
      {
         if (((PostgreSQLAdapter)adapter).isClouded())
         {
            return SQLAdapter.BIND_BINARY.getValue(rs, nOrdinal, adapter);
         }

         Connection connection = unwrap(adapter.getConnection().m_connection);

         if (s_PGXAConnection == null || !s_getLargeObjectAPI.getDeclaringClass().isAssignableFrom(connection.getClass()))
         {
            return SQLAdapter.BIND_BLOB.getValue(rs, nOrdinal, adapter);
         }

         long nLO = rs.getLong(nOrdinal + 1);

         if (nLO == 0) // null loid
         {
            return null;
         }

         boolean bAutoCommit = connection.getAutoCommit();
         Object lo = null;

         try
         {
            if (bAutoCommit)
            {
               connection.setAutoCommit(false);
            }

            Object loManager = s_getLargeObjectAPI.invoke(connection, null);
            lo = s_open.invoke(loManager, new Object[]{new Long(rs.getLong(nOrdinal+1)), Primitive.createInteger(s_READ.getInt(loManager))});

            ByteArrayOutputStream out = new ByteArrayOutputStream();

            IOUtil.copy(out, (InputStream)s_getInputStream.invoke(lo, null));

            return new Binary(out.toByteArray());
         }
         catch (Exception e)
         {
            if (e instanceof InvocationTargetException &&
               ((InvocationTargetException)e).getTargetException() instanceof SQLException)
            {
               return (SQLException)((InvocationTargetException)e).getTargetException();
            }

            SQLException x = new SQLException("Error getting BLOB: " + nLO);

            x.initCause(e);

            throw x;
         }
         finally
         {
            // close the largeobject
            if (lo != null)
            {
               try
               {
                  s_close.invoke(lo, null);
               }
               catch (Exception e)
               {}
            }

            if (bAutoCommit)
            {
               connection.setAutoCommit(true);
            }
         }
      }
   };

   /**
    * Bind factory array: BindFactory[nPrimitiveOrdinal].
    */
   protected final static BindFactory[] s_bindFactoryArray = new BindFactory[Primitive.MAX_COUNT];

   static
   {
      System.arraycopy(SQLAdapter.s_bindFactoryArray, 0, s_bindFactoryArray, 0, s_bindFactoryArray.length);

      s_bindFactoryArray[Primitive.STRING_ORDINAL] = new BindFactory()
      {
         public Bind create(Column column)
         {
            return (column.isLOB(MAX_VARCHAR_PRECISION, 0) || column.getAllocation() != Column.FIXED) ? BIND_STRING : BIND_CHAR;
         }

         public Bind create(Primitive type)
         {
            return BIND_STRING;
         }
      };

      s_bindFactoryArray[Primitive.BINARY_ORDINAL] = new BindFactory()
      {
         public Bind create(Column column)
         {
            return column.isLOB(0, MAX_BYTEA_PRECISION) ? BIND_BLOB : BIND_BINARY;
         }

         public Bind create(Primitive type)
         {
            return BIND_BINARY;
         }
      };
   }

   // constructors

   /**
    * Constructor.
    */
   public PostgreSQLAdapter()
   {
      m_schemaManager = new PostgreSQLSchemaManager(this);
   }

   // operations

   /**
    * Obtains the original SQL connection.
    * @param connection The connection wrapper.
    * @return The original SQL connection.
    */
   protected static Connection unwrap(Connection connection) throws SQLException
   {
      Connection con = SQLAdapter.unwrap(connection);

      if (!(con instanceof Proxy))
      {
         return con;
      }

      try
      {
         return (Connection)s_AbstractJdbc23PooledConnection$ConnectionHandler_Con.get(
                  Proxy.getInvocationHandler(
                     s_PGXAConnection$ConnectionHandler_Con.get(
                        Proxy.getInvocationHandler(con))));
      }
      catch (Exception e)
      {
         throw new SQLException("Error unwrapping connection: " + con);
      }
   }

   /**
    * @return The keyword table.
    */
   protected Set getKeywords()
   {
      return KEYWORDS;
   }

   /**
    * @return The clouded DB product flag.
    */
   public boolean isClouded()
   {
      return m_bClouded;
   }

   /**
    * Sets the database product version.
    * @param sVersion The product version.
    */
   public void setVersion(String sVersion)
   {
      m_bClouded = (sVersion != null && sVersion.endsWith("DB"));
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendQuoted(java.lang.StringBuffer, java.lang.String)
    */
   public void appendQuoted(StringBuffer buf, String sName)
   {
      buf.append('"');
      StringUtil.appendLowerCase(buf, sName);
      buf.append('"');
   }

   /**
    * @see nexj.core.persistence.sql.CaseInsensitiveSQLAdapter#appendCaseConversion(java.lang.StringBuffer, int)
    */
   protected String appendCaseConversion(StringBuffer buf, int nCI)
   {
      switch (nCI)
      {
         case CI_ATTR:
            return "$";

         case CI_EXPR:
            buf.append("upper(");

            return ")";
      }

      return null;
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendCaseConvertedBind(java.lang.StringBuffer, int, nexj.core.meta.persistence.sql.Column)
    */
   public void appendCaseConvertedBind(StringBuffer buf, int nOrdinal, Column column)
   {
      buf.append("upper(?)");
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendCaseConvertedLiteral(java.lang.StringBuffer, java.lang.String)
    */
   public void appendCaseConvertedLiteral(StringBuffer buf, String sLiteral)
   {
      buf.append("upper(");
      appendLiteral(buf, sLiteral);
      buf.append(")");
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendLikeEscape(java.lang.StringBuffer)
    */
   public void appendLikeEscape(StringBuffer buf)
   {
      if (m_bClouded)
      {
         buf.append(" escape '\\\\'");
      }
      else
      {
         super.appendLikeEscape(buf);
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendIdentityColumn(java.lang.StringBuffer, nexj.core.persistence.sql.SQLInsert)
    */
   public boolean appendIdentityColumn(StringBuffer buf, SQLInsert work)
   {
      return false;
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendIdentityPrefix(java.lang.StringBuffer,nexj.core.persistence.sql.SQLInsert)
    */
   public void appendIdentityPrefix(StringBuffer buf, SQLInsert work)
   {
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendIdentitySuffix(java.lang.StringBuffer, nexj.core.persistence.sql.SQLInsert)
    */
   public boolean appendIdentitySuffix(StringBuffer buf, SQLInsert work)
   {
      buf.append("; select lastval()");
      return true;
   }

   /**
    * PostreSQL has SERIAL and BIGSERIAL data types for this purpose, it is added automatically
    * @see nexj.core.persistence.sql.SQLAdapter#appendIdentityValue(java.lang.StringBuffer, nexj.core.persistence.sql.SQLInsert)
    */
   public boolean appendIdentityValue(StringBuffer buf, SQLInsert work)
   {
      return false;
   }

   /**
    * PostgreSQL doens't have hints
    * @see nexj.core.persistence.sql.SQLAdapter#appendInfixHint(java.lang.StringBuffer, nexj.core.persistence.Query)
    */
   public void appendInfixHint(StringBuffer buf, Query query)
   {
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendLiteral(java.lang.StringBuffer, nexj.core.meta.Primitive,java.lang.Object)
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
            appendLiteral(buf, value.toString());

            break;

         case Primitive.BINARY_ORDINAL:
            buf.append("decode('");
            Binary.append(buf, ((Binary)value).getData(), -1);
            buf.append("','hex')");

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
            buf.append("timestamptz'");
            StringUtil.appendUTC(buf, (Timestamp)value, false);
            buf.append("+00\'");

            break;

         case Primitive.BOOLEAN_ORDINAL:
            buf.append(((Boolean)value).booleanValue() ? "TRUE" : "FALSE");

            break;

         default:
            throw new IllegalArgumentException("Invalid literal type");
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendLiteral(java.lang.StringBuffer, java.lang.String)
    */
   protected void appendLiteral(StringBuffer buf, String sValue)
   {
      // Prefix recommended for future compliance http://www.postgresql.org/docs/9.0/interactive/sql-syntax-lexical.html
      buf.append("E'");
      appendEscaped(buf, sValue);
      buf.append('\'');
   }

   /**
    * Appends a given character sequence to a buffer while escaping the characters.
    * @param buf The destination buffer.
    * @param value The character sequence to append.
    */
   protected void appendEscaped(StringBuffer buf, CharSequence value)
   {
      for (int i = 0, nCount = value.length(); i < nCount; ++i)
      {
         char ch = value.charAt(i);

         switch (ch)
         {
            case '\\':
            case '\'':
               buf.append(ch);
               // fall through

            default:
               buf.append(ch);
         }
      }
   }

   /**
    * Append PostgreSQL specific match expression syntax for a given column.
    * The symbols used in this function have to map 1-to-1 to MatchNode.SYMBOL
    *
    * @param buf The buffer to append to.
    * @param expression The full-text search expression to search for.
    * @param sAlias The alias of the table containing the column.
    */
   protected void appendMatchExpression(StringBuffer buf, Object expression, String sAlias)
   {
      if (expression instanceof String)
      {
         appendEscaped(buf, expression.toString()); // sanitize
      }
      else if (expression instanceof Pair)
      {
         if (((Pair)expression).getHead() instanceof String)
         {
            appendEscaped(buf, ((Pair)expression).getHead().toString()); // sanitize
         }
         else if (Symbol.AND.equals(((Pair)expression).getHead())) // MatchNode.AND.SYMBOL
         {
            buf.append('(');

            for (Pair next = ((Pair)expression).getNext(); next != null;)
            {
               appendMatchExpression(buf, next.getHead(), sAlias);

               if ((next = next.getNext()) != null)
               {
                  buf.append(" & ");
               }
            }

            buf.append(')');
         }
         else if (Symbol.LIKE_P.equals(((Pair)expression).getHead())) // MatchNode.FUZZY.SYMBOL
         {
            appendEscaped(buf, ((Pair)expression).getNext().getHead().toString()); // sanitize
            buf.append(":*"); // prefix matching
         }
         else if (Symbol.OR.equals(((Pair)expression).getHead())) // MatchNode.OR.SYMBOL
         {
            buf.append('(');

            for (Pair next = ((Pair)expression).getNext(); next != null;)
            {
               appendMatchExpression(buf, next.getHead(), sAlias);

               if ((next = next.getNext()) != null)
               {
                  buf.append(" | ");
               }
            }

            buf.append(')');
         }
         else if (Symbol.NOT.equals(((Pair)expression).getHead())) // MatchNode.NOT.SYMBOL
         {
            for (Pair next = ((Pair)expression).getNext(); next != null; next = next.getNext())
            {
               buf.append("!(");
               appendMatchExpression(buf, next.getHead(), sAlias);
               buf.append(')');
            }
         }
         else if (Symbol.MUL.equals(((Pair)expression).getHead())) // MatchNode.WEIGHT.SYMBOL
         {
            // PostgreSQL doesn't support adding weights to ts_query tokens
            // Ignore weights and turn expression to OR

            Pair querys = (Pair)((Pair)expression).getNext(); // (* (1.0 foo)(1.1 bar)) => ((1.0 foo)(1.1 bar))

            buf.append('(');
            appendMatchExpression(buf, ((Pair)querys.getHead()).getTail(), sAlias); // (1.0 foo) => (foo)
            buf.append(" | ");
            appendMatchExpression(buf, ((Pair)((Pair)querys.getTail()).getHead()).getTail(), sAlias); // ((1.1 bar)) => (bar)
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
    * @see nexj.core.persistence.sql.SQLAdapter#appendMatchStatement(java.lang.StringBuffer, java.lang.String, nexj.core.meta.persistence.sql.Column, nexj.core.persistence.sql.SQLJoin, nexj.core.scripting.Pair)
    */
   public void appendMatchStatement(StringBuffer buf, String sAlias, Column column, SQLJoin join, Pair expression)
   {
      buf.append("nullif(ts_rank_cd(");
      buf.append(sAlias).append('.');
      appendColumn(buf, column); //use tsvector column
      buf.append("_, to_tsquery('");
      appendMatchExpression(buf, expression, sAlias);
      buf.append("'),32),0)"); // normalized ranking 32 (rank/rank+1) http://www.postgresql.org/docs/9.0/static/textsearch-controls.html#TEXTSEARCH-RANKING
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendNoRowsBlock(java.lang.StringBuffer)
    */
   public boolean appendNoRowsBlock(StringBuffer buf)
   {
      return false;
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
    * PostgreSQL doesn't have hints
    * see http://xzilla.net/blog/2011/Feb/Why-the-F%25-Doesnt-Postgres-Have-Hints!!.html
    * @see nexj.core.persistence.sql.SQLAdapter#appendPrefixHint(java.lang.StringBuffer, nexj.core.persistence.Query)
    */
   public void appendPrefixHint(StringBuffer buf, Query query)
   {
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendSortDirection(java.lang.StringBuffer, nexj.core.persistence.Operator, boolean)
    */
   public void appendSortDirection(StringBuffer buf, Operator op, boolean bAscending)
   {
      if (bAscending)
      {
         buf.append(" nulls first");
      }
      else
      {
         buf.append(" desc nulls last");
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendBooleanPrefix(java.lang.StringBuffer, nexj.core.persistence.Operator)
    */
   public String appendBooleanPrefix(StringBuffer buf, Operator op)
   {
      return null;
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendLogicalPrefix(java.lang.StringBuffer, nexj.core.persistence.Operator)
    */
   public String appendLogicalPrefix(StringBuffer buf, Operator op)
   {
      return null;
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
      buf.append("substring(");

      return ")";
   }

   /**
    * Appends an aggregate function prefix.
    * @param buf The destination buffer.
    * @param sName The function name.
    * @param op The expression type.
    * @return The suffix to append.
    */
   protected String appendAggregatePrefix(StringBuffer buf, String sName, Primitive type)
   {
      if (type == Primitive.BINARY)
      {
         buf.append("decode(");
         buf.append(sName);
         buf.append("(encode(");

         return ",'hex')),'hex')";
      }

      if (type == Primitive.BOOLEAN)
      {
         buf.append("(");
         buf.append(sName);
         buf.append("((");

         return ")::integer) <> 0)";
      }

      buf.append(sName);
      buf.append('(');

      return ")";
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendMinimumPrefix(java.lang.StringBuffer, nexj.core.persistence.operator.AggregateOperator)
    */
   public String appendMinimumPrefix(StringBuffer buf, AggregateOperator op)
   {
      return appendAggregatePrefix(buf, "min", (Primitive)op.getType());
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendMaximumPrefix(java.lang.StringBuffer, nexj.core.persistence.operator.AggregateOperator)
    */
   public String appendMaximumPrefix(StringBuffer buf, AggregateOperator op)
   {
      return appendAggregatePrefix(buf, "max", (Primitive)op.getType());
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#appendUngroupPrefix(java.lang.StringBuffer, Primitive)
    */
   public String appendUngroupPrefix(StringBuffer buf, Primitive type)
   {
      return appendAggregatePrefix(buf, "min", type);
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

      if (query.isLocking())
      {
         buf.append(" for update");

         SQLJoin join = (SQLJoin)query.getMapping();

         if (join.isInner && join.isEnabled)
         {
            // see http://www.postgresql.org/docs/9.0/static/sql-select.html
            // driver throws exception 0A000 - ERROR: SELECT FOR UPDATE/SHARE cannot be applied to the nullable side of an outer join
            // so do not lock table that is on nullable side of outer join
            buf.append(" of ");
            buf.append(StringUtil.isEmpty(join.alias) ? join.table.getTableName() : join.alias);
         }
      }
   }

   /**
    * PostgreSQL doesn't have hints
    * @see nexj.core.persistence.sql.SQLAdapter#appendTableHint(java.lang.StringBuffer, nexj.core.persistence.sql.SQLJoin, nexj.core.persistence.Query)
    */
   public void appendTableHint(StringBuffer buf, SQLJoin join, Query query)
   {
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
                     sPrefix = "decode(";
                     sSuffix = ",'hex')";

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
                     sPrefix = "(";
                     sSuffix = ")::integer";

                     break;

                  case Primitive.DECIMAL_ORDINAL:   // fall through
                  case Primitive.DOUBLE_ORDINAL:    // fall through
                  case Primitive.FLOAT_ORDINAL:     // doubleValue
                  case Primitive.INTEGER_ORDINAL:   // fall through
                  case Primitive.LONG_ORDINAL:      // longValue
                  case Primitive.STRING_ORDINAL:    // bigDecimal
                     sPrefix = "";

                     break;

                  case Primitive.TIMESTAMP_ORDINAL: // Returns up to microseconds
                     sPrefix = "extract(epoch from ";
                     sSuffix = ")";

                     break;
               }

               break;

            case Primitive.STRING_ORDINAL:
               switch(fromType.getOrdinal())
               {
                  case Primitive.BINARY_ORDINAL:    // hex
                     sPrefix = "encode(";
                     sSuffix = ",'hex')";

                     break;

                  case Primitive.BOOLEAN_ORDINAL:   // fall through
                  case Primitive.DECIMAL_ORDINAL:   // fall through
                  case Primitive.DOUBLE_ORDINAL:    // fall through
                  case Primitive.FLOAT_ORDINAL:     // fall through
                  case Primitive.INTEGER_ORDINAL:   // fall through
                  case Primitive.LONG_ORDINAL:      // PostgreSQL will automatically handle a cast to string type by invoking the other type's output function
                     sPrefix = "";

                     break;

                  case Primitive.TIMESTAMP_ORDINAL:
                     sPrefix = "to_char("; // see Primitive.s_timestampInFormat
                     sSuffix = ", '%Y-%m-%d %H:%i:%s.%f')";

                     break;
               }

               break;

            case Primitive.TIMESTAMP_ORDINAL:
               switch(fromType.getOrdinal())
               {
                  case Primitive.DECIMAL_ORDINAL:   // fall through
                  case Primitive.DOUBLE_ORDINAL:    // fall through
                  case Primitive.FLOAT_ORDINAL:     // fall through
                  case Primitive.INTEGER_ORDINAL:   // fall through
                  case Primitive.LONG_ORDINAL:      // up to microseconds
                     sPrefix = "to_timestamp(";
                     sSuffix = ")";

                     break;

                  case Primitive.STRING_ORDINAL:
                     sPrefix = "to_timestamp("; // see StringUtil.s_timestampOutFormat
                     sSuffix = ", '%Y-%m-%d %H:%i:%s.%f')";

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
    * @see nexj.core.persistence.sql.SQLAdapter#createSchemaManager()
    */
   public SQLSchemaManager createSchemaManager()
   {
      return new PostgreSQLSchemaManager(this);
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#createConnection(java.sql.Connection)
    */
   public SQLConnection createConnection(Connection connection)
   {
      return super.createConnection(new PostgreSQLConnectionWrapper(connection, !m_bClouded));
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#getBindFactory(nexj.core.meta.Primitive)
    */
   protected BindFactory getBindFactory(Primitive type)
   {
      return s_bindFactoryArray[type.getOrdinal()];
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

      return matcher.find() ? matcher.group(2) : null;
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#getIdentityValue(java.sql.PreparedStatement, nexj.core.meta.persistence.sql.Column, nexj.core.persistence.sql.SQLInsert)
    */
   public Object getIdentityValue(PreparedStatement stmt, Column column, SQLInsert work) throws SQLException
   {
      ResultSet rs;
      Object value;

      stmt.getMoreResults();
      rs = stmt.getResultSet();
      rs.next();

      value = getBind(column).getValue(rs, 0, this);

      stmt.getMoreResults();

      return value;
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#getMaxDecimalPrecision()
    */
   protected int getMaxDecimalPrecision()
   {
      return 1000; //http://www.postgresql.org/docs/8.4/static/datatype-numeric.html#DATATYPE-NUMERIC-DECIMAL
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#getMaxTime()
    */
   protected long getMaxTime()
   {
      // 294276AD http://www.postgresql.org/docs/8.4/interactive/datatype-datetime.html
      return 9224286393600000L;
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#getMinTime()
    */
   protected long getMinTime()
   {
      // 4713BC http://www.postgresql.org/docs/8.4/interactive/datatype-datetime.html
      return -210866803200000L;
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
      return sPhysicalName.equals(m_schemaManager.getIndexName(
         m_schemaManager.toDatabaseCase(index.getName()),
                null, null, false, false));
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#isBatchable(nexj.core.persistence.sql.SQLWork)
    */
   public boolean isBatchable(SQLWork work)
   {
      return true;
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
    * @see nexj.core.persistence.sql.SQLAdapter#isCompatible(java.sql.Connection, nexj.core.meta.persistence.sql.RelationalDatabase)
    */
   protected boolean isCompatible(Connection con, RelationalDatabase db) throws SQLException
   {
      if (db != null && !getClass().equals(db.getAdapter().getClassObject()))
      {
         return false;
      }

      String sDriver = (db == null) ? null : db.getDriver();

      if (StringUtil.isEmpty(sDriver))
      {
         return true;
      }

      int i = sDriver.indexOf('.');

      if (i < 0)
      {
         return true;
      }

      return sDriver.regionMatches(0, unwrap(con).getClass().getName(), 0, 14 /*"org.postgresql".length()*/);
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#isDateRangeException(java.sql.SQLException)
    */
   protected boolean isDateRangeException(SQLException e)
   {
      return "22008".equals(e.getSQLState());
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#isDeadlockException(java.sql.SQLException)
    */
   protected boolean isDeadlockException(SQLException e)
   {
      return "40P01".equals(e.getSQLState());
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#isDuplicateKeyException(java.sql.SQLException)
    */
   protected boolean isDuplicateKeyException(SQLException e)
   {
      return "23505".equals(e.getSQLState());
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#isLiteral(nexj.core.meta.Primitive, java.lang.Object)
    */
   public boolean isLiteral(Primitive type, Object value)
   {
      switch (type.getOrdinal())
      {
         case Primitive.STRING_ORDINAL:
            return StringUtil.utf8Length((String)value) <=  MAX_VARCHAR_PRECISION;

         case Primitive.BINARY_ORDINAL:
            return ((Binary)value).getSize() <= MAX_VARCHAR_PRECISION;

         default:
            return true;
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#isLockTimeoutException(java.sql.SQLException)
    */
   protected boolean isLockTimeoutException(SQLException e)
   {
      // This is only thrown for SELECT FOR UPDATE NOWAIT
      return "55P03".equals(e.getSQLState()) ||
         m_bClouded && "XX000".equals(e.getSQLState()) &&
            e.getMessage() != null && e.getMessage().endsWith("mS)");
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#isQueryTimeoutException(java.sql.SQLException)
    */
   protected boolean isQueryTimeoutException(SQLException e)
   {
      return "57014".equals(e.getSQLState());
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapter#isUnicode(nexj.core.meta.persistence.sql.RelationalSchema, java.sql.ResultSet, int)
    */
   protected Boolean isUnicode(RelationalSchema schema, ResultSet rs, int nColumn) throws SQLException
   {
      Boolean unicode = null;
      ResultSetMetaData rsMeta = rs.getMetaData();
      int nColumnType = rsMeta.getColumnType(nColumn);

      if (nColumnType == Types.VARCHAR || nColumnType == Types.CHAR) // only check character columns
      {
         if (s_PGXAConnection == null)
         {
            throw new RuntimeException("Unable to load PGResultSetMetaData: Missing PGXAConnection");
         }

         //PostgreSQL JDBC ResultSetMetaData getSchemaName and getTableName do not work, while rsMeta.getColumnName(nColumn) returns column alias
         //http://archives.postgresql.org/pgsql-jdbc/2005-01/msg00220.php
         String sSQL = "select pg_encoding_to_char(b.encoding) from pg_catalog.pg_attribute a, pg_database b " +
                       "where a.attnum > 0 and a.attname = ? and a.attrelid = (select c.oid from pg_catalog.pg_class c left join " +
                       "pg_catalog.pg_namespace d on d.oid = c.relnamespace where c.relname = ? and d.nspname = ? and b.datname = ?)";
         Integer nCol = Primitive.createInteger(nColumn);
         PreparedStatement stmt = null;
         ResultSet rsColumn = null;

         log(sSQL);

         try
         {
            stmt = rs.getStatement().getConnection().prepareStatement(sSQL);

            stmt.setString(1, (String)s_getBaseColumnName.invoke(rsMeta, new Object[]{nCol}));
            stmt.setString(2, (String)s_getBaseTableName.invoke(rsMeta, new Object[]{nCol}));
            stmt.setString(3, (String)s_getBaseSchemaName.invoke(rsMeta, new Object[]{nCol}));
            stmt.setString(4, ((RelationalDatabaseFragment)((RelationalDatabase)schema.getDataSource()).getDefaultFragment()).getDatabase());

            rsColumn = executeQuery(stmt);

            if (rsColumn.next())
            {
               String sCharSet = rsColumn.getString(1);

               if ("UTF8".equals(sCharSet))
               {
                  unicode = Boolean.TRUE;
               }
               else if (sCharSet != null)
               {
                  unicode = Boolean.FALSE;
               }
            }
         }
         catch (SQLException e)
         {
            throw e;
         }
         catch (Exception e) // Exceptions from reflection
         {
            throw new RuntimeException("Unable to load PGResultSetMetaData", e);
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
      }

      return unicode;
   }

   // inner classes

   /**
    * A wrapper that returns PostgreSQLPreparedStatementWrapper.
    */
   protected static class PostgreSQLConnectionWrapper extends ConnectionWrapper
   {
      // attributes

      /**
       * True to emulate atomic DML.
       */
      protected boolean m_bDMLAtomic;

      // constructor

      /**
       * Constructor.
       * @param connection The connection that is to be wrapped.
       * @param bDMLAtomic True to emulate atomic DML.
       */
      public PostgreSQLConnectionWrapper(Connection connection, boolean bDMLAtomic)
      {
         super(connection);
         m_bDMLAtomic = bDMLAtomic;
      }

      // operations

      /**
       * @return True if atomic DML emulation is enabled. 
       */
      public boolean isDMLAtomic()
      {
         return m_bDMLAtomic;
      }

      /**
       * @see nexj.core.util.sql.ConnectionWrapper#prepareStatement(java.lang.String, int, int, int)
       */
      public PreparedStatement prepareStatement(String sql, int resultSetType, int resultSetConcurrency, int resultSetHoldability) throws SQLException
      {
         return new PostgreSQLPreparedStatementWrapper(super.prepareStatement(sql,
            resultSetType, resultSetConcurrency, resultSetHoldability), m_bDMLAtomic);
      }

      /**
       * @see nexj.core.util.sql.ConnectionWrapper#prepareStatement(java.lang.String, int, int)
       */
      public PreparedStatement prepareStatement(String sql, int resultSetType, int resultSetConcurrency) throws SQLException
      {
         return new PostgreSQLPreparedStatementWrapper(super.prepareStatement(sql,
            resultSetType, resultSetConcurrency), m_bDMLAtomic);
      }

      /**
       * @see nexj.core.util.sql.ConnectionWrapper#prepareStatement(java.lang.String, int)
       */
      public PreparedStatement prepareStatement(String sql, int autoGeneratedKeys) throws SQLException
      {
         return new PostgreSQLPreparedStatementWrapper(super.prepareStatement(sql,
            autoGeneratedKeys), m_bDMLAtomic);
      }

      /**
       * @see nexj.core.util.sql.ConnectionWrapper#prepareStatement(java.lang.String, int[])
       */
      public PreparedStatement prepareStatement(String sql, int[] columnIndexes) throws SQLException
      {
         return new PostgreSQLPreparedStatementWrapper(super.prepareStatement(sql,
            columnIndexes), m_bDMLAtomic);
      }

      /**
       * @see nexj.core.util.sql.ConnectionWrapper#prepareStatement(java.lang.String, java.lang.String[])
       */
      public PreparedStatement prepareStatement(String sql, String[] columnNames) throws SQLException
      {
         return new PostgreSQLPreparedStatementWrapper(super.prepareStatement(sql, columnNames), m_bDMLAtomic);
      }

      /**
       * @see nexj.core.util.sql.ConnectionWrapper#prepareStatement(java.lang.String)
       */
      public PreparedStatement prepareStatement(String sql) throws SQLException
      {
         return new PostgreSQLPreparedStatementWrapper(super.prepareStatement(sql), m_bDMLAtomic);
      }
   }

   /**
    * A wrapper that adds query timeout functionality and implicit savepoints for statements in transaction blocks.
    */
   protected static class PostgreSQLPreparedStatementWrapper extends PreparedStatementWrapper
   {
      // attributes

      /**
       *  The number of seconds the driver will wait for a Statement object to execute.
       */
      protected int m_nSeconds;

      /**
       * True to emulate atomic DML.
       */
      protected boolean m_bDMLAtomic;

      // associations

      /**
       * The wrapped prepared statement.
       */
      protected PreparedStatement m_stmt;

      /**
      * The connection handle that requested this statement.
      */
      protected Connection m_connection;

      // constructor

      /**
       * Constructor.
       * @param stmt The statement to wrap (not null).
       * @param bDMLAtomic True to emulate atomic DML.
       */
      public PostgreSQLPreparedStatementWrapper(PreparedStatement stmt, boolean bDMLAtomic) throws SQLException
      {
         super(stmt);
         m_stmt = stmt;
         m_connection = stmt.getConnection();
         m_bDMLAtomic = bDMLAtomic;
      }

      // operations

      /**
       * @see nexj.core.util.sql.StatementWrapper#getConnection()
       */
      public Connection getConnection() throws SQLException
      {
         return m_connection;
      }

      /**
       * @see nexj.core.persistence.sql.PreparedStatementWrapper#getStatement()
       */
      public PreparedStatement getStatement()
      {
         return m_stmt;
      }

      /**
       * @see nexj.core.persistence.sql.PreparedStatementWrapper#executeQuery()
       */
      public ResultSet executeQuery() throws SQLException
      {
         try
         {
            Connection connection = unwrap(m_connection);

            if (m_nSeconds <= 0 ||
                s_PGXAConnection == null ||
                !s_getQueryExecutor.getDeclaringClass().isAssignableFrom(connection.getClass()))
            {
               return super.executeQuery();
            }

            Object lock = null;
            ResultSet rs;

            try
            {
               // find synchronization lock used by Postgresql JDBC adapter for running statements on connection
               lock = s_getQueryExecutor.invoke(connection, null);
            }
            catch (Exception e)
            {
               lock = connection;
            }

            synchronized (lock) // ensure cancel() doesn't overlap with other statements
            {
               TimerTask task = StatementCancelationTask.schedule(this);

               try
               {
                  rs = super.executeQuery();
               }
               catch (SQLException e)
               {
                  StatementCancelationTask.cancel(task); // no longer need cancel() task since statement already executed

                  throw e;
               }

               if (!StatementCancelationTask.cancel(task)) // cancel() task was registered and already ran
               {
                  // the current result set may have been corrupted by the statement cancellation
                  // avoid using a potentially corrupt returned ResultSet as its use may cause
                  // bind/internal/cursor errors on future statement invocations
                  throw new SQLException(
                     "Possible ERROR: query cancelled",
                     "57014",
                     0);
               }
            }

            return rs;
         }
         catch (SQLException e)
         {
            throw e;
         }
      }

      /**
       * @see nexj.core.persistence.sql.PreparedStatementWrapper#executeUpdate()
       */
      public int executeUpdate() throws SQLException
      {
         boolean bSavepoint = requestSavepoint();

         try
         {
            return super.executeUpdate();
         }
         catch (SQLException e)
         {
            if (bSavepoint)
            {
               rollbackSavepoint();
            }

            throw e;
         }
         finally
         {
            if (bSavepoint)
            {
               releaseSavepoint();
            }
         }
      }

      /**
       * @see nexj.core.util.sql.StatementWrapper#executeBatch()
       */
      public int[] executeBatch() throws SQLException
      {
         boolean bSavepoint = requestSavepoint();

         try
         {
            return super.executeBatch();
         }
         catch (SQLException e)
         {
            if (bSavepoint)
            {
               rollbackSavepoint();
            }

            throw e;
         }
         finally
         {
            if (bSavepoint)
            {
               releaseSavepoint();
            }
         }
      }

      /**
       * Releases the savepoint 'sp' for the connection provided by this statement.
       * @throws SQLException
       */
      protected void releaseSavepoint() throws SQLException
      {
         // SQL requires a savepoint to be destroyed automatically when another savepoint with the same name is established.
         // In PostgreSQL, the old savepoint is kept.
         // http://www.postgresql.org/docs/9.0/static/sql-savepoint.html#AEN68365
         PreparedStatement stmt = null;

         try
         {
            stmt = m_connection.prepareStatement("release sp");
            stmt.executeUpdate();
         }
         finally
         {
            if (stmt != null)
            {
               stmt.close();
            }
         }
      }

      /**
       * If this statement's connection is not in auto commit mode, then create a savepoint.
       * @return true if a save point was created.
       * @throws SQLException
       */
      protected boolean requestSavepoint() throws SQLException
      {
         if (!m_bDMLAtomic || m_connection.getAutoCommit())
         {
            return false;
         }

         PreparedStatement stmt = null;

         try
         {
            stmt = m_connection.prepareStatement("savepoint sp");
            stmt.executeUpdate();

            return true;
         }
         finally
         {
            if (stmt != null)
            {
               stmt.close();
            }
         }
      }

      /**
       * Rollbacks back the transaction to savepoint 'sp' for connection provided by this statement.
       * @throws SQLException
       */
      protected void rollbackSavepoint() throws SQLException
      {
         PreparedStatement stmt = null;

         try
         {
            stmt = m_connection.prepareStatement("rollback to sp");
            stmt.executeUpdate();
         }
         finally
         {
            if (stmt != null)
            {
               stmt.close();
            }
         }
      }

      /**
       * @see nexj.core.util.sql.StatementWrapper#setQueryTimeout(int)
       */
      public void setQueryTimeout(int nSeconds) throws SQLException
      {
         if (s_PGXAConnection == null ||
             !s_getQueryExecutor.getDeclaringClass().isAssignableFrom(unwrap(m_connection).getClass()))
         {
            super.setQueryTimeout(nSeconds);
         }
         else
         {
            if (nSeconds < 0)
            {
               throw new SQLException("Query timeout must be a non-negative value");
            }

            m_nSeconds = nSeconds;
         }
      }

      /**
       * @see nexj.core.util.sql.StatementWrapper#getQueryTimeout()
       */
      public int getQueryTimeout() throws SQLException
      {
         if (s_PGXAConnection == null ||
             !s_getQueryExecutor.getDeclaringClass().isAssignableFrom(unwrap(m_connection).getClass()))
         {
            return super.getQueryTimeout();
         }

         return m_nSeconds;
      }
   }
}