// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;

import nexj.core.meta.MetadataException;
import nexj.core.scripting.GenericParser;
import nexj.core.util.SubstReader;

/**
 * Reader expanding SQL macro variables.
 */
public abstract class SQLSubstReader extends SubstReader
{
   /**
    * Microsecond portion of timestamp.
    */
   public static final byte TS_USEC = 0;

   /**
    * Second portion of timestamp.
    */
   public static final byte TS_SEC = 1;

   /**
    * Minute portion of timestamp.
    */
   public static final byte TS_MIN = 2;

   /**
    * Hour portion of timestamp.
    */
   public static final byte TS_HOUR = 3;

   /**
    * Day portion of timestamp.
    */
   public static final byte TS_DAY = 4;

   /**
    * Week in year portion of timestamp.
    */
   public static final byte TS_WEEK = 5;

   /**
    * Month portion of timestamp.
    */
   public static final byte TS_MONTH = 6;

   /**
    * Quarter in year portion of timestamp.
    */
   public static final byte TS_QUARTER = 7;

   /**
    * Year portion of timestamp.
    */
   public static final byte TS_YEAR = 8;

   // associations

   /**
    * Return Metadata names (false == return SQL token names).
    */
   protected boolean m_bMetadataExpansion;

   /**
    * The relational schema.
    */
   protected RelationalSchema m_schema;

   /**
    * The context table. Can be null.
    */
   protected Table m_table;

   // constructors

   /**
    * Constructs the reader.
    * @param reader The reader to wrap.
    * @param schema The relational schema.
    * @param table The context table. Can be null.
    */
   protected SQLSubstReader(Reader reader, RelationalSchema schema, Table table)
   {
      super(reader);
      m_schema = schema;
      m_table = table;
   }

   // operations

   /**
    * @see nexj.core.util.SubstReader#getValue(java.lang.String)
    */
   protected String getValue(String sName) throws IOException
   {
      if (sName.startsWith("table:"))
      {
         Table table = getTable(sName.substring("table:".length()));

         return (m_bMetadataExpansion) ? table.getName() : getQuotedTableName(table);
      }

      if (sName.equals("table"))
      {
         if (m_table == null)
         {
            throw new MetadataException("err.meta.persistence.sql.contextTable");
         }

         return (m_bMetadataExpansion) ? m_table.getName() : getQuotedTableName(m_table);
      }

      if (sName.startsWith("object:"))
      {
         SQLObject obj = getObject(sName.substring("object:".length()));

         return (m_bMetadataExpansion) ? obj.getName() : getQuotedObjectName(obj);
      }

      if (sName.startsWith("index:"))
      {
         Index index = getIndex(sName.substring("index:".length()));

         return (m_bMetadataExpansion) ? index.getName() : getQuotedIndexName(index);
      }

      if (sName.startsWith("ifci:"))
      {
         String sColumn;
         boolean bMetadataExpansion = m_bMetadataExpansion;
         StringReader input = new StringReader(sName);
         SQLDelimTokenizer tokenizer = new SQLDelimTokenizer(false)
         {
            protected boolean isDelimiter(char ch) { return ch == ':'; }
         };

         input.skip("ifci:".length());
         m_bMetadataExpansion = true;
         sColumn = substitute((String)tokenizer.parse(input, null));
         m_bMetadataExpansion = bMetadataExpansion;
         input.mark(1);

         if (input.read() >= 0) // have a second argument
         {
            int i = sColumn.lastIndexOf('.');

            input.reset();

            if (i > 0)
            {
               Table table = getTable(sColumn.substring(0, i));

               return (isColumnCaseInsensitive(table.getColumn(sColumn.substring(i + 1))))
                      ? substitute(new StringWriter(Math.max(32, sName.length())), input).toString()
                      : null;
            }
         }
      }

      if (sName.startsWith("binary:"))
      {
         return getBinaryLiteral(sName.substring(7));
      }

      if (sName.startsWith("quote:"))
      {
         return sName.substring(6);
      }

      if (sName.startsWith("keyword:"))
      {
         return getQuotedKeyword(sName.substring("keyword:".length()));
      }

      if (sName.equals("owner"))
      {
         return getQuotedOwnerName();
      }

      if (sName.equals("owner."))
      {
         String sOwner = getQuotedOwnerName();

         if (sOwner == null || sOwner.length() == 0)
         {
            return "";
         }

         return sOwner + '.';
      }

      if (sName.equals("role"))
      {
         return getQuotedRoleName();
      }

      if (sName.endsWith(")")) // function call in the form of <identifier>(<arg>,<arg>,...)
      {
         int i = sName.indexOf('(');

         if (i >= 0)
         {
            StringReader reader =
               new StringReader(sName.substring(i + 1, sName.length() - ")".length()));
            List/*<String>*/ argList = new ArrayList();
            SQLDelimTokenizer tokenizer = new SQLDelimTokenizer(false)
            {
               protected boolean isDelimiter(char ch) { return ch == ','; }
            };

            for (String sToken; (sToken = (String)tokenizer.parse(reader, null)) != null;)
            {
               argList.add(substitute(sToken));
            }

            StringBuffer buf = appendFunction(
               new StringBuffer(sName.length()),
               sName.substring(0, i),
               (CharSequence[])argList.toArray(new CharSequence[argList.size()]));

            return (buf.length() > 0) ? buf.toString() : null;
         }
      }

      String[] variableArray = new String[]
      {
         "${table:<name>}",
         "${object:<name>}",
         "${index:<name>}",
         "${ifci:<table.column>:<sql>}",
         "${owner}",
         "${owner.}",
         "${role}",
         "${binary:<hex>}",
         "${quote:<text>}",
         "${keyword:<name>}"
      };

      throw new MetadataException("err.meta.persistence.sql.variable",
                                  new Object[]{sName, variableArray});
   }

   /**
    * Return the SQL for specified function invocation with the specified arguments.
    * For a list of macro function naming convention:
    * @see http://db.apache.org/derby/docs/10.2/ref/rrefjdbc88908.html#rrefjdbc88908
    * @param buf The destination buffer.
    * @param sName The function identifier.
    * @param argArray The function arguments (not null).
    * @return The buffer containing the RDBMS SQL equivalent to the specified function invocation.
    */
   protected StringBuffer appendFunction(StringBuffer buf, String sName, CharSequence[] argArray)
   {
      assert argArray != null;

      String sValue;

      if ("concat".equals(sName))
      {
         return appendConcatenate(buf, argArray);
      }

      if ("extract".equals(sName) && argArray.length == 2)
      {
         return appendTSExtract(
            buf, argArray[1], parseTimestampField(argArray[0].toString().trim()));
       }

      if ("guid".equals(sName) && argArray.length == 0)
      {
         sValue = getGUID();

         return (sValue == null) ? buf : buf.append(sValue);
      }

      if ("now".equals(sName) && argArray.length == 0)
      {
         sValue = getNow();

         return (sValue == null) ? buf : buf.append(sValue);
      }

      if ("sysPublicId".equals(sName) && argArray.length == 0)
      {
         sValue = getSysPublicId();

         return (sValue == null) ? buf : buf.append(sValue);
      }

      if ("sysUserId".equals(sName) && argArray.length == 0)
      {
         sValue = getSysUserId();

         return (sValue == null) ? buf : buf.append(sValue);
      }

      if ("sysUserAlias".equals(sName) && argArray.length == 0)
      {
         sValue = getSysUserAlias();

         return (sValue == null) ? buf : buf.append(sValue);
      }

      if ("timestampadd".equals(sName) && argArray.length == 3)
      {
         return appendTSIncrement(
            buf, argArray[2], argArray[1], parseTimestampField(argArray[0].toString().trim()));
      }

      String[] variableArray = new String[]
      {
         "${concat(<arg1>,<arg2>,...)}",
         "${extract(<year|quarter|month|week|day|hour|minute|second|frac_second|microsecond>,<timestamp>)}",
         "${guid()}",
         "${now()}",
         "${sysPublicId()}",
         "${sysUserId()}",
         "${sysUserAlias()}",
         "${timestampadd(<year|quarter|month|week|day|hour|minute|second|frac_second|microsecond>,<delta>,<timestamp>)}",
      };

      throw new MetadataException("err.meta.persistence.sql.function",
                                  new Object[]{sName, argArray, variableArray});
   }

   /**
    * Parses the timestamp field from a string.
    * @param sField The Timestamp field value to parse (not null).
    * @return One of TS_* constants.
    * @throws MetadataException on unsupported timestamp field.
    */
   protected byte parseTimestampField(String sField)
   {
      assert sField != null;

      if (sField.length() > "sql_tsi_".length() &&
          "sql_tsi_".equalsIgnoreCase(sField.substring(0, "sql_tsi_".length())))
      {
         sField = sField.substring("sql_tsi_".length()); // discard "sql_tsi_" prefix
      }

      if ("frac_second".equalsIgnoreCase(sField) || "microsecond".equalsIgnoreCase(sField))
      {
         return TS_USEC;
      }

      if ("second".equalsIgnoreCase(sField))
      {
         return TS_SEC;
      }

      if ("minute".equalsIgnoreCase(sField))
      {
         return TS_MIN;
      }

      if ("hour".equalsIgnoreCase(sField))
      {
         return TS_HOUR;
      }

      if ("day".equalsIgnoreCase(sField))
      {
         return TS_DAY;
      }

      if ("week".equalsIgnoreCase(sField))
      {
         return TS_WEEK;
      }

      if ("month".equalsIgnoreCase(sField))
      {
         return TS_MONTH;
      }

      if ("quarter".equalsIgnoreCase(sField))
      {
         return TS_QUARTER;
      }

      if ("year".equalsIgnoreCase(sField))
      {
         return TS_YEAR;
      }

      String[] variableArray = new String[]
      {
         "frac_second",
         "microsecond",
         "second",
         "minute",
         "hour",
         "day",
         "week",
         "month",
         "quarter",
         "year"
      };

      throw new MetadataException("err.meta.persistence.sql.tsfield",
                                  new Object[]{sField, variableArray});
   }

   /**
    * Retrieve a table by the specified name from the schema.
    * @param sName The name of the table to retrieve.
    * @return The table by the specified name.
    * @throws MetadataLookupException If the table does not exist.
    */
   protected Table getTable(String sName)
   {
      return m_schema.getTable(sName);
   }

   /**
    * Retrieve an SQLObject by the specified name from the schema.
    * @param sName The name of the SQLObject to retrieve.
    * @return The SQLObject by the specified name.
    * @throws MetadataLookupException If the SQLObject does not exist.
    */
   protected SQLObject getObject(String sName)
   {
      return m_schema.getObject(sName);
   }

   /**
    * Retrieve an index by the specified name from the schema.
    * @param sName The name of the index to retrieve.
    * @return The index by the specified name.
    * @throws MetadataLookupException If the index does not exist.
    */
   protected Index getIndex(String sName)
   {
      return m_schema.getIndex(sName);
   }

   /**
    * Gets a quoted table name.
    * @param table The table object.
    * @return The quoted table name.
    */
   protected abstract String getQuotedTableName(Table table);

   /**
    * Gets a quoted SQLObject name.
    * @param object The SQLObject to return the quoted name for.
    * @return The quoted SQLObject name.
    */
   protected abstract String getQuotedObjectName(SQLObject object);

   /**
    * Gets a quoted index name.
    * @param index The index object.
    * @return The quoted index name.
    */
   protected abstract String getQuotedIndexName(Index index);

   /**
    * Gets a quoted keyword.
    * @param sName The keyword to quote.
    * @return The quoted keyword.
    */
   protected abstract String getQuotedKeyword(String sName);

   /**
    * @return The quoted schema owner name.
    */
   protected abstract String getQuotedOwnerName();

   /**
    * @return The quoted schema role name.
    */
   protected abstract String getQuotedRoleName();

   /**
    * Determines if a column is case insensitive.
    * @param column The column object.
    * @return True is the column is case insensitive.
    */
   protected abstract boolean isColumnCaseInsensitive(Column column);

   /**
    * Gets a binary literal representation of a hexadecimal string.
    * @param sHex The hexadecimal string.
    * @return The binary literal.
    */
   protected abstract String getBinaryLiteral(String sHex);

   /**
    * @return Expression returning the current UTC timestamp.
    */
   protected abstract String getNow();

   /**
    * @return Expression returning a new GUID.
    */
   protected abstract String getGUID();

   /**
    * @return Expression with the public principal Id.
    */
   protected abstract String getSysPublicId();

   /**
    * @return Expression with the system user Id. 
    */
   protected abstract String getSysUserId();

   /**
    * @return Expression with the system user alias.
    */
   protected abstract String getSysUserAlias();

   /**
    * Wrap the list into a RDBMS dependent string concatenation syntax.
    * @param buf The destination buffer.
    * @param argArray The list of string values to wrap in string concatenation SQL.
    * @return The destination buffer.
    */
   protected abstract StringBuffer appendConcatenate(StringBuffer buf, CharSequence[] argArray);

   /**
    * Wrap the timestamp and field type into a RDBMS dependent timestamp field extraction syntax.
    * @param buf The destination buffer.
    * @param sTS The timestamp value.
    * @param nField one of TS_* constants.
    * @return The destination buffer.
    */
   protected abstract StringBuffer appendTSExtract(StringBuffer buf, CharSequence sTS, byte nField);

   /**
    * Wrap the timestamp and delta into a RDBMS dependent timestamp modification syntax.
    * @param buf The destination buffer.
    * @param sTS The timestamp value.
    * @param sDelta The number of units to alter the value in the calculation.
    * @param nField one of TS_* constants representing units of sDelta.
    * @return The destination buffer.
    */
   protected abstract StringBuffer appendTSIncrement(
      StringBuffer buf, CharSequence sTS, CharSequence sDelta, byte nField);

   /**
    * Tokeniser for delimited SQL strings.
    * Note: Empty string past trailing delimiter will be ignored same as StringUtil.split(...).
    */
   protected abstract static class SQLDelimTokenizer extends GenericParser
   {
      /**
       * Collapse multiple delimiter repetitions into a single delimiter.
       */
      protected boolean m_bCollapse;

      /**
       * Constructor.
       * @param bCollapse Collapse multiple delimiter repetitions into a single delimiter.
       */
      public SQLDelimTokenizer(boolean bCollapse)
      {
         super(null);

         m_bCollapse = bCollapse;
      }

      /**
       * Consume a macro string, all data between and including braces is consumed.
       */
      protected void consumeMacro()
      {
         m_tokenBuf.append((char)m_ch);
         forgetChar();

         if (super.getCurChar() != '{') // want to explicitly see '$$' or any other char after $
         {
            return; // not start of a macro
         }

         consumeUntil('}');
      }

      /**
       * Consume input until and including 'ch' is reached.
       * @param ch The character after which to stop consumption.
       */
      protected void consumeUntil(char ch)
      {
         m_tokenBuf.append((char)m_ch);

         do
         {
            if (getNextChar() == CHAR_EOF) // end of stream
            {
               fail("err.parser.unterminatedString", null, getCurTextPosition());
            }

            m_tokenBuf.append((char)m_ch);
         }
         while (getCurChar() != ch);

         forgetChar();
      }

      /**
       * @see nexj.core.scripting.GenericParser#getCurChar()
       */
      protected int getCurChar()
      {
         if (m_ch == CHAR_NONE)
         {
            if (super.getCurChar() == '$')
            {
               consumeMacro();
            }

            return getCurChar(); // consumeMacro() may or may not leave a character behind
         }

         return m_ch;
      }

      /**
       * @return Is 'ch' a delimiter character.
       */
      protected abstract boolean isDelimiter(char ch);

      /**
       * @see nexj.core.scripting.GenericParser#parseElement()
       */
      protected Object parseElement()
      {
         forgetChar(); // forget character from previous invocation
         m_tokenBuf.setLength(0);

         if (m_bCollapse)
         {
            while (getCurChar() != CHAR_EOF && isDelimiter((char)getCurChar()))
            {
               forgetChar(); // collapse repeating delimiter prefix
            }
         }

         if (getCurChar() == CHAR_EOF && m_tokenBuf.length() == 0) // getCurChar can fill m_tokenBuf
         {
            return null; // base case
         }

         int nNest = 0;

         while (getCurChar() != CHAR_EOF && !(nNest == 0 && isDelimiter((char)getCurChar()))) // EOF
         {
            if (getCurChar() == '"') // doublequoted string parsing
            {
               consumeUntil('"');

               continue;
            }

            if (getCurChar() == '[') // MSSQL quazi-quoted string parsing
            {
               consumeUntil(']');

               continue;
            }

            if (getCurChar() == '\'') // single quoted string parsing
            {
               consumeUntil('\'');

               continue;
            }

            if (getCurChar() == '(') // start of nested block
            {
               ++nNest;
            }

            if (getCurChar() == ')' && nNest > 0) // end of nested block (ignore multiple trailing)
            {
               --nNest;
            }

            m_tokenBuf.append((char)m_ch);
            forgetChar();
         }

         return m_tokenBuf.toString();
      }

      /**
       * @see nexj.core.scripting.GenericParser#parseToken()
       */
      protected int parseToken()
      {
         return 0;
      }
   }
}