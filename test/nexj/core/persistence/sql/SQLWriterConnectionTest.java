// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import java.io.StringWriter;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.Calendar;

import nexj.core.meta.Primitive;
import nexj.core.meta.persistence.sql.Column;
import nexj.core.meta.persistence.sql.Index;
import nexj.core.meta.persistence.sql.RelationalSchema;
import nexj.core.meta.persistence.sql.Table;
import nexj.core.persistence.Operator;
import nexj.core.persistence.Query;
import nexj.core.persistence.SchemaVersion;
import nexj.core.persistence.operator.FunctionOperator;
import nexj.core.scripting.Pair;

import junit.framework.TestCase;

/**
 * Testcases for SQLWriterConnectionFactory.
 */
public class SQLWriterConnectionTest extends TestCase
{
   /**
    * Dummy adapter used for testing.
    */
   private SQLAdapter m_adapter = new SQLAdapter()
   {
      String m_sBindPrefix;

      public void appendBind(StringBuffer buf, int nOrdinal)
      {
         if (nOrdinal == Integer.MIN_VALUE) // allow a way to reset bind token for testing
         {
            m_sBindPrefix = (buf == null) ? null : buf.toString();
         }
         else
         {
            buf.append((m_sBindPrefix == null) ? "?" : m_sBindPrefix + (nOrdinal + 1));
         }
      }

      // dummy values
      public boolean appendIdentityColumn(StringBuffer buf, SQLInsert work) { return false; }
      public void appendIdentityPrefix(StringBuffer buf, SQLInsert work) { }
      public boolean appendIdentitySuffix(StringBuffer buf, SQLInsert work) { return false; }
      public boolean appendIdentityValue(StringBuffer buf, SQLInsert work) { return false; }
      public void appendInfixHint(StringBuffer buf, Query query) { }

      public void appendLiteral(StringBuffer buf, Primitive type, Object value)
      {
         buf.append(type).append(':').append(value);
      }

      // dummy values
      public void appendMatchStatement(StringBuffer buf, String alias, Column column,
         SQLJoin join, Pair expression) { }
      public String appendBooleanPrefix(StringBuffer buf, Operator op) { return null; }
      public boolean appendNoRowsBlock(StringBuffer buf) { return false; }
      public void appendNoRowsEnd(StringBuffer buf) { }
      public void appendNoRowsStart(StringBuffer buf) { }
            public void appendPrefixHint(StringBuffer buf, Query query) { }
      public void appendSuffixHint(StringBuffer buf, Query query) { }
      public void appendTableHint(StringBuffer buf, SQLJoin join, Query query) { }
      public void appendTypeConversion(StringBuffer buf, Object op, Primitive fromType,
         Primitive type, SQLGenerator gen) { }
      public void bindIdentity(PreparedStatement stmt, SQLInsert work) throws SQLException { }

      public SQLSchemaManager createSchemaManager()
      {
         return new SQLSchemaManager(this)
         {
            // dummy values
            public void analyzeTable(Table table) { }
            protected void appendColumnType(StringBuffer buf, Column column) { }
            protected StringBuffer appendConcatenate(
               StringBuffer buf, CharSequence[] argArray) { return buf; }
            protected StringBuffer appendTSExtract(
               StringBuffer buf, CharSequence sTS, byte nField) { return buf; }
            protected StringBuffer appendTSIncrement(
               StringBuffer buf, CharSequence sTS, CharSequence sDelta, byte nField) { return buf; }
            protected void appendPrint(StringBuffer buf, String msg) { }
            protected void appendUpgradeStepEnd(StringBuffer buf, RelationalSchema schema,
               SchemaVersion version, SchemaVersion prev, String sFault) { }
            protected void appendUpgradeStepStart(StringBuffer buf, RelationalSchema schema,
               SchemaVersion version) { }
            protected String getCreateEtcScriptName() { return null; }
            protected String getDefaultIndexspace() { return null; }
            protected String getDefaultLongspace() { return null; }
            protected String getDefaultTablespace() { return null; }
            protected String getDropEtcScriptName() { return null; }
            protected SQLAppender getDynamicSQLAppender(StringBuffer buf) { return null; }
            protected String getGUIDExpr() { return null; }
            protected String getNowExpr() { return null; }
            protected String getSetupEtcScriptName() { return null; }

            public String getSeparator()
            {
               return "|";
            }
            protected boolean isImplicitConversion(Column source, Column target) { return true; }
         };
      }

      // dummy values
      protected String getDuplicateKeyName(SQLException e) { return null; }
      public Object getIdentityValue(PreparedStatement stmt, Column column, SQLInsert work)
         throws SQLException { return null; }
      protected long getMaxTime() { return 0; }
      protected long getMinTime() { return 0; }
      public String getTestSQL() { return null; }
      protected boolean indexNameMatches(Index index, String physicalName) { return false; }
      public boolean isBatchSupported() { return true; } // ensure batching is tested
      public boolean isBatchUpdateCountSupported() { return false; }
      public boolean isBatchable(SQLWork work) { return true; } // ensure batching is tested
      protected boolean isDateRangeException(SQLException e) { return false; }
      protected boolean isDeadlockException(SQLException e) { return false; }
      protected boolean isLockTimeoutException(SQLException e) { return false; }
      protected boolean isDuplicateKeyException(SQLException e) { return false; }
      public boolean isLiteral(Primitive type, Object value) { return false; }
      protected boolean isQueryTimeoutException(SQLException e) { return false; }
      protected Boolean isUnicode(RelationalSchema schema, ResultSet rs, int column)
         throws SQLException { return null; }
      public String appendStringLengthPrefix(StringBuffer buf, FunctionOperator op) { return null; }
      public String appendSubstringPrefix(StringBuffer buf, FunctionOperator op) { return null; }
   };

   public void testBatch() throws Exception
   {
      StringWriter output = new StringWriter();
      Connection con = new SQLWriterConnection(m_adapter, output, false);

      m_adapter.appendBind(null, Integer.MIN_VALUE); // test SQL placeholders

      PreparedStatement stmt = con.prepareStatement("<prepared> ?");

      stmt.setInt(1, 1);
      stmt.addBatch();
      stmt.setInt(1, 2);
      stmt.addBatch();
      stmt.setInt(1, 3);
      stmt.addBatch();
      stmt.executeBatch();

      assertEquals(
         "<prepared> Primitive integer:1|" +
         "<prepared> Primitive integer:2|" +
         "<prepared> Primitive integer:3|",
         output.toString());

      output.getBuffer().setLength(0);
      stmt.setInt(1, 4);
      stmt.addBatch();
      stmt.setInt(1, 5);
      stmt.addBatch();
      stmt.executeBatch();

      assertEquals(
         "<prepared> Primitive integer:4|" +
         "<prepared> Primitive integer:5|",
         output.toString());
   }

   public void testQueryWriteThough() throws Exception
   {
      StringWriter output = new StringWriter();
      Connection con = new SQLWriterConnection(m_adapter, output, false);
      PreparedStatement stmt = con.prepareStatement("<SQL>");

      stmt.executeQuery();
      assertEquals(0, output.getBuffer().length());

      stmt.executeQuery("<SQL> direct");
      assertEquals(0, output.getBuffer().length());

      stmt.execute();
      assertTrue(output.getBuffer().length() > 0);

      con = new SQLWriterConnection(m_adapter, output);
      stmt = con.prepareStatement("<SQL>");

      stmt.executeQuery();
      assertTrue(output.getBuffer().length() > 0);

      output.getBuffer().setLength(0);
      stmt.executeQuery("<SQL> direct");
      assertTrue(output.getBuffer().length() > 0);

      output.getBuffer().setLength(0);
      stmt.execute();
      assertTrue(output.getBuffer().length() > 0);
   }

   public void testQuotedParams() throws Exception
   {
      StringWriter output = new StringWriter();
      Connection con = new SQLWriterConnection(m_adapter, output);

      m_adapter.appendBind(null, Integer.MIN_VALUE); // test SQL placeholders

      PreparedStatement stmt = // test quoted literal placeholder
         con.prepareStatement("<prepared> 'ab ? cd' efg ? hij '''kl ? mn'' op'");

      stmt.setString(1, "p1");
      stmt.execute();

      assertEquals("<prepared> 'ab ? cd' efg Primitive string:p1 hij '''kl ? mn'' op'|",
                   output.toString());
   }

   public void testSubstitution() throws Exception
   {
      StringWriter output = new StringWriter();
      Connection con = new SQLWriterConnection(m_adapter, output);

      m_adapter.appendBind(null, Integer.MIN_VALUE); // test SQL placeholders

      PreparedStatement stmt = con.prepareStatement("<prepared> abc ? def ? ghi ? jkl ? mno");

      try
      {
         stmt.execute();
         fail();
      }
      catch (SQLException e)
      {} // expected unset parameter

      stmt.setString(1, "p1");
      stmt.setByte(4, (byte)42);
      stmt.setTimestamp(2, new Timestamp(1234567890l), Calendar.getInstance());
      stmt.setBlob(3, null);

      try
      {
         stmt.setDouble(5, 3.1415926535897932384626433832795d);
         fail();
      }
      catch (SQLException e)
      {} // expected undefined parameter

      stmt.execute("<regular>");
      stmt.execute();

      try
      {
         stmt.execute();
         fail();
      }
      catch (SQLException e)
      {} // expected unset parameter

      m_adapter.appendBind(new StringBuffer(":"), Integer.MIN_VALUE); // test unique placeholders
      stmt = con.prepareStatement("<prepared> pqr :1 stu :3 vwx :2 yza :3 bcd");
      stmt.setString(1, "p1");
      stmt.setByte(2, (byte)42);
      stmt.setTimestamp(3, new Timestamp(1234567890l), Calendar.getInstance());

      try
      {
         stmt.setDouble(4, 3.1415926535897932384626433832795d);
         fail();
      }
      catch (SQLException e)
      {} // expected undefined parameter

      stmt.execute();
      con.commit();

      assertEquals("<regular>|" +
                   "<prepared> abc Primitive string:p1 " +
                   "def Primitive timestamp:1970-01-14 20:56:07.89 " +
                   "ghi Primitive binary:null " +
                   "jkl Primitive binary:2A mno|" +
                   "<prepared> pqr Primitive string:p1 " +
                   "stu Primitive timestamp:1970-01-14 20:56:07.89 " +
                   "vwx Primitive binary:2A " +
                   "yza Primitive timestamp:1970-01-14 20:56:07.89 bcd|" +
                   "commit|",
                   output.toString());
   }
}