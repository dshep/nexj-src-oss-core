// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.Arrays;

import nexj.core.meta.Repository;
import nexj.core.scripting.ParserException;
import nexj.core.util.IOUtil;

import junit.framework.TestCase;

public class SQLSubstReaderTest extends TestCase
{
   public void testConcat() throws IOException
   {
      RelationalSchema schema =
         (RelationalSchema)Repository.getMetadata()
            .getDataSource("DefaultRelationalDatabase").getSchema();

      StringReader input = new StringReader("${concat()}");
      StringWriter writer = new StringWriter();

      IOUtil.copy(writer, new MockSQLSubstReader(input, schema, null));
      assertEquals("[]0", writer.toString());

      input = new StringReader("${concat(abc)}");
      writer = new StringWriter();
      IOUtil.copy(writer, new MockSQLSubstReader(input, schema, null));
      assertEquals("[abc]1", writer.toString());

      input = new StringReader("${concat(abc,def)}");
      writer = new StringWriter();
      IOUtil.copy(writer, new MockSQLSubstReader(input, schema, null));
      assertEquals("[abc, def]2", writer.toString());

      input = new StringReader("${concat(abc,def,ghi)}");
      writer = new StringWriter();
      IOUtil.copy(writer, new MockSQLSubstReader(input, schema, null));
      assertEquals("[abc, def, ghi]3", writer.toString());

      input = new StringReader("${concat(abc,,ghi)}");
      writer = new StringWriter();
      IOUtil.copy(writer, new MockSQLSubstReader(input, schema, null));
      assertEquals("[abc, , ghi]3", writer.toString());
   }

   public void testExtract() throws IOException
   {
      RelationalSchema schema =
         (RelationalSchema)Repository.getMetadata()
            .getDataSource("DefaultRelationalDatabase").getSchema();
      StringReader input =
         new StringReader("abc ${extract(year,${table:test.Contact}.birthdate)}");
      MockSQLSubstReader reader = new MockSQLSubstReader(input, schema, null);
      StringWriter writer = new StringWriter();

      reader.setColumnCaseInsensitive(true);
      reader.setQuotedTableName("test.Contact");
      IOUtil.copy(writer, reader);
      assertEquals("abc test.Contact.birthdate,8", writer.toString());

      input = new StringReader("abc ${extract(quarter,${table:test.Contact}.birthdate)}");
      reader = new MockSQLSubstReader(input, schema, null);
      reader.setColumnCaseInsensitive(true);
      reader.setQuotedTableName("test.Contact");
      writer.getBuffer().setLength(0);
      IOUtil.copy(writer, reader);
      assertEquals("abc test.Contact.birthdate,7", writer.toString());

      input = new StringReader("abc ${extract(month,${table:test.Contact}.birthdate)}");
      reader = new MockSQLSubstReader(input, schema, null);
      reader.setColumnCaseInsensitive(true);
      reader.setQuotedTableName("test.Contact");
      writer.getBuffer().setLength(0);
      IOUtil.copy(writer, reader);
      assertEquals("abc test.Contact.birthdate,6", writer.toString());

      input = new StringReader("abc ${extract(week,${table:test.Contact}.birthdate)}");
      reader = new MockSQLSubstReader(input, schema, null);
      reader.setColumnCaseInsensitive(true);
      reader.setQuotedTableName("test.Contact");
      writer.getBuffer().setLength(0);
      IOUtil.copy(writer, reader);
      assertEquals("abc test.Contact.birthdate,5", writer.toString());

      input = new StringReader("abc ${extract(day,${table:test.Contact}.birthdate)}");
      reader = new MockSQLSubstReader(input, schema, null);
      reader.setColumnCaseInsensitive(true);
      reader.setQuotedTableName("test.Contact");
      writer.getBuffer().setLength(0);
      IOUtil.copy(writer, reader);
      assertEquals("abc test.Contact.birthdate,4", writer.toString());

      input = new StringReader("abc ${extract(hour,${table:test.Contact}.birthdate)}");
      reader = new MockSQLSubstReader(input, schema, null);
      reader.setColumnCaseInsensitive(true);
      reader.setQuotedTableName("test.Contact");
      writer.getBuffer().setLength(0);
      IOUtil.copy(writer, reader);
      assertEquals("abc test.Contact.birthdate,3", writer.toString());

      input = new StringReader("abc ${extract(minute,${table:test.Contact}.birthdate)}");
      reader = new MockSQLSubstReader(input, schema, null);
      reader.setColumnCaseInsensitive(true);
      reader.setQuotedTableName("test.Contact");
      writer.getBuffer().setLength(0);
      IOUtil.copy(writer, reader);
      assertEquals("abc test.Contact.birthdate,2", writer.toString());

      input = new StringReader("abc ${extract(second,${table:test.Contact}.birthdate)}");
      reader = new MockSQLSubstReader(input, schema, null);
      reader.setColumnCaseInsensitive(true);
      reader.setQuotedTableName("test.Contact");
      writer.getBuffer().setLength(0);
      IOUtil.copy(writer, reader);
      assertEquals("abc test.Contact.birthdate,1", writer.toString());

      input = new StringReader("abc ${extract(frac_second,${table:test.Contact}.birthdate)}");
      reader = new MockSQLSubstReader(input, schema, null);
      reader.setColumnCaseInsensitive(true);
      reader.setQuotedTableName("test.Contact");
      writer.getBuffer().setLength(0);
      IOUtil.copy(writer, reader);
      assertEquals("abc test.Contact.birthdate,0", writer.toString());

      input = new StringReader("abc ${extract(microsecond,${table:test.Contact}.birthdate)}");
      reader = new MockSQLSubstReader(input, schema, null);
      reader.setColumnCaseInsensitive(true);
      reader.setQuotedTableName("test.Contact");
      writer.getBuffer().setLength(0);
      IOUtil.copy(writer, reader);
      assertEquals("abc test.Contact.birthdate,0", writer.toString());
   }

   public void testRecursiveIFCI() throws IOException
   {
      RelationalSchema schema =
         (RelationalSchema)Repository.getMetadata()
            .getDataSource("DefaultRelationalDatabase").getSchema();
      StringReader input =
         new StringReader("abc ${ifci:test.Contact.first_name:${table:test.Contact}}");
      MockSQLSubstReader reader = new MockSQLSubstReader(input, schema, null);
      StringWriter writer = new StringWriter();

      reader.setColumnCaseInsensitive(true);
      reader.setQuotedTableName("def");
      IOUtil.copy(writer, reader);
      assertEquals("abc def", writer.toString());
   }

   public void testRecursiveIFCICondition() throws IOException
   {
      RelationalSchema schema =
         (RelationalSchema)Repository.getMetadata()
            .getDataSource("DefaultRelationalDatabase").getSchema();
      StringReader input =
         new StringReader("abc ${ifci:${table:test.Contact}.first_name:${table:test.Contact}}");
      MockSQLSubstReader reader = new MockSQLSubstReader(input, schema, null);
      StringWriter writer = new StringWriter();

      reader.setColumnCaseInsensitive(true);
      reader.setQuotedTableName("test.Contact");
      IOUtil.copy(writer, reader);
      assertEquals("abc test.Contact", writer.toString());
   }

   public void testRecursiveIFCIConditionDefaultOwner() throws IOException
   {
      RelationalSchema schema =
         (RelationalSchema)Repository.getMetadata()
            .getDataSource("DefaultRelationalDatabase").getSchema();
      Table table = schema.getTable("Version"); // arbitrary table without an owner prefix
      StringReader input = new StringReader("abc ${ifci:${table}.namespace:${table}}");
      MockSQLSubstReader reader = new MockSQLSubstReader(input, schema, table);
      StringWriter writer = new StringWriter();

      reader.setColumnCaseInsensitive(true);
      reader.setQuotedOwnerName("OwNeR");
      reader.setQuotedTableName("test.Version");
      IOUtil.copy(writer, reader);
      assertEquals("abc test.Version", writer.toString());
   }

   public void testSQLDelimTokeniser() throws IOException
   {
      StringReader input = new StringReader(" abc def   ghi ");
      SQLSubstReader.SQLDelimTokenizer tokenizer = new SQLSubstReader.SQLDelimTokenizer(false)
      {
         protected boolean isDelimiter(char ch)
         {
            return Character.isWhitespace(ch);
         }
      };

      assertEquals("", tokenizer.parse(input, null));
      assertEquals("abc", tokenizer.parse(input, null));
      assertEquals("def", tokenizer.parse(input, null));
      assertEquals("", tokenizer.parse(input, null));
      assertEquals("", tokenizer.parse(input, null));
      assertEquals("ghi", tokenizer.parse(input, null));
      assertNull(tokenizer.parse(input, null));

      input.reset();
      tokenizer = new SQLSubstReader.SQLDelimTokenizer(true)
      {
         protected boolean isDelimiter(char ch)
         {
            return Character.isWhitespace(ch);
         }
      };

      assertEquals("abc", tokenizer.parse(input, null));
      assertEquals("def", tokenizer.parse(input, null));
      assertEquals("ghi", tokenizer.parse(input, null));
      assertNull(tokenizer.parse(input, null));

      input = new StringReader("abc${def  ghi} jkl(mno  pqr) [stu  vwx yz1] '234  567  890' " +
                               "'abc [def (jkl' [mno 'pqr (vwx] $'[' ${abc 'def [hgi (jkl ${mno}}");
      assertEquals("abc${def  ghi}", tokenizer.parse(input, null));
      assertEquals("jkl(mno  pqr)", tokenizer.parse(input, null));
      assertEquals("[stu  vwx yz1]", tokenizer.parse(input, null));
      assertEquals("'234  567  890'", tokenizer.parse(input, null));
      assertEquals("'abc [def (jkl'", tokenizer.parse(input, null));
      assertEquals("[mno 'pqr (vwx]", tokenizer.parse(input, null));
      assertEquals("$'['", tokenizer.parse(input, null));
      assertEquals("${abc 'def [hgi (jkl ${mno}}", tokenizer.parse(input, null));
      assertNull(tokenizer.parse(input, null));

      try
      {
         tokenizer.parse(new StringReader("'abc"), null);
      }
      catch (ParserException e) // "err.parser.unterminatedString" expected
      {
         assertEquals("err.parser.unterminatedString", e.getErrorCode());
      }

      try
      {
         tokenizer.parse(new StringReader("[abc"), null);
      }
      catch (ParserException e) // "err.parser.unterminatedString" expected
      {
         assertEquals("err.parser.unterminatedString", e.getErrorCode());
      }

      try
      {
         tokenizer.parse(new StringReader("(abc"), null);
      }
      catch (ParserException e) // "err.parser.unterminatedString" expected
      {
         assertEquals("err.parser.unterminatedString", e.getErrorCode());
      }

      try
      {
         tokenizer.parse(new StringReader("${abc"), null);
      }
      catch (ParserException e) // "err.parser.unterminatedString" expected
      {
         assertEquals("err.parser.unterminatedString", e.getErrorCode());
      }
   }

   public void testTimestampadd() throws IOException
   {
      RelationalSchema schema =
         (RelationalSchema)Repository.getMetadata()
            .getDataSource("DefaultRelationalDatabase").getSchema();
      StringReader input =
         new StringReader("abc ${timestampadd(year,-1,${table:test.Contact}.birthdate)}");
      MockSQLSubstReader reader = new MockSQLSubstReader(input, schema, null);
      StringWriter writer = new StringWriter();

      reader.setColumnCaseInsensitive(true);
      reader.setQuotedTableName("test.Contact");
      IOUtil.copy(writer, reader);
      assertEquals("abc test.Contact.birthdate,-1,8", writer.toString());

      input = new StringReader("abc ${timestampadd(quarter,-1,${table:test.Contact}.birthdate)}");
      reader = new MockSQLSubstReader(input, schema, null);
      reader.setColumnCaseInsensitive(true);
      reader.setQuotedTableName("test.Contact");
      writer.getBuffer().setLength(0);
      IOUtil.copy(writer, reader);
      assertEquals("abc test.Contact.birthdate,-1,7", writer.toString());

      input = new StringReader("abc ${timestampadd(month,-1,${table:test.Contact}.birthdate)}");
      reader = new MockSQLSubstReader(input, schema, null);
      reader.setColumnCaseInsensitive(true);
      reader.setQuotedTableName("test.Contact");
      writer.getBuffer().setLength(0);
      IOUtil.copy(writer, reader);
      assertEquals("abc test.Contact.birthdate,-1,6", writer.toString());

      input = new StringReader("abc ${timestampadd(week,-1,${table:test.Contact}.birthdate)}");
      reader = new MockSQLSubstReader(input, schema, null);
      reader.setColumnCaseInsensitive(true);
      reader.setQuotedTableName("test.Contact");
      writer.getBuffer().setLength(0);
      IOUtil.copy(writer, reader);
      assertEquals("abc test.Contact.birthdate,-1,5", writer.toString());

      input = new StringReader("abc ${timestampadd(day,-1,${table:test.Contact}.birthdate)}");
      reader = new MockSQLSubstReader(input, schema, null);
      reader.setColumnCaseInsensitive(true);
      reader.setQuotedTableName("test.Contact");
      writer.getBuffer().setLength(0);
      IOUtil.copy(writer, reader);
      assertEquals("abc test.Contact.birthdate,-1,4", writer.toString());

      input = new StringReader("abc ${timestampadd(hour,-1,${table:test.Contact}.birthdate)}");
      reader = new MockSQLSubstReader(input, schema, null);
      reader.setColumnCaseInsensitive(true);
      reader.setQuotedTableName("test.Contact");
      writer.getBuffer().setLength(0);
      IOUtil.copy(writer, reader);
      assertEquals("abc test.Contact.birthdate,-1,3", writer.toString());

      input = new StringReader("abc ${timestampadd(minute,-1,${table:test.Contact}.birthdate)}");
      reader = new MockSQLSubstReader(input, schema, null);
      reader.setColumnCaseInsensitive(true);
      reader.setQuotedTableName("test.Contact");
      writer.getBuffer().setLength(0);
      IOUtil.copy(writer, reader);
      assertEquals("abc test.Contact.birthdate,-1,2", writer.toString());

      input = new StringReader("abc ${timestampadd(second,-1,${table:test.Contact}.birthdate)}");
      reader = new MockSQLSubstReader(input, schema, null);
      reader.setColumnCaseInsensitive(true);
      reader.setQuotedTableName("test.Contact");
      writer.getBuffer().setLength(0);
      IOUtil.copy(writer, reader);
      assertEquals("abc test.Contact.birthdate,-1,1", writer.toString());

      input = new StringReader(
         "abc ${timestampadd(frac_second,-1,${table:test.Contact}.birthdate)}");
      reader = new MockSQLSubstReader(input, schema, null);
      reader.setColumnCaseInsensitive(true);
      reader.setQuotedTableName("test.Contact");
      writer.getBuffer().setLength(0);
      IOUtil.copy(writer, reader);
      assertEquals("abc test.Contact.birthdate,-1,0", writer.toString());

      input = new StringReader(
         "abc ${timestampadd(microsecond,-1,${table:test.Contact}.birthdate)}");
      reader = new MockSQLSubstReader(input, schema, null);
      reader.setColumnCaseInsensitive(true);
      reader.setQuotedTableName("test.Contact");
      writer.getBuffer().setLength(0);
      IOUtil.copy(writer, reader);
      assertEquals("abc test.Contact.birthdate,-1,0", writer.toString());
   }

   /**
    * Mock SQLSubstReader interface.
    */
   protected class MockSQLSubstReader extends SQLSubstReader
   {
      /**
       * Return value for @see nexj.core.meta.persistence.sql.SQLSubstReader#isColumnCaseInsensitive(nexj.core.meta.persistence.sql.Column)
       */
      private boolean m_bCI;

      /**
       * Return value for @see nexj.core.meta.persistence.sql.SQLSubstReader#getQuotedOwnerName()
       */
      private String m_sQuotedOwnerName;

      /**
       * Return value for @see nexj.core.meta.persistence.sql.SQLSubstReader#getQuotedTableName(nexj.core.meta.persistence.sql.Table)
       */
      private String m_sQuotedTableName;

      /**
       * @see nexj.core.meta.persistence.sql.SQLSubstReader#SQLSubstReader(Reader, RelationalSchema, Table)
       */
      protected MockSQLSubstReader(Reader reader, RelationalSchema schema, Table table)
      {
         super(reader, schema, table);
      }

      /**
       * @see nexj.core.meta.persistence.sql.SQLSubstReader#appendConcatenate(java.lang.StringBuffer, java.lang.CharSequence[])
       */
      protected StringBuffer appendConcatenate(StringBuffer buf, CharSequence[] argArray)
      {
         if (argArray != null)
         {
            buf.append(Arrays.asList(argArray).toString()).append(argArray.length);
         }

         return buf;
      }

      /**
       * @see nexj.core.meta.persistence.sql.SQLSubstReader#appendTSExtract(java.lang.StringBuffer, java.lang.CharSequence, byte)
       */
      protected StringBuffer appendTSExtract(StringBuffer buf, CharSequence sTS, byte nField)
      {
         return buf.append(sTS).append(',').append(nField);
      }

      /**
       * @see nexj.core.meta.persistence.sql.SQLSubstReader#appendTSIncrement(java.lang.StringBuffer, java.lang.CharSequence, java.lang.CharSequence, byte)
       */
      protected StringBuffer appendTSIncrement(
         StringBuffer buf, CharSequence sTS, CharSequence sDelta, byte nField)
      {
         return buf.append(sTS).append(',').append(sDelta).append(',').append(nField);
      }

      /**
       * @see nexj.core.meta.persistence.sql.SQLSubstReader#getBinaryLiteral(java.lang.String)
       */
      protected String getBinaryLiteral(String sHex)
      {
         return null;
      }

      /**
       * @see nexj.core.meta.persistence.sql.SQLSubstReader#getGUID()
       */
      protected String getGUID()
      {
         return null;
      }

      /**
       * @see nexj.core.meta.persistence.sql.SQLSubstReader#getNow()
       */
      protected String getNow()
      {
         return null;
      }

      /**
       * @see nexj.core.meta.persistence.sql.SQLSubstReader#getQuotedIndexName(nexj.core.meta.persistence.sql.Index)
       */
      protected String getQuotedIndexName(Index index)
      {
         return null;
      }

      /**
       * @see nexj.core.meta.persistence.sql.SQLSubstReader#getQuotedObjectName(nexj.core.meta.persistence.sql.SQLObject)
       */
      protected String getQuotedObjectName(SQLObject object)
      {
         return null;
      }

      /**
       * @see nexj.core.meta.persistence.sql.SQLSubstReader#getQuotedKeyword(java.lang.String)
       */
      protected String getQuotedKeyword(String sName)
      {
         return sName;
      }

      /**
       * @see nexj.core.meta.persistence.sql.SQLSubstReader#getQuotedOwnerName()
       */
      protected String getQuotedOwnerName()
      {
         return m_sQuotedOwnerName;
      }

      /**
       * @see nexj.core.meta.persistence.sql.SQLSubstReader#getQuotedRoleName()
       */
      protected String getQuotedRoleName()
      {
         return null;
      }

      /**
       * @see nexj.core.meta.persistence.sql.SQLSubstReader#getQuotedTableName(nexj.core.meta.persistence.sql.Table)
       */
      protected String getQuotedTableName(Table table)
      {
         return m_sQuotedTableName;
      }

      /**
       * @see nexj.core.meta.persistence.sql.SQLSubstReader#getSysPublicId()
       */
      protected String getSysPublicId()
      {
         return null;
      }

      /**
       * @see nexj.core.meta.persistence.sql.SQLSubstReader#getSysUserAlias()
       */
      protected String getSysUserAlias()
      {
         return null;
      }

      /**
       * @see nexj.core.meta.persistence.sql.SQLSubstReader#getSysUserId()
       */
      protected String getSysUserId()
      {
         return null;
      }

      /**
       * @see nexj.core.meta.persistence.sql.SQLSubstReader#isColumnCaseInsensitive(nexj.core.meta.persistence.sql.Column)
       */
      protected boolean isColumnCaseInsensitive(Column column)
      {
         return m_bCI;
      }

      /**
       * @see nexj.core.meta.persistence.sql.SQLSubstReaderTest.MockSQLSubstReader#m_bCI
       */
      public void setColumnCaseInsensitive(boolean bCI)
      {
         m_bCI = bCI;
      }

      /**
       * @see nexj.core.meta.persistence.sql.SQLSubstReaderTest.MockSQLSubstReader#m_QuotedOwnerName
       */
      public String setQuotedOwnerName(String sQuotedOwnerName)
      {
         return m_sQuotedOwnerName = sQuotedOwnerName;
      }

      /**
       * @see nexj.core.meta.persistence.sql.SQLSubstReaderTest.MockSQLSubstReader#m_sStrCat
       */
      public void setQuotedTableName(String sQuotedTableName)
      {
         m_sQuotedTableName = sQuotedTableName;
      }
   };
}