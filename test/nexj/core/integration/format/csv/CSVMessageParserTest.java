// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.csv;


import java.io.StringWriter;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.TimeZone;

import junit.framework.TestCase;

import nexj.core.integration.IntegrationException;
import nexj.core.integration.MessageFormatter;
import nexj.core.integration.MessageParser;
import nexj.core.integration.io.StringInput;
import nexj.core.integration.io.WriterOutput;
import nexj.core.meta.MetadataException;
import nexj.core.meta.Primitive;
import nexj.core.meta.Repository;
import nexj.core.meta.integration.Format;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessageTable;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.InvocationContext;
import nexj.core.util.PagedArrayList;

public class CSVMessageParserTest extends TestCase
{
   protected Format m_format;
   protected MessageParser m_parser;
   protected MessageFormatter m_formatter;
   protected InvocationContext m_context;

   protected Message m_simple_message;
   protected Message m_tsv_message;
   protected Message m_noheader_message;
   protected Message m_doublequote_message;
   protected Message m_noquote_message;
   
   protected Message m_skippedcolumns_message;
   protected Message m_skippedOptionalBeginning_message;
   protected Message m_skippedRequiredBeginning_message;
   
   protected Message m_cisco_message;

   protected Message m_formatStringMessage;

   protected void setUp() throws Exception
   {
      m_context = (InvocationContext)Repository.getMetadata().getComponent("System.InvocationContext").getInstance(null);
      m_context.setLocale(Locale.ENGLISH);
      m_context.setTimeZone(TimeZone.getTimeZone("America/New_York"));

      m_format = Repository.getMetadata().getFormat("CSV");
      m_parser = (MessageParser)m_format.getParser().getInstance(m_context);
      m_formatter = (MessageFormatter)m_format.getFormatter().getInstance(m_context);
      m_simple_message = Repository.getMetadata().getMessage("CSV_SimpleTable");
      m_tsv_message = Repository.getMetadata().getMessage("CSV_SimpleTable_TSV");
      m_noheader_message = Repository.getMetadata().getMessage("CSV_SimpleTable_NoHeader");
      m_doublequote_message = Repository.getMetadata().getMessage("CSV_SimpleTable_DoubleQuoting");
      m_noquote_message = Repository.getMetadata().getMessage("CSV_SimpleTable_NoQuote");
      
      m_skippedcolumns_message = Repository.getMetadata().getMessage("CSV_SkippedColumns");
      m_skippedOptionalBeginning_message = Repository.getMetadata().getMessage("CSV_SkippedOptionalBeginning");
      m_skippedRequiredBeginning_message = Repository.getMetadata().getMessage("CSV_SkippedRequiredBeginning");
      
      m_cisco_message = Repository.getMetadata().getMessage("CSV_CiscoFormat");
      m_formatStringMessage = m_context.getMetadata().getMessage("CSV_FormatString");
   }

   /**
    * Tests basic parser operation, handling of optional fields, and comment characters.
    * 
    * Also tests that CSV input can be parsed if the input does not end in a newline.
    */
   public void testParseSimpleTable() throws Exception
   {
      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         ";\n" +
         "firstName,familyName,age,Phone1_location,Phone1_number,location,number,balance\n" +
         "John,Doe,42,,,,,108\n" +
         ";Jane,Doe,67,,,,,\n" +
         "Jane,Doe,,,,,,\n" +
         "# Old data follows:\n" +
         "#Jack,Lin,46,,,,,\n" +
         "#\n" +
         "Jack,Lin,26,,,,,"
         ), m_simple_message);
      
      
      assertEquals("CSV_SimpleTable", root.getClassName());
      
      List recordList = (List)root.getValue("Row");

      assertTrue(recordList instanceof PagedArrayList);
      assertEquals(3, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("John", record.getValue("firstName"));
      assertEquals("Doe",  record.getValue("lastName"));
      assertEquals(Primitive.createInteger(42), record.getValue("age"));
      assertEquals(BigDecimal.valueOf(108), record.getValue("balance"));
      assertFalse(record.hasValue("Phone1"));
      assertFalse(record.hasValue("Phone2"));
      assertNull(record.getClassName());
     
      record = (TransferObject)recordList.get(1);
      assertEquals("Jane", record.getValue("firstName"));
      assertEquals("Doe",  record.getValue("lastName"));
      assertFalse(record.hasValue("age"));
      assertFalse(record.hasValue("balance"));
      assertFalse(record.hasValue("Phone1"));
      assertFalse(record.hasValue("Phone2"));
      assertNull(record.getClassName());
      
      record = (TransferObject)recordList.get(2);
      assertEquals("Jack", record.getValue("firstName"));
      assertEquals("Lin",  record.getValue("lastName"));
      assertEquals(Primitive.createInteger(26), record.getValue("age"));
      assertFalse(record.hasValue("balance"));
      assertFalse(record.hasValue("Phone1"));
      assertFalse(record.hasValue("Phone2"));
      assertNull(record.getClassName());

      ((PagedArrayList)recordList).dispose();
   }

   /**
    * Tests that a message that doesn't have an escape or quote character
    * can still be parsed.
    */
   public void testParseSimpleTableNoEscapeOrQuote() throws Exception
   {
      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         ";\n" +
         "firstName,familyName,age,Phone1_location,Phone1_number,location,number,balance\n" +
         "John,Doe,42,,,,,108\n" +
         ";Jane,Doe,67,,,,,\n" +
         "Jane,Doe,,,,,,\n" +
         "# Old data follows:\n" +
         "#Jack,Lin,46,,,,,\n" +
         "#\n" +
         "Jack,Lin,26,,,,,"
         ), Repository.getMetadata().getMessage("CSV_SimpleTable_NoEscapeOrQuote"));


      assertEquals("CSV_SimpleTable_NoEscapeOrQuote", root.getClassName());

      List recordList = (List)root.getValue("Row");

      assertTrue(recordList instanceof PagedArrayList);
      assertEquals(3, recordList.size());

      record = (TransferObject)recordList.get(0);
      assertEquals("John", record.getValue("firstName"));
      assertEquals("Doe",  record.getValue("lastName"));
      assertEquals(Primitive.createInteger(42), record.getValue("age"));
      assertEquals(BigDecimal.valueOf(108), record.getValue("balance"));
      assertFalse(record.hasValue("Phone1"));
      assertFalse(record.hasValue("Phone2"));
      assertNull(record.getClassName());

      record = (TransferObject)recordList.get(1);
      assertEquals("Jane", record.getValue("firstName"));
      assertEquals("Doe",  record.getValue("lastName"));
      assertFalse(record.hasValue("age"));
      assertFalse(record.hasValue("balance"));
      assertFalse(record.hasValue("Phone1"));
      assertFalse(record.hasValue("Phone2"));
      assertNull(record.getClassName());

      record = (TransferObject)recordList.get(2);
      assertEquals("Jack", record.getValue("firstName"));
      assertEquals("Lin",  record.getValue("lastName"));
      assertEquals(Primitive.createInteger(26), record.getValue("age"));
      assertFalse(record.hasValue("balance"));
      assertFalse(record.hasValue("Phone1"));
      assertFalse(record.hasValue("Phone2"));
      assertNull(record.getClassName());

      ((PagedArrayList)recordList).dispose();
   }


   public void testParseSimpleTableWithCrazyNewlines() throws Exception
   {
      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         "firstName,familyName,age,Phone1_location,Phone1_number,location,number,balance\n\n\n\n\n" +  //multiple newline
         "John,Doe,42,,,,,108\r\n" +
         "Jane,Doe,37,,,,,\n\r" +  //crazy newline!
         "Jack,Lin,26,,,,,\r" +
         ",,,,,,,\n"
         ), m_simple_message);
      
      
      List recordList = (List)root.getValue("Row");
      
      assertEquals(4, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("John", record.getValue("firstName"));
      assertEquals("Doe",  record.getValue("lastName"));
      assertEquals(Primitive.createInteger(42), record.getValue("age"));
      assertEquals(BigDecimal.valueOf(108), record.getValue("balance"));
      assertFalse(record.hasValue("Phone1"));
      assertFalse(record.hasValue("Phone2"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("Jane", record.getValue("firstName"));
      assertEquals("Doe",  record.getValue("lastName"));
      assertEquals(Primitive.createInteger(37), record.getValue("age"));
      assertFalse(record.hasValue("balance"));
      assertFalse(record.hasValue("Phone1"));
      assertFalse(record.hasValue("Phone2"));
      
      record = (TransferObject)recordList.get(2);
      assertEquals("Jack", record.getValue("firstName"));
      assertEquals("Lin",  record.getValue("lastName"));
      assertEquals(Primitive.createInteger(26), record.getValue("age"));
      assertFalse(record.hasValue("balance"));
      assertFalse(record.hasValue("Phone1"));
      assertFalse(record.hasValue("Phone2"));
      
      record = (TransferObject)recordList.get(3);
      assertNull(record);
   }
   
   
   public void testParseSingleRow() throws Exception
   {
      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         "firstName,familyName,age,Phone1_location,Phone1_number,location,number,balance\n" +
         "John,Doe,42,,,,,108\n"         
         ), m_simple_message);
      
      
      List recordList = (List)root.getValue("Row");
      
      assertEquals(1, recordList.size());
      
      record = (TransferObject)recordList.get(0);      
      
      assertEquals("John", record.getValue("firstName"));
      assertEquals("Doe",  record.getValue("lastName"));
      assertEquals(Primitive.createInteger(42), record.getValue("age"));
      assertEquals(BigDecimal.valueOf(108), record.getValue("balance"));
      assertFalse(record.hasValue("Phone1"));
      assertFalse(record.hasValue("Phone2"));
   }


   /**
    * A CSV message with a header and zero rows of data should return a transfer object with
    * a collection of length zero.
    * 
    * (This was a real-world bug--it used to return a null transfer object)
    */
   public void testParseHeaderButNoData() throws Exception
   {
      List recordList;
      TransferObject root = m_parser.parse(new StringInput(
         "firstName,familyName,age,Phone1_location,Phone1_number,location,number,balance\n"
         ), m_simple_message);

      assertNotNull(root);
      recordList = (List)root.getValue("Row");
      assertTrue(recordList.isEmpty());
   }


   public void testParseSimpleTableAndLogicalComponents() throws Exception
   {
      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         "firstName,familyName,age,Phone1_location,Phone1_number,location,number,balance\n" +
         "John,Doe,42,,,,,108\n" +
         "Hui Mei,Zhang,37,cell,0932077773,,,\n" +
         "Jack,Lin,26,,,test,012345678,\n"
         ), m_simple_message);
      
      
      List recordList = (List)root.getValue("Row");
      
      assertEquals(3, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("John", record.getValue("firstName"));
      assertEquals("Doe",  record.getValue("lastName"));
      assertEquals(Primitive.createInteger(42), record.getValue("age"));
      assertEquals(BigDecimal.valueOf(108), record.getValue("balance"));
      assertFalse(record.hasValue("Phone1"));
      assertFalse(record.hasValue("Phone2"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("Hui Mei", record.getValue("firstName"));
      assertEquals("Zhang",  record.getValue("lastName"));
      assertEquals(Primitive.createInteger(37), record.getValue("age"));
      assertFalse(record.hasValue("balance"));
      assertFalse(record.hasValue("Phone2"));
      record = (TransferObject)record.getValue("Phone1");
      assertEquals("cell", record.getValue("location"));
      assertEquals("0932077773", record.getValue("number"));
      assertNull(record.getClassName());
      
      record = (TransferObject)recordList.get(2);
      assertEquals("Jack", record.getValue("firstName"));
      assertEquals("Lin",  record.getValue("lastName"));
      assertEquals(Primitive.createInteger(26), record.getValue("age"));
      assertFalse(record.hasValue("balance"));
      assertFalse(record.hasValue("Phone1"));
      record = (TransferObject)record.getValue("Phone2");
      assertEquals("test", record.getValue("location"));
      assertEquals("012345678", record.getValue("number"));
      assertNull(record.getClassName());
   }
   
   
   /**
    * Verify that the data fields may be reordered if a header is present.
    */
   public void testParseSimpleTableWithDifferentHeaderOrder() throws Exception
   {
      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         "familyName,Phone1_location,Phone1_number,location,number,balance,firstName,age\n" +
         "Doe,,,,,108,John,42\n" +
         "Zhang,cell,0932077773,,,,Hui Mei,37\n" +
         "Lin,,,test,012345678,,Jack,26\n"
         ), m_simple_message);
      
      
      List recordList = (List)root.getValue("Row");
      
      assertEquals(3, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("John", record.getValue("firstName"));
      assertEquals("Doe",  record.getValue("lastName"));
      assertEquals(Primitive.createInteger(42), record.getValue("age"));
      assertEquals(BigDecimal.valueOf(108), record.getValue("balance"));
      assertFalse(record.hasValue("Phone1"));
      assertFalse(record.hasValue("Phone2"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("Hui Mei", record.getValue("firstName"));
      assertEquals("Zhang",  record.getValue("lastName"));
      assertEquals(Primitive.createInteger(37), record.getValue("age"));
      assertFalse(record.hasValue("balance"));
      assertFalse(record.hasValue("Phone2"));
      record = (TransferObject)record.getValue("Phone1");
      assertEquals("cell", record.getValue("location"));
      assertEquals("0932077773", record.getValue("number"));
      
      record = (TransferObject)recordList.get(2);
      assertEquals("Jack", record.getValue("firstName"));
      assertEquals("Lin",  record.getValue("lastName"));
      assertEquals(Primitive.createInteger(26), record.getValue("age"));
      assertFalse(record.hasValue("balance"));
      assertFalse(record.hasValue("Phone1"));
      record = (TransferObject)record.getValue("Phone2");
      assertEquals("test", record.getValue("location"));
      assertEquals("012345678", record.getValue("number"));
   }
   
   
   /**
    * Verify that the data fields may be reordered if a header is present.
    */
   public void testParseSimpleTableWithMissingColumnsAndDifferentOrder() throws Exception
   {
      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         "familyName,Phone1_number,location,number,firstName,age\n" +
         "Doe,,,,John,42\n" +
         "Zhang,0932077773,,,Hui Mei,37\n" +
         "Lin,,test,012345678,Jack,26\n"
         ), m_simple_message);
      
      
      List recordList = (List)root.getValue("Row");
      
      assertEquals(3, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("John", record.getValue("firstName"));
      assertEquals("Doe",  record.getValue("lastName"));
      assertEquals(Primitive.createInteger(42), record.getValue("age"));
      assertFalse(record.hasValue("balance"));
      assertFalse(record.hasValue("Phone1"));
      assertFalse(record.hasValue("Phone2"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("Hui Mei", record.getValue("firstName"));
      assertEquals("Zhang",  record.getValue("lastName"));
      assertEquals(Primitive.createInteger(37), record.getValue("age"));
      assertFalse(record.hasValue("balance"));
      assertFalse(record.hasValue("Phone2"));
      record = (TransferObject)record.getValue("Phone1");
      assertFalse(record.hasValue("location"));
      assertEquals("0932077773", record.getValue("number"));
      
      record = (TransferObject)recordList.get(2);
      assertEquals("Jack", record.getValue("firstName"));
      assertEquals("Lin",  record.getValue("lastName"));
      assertEquals(Primitive.createInteger(26), record.getValue("age"));
      assertFalse(record.hasValue("balance"));
      assertFalse(record.hasValue("Phone1"));
      record = (TransferObject)record.getValue("Phone2");
      assertEquals("test", record.getValue("location"));
      assertEquals("012345678", record.getValue("number"));
   }
   
   
   /**
    * Verify exception is thrown if a field appears twice (i.e. a header name
    * is duplicated)
    */
   public void testErrorOnDuplicateFieldName() throws Exception
   {
      try
      {
         m_parser.parse(new StringInput(
            "familyName,Phone1_number,location,number,Phone1_number,firstName,age\n" +
            "Doe,,,,,John,42\n" +
            "Zhang,0932077773,,,0000,Hui Mei,37\n" +
            "Lin,,test,012345678,,Jack,26\n"
            ), m_simple_message);
         fail();
      }
      catch (IntegrationException outerEx)
      {
         Throwable innerEx = outerEx.getCause();
         
         if (!(innerEx instanceof IntegrationException))
         {
            throw outerEx;
         }
         
         IntegrationException realEx = (IntegrationException)innerEx;
         
         assertEquals("err.integration.csv.duplicateFieldInHeader", realEx.getErrorCode());
         assertEquals("Phone1_number", realEx.getErrorArgs()[0]);
         assertEquals(Primitive.createInteger(5), realEx.getErrorArgs()[1]);
      }
   }

   /**
    * Verifies that values are verified against the enumerations in the message.
    */
   public void testErrorOnEnumerationValue() throws Exception
   {
      try
      {
         m_parser.parse(new StringInput(
            "familyName,Phone1_number,location,number,firstName,age\n" +
            "Doe,,,,John,42\n" +
            "Zhang,0932077773,,,Hui Mei,37\n" +
            "Lin,,invalid,012345678,Jack,26\n"
            ), m_simple_message);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException outerEx)
      {
         Throwable innerEx = outerEx.getCause();

         if (!(innerEx instanceof MetadataException))
         {
            throw outerEx;
         }

         MetadataException realEx = (MetadataException)innerEx;

         assertEquals("err.meta.integration.enumerationValue", realEx.getErrorCode());
         assertEquals("invalid", realEx.getErrorArgs()[0]);
         assertEquals("CSV_SimpleTable Row Phone2 location", realEx.getErrorArgs()[1]);
      }
   }

   /**
    * Test that when the header isn't ignored we get all rows of data.
    * 
    * Test parsing laxness: allow the quote character to appear in the middle of
    * fields that aren't quoted.
    */
   public void testParseSimpleTableAndNoHeader() throws Exception
   {
      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         "'Doe,\\\u001eJohn\\\u001e,42,\r" +
         "\u001eZhang\u001e,Hui\\,Mei,37,2007-07-27 05:24:00.000000000\r" +
         "Li\u001en,Ja\u001e\u001eck,26,\r"
         ), m_noheader_message);
      
      
      List recordList = (List)root.getValue("Data");
      
      assertEquals(3, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("\u001eJohn\u001e", record.getValue("firstName"));
      assertEquals("'Doe",  record.getValue("lastName"));
      assertEquals(Primitive.createInteger(42), record.getValue("age"));
      assertFalse(record.hasValue("eventtime"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("Hui,Mei", record.getValue("firstName"));
      assertEquals("Zhang",  record.getValue("lastName"));
      assertEquals(Primitive.createInteger(37), record.getValue("age"));
      assertEquals(Primitive.toTimestamp(new Long(1185513840000L)), record.getValue("eventtime"));
      
      record = (TransferObject)recordList.get(2);
      assertEquals("Ja\u001e\u001eck", record.getValue("firstName"));
      assertEquals("Li\u001en",  record.getValue("lastName"));
      assertEquals(Primitive.createInteger(26), record.getValue("age"));
      assertFalse(record.hasValue("eventtime"));
   }
   
   
   /**
    * Test that when no escape character is specified, quoted fields can
    * escape a quote character with another quote character.
    * 
    * Test the laxness of this: when the field isn't quoted, quote characters
    * within the field behave normally.
    */
   public void testParseSimpleTableAndDoubleQuoting() throws Exception
   {
      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         "\\\"John\\\",'Doe,42,\r" +
         "\"Hui,Mei\",\"Zha\"\"ng\",37,\"2007-07-27 05:24:00.000000000\"\r" +
         "Ja\"\"ck,Li\"n,26,\r"
         ), m_doublequote_message);
      
      
      List recordList = (List)root.getValue("Data");
      
      assertEquals(3, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("\\\"John\\\"", record.getValue("firstName"));
      assertEquals("'Doe",  record.getValue("lastName"));
      assertEquals(Primitive.createInteger(42), record.getValue("age"));
      assertFalse(record.hasValue("eventtime"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("Hui,Mei", record.getValue("firstName"));
      assertEquals("Zha\"ng",  record.getValue("lastName"));
      assertEquals(Primitive.createInteger(37), record.getValue("age"));
      assertEquals(Primitive.toTimestamp(new Long(1185513840000L)), record.getValue("eventtime"));
      
      record = (TransferObject)recordList.get(2);
      assertEquals("Ja\"\"ck", record.getValue("firstName"));
      assertEquals("Li\"n",  record.getValue("lastName"));
      assertEquals(Primitive.createInteger(26), record.getValue("age"));
      assertFalse(record.hasValue("eventtime"));
   }
   
   
   /**
    * Test parsing with only escape character an no quote character.
    */
   public void testParseSimpleTableNoQuote() throws Exception
   {
      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         "\\\"John\\\",'Doe,42,\r" +
         "Hui\\,Mei,\"Zha\"\"ng\",37,2007-07-27 05:24:00.000000000\r" +
         "Ja\"\"ck,Li\"n,26,\r"
         ), m_noquote_message);
      
      
      List recordList = (List)root.getValue("Data");
      
      assertEquals(3, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("\"John\"", record.getValue("firstName"));
      assertEquals("'Doe",  record.getValue("lastName"));
      assertEquals(Primitive.createInteger(42), record.getValue("age"));
      assertFalse(record.hasValue("eventtime"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("Hui,Mei", record.getValue("firstName"));
      assertEquals("\"Zha\"\"ng\"",  record.getValue("lastName"));
      assertEquals(Primitive.createInteger(37), record.getValue("age"));
      assertEquals(Primitive.toTimestamp(new Long(1185513840000L)), record.getValue("eventtime"));
      
      record = (TransferObject)recordList.get(2);
      assertEquals("Ja\"\"ck", record.getValue("firstName"));
      assertEquals("Li\"n",  record.getValue("lastName"));
      assertEquals(Primitive.createInteger(26), record.getValue("age"));
      assertFalse(record.hasValue("eventtime"));
   }
   
   
   /**
    * Test that newlines get counted as data when they are inside a quoted field. If the
    * newline is escaped, then the escape is dropped and the newline remains.
    * 
    * Also tests that parse can handle EOF on comment line.
    */
   public void testParseSimpleTableWithMultilineFields() throws Exception
   {
      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         "\u001eDoe\ra\r\ndeer\n\n\ndoe\u001e,John,42,\r" +
         "Zhang,\u001eHui\\\nMei\u001e,37,2007-07-27 05:24:00.000000000\r" +
         "Lin,Jack,26,\r" +
         ";"
         ), m_noheader_message);
      
      
      List recordList = (List)root.getValue("Data");
      
      assertEquals(3, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("John", record.getValue("firstName"));
      assertEquals("Doe\ra\r\ndeer\n\n\ndoe",  record.getValue("lastName"));
      assertEquals(Primitive.createInteger(42), record.getValue("age"));
      assertFalse(record.hasValue("eventtime"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("Hui\nMei", record.getValue("firstName"));
      assertEquals("Zhang",  record.getValue("lastName"));
      assertEquals(Primitive.createInteger(37), record.getValue("age"));
      assertEquals(Primitive.toTimestamp(new Long(1185513840000L)), record.getValue("eventtime"));
      
      record = (TransferObject)recordList.get(2);
      assertEquals("Jack", record.getValue("firstName"));
      assertEquals("Lin",  record.getValue("lastName"));
      assertEquals(Primitive.createInteger(26), record.getValue("age"));
      assertFalse(record.hasValue("eventtime"));
   }
   
   
   /**
    * Test behaviour when a row doesn't have all fields filled
    * 
    * The parser should look at the message definition to determine which
    * fields are required and which are optional.
    * 
    * Also tests that the file can still be parsed if it doesn't end
    * with a newline.
    */
   public void testParseSimpleTableWithOptionalFields() throws Exception
   {
      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         "Doe,John\r" +
         "Zhang,Hui Mei,37,2007-07-27 05:24:00.000000000\r" +
         ",Jack\r" +  //Required field is null but not blank
         "Lin"  //still valid: message has only one required field.
         ), m_noheader_message);
      
      
      List recordList = (List)root.getValue("Data");
      
      assertEquals(4, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("John", record.getValue("firstName"));
      assertEquals("Doe",  record.getValue("lastName"));
      assertFalse(record.hasValue("age"));
      assertFalse(record.hasValue("eventtime"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("Hui Mei", record.getValue("firstName"));
      assertEquals("Zhang",  record.getValue("lastName"));
      assertEquals(Primitive.createInteger(37), record.getValue("age"));
      assertEquals(Primitive.toTimestamp(new Long(1185513840000L)), record.getValue("eventtime"));
      
      record = (TransferObject)recordList.get(2);
      assertEquals(null, record.getValue("lastName"));
      assertEquals("Jack", record.getValue("firstName"));
      assertFalse(record.hasValue("age"));
      assertFalse(record.hasValue("eventtime"));
      
      record = (TransferObject)recordList.get(3);
      assertEquals("Lin", record.getValue("lastName"));
      assertFalse(record.hasValue("firstName"));
      assertFalse(record.hasValue("age"));
      assertFalse(record.hasValue("eventtime"));
   }
      
   
   /**
    * Test quoting to make sure it deals with the boundary case of the
    * last field, and also if file ends without a newline.
    */
   public void testParseSimpleTableWithQuotedLastField() throws Exception
   {
      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         "Doe,\u001eJohn\u001e\r" +
         "Zhang,Hui Mei,37,2007-07-27 05:24:00.000000000\r" +
         "\u001eLin\u001e"
         ), m_noheader_message);
      
      
      List recordList = (List)root.getValue("Data");
      
      assertEquals(3, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("John", record.getValue("firstName"));
      assertEquals("Doe",  record.getValue("lastName"));
      assertFalse(record.hasValue("age"));
      assertFalse(record.hasValue("eventtime"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("Hui Mei", record.getValue("firstName"));
      assertEquals("Zhang",  record.getValue("lastName"));
      assertEquals(Primitive.createInteger(37), record.getValue("age"));
      assertEquals(Primitive.toTimestamp(new Long(1185513840000L)), record.getValue("eventtime"));
      
      record = (TransferObject)recordList.get(2);
      assertEquals("Lin", record.getValue("lastName"));
      assertFalse(record.hasValue("firstName"));
      assertFalse(record.hasValue("age"));
      assertFalse(record.hasValue("eventtime"));
   }
   
   
   /**
    * Unexpected EOF: End of File inside a quoted field.
    */
   public void testParseSimpleTableWithEofBeforeCloseQuote() throws Exception
   {
      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         "Doe,\u001eJohn\u001e\r" +
         "Zhang,Hui Mei,37,2007-07-27 05:24:00.000000000\r" +
         "\u001eLin"
         ), m_noheader_message);
      
      
      List recordList = (List)root.getValue("Data");
      
      assertEquals(3, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("John", record.getValue("firstName"));
      assertEquals("Doe",  record.getValue("lastName"));
      assertFalse(record.hasValue("age"));
      assertFalse(record.hasValue("eventtime"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("Hui Mei", record.getValue("firstName"));
      assertEquals("Zhang",  record.getValue("lastName"));
      assertEquals(Primitive.createInteger(37), record.getValue("age"));
      assertEquals(Primitive.toTimestamp(new Long(1185513840000L)), record.getValue("eventtime"));
      
      record = (TransferObject)recordList.get(2);
      assertEquals("Lin", record.getValue("lastName"));
      assertFalse(record.hasValue("firstName"));
      assertFalse(record.hasValue("age"));
      assertFalse(record.hasValue("eventtime"));
   }
   
   
   
   /**
    * Unexpected EOF: Immediately after an open quote.
    */
   public void testParseSimpleTableWithEofAfterOpenQuote() throws Exception
   {
      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         "Doe,\u001eJohn\u001e\r" +
         "Zhang,\u001e"
         ), m_noheader_message);
      
      
      List recordList = (List)root.getValue("Data");
      
      assertEquals(2, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("John", record.getValue("firstName"));
      assertEquals("Doe",  record.getValue("lastName"));
      assertFalse(record.hasValue("age"));
      assertFalse(record.hasValue("eventtime"));
     
      record = (TransferObject)recordList.get(1);
      assertFalse(record.hasValue("firstName"));
      assertFalse(record.hasValue("age"));
      assertFalse(record.hasValue("eventtime"));
      assertEquals("Zhang",  record.getValue("lastName"));
   }
   
   
   /**
    * Unexpected EOF: Immediately after a close quote.
    */
   public void testParseSimpleTableWithEofAfterCloseQuote() throws Exception
   {
      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         "Doe,\u001eJohn\u001e\r" +
         "Zhang,\u001eSarah\u001e"
         ), m_noheader_message);
      
      
      List recordList = (List)root.getValue("Data");
      
      assertEquals(2, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("John", record.getValue("firstName"));
      assertEquals("Doe",  record.getValue("lastName"));
      assertFalse(record.hasValue("age"));
      assertFalse(record.hasValue("eventtime"));
     
      record = (TransferObject)recordList.get(1);
      assertFalse(record.hasValue("age"));
      assertFalse(record.hasValue("eventtime"));
      assertEquals("Zhang",  record.getValue("lastName"));
      assertEquals("Sarah",  record.getValue("firstName"));
   }
   
   
   /**
    * Unexpected EOF: Immediately after an escape character.
    */
   public void testParseSimpleTableWithEofAfterEscape() throws Exception
   {
      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         "Doe,\u001eJohn\u001e\r" +
         "Zhang,Hui Mei\\"
         ), m_noheader_message);
      
      
      List recordList = (List)root.getValue("Data");
      
      assertEquals(2, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("John", record.getValue("firstName"));
      assertEquals("Doe",  record.getValue("lastName"));
      assertFalse(record.hasValue("age"));
      assertFalse(record.hasValue("eventtime"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("Hui Mei", record.getValue("firstName"));
      assertEquals("Zhang",  record.getValue("lastName"));
      assertFalse(record.hasValue("age"));
      assertFalse(record.hasValue("eventtime"));
   }
   
   
   /**
    * Unexpected EOF: Inside quotes, immediately after an escape character.
    */
   public void testParseSimpleTableWithEofAfterEscapeInQuotes() throws Exception
   {
      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         "Doe,\u001eJohn\u001e\r" +
         "Zhang,\u001eHui Mei\\"
         ), m_noheader_message);
      
      
      List recordList = (List)root.getValue("Data");
      
      assertEquals(2, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("John", record.getValue("firstName"));
      assertEquals("Doe",  record.getValue("lastName"));
      assertFalse(record.hasValue("age"));
      assertFalse(record.hasValue("eventtime"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("Hui Mei", record.getValue("firstName"));
      assertEquals("Zhang",  record.getValue("lastName"));
      assertFalse(record.hasValue("age"));
      assertFalse(record.hasValue("eventtime"));
   }
   
   
   /**
    * Verify that the correct exception is thrown when a required
    * field is missing in the input.
    */
   public void testMissingRequiredField() throws Exception
   {
      StringInput input = new StringInput(
         "a1,a2,a3,a4,a5,a6\r" +
         "b1,b2,b3,b4,b5\r" +
         "c1,c2,c3,c4\r"
         );
      
      try
      {
         m_parser.parse(input, m_cisco_message);
         fail();
      }
      catch (IntegrationException outerEx)
      {
         Throwable innerEx = outerEx.getCause();
         
         if (!(innerEx instanceof IntegrationException))
         {
            //Re-throw, if it isn't what we're looking for.
            throw outerEx;
         }
         
         IntegrationException realEx = (IntegrationException)innerEx;
         
         assertEquals("err.integration.minPartCount", realEx.getErrorCode());
         assertEquals("CSV_CiscoFormat Data userField1", realEx.getErrorArgs()[0]);         
      }
   }
   
   
   /**
    * This Message has data in the first and fifth columns, and the
    * other columns are unmapped. All columns are optional.
    */
   public void testParseSkippedColumns() throws Exception
   {
      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         "Doe,,,,John\n" +
         "Doe,,,,Jane\n" +
         "Lin,,,\n"
         ), m_skippedcolumns_message);
      
      
      List recordList = (List)root.getValue("Data");
      
      assertEquals(3, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("John", record.getValue("firstName"));
      assertEquals("Doe",  record.getValue("lastName"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("Jane", record.getValue("firstName"));
      assertEquals("Doe",  record.getValue("lastName"));
      
      record = (TransferObject)recordList.get(2);
      assertFalse(record.hasValue("firstName"));
      assertEquals("Lin",  record.getValue("lastName"));
   }
   
   
   /**
    * This Message has data in the third and sixth columns, and the
    * other columns are unmapped. All columns are optional.
    */
   public void testParseSkippedOptionalBeginning() throws Exception
   {
      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         "a1,a2,realdata1,a4,a5,realdata2\n" +
         ",,nextrealdata1,,,nextrealdata2\n" +
         "c1,c2\n"+
         ",\n"
         ), m_skippedOptionalBeginning_message);
      
      
      List recordList = (List)root.getValue("Data");
      
      assertEquals(4, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("realdata1", record.getValue("first"));
      assertEquals("realdata2",  record.getValue("second"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("nextrealdata1", record.getValue("first"));
      assertEquals("nextrealdata2",  record.getValue("second"));
      
      record = (TransferObject)recordList.get(2);
      assertEquals(null, record);
      
      record = (TransferObject)recordList.get(3);
      assertEquals(null, record);
   }
   
   
   /**
    * This Message has data in the third and sixth columns, and the
    * other columns are unmapped. All columns are required.
    */
   public void testParseSkippedRequiredBeginning1() throws Exception
   {
      StringInput input = new StringInput(
         "a1,a2,realdata1,a4,a5,realdata2\n" +
         ",,nextrealdata1,,,nextrealdata2\n" +
         "c1,c2\n"
         );
      
      try
      {
         m_parser.parse(input, m_skippedRequiredBeginning_message);
         fail();
      }
      catch (IntegrationException outerEx)
      {
         Throwable innerEx = outerEx.getCause();

         if (!(innerEx instanceof IntegrationException))
         {
            //Re-throw, if it isn't what we're looking for.
            throw outerEx;
         }
         
         IntegrationException realEx = (IntegrationException)innerEx;
         
         assertEquals("err.integration.minPartCount", realEx.getErrorCode());
         assertEquals("CSV_SkippedRequiredBeginning Data first", realEx.getErrorArgs()[0]);         
      }
   }
   

   /**
    * This Message has data in the third and sixth columns, and the
    * other columns are unmapped. All columns are required.
    * 
    * Test that correct exception is generated when the missing
    * piece of required data is not mapped to any field at all.
    */
   public void testParseSkippedRequiredBeginning2() throws Exception
   {
      StringInput input = new StringInput(
         "a1,a2,realdata1,a4,a5,realdata2\n" +
         ",,nextrealdata1,,,nextrealdata2\n" +
         "c1\n"  //second column is missing, unmapped, but required (because third column is required)
         );
      
      try
      {
         m_parser.parse(input, m_skippedRequiredBeginning_message);
         fail();
      }
      catch (IntegrationException outerEx)
      {
         Throwable innerEx = outerEx.getCause();
         
         if (!(innerEx instanceof IntegrationException))
         {
            //Re-throw, if it isn't what we're looking for.
            throw outerEx;
         }
         
         IntegrationException realEx = (IntegrationException)innerEx;
         
         assertEquals("err.integration.minPartCount", realEx.getErrorCode());
         assertEquals("CSV_SkippedRequiredBeginning Data first", realEx.getErrorArgs()[0]);         
      }
   }
   
   
   /**
    * Tests that two Message objects, both with headers but different
    * delimiter/quote/escape, cannot be added to the message table.
    */
   public void testInvalidMessageTableWithHeaders() throws Exception
   {
      MessageTable table = new MessageTable();
      
      table.addMessage(m_simple_message);
      table.addMessage(m_tsv_message);
      
      try
      {
         m_parser.initializeMessageTable(table);
         fail();
      }
      catch (IntegrationException ex)
      {         
         assertEquals("err.integration.csv.messageTableConflict", ex.getErrorCode());
         assertEquals("CSV_SimpleTable", ex.getErrorArgs()[0]);
         assertEquals("CSV_SimpleTable_TSV", ex.getErrorArgs()[1]);
      }
   }
   
   
   /**
    * Messages without can only be distinguished from eachother if they have
    * the same configuration (delimiter, quoting, etc.).
    */
   public void testInvalidMessageTableDifferentConfigurations() throws Exception
   {
      MessageTable table = new MessageTable();
      
      table.addMessage(m_noheader_message);
      table.addMessage(m_noquote_message);
      
      try
      {
         m_parser.initializeMessageTable(table);
         fail();
      }
      catch (IntegrationException ex)
      {         
         assertEquals("err.integration.csv.messageTableConflict", ex.getErrorCode());
         assertEquals("CSV_SimpleTable_NoHeader", ex.getErrorArgs()[0]);
         assertEquals("CSV_SimpleTable_NoQuote", ex.getErrorArgs()[1]);
      }
   }
   
   
   /**
    * Messages do not conflict:
    * 
    * M1: Req, Req, Req, Req
    * M2: Req, Req, Req
    * 
    * First data row must contain either 3 or 4 fields, and so
    * correct Message may be determined.
    */
   public void testGoodMessageTableNoHeadersLengthDifferent1() throws Exception
   {
      MessageTable table = new MessageTable();
      
      table.addMessage(Repository.getMetadata().getMessage("CSV_TableConflict_4Required"));
      table.addMessage(Repository.getMetadata().getMessage("CSV_TableConflict_3Required"));
      
      m_parser.initializeMessageTable(table);
      
      TransferObject root, record;
      List recordList;
      
      root = m_parser.parse(new StringInput(
         "a,b,c,d\n" +
         "a2,b2,c2,d2\n"
         ), table);
      
      recordList = (List)root.getValue("Data4R");
      
      assertEquals(2, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("a", record.getValue("first"));
      assertEquals("b", record.getValue("second"));
      assertEquals("c", record.getValue("third"));
      assertEquals("d", record.getValue("fourth"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("a2", record.getValue("first"));
      assertEquals("b2", record.getValue("second"));
      assertEquals("c2", record.getValue("third"));
      assertEquals("d2", record.getValue("fourth"));


      // Try parsing a single row of data (motivated by a real-world bug)
      root = m_parser.parse(new StringInput(
         "a,b,c,d\n"
         ), table);

      assertNotNull(root);
      recordList = (List)root.getValue("Data4R");

      assertEquals(1, recordList.size());

      record = (TransferObject)recordList.get(0);
      assertEquals("a", record.getValue("first"));
      assertEquals("b", record.getValue("second"));
      assertEquals("c", record.getValue("third"));
      assertEquals("d", record.getValue("fourth"));



      root = m_parser.parse(new StringInput(
         "a,b,c\n" +
         "a2,b2,c2\n"
         ), table);
      
      recordList = (List)root.getValue("Data3R");
      
      assertEquals(2, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("a", record.getValue("first"));
      assertEquals("b", record.getValue("second"));
      assertEquals("c", record.getValue("third"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("a2", record.getValue("first"));
      assertEquals("b2", record.getValue("second"));
      assertEquals("c2", record.getValue("third"));


      // Try parsing a single row of data (motivated by a real-world bug)
      root = m_parser.parse(new StringInput(
         "a,b,c\n"
         ), table);

      assertNotNull(root);
      recordList = (List)root.getValue("Data3R");

      assertEquals(1, recordList.size());

      record = (TransferObject)recordList.get(0);
      assertEquals("a", record.getValue("first"));
      assertEquals("b", record.getValue("second"));
      assertEquals("c", record.getValue("third"));



      try
      {
         root = m_parser.parse(new StringInput(
            "a,b\n" +
            "a2,b2\n"
            ), table);
         fail();
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.csv.undetectableMessage", ex.getErrorCode());
      }
      
      try
      {
         root = m_parser.parse(new StringInput(
            "a,b,c,d,e\n" +
            "a2,b2,c2,d2,e2\n"
            ), table);
         fail();
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.csv.undetectableMessage", ex.getErrorCode());
      }
   }
   
   
   /**
    * Messages do not conflict:
    * 
    * M1: Req, Req, Req, Req
    * M2: Opt, Opt, Opt
    * 
    * First data row may have 4 fields, implying M1. If it has
    * 3, 2, 1, or 0 fields, then M2 is implied.
    */
   public void testGoodMessageTableNoHeadersLengthDifferent2() throws Exception
   {
      MessageTable table = new MessageTable();
      
      table.addMessage(Repository.getMetadata().getMessage("CSV_TableConflict_4Required"));
      table.addMessage(Repository.getMetadata().getMessage("CSV_TableConflict_3Optional"));
      
      m_parser.initializeMessageTable(table);
      
      TransferObject root, record;
      List recordList;
      
      root = m_parser.parse(new StringInput(
         "a,b,c,d\n" +
         "a2,b2,c2,d2\n"
         ), table);
      
      recordList = (List)root.getValue("Data4R");
      
      assertEquals(2, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("a", record.getValue("first"));
      assertEquals("b", record.getValue("second"));
      assertEquals("c", record.getValue("third"));
      assertEquals("d", record.getValue("fourth"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("a2", record.getValue("first"));
      assertEquals("b2", record.getValue("second"));
      assertEquals("c2", record.getValue("third"));
      assertEquals("d2", record.getValue("fourth"));
      
      
      
      root = m_parser.parse(new StringInput(
         "a,b,c\n" +
         "a2,b2,c2\n"
         ), table);
      
      recordList = (List)root.getValue("Data3O");
      
      assertEquals(2, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("a", record.getValue("first"));
      assertEquals("b", record.getValue("second"));
      assertEquals("c", record.getValue("third"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("a2", record.getValue("first"));
      assertEquals("b2", record.getValue("second"));
      assertEquals("c2", record.getValue("third"));
      
      
      root = m_parser.parse(new StringInput(
         "; A comment here should not ruin things.\r\n" +
         "a,b\n" +
         "; Nor should a comment here...\n" +
         "a2,b2,c2\n"
         ), table);
      
      recordList = (List)root.getValue("Data3O");
      
      assertEquals(2, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("a", record.getValue("first"));
      assertEquals("b", record.getValue("second"));
      assertFalse(record.hasValue("third"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("a2", record.getValue("first"));
      assertEquals("b2", record.getValue("second"));
      assertEquals("c2", record.getValue("third"));
      
      
      root = m_parser.parse(new StringInput(
         "a\n" +
         "a2,b2,c2\n"
         ), table);
      
      recordList = (List)root.getValue("Data3O");
      
      assertEquals(2, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("a", record.getValue("first"));
      assertFalse(record.hasValue("second"));
      assertFalse(record.hasValue("third"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("a2", record.getValue("first"));
      assertEquals("b2", record.getValue("second"));
      assertEquals("c2", record.getValue("third"));
      
      
      try
      {
         root = m_parser.parse(new StringInput(
            "a,b,c,d,e\n" +
            "a2,b2,c2,d2,e2\n"
            ), table);
         fail();
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.csv.undetectableMessage", ex.getErrorCode());
      }
   }
   
   
   /**
    * Messages do not conflict:
    * 
    * M1: Req, Req, Req, Req
    * M2: Req, Req, Opt
    * 
    * First data row may have 4 fields, implying M1. If it has
    * 3 or 2 fields, then M2 is implied.
    */
   public void testGoodMessageTableNoHeadersLengthDifferent3() throws Exception
   {
      MessageTable table = new MessageTable();
      
      table.addMessage(Repository.getMetadata().getMessage("CSV_TableConflict_4Required"));
      table.addMessage(Repository.getMetadata().getMessage("CSV_TableConflict_2Required1Optional"));
      
      m_parser.initializeMessageTable(table);
      
      TransferObject root, record;
      List recordList;
      
      root = m_parser.parse(new StringInput(
         "a,b,c,d\n" +
         "a2,b2,c2,d2\n"
         ), table);
      
      recordList = (List)root.getValue("Data4R");
      
      assertEquals(2, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("a", record.getValue("first"));
      assertEquals("b", record.getValue("second"));
      assertEquals("c", record.getValue("third"));
      assertEquals("d", record.getValue("fourth"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("a2", record.getValue("first"));
      assertEquals("b2", record.getValue("second"));
      assertEquals("c2", record.getValue("third"));
      assertEquals("d2", record.getValue("fourth"));
      
      
      
      root = m_parser.parse(new StringInput(
         "a,b,c\n" +
         "a2,b2,c2\n"
         ), table);
      
      recordList = (List)root.getValue("Data2R1O");
      
      assertEquals(2, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("a", record.getValue("first"));
      assertEquals("b", record.getValue("second"));
      assertEquals("c", record.getValue("third"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("a2", record.getValue("first"));
      assertEquals("b2", record.getValue("second"));
      assertEquals("c2", record.getValue("third"));
      
      
      root = m_parser.parse(new StringInput(
         "a,b\n" +
         "a2,b2,c2\n"
         ), table);
      
      recordList = (List)root.getValue("Data2R1O");
      
      assertEquals(2, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("a", record.getValue("first"));
      assertEquals("b", record.getValue("second"));
      assertFalse(record.hasValue("third"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("a2", record.getValue("first"));
      assertEquals("b2", record.getValue("second"));
      assertEquals("c2", record.getValue("third"));
      

      try
      {
         root = m_parser.parse(new StringInput(
            "a\n" +
            "a2,b2,c2\n"
            ), table);
         fail();
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.csv.undetectableMessage", ex.getErrorCode());
      }
   }
   
   
   /**
    * Messages do not conflict:
    * 
    * M1: Hdr, Hdr, Hdr, Hdr  (all fields required)
    * M2: Opt, Opt, Opt
    * 
    * First data row may have 4 header fields, implying M1. If it has
    * 3, 2, 1, or 0 data fields, then M2 is implied.
    */
   public void testGoodMessageTableNoHeadersLengthDifferent4() throws Exception
   {
      MessageTable table = new MessageTable();
      
      table.addMessage(Repository.getMetadata().getMessage("CSV_TableConflict_4HeaderAllRequired"));
      table.addMessage(Repository.getMetadata().getMessage("CSV_TableConflict_3Optional"));
      
      m_parser.initializeMessageTable(table);
      
      TransferObject root, record;
      List recordList;
      
      root = m_parser.parse(new StringInput(
         "first,second,third,fourth\n" +
         "a,b,c,d\n" +
         "a2,b2,c2,d2\n"
         ), table);

      assertEquals("CSV_TableConflict_4HeaderAllRequired", root.getClassName());
      recordList = (List)root.getValue("Data4H");
      
      assertEquals(2, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("a", record.getValue("first"));
      assertEquals("b", record.getValue("second"));
      assertEquals("c", record.getValue("third"));
      assertEquals("d", record.getValue("fourth"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("a2", record.getValue("first"));
      assertEquals("b2", record.getValue("second"));
      assertEquals("c2", record.getValue("third"));
      assertEquals("d2", record.getValue("fourth"));
      
      
      
      //Test re-ordered header
      root = m_parser.parse(new StringInput(
         "fourth,second,third,first\n" +
         "d,b,c,a\n" +
         "d2,b2,c2,a2\n"
         ), table);
      
      recordList = (List)root.getValue("Data4H");
      
      assertEquals(2, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("a", record.getValue("first"));
      assertEquals("b", record.getValue("second"));
      assertEquals("c", record.getValue("third"));
      assertEquals("d", record.getValue("fourth"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("a2", record.getValue("first"));
      assertEquals("b2", record.getValue("second"));
      assertEquals("c2", record.getValue("third"));
      assertEquals("d2", record.getValue("fourth"));
      

      // Test header but no data.
      root = m_parser.parse(new StringInput(
         "fourth,second,third,first\n"
         ), table);

      assertNotNull(root);
      assertEquals("CSV_TableConflict_4HeaderAllRequired", root.getClassName());
      recordList = (List)root.getValue("Data4H");
      assertTrue(recordList.isEmpty());


      
      root = m_parser.parse(new StringInput(
         "a,b,c\n" +
         "a2,b2,c2\n"
         ), table);

      assertEquals("CSV_TableConflict_3Optional", root.getClassName());
      recordList = (List)root.getValue("Data3O");
      
      assertEquals(2, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("a", record.getValue("first"));
      assertEquals("b", record.getValue("second"));
      assertEquals("c", record.getValue("third"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("a2", record.getValue("first"));
      assertEquals("b2", record.getValue("second"));
      assertEquals("c2", record.getValue("third"));
      
      
      root = m_parser.parse(new StringInput(
         "a,b\n" +
         "a2,b2,c2\n"
         ), table);

      assertEquals("CSV_TableConflict_3Optional", root.getClassName());
      recordList = (List)root.getValue("Data3O");
      
      assertEquals(2, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("a", record.getValue("first"));
      assertEquals("b", record.getValue("second"));
      assertFalse(record.hasValue("third"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("a2", record.getValue("first"));
      assertEquals("b2", record.getValue("second"));
      assertEquals("c2", record.getValue("third"));
      
      
      root = m_parser.parse(new StringInput(
         "a\n" +
         "a2,b2,c2\n"
         ), table);

      assertEquals("CSV_TableConflict_3Optional", root.getClassName());
      recordList = (List)root.getValue("Data3O");
      
      assertEquals(2, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("a", record.getValue("first"));
      assertFalse(record.hasValue("second"));
      assertFalse(record.hasValue("third"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("a2", record.getValue("first"));
      assertEquals("b2", record.getValue("second"));
      assertEquals("c2", record.getValue("third"));
      
      
      try
      {
         root = m_parser.parse(new StringInput(
            "a,b,c,d,e\n" +
            "a2,b2,c2,d2,e2\n"
            ), table);
         fail();
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.csv.undetectableMessage", ex.getErrorCode());
      }
   }
   
   
   
   /**
    * Messages do not conflict:
    * 
    * M1: Req, Req, Req, Req, Req
    * M2: Hdr, Hdr, Hdr, Hdr  (all required)
    * M3: Req, Req, Opt
    * 
    * 5 fields => M1
    * 4 fields => M2 (with first row header)
    * 3, 2 fields => M3
    * 
    * Also tests that leading newlines are ignored.
    */
   public void testGoodMessageTableNoHeadersLengthDifferent5() throws Exception
   {
      MessageTable table = new MessageTable();
      
      table.addMessage(Repository.getMetadata().getMessage("CSV_TableConflict_4HeaderAllRequired"));
      table.addMessage(Repository.getMetadata().getMessage("CSV_TableConflict_2Required1Optional"));
      table.addMessage(Repository.getMetadata().getMessage("CSV_TableConflict_5Required"));
      
      m_parser.initializeMessageTable(table);
      
      TransferObject root, record;
      List recordList;
      
      root = m_parser.parse(new StringInput(
         "\nfirst,second,third,fourth\n" +
         "a,b,c,d\n" +
         "a2,b2,c2,d2\n"
         ), table);
      
      recordList = (List)root.getValue("Data4H");
      
      assertEquals(2, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("a", record.getValue("first"));
      assertEquals("b", record.getValue("second"));
      assertEquals("c", record.getValue("third"));
      assertEquals("d", record.getValue("fourth"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("a2", record.getValue("first"));
      assertEquals("b2", record.getValue("second"));
      assertEquals("c2", record.getValue("third"));
      assertEquals("d2", record.getValue("fourth"));
      
      
      //Test re-ordered header
      root = m_parser.parse(new StringInput(
         "fourth,second,third,first\n" +
         "d,b,c,a\n" +
         "d2,b2,c2,a2\n"
         ), table);
      
      recordList = (List)root.getValue("Data4H");
      
      assertEquals(2, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("a", record.getValue("first"));
      assertEquals("b", record.getValue("second"));
      assertEquals("c", record.getValue("third"));
      assertEquals("d", record.getValue("fourth"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("a2", record.getValue("first"));
      assertEquals("b2", record.getValue("second"));
      assertEquals("c2", record.getValue("third"));
      assertEquals("d2", record.getValue("fourth"));
      
      
      
      
      root = m_parser.parse(new StringInput(
         "\n\na,b,c\n" +
         "a2,b2,c2\n"
         ), table);
      
      recordList = (List)root.getValue("Data2R1O");
      
      assertEquals(2, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("a", record.getValue("first"));
      assertEquals("b", record.getValue("second"));
      assertEquals("c", record.getValue("third"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("a2", record.getValue("first"));
      assertEquals("b2", record.getValue("second"));
      assertEquals("c2", record.getValue("third"));
      
      
      root = m_parser.parse(new StringInput(
         "\n\n\na,b\n" +
         "a2,b2,c2\n"
         ), table);
      
      recordList = (List)root.getValue("Data2R1O");
      
      assertEquals(2, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("a", record.getValue("first"));
      assertEquals("b", record.getValue("second"));
      assertFalse(record.hasValue("third"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("a2", record.getValue("first"));
      assertEquals("b2", record.getValue("second"));
      assertEquals("c2", record.getValue("third"));
      
      
      root = m_parser.parse(new StringInput(
         "\n\n\n\na,b,c,d,e\n" +
         "a2,b2,c2,d2,e2\n"
         ), table);
      
      recordList = (List)root.getValue("Data5R");
      
      assertEquals(2, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("a", record.getValue("first"));
      assertEquals("b", record.getValue("second"));
      assertEquals("c", record.getValue("third"));
      assertEquals("d", record.getValue("fourth"));
      assertEquals("e", record.getValue("fifth"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("a2", record.getValue("first"));
      assertEquals("b2", record.getValue("second"));
      assertEquals("c2", record.getValue("third"));
      assertEquals("d2", record.getValue("fourth"));
      assertEquals("e2", record.getValue("fifth"));
      
      
      
      try
      {
         root = m_parser.parse(new StringInput(
            "a,b,c,d,e,f\n" +
            "a2,b2,c2,d2,e2,f2\n"
            ), table);
         fail();
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.csv.undetectableMessage", ex.getErrorCode());
      }
      
      try
      {
         root = m_parser.parse(new StringInput(
            "a\n" +
            "a2,b2,c2,d2\n"
            ), table);
         fail();
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.csv.undetectableMessage", ex.getErrorCode());
      }
   }
   
   
   /**
    * Messages do not conflict:
    * 
    * M1: Req, Req, Req, Req, Req
    * M2: Hdr, Hdr, Hdr, Hdr  (last optional)
    * M3: Req, Opt
    * 
    * 5 fields => M1
    * 4, 3 fields => M2 (with first row header) 
    * 2, 1 fields => M3
    * 
    * Also tests that leading newlines are ignored.
    */
   public void testGoodMessageTableNoHeadersLengthDifferent6() throws Exception
   {
      MessageTable table = new MessageTable();
      
      table.addMessage(Repository.getMetadata().getMessage("CSV_TableConflict_4Header"));
      table.addMessage(Repository.getMetadata().getMessage("CSV_TableConflict_5Required"));
      table.addMessage(Repository.getMetadata().getMessage("CSV_TableConflict_1Required1Optional"));
      
      m_parser.initializeMessageTable(table);
      
      TransferObject root, record;
      List recordList;
      
      root = m_parser.parse(new StringInput(
         "\nfirst,second,third,fourth\n" +
         ";This is a comment\n" +
         "a,b,c,d\n" +
         ";So is this\n" +
         "a2,b2,c2,d2\n"
         ), table);
      
      recordList = (List)root.getValue("Data4H");
      
      assertEquals(2, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("a", record.getValue("first"));
      assertEquals("b", record.getValue("second"));
      assertEquals("c", record.getValue("third"));
      assertEquals("d", record.getValue("fourth"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("a2", record.getValue("first"));
      assertEquals("b2", record.getValue("second"));
      assertEquals("c2", record.getValue("third"));
      assertEquals("d2", record.getValue("fourth"));
      
      
      //Test re-ordered header
      root = m_parser.parse(new StringInput(
         "fourth,second,third,first\n" +
         "d,b,c,a\n" +
         "d2,b2,c2,a2\n"
         ), table);
      
      recordList = (List)root.getValue("Data4H");
      
      assertEquals(2, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("a", record.getValue("first"));
      assertEquals("b", record.getValue("second"));
      assertEquals("c", record.getValue("third"));
      assertEquals("d", record.getValue("fourth"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("a2", record.getValue("first"));
      assertEquals("b2", record.getValue("second"));
      assertEquals("c2", record.getValue("third"));
      assertEquals("d2", record.getValue("fourth"));
      
      
      
      //Test re-ordered, short header
      root = m_parser.parse(new StringInput(
         "second,third,first\n" +
         "b,c,a\n" +
         "b2,c2,a2\n"
         ), table);
      
      recordList = (List)root.getValue("Data4H");
      
      assertEquals(2, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("a", record.getValue("first"));
      assertEquals("b", record.getValue("second"));
      assertEquals("c", record.getValue("third"));
      assertFalse(record.hasValue("fourth"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("a2", record.getValue("first"));
      assertEquals("b2", record.getValue("second"));
      assertEquals("c2", record.getValue("third"));
      assertFalse(record.hasValue("fourth"));
            
      
      root = m_parser.parse(new StringInput(
         "\n\nfirst,second,third\n" +
         "\n\na,b,c\n" +
         "a2,b2,c2\n"
         ), table);
      
      recordList = (List)root.getValue("Data4H");
      
      assertEquals(2, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("a", record.getValue("first"));
      assertEquals("b", record.getValue("second"));
      assertEquals("c", record.getValue("third"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("a2", record.getValue("first"));
      assertEquals("b2", record.getValue("second"));
      assertEquals("c2", record.getValue("third"));
      
      
      root = m_parser.parse(new StringInput(
         "\n\n\na,b\n" +
         "a2,b2,c2\n"
         ), table);
      
      recordList = (List)root.getValue("Data1R1O");
      
      assertEquals(2, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("a", record.getValue("first"));
      assertEquals("b", record.getValue("second"));
      assertFalse(record.hasValue("third"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("a2", record.getValue("first"));
      assertEquals("b2", record.getValue("second"));
      assertFalse(record.hasValue("third"));
      
      
      root = m_parser.parse(new StringInput(
         "\n\n\n\na,b,c,d,e\n" +
         "a2,b2,c2,d2,e2\n"
         ), table);
      
      recordList = (List)root.getValue("Data5R");
      
      assertEquals(2, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("a", record.getValue("first"));
      assertEquals("b", record.getValue("second"));
      assertEquals("c", record.getValue("third"));
      assertEquals("d", record.getValue("fourth"));
      assertEquals("e", record.getValue("fifth"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("a2", record.getValue("first"));
      assertEquals("b2", record.getValue("second"));
      assertEquals("c2", record.getValue("third"));
      assertEquals("d2", record.getValue("fourth"));
      assertEquals("e2", record.getValue("fifth"));
      
      
      
      try
      {
         root = m_parser.parse(new StringInput(
            "a,b,c,d,e,f\n" +
            "a2,b2,c2,d2,e2,f2\n"
            ), table);
         fail();
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.csv.undetectableMessage", ex.getErrorCode());
      }
   }

   
   
   /**
    * Message conflict:
    * 
    * M1: Opt, Opt, Opt, Opt
    * M2: Opt, Opt, Opt
    * 
    * First data row could contain less than 4 fields, making it impossible
    * to know which Message is being parsed.
    */
   public void testInvalidMessageTableNoHeadersLengthTooSimilar1() throws Exception
   {
      MessageTable table = new MessageTable();
      
      table.addMessage(Repository.getMetadata().getMessage("CSV_TableConflict_4Optional"));
      table.addMessage(Repository.getMetadata().getMessage("CSV_TableConflict_3Optional"));
      
      try
      {
         m_parser.initializeMessageTable(table);
         fail();
      }
      catch (IntegrationException ex)
      {         
         assertEquals("err.integration.csv.messageTableConflict", ex.getErrorCode());
         assertEquals("CSV_TableConflict_4Optional", ex.getErrorArgs()[0]);
         assertEquals("CSV_TableConflict_3Optional", ex.getErrorArgs()[1]);
      }
   }
   
   
   /**
    * Message conflict:
    * 
    * M1: Req, Req, Req, Req
    * M2: Req, Req, Req, Opt
    * 
    * First data row could contain 4 fields, making it impossible to know
    * which Message is being parsed.
    */
   public void testInvalidMessageTableNoHeadersLengthTooSimilar2() throws Exception
   {
      MessageTable table = new MessageTable();
      
      table.addMessage(Repository.getMetadata().getMessage("CSV_TableConflict_4Required"));
      table.addMessage(Repository.getMetadata().getMessage("CSV_TableConflict_3Required1Optional"));
      
      try
      {
         m_parser.initializeMessageTable(table);
         fail();
      }
      catch (IntegrationException ex)
      {         
         assertEquals("err.integration.csv.messageTableConflict", ex.getErrorCode());
         assertEquals("CSV_TableConflict_4Required", ex.getErrorArgs()[0]);
         assertEquals("CSV_TableConflict_3Required1Optional", ex.getErrorArgs()[1]);
      }
   }
   
   
   /**
    * Message conflict:
    * 
    * M1: Hdr, Hdr, Hdr, Hdr
    * M2: Opt, Opt, Opt, Opt
    * 
    * First data row of an M2 could contain 4 fields, all set to the names of
    * the headers of the M1 message, causing the M2 data to be detected as
    * M1 data.
    */
   public void testInvalidMessageTableNoHeadersLengthTooSimilar3() throws Exception
   {
      MessageTable table = new MessageTable();
      
      table.addMessage(Repository.getMetadata().getMessage("CSV_TableConflict_4Header"));
      table.addMessage(Repository.getMetadata().getMessage("CSV_TableConflict_4Optional"));
      
      try
      {
         m_parser.initializeMessageTable(table);
         fail();
      }
      catch (IntegrationException ex)
      {         
         assertEquals("err.integration.csv.messageTableConflict", ex.getErrorCode());
         assertEquals("CSV_TableConflict_4Header", ex.getErrorArgs()[0]);
         assertEquals("CSV_TableConflict_4Optional", ex.getErrorArgs()[1]);
      }
   }
   
   
   public void testParseHeaderWithMissingRequired() throws Exception
   {
      TransferObject root, record;
      List recordList;
      root = m_parser.parse(new StringInput(
         "first,second,third\n" +
         "a,b,c\n" +
         "a2,b2,c2\n"
         ), Repository.getMetadata().getMessage("CSV_TableConflict_4Header"));
      
      recordList = (List)root.getValue("Data4H");
      
      assertEquals(2, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("a", record.getValue("first"));
      assertEquals("b", record.getValue("second"));
      assertEquals("c", record.getValue("third"));
      assertFalse(record.hasValue("fourth"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("a2", record.getValue("first"));
      assertEquals("b2", record.getValue("second"));
      assertEquals("c2", record.getValue("third"));
      assertFalse(record.hasValue("fourth"));
      
      
      //Missing "third" column, which is marked as required.
      root = m_parser.parse(new StringInput(
         "first,second,fourth\n" +
         "a,b,d\n" +
         "a2,b2,d2\n"
         ), Repository.getMetadata().getMessage("CSV_TableConflict_4Header"));
      
      recordList = (List)root.getValue("Data4H");
      
      assertEquals(2, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("a", record.getValue("first"));
      assertEquals("b", record.getValue("second"));
      assertEquals("d", record.getValue("fourth"));
      assertFalse(record.hasValue("third"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("a2", record.getValue("first"));
      assertEquals("b2", record.getValue("second"));
      assertEquals("d2", record.getValue("fourth"));
      assertFalse(record.hasValue("third"));
      
      
      //"third" column is required and "fourth" column is optional, but still allow
      //as long as data doesn't violate.
      root = m_parser.parse(new StringInput(
         "first,second,fourth,third\n" +
         "a,b,d,c\n" +
         "a2,b2,d2,c2\n"
         ), Repository.getMetadata().getMessage("CSV_TableConflict_4Header"));
      
      recordList = (List)root.getValue("Data4H");
      
      assertEquals(2, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("a", record.getValue("first"));
      assertEquals("b", record.getValue("second"));
      assertEquals("d", record.getValue("fourth"));
      assertEquals("c", record.getValue("third"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("a2", record.getValue("first"));
      assertEquals("b2", record.getValue("second"));
      assertEquals("d2", record.getValue("fourth"));
      assertEquals("c2", record.getValue("third"));
      
      
      try
      {
         root = m_parser.parse(new StringInput(
            "fourth,first,second,third\n" +
            "d,a,b,c\n" +
            "d2,a2,b2,c2\n" +
            "d3,a3\n"
            ), Repository.getMetadata().getMessage("CSV_TableConflict_4Header"));
         fail();
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.minPartCount", ((IntegrationException)ex.getCause()).getErrorCode());
         assertEquals("CSV_TableConflict_4Header Data4H second", ((IntegrationException)ex.getCause()).getErrorArgs()[0]);
      }
      
      
      try
      {
         root = m_parser.parse(new StringInput(
            "fourth,first,second,third\n" +
            "d,a,b,c\n" +
            "d2,a2,b2,c2\n" +
            "d3,a3,b3\n"
            ), Repository.getMetadata().getMessage("CSV_TableConflict_4Header"));
         fail();
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.minPartCount", ((IntegrationException)ex.getCause()).getErrorCode());
         assertEquals("CSV_TableConflict_4Header Data4H third", ((IntegrationException)ex.getCause()).getErrorArgs()[0]);
      }
      
      
      try
      {
         root = m_parser.parse(new StringInput(
            "fourth,first\n" +
            "d,a\n" +
            "d2,a2\n" +
            "d3\n"
            ), Repository.getMetadata().getMessage("CSV_TableConflict_4Header"));
         fail();
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.minPartCount", ((IntegrationException)ex.getCause()).getErrorCode());
         assertEquals("CSV_TableConflict_4Header Data4H first", ((IntegrationException)ex.getCause()).getErrorArgs()[0]);
      }
      
      
      //This final test makes sure that the parse looks ahead, past just the immediate
      //column, which may be optional, and finds that there is a subesequent required column.
      try
      {
         root = m_parser.parse(new StringInput(
            "first,fourth,second\n" +
            "a,d,b\n" +
            "a2,d2,b2\n" +
            "a3\n"
            ), Repository.getMetadata().getMessage("CSV_TableConflict_4Header"));
         fail();
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.minPartCount", ((IntegrationException)ex.getCause()).getErrorCode());
         assertEquals("CSV_TableConflict_4Header Data4H second", ((IntegrationException)ex.getCause()).getErrorArgs()[0]);
      }
   }
   
   
   /**
    * Test that messages that use varying delimiters and quoting can be parsed.
    */
   public void testFormatNonUniformDelimiters() throws Exception
   {
      TransferObject root, record;
      List recordList;
 
      root = m_parser.parse(new StringInput(
         "first:second:third:fourth\n" +
         "realdata1,realdata2:realdata3,\"realdata4\"\n" +
         "a\\,a:a,'b,b:b':c\\,c:c,\"d,d:d\"\n" +
         "a\\,a:a,'b,b:b':c\\,c:c\n"
         ), Repository.getMetadata().getMessage("CSV_NonUniformDelimiter"));
      
      recordList = (List)root.getValue("Data");
      
      assertEquals(3, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("realdata1", record.getValue("first"));
      assertEquals("realdata2", record.getValue("second"));
      assertEquals("realdata3", record.getValue("third"));
      assertEquals("realdata4", record.getValue("fourth"));
     
      record = (TransferObject)recordList.get(1);
      assertEquals("a,a:a", record.getValue("first"));
      assertEquals("b,b:b", record.getValue("second"));
      assertEquals("c,c:c", record.getValue("third"));
      assertEquals("d,d:d", record.getValue("fourth"));
      
      record = (TransferObject)recordList.get(2);
      assertEquals("a,a:a", record.getValue("first"));
      assertEquals("b,b:b", record.getValue("second"));
      assertEquals("c,c:c", record.getValue("third"));
      assertFalse(record.hasValue("fourth"));
   }

   /**
    * Test degenerate case of no data.
    */
   public void testParseNothing() throws Exception
   {
      List recordList;
      TransferObject root = m_parser.parse(new StringInput(
         ""
         ), m_simple_message);

      assertNotNull(root);
      recordList = (List)root.getValue("Row");
      assertTrue(recordList.isEmpty());
   }

   /**
    * Tests degenerate case of no data being parsed to one of a table of messages.
    */
   public void testParseNothingWithMessageTable() throws Exception
   {
      MessageTable table = new MessageTable();

      table.addMessage(Repository.getMetadata().getMessage("CSV_TableConflict_4Required"));
      table.addMessage(Repository.getMetadata().getMessage("CSV_TableConflict_3Required"));

      m_parser.initializeMessageTable(table);

      List recordList;
      TransferObject root = m_parser.parse(new StringInput(
         ""
         ), table);

      assertNotNull(root);
      assertEquals("CSV_TableConflict_4Required", root.getClassName());
      recordList = (List)root.getValue("Data4R");
      assertTrue(recordList.isEmpty());
   }

   /**
    * Test degenerate case of just a comment followed by EOF,
    * with no data.
    */
   public void testParseJustComment() throws Exception
   {
      List recordList;
      TransferObject root = m_parser.parse(new StringInput(
         ";"         
         ), m_simple_message);

      assertNotNull(root);
      recordList = (List)root.getValue("Row");
      assertTrue(recordList.isEmpty());
   }
   
   /**
    * Test degenerate case of comment, newline, and EOF, with
    * no data.
    */
   public void testParseJustCommentAndNewline() throws Exception
   {
      List recordList;
      TransferObject root = m_parser.parse(new StringInput(
         ";\n"         
         ), m_simple_message);

      assertNotNull(root);
      recordList = (List)root.getValue("Row");
      assertTrue(recordList.isEmpty());
   }
   
   /**
    * Test degenerate case of just a newline, with no
    * actual data.
    */
   public void testParseJustNewline() throws Exception
   {
      List recordList;
      TransferObject root = m_parser.parse(new StringInput(
         "\n"         
         ), m_simple_message);

      assertNotNull(root);
      recordList = (List)root.getValue("Row");
      assertTrue(recordList.isEmpty());
   }
   
   /**
    * Test degenerate case of newline followed by comment,
    * and no data.
    */
   public void testParseJustNewlineThenComment() throws Exception
   {
      List recordList;
      TransferObject root = m_parser.parse(new StringInput(
         "\n;"         
         ), m_simple_message);

      assertNotNull(root);
      recordList = (List)root.getValue("Row");
      assertTrue(recordList.isEmpty());
   }


   /**
    * This test forced implementation of comments, and also clarified
    * handling of required fields that were blank. (The corresponding
    * datum in the TransferObject is set to null).
    * 
    * Data from: http://www.cisco.com/univercd/cc/td/doc/product/rtrmgmt/cw2000/cw2000e/rme_ltu/ug_appa.htm
    */
   public void testRoundtripRealWorldCisco() throws Exception
   {
      final String sHeader = "; The following header line is mandatory - only the value of the\n" +
         "; source attribute can be modified (e.g. source = My Excel spreadsheet).\n" +
         
         //This next line wasn't commented in original, but I commented it because it
         //is non-standard CSV. Another way to handle this would be to specify an
         //option on the parser that allows it to skip the first n lines of input.
         ";cisco Systems NM data import, source = Hand edit; Version = 1.0; Type = Csv\n" + 
         ";\n" +
         "; Here are the columns of the table.\n" +
         ";\n" +
         ";Col# = 1; Name = Device name (include domain unless your stie has\n" +
         "; unqualified device names registered in the name services\n" +
         "; - or -\n" +
         "; IP address in dotted decimal notation\n" +
         ";Col# = 2: Name = RO community string\n" +
         ";Col# = 3: Name = RW community string\n" +
         ";Col# = 4: Name = Serial Number\n" +
         ";Col# = 5: Name = User Field 1\n" +
         ";Col# = 6: Name = User Field 2\n" +
         ";Col# = 7: Name = User Field 3\n" +
         ";Col# = 8: Name = User Field 4\n" +
         ";Col# = 9; Name = Telnet password\n" +
         ";Col# = 10; Name = Enable password\n" +
         ";Col# = 11; Name = Enable secret\n" +
         ";Col# = 12; Name = Tacacs user\n" +
         ";Col# = 13; Name = Tacacs password\n" +
         ";Col# = 14; Name = Tacacs enable user\n" +
         ";Col# = 15; Name = Tacacs enable password\n" +
         ";Col# = 16; Name = Local user\n" +
         ";Col# = 17; Name = Local password\n" +
         ";\n" +
         "; Here are the rows of data.\n" +
         ";\n";
      
      final String sBody = "bigrouter.yourcompany.com,public,private,,\n" +
         "dev-2501.yourcompany.com,\"Not so, \"\" public as, thought\",private,sn2501,\n" +
         "dev-2502.yourcompany.com,public,\"private\",sn2502,\n" +
         "dev-2503.yourcompany.com,public,private,sn2503,\"\"\n" +
         
         //This next row has two extra fields!
         "dev-2504.yourco.com,public,private,sn2504,us1,us2,us3,us4,tPass,ePass,eSecret,tUsr,tPass,teUsr,tePass,LUsr,LPass,rUsr,rPass\n" +
         
         "dev-2505.yourco.com,public,private,sn2505,usr1,,,usr4,,,esecret,,tUsr,tPass,,,LUsr,lPass\n";
      
      final String sBody2 = "dev-2507.yourcompany.com,public,private,sn2507,\n" +
         "dev-2509.yourcompany.com,public,private,sn2509,\n" +
         "dev-2510.yourcompany.com,public,private,sn2510,\n" +
         "dev-2511.yourcompany.com,public,private,sn2511,\n" +
         "dev-2512.yourcompany.com,public,private,sn2512,\n" +
         "dev-2513.yourcompany.com,public,private,sn2513,\n" +
         "dev-2514.yourcompany.com,public,private,sn2514,\n" +
         "dev-2515.yourcompany.com,public,private,sn2515,\n" +
         "dev-2516.yourcompany.com,public,private,sn2516,\n" +
         "dev-4000.yourcompany.com,public,private,,Big Boys\n" +
         "dev-4500.yourcompany.com,public,private,,Big Boys\n" +
         "dev-7000.yourcompany.com,public,private,,Big Boys\n" +
         "dev-7010.yourcompany.com,public,private,,Big Boys\n" +
         "dev-2517.yourcompany.com,public,private,,,nm 25xx\n" +
         "dev-2518.yourcompany.com,public,private,,,mylabel2\n" +
         "dev-2520.yourcompany.com,public,private,,,mylabel2\n" +
         "dev-2521.yourcompany.com,public,private,,,mylabel2\n" +
         "dev-2522.yourcompany.com,public,private,,,mylabel2\n" +
         "dev-2523.yourcompany.com,public,private,,,mylabel2\n" +
         "dev-2524.yourcompany.com,public,private,,,mylabel2\n" +
         "dev-2525.yourcompany.com,public,private,,,mylabel2\n" +
         "dev-4700.yourcompany.com,public,private,,yourlabel1,,yourlabel3,yourlabel4\n" +
         "dev-7206.yourcompany.com,public,private,,\n" +
         "dev-7505.yourcompany.com,public,private,,,,,yourlabel4\n" +
         "dev-7507.yourcompany.com,public,private,,\n" +
         "dev-7513.yourcompany.com,public,private,,\n" +
         "dev-1200.yourcompany.com,public,private,,\n" +
         "dev-2900.yourcompany.com,public,private,,\n" +
         "dev-3000.yourcompany.com,public,private,,\n" +
         "dev-5000.yourcompany.com,public,private,,\n" +
         "111.222.33.44,public,public,,\n";
      
      final String sData = sHeader + sBody + sBody2;
      
      //We should expect to get the body, but quoting will be a bit different
      //and the extra data on the two last lines below will be excluded.
      String sExpected = "bigrouter.yourcompany.com,public,private,,\n" +
         "dev-2501.yourcompany.com,\"Not so, \"\" public as, thought\",private,sn2501,\n" +         
         "dev-2502.yourcompany.com,public,private,sn2502,\n" +
         "dev-2503.yourcompany.com,public,private,sn2503,\n" +
         "dev-2504.yourco.com,public,private,sn2504,us1,us2,us3,us4,tPass,ePass,eSecret,tUsr,tPass,teUsr,tePass,LUsr,LPass\n" +      
         "dev-2505.yourco.com,public,private,sn2505,usr1,,,usr4,,,esecret,,tUsr,tPass,,,LUsr\n" +
         sBody2;
      
            
      TransferObject root = m_parser.parse(
         new StringInput(sData),
         m_cisco_message);
      
      
      StringWriter writer = new StringWriter();
      m_formatter.format(root, m_cisco_message, new WriterOutput(writer));
      
      assertEquals(sExpected, writer.toString());
   }


   /**
    * Test that parser handles (gracefully, by skipping) columns whose header
    * name it doesn't know.
    */
   public void testParseRealWorldBug1() throws Exception
   {
      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         "'firstName'\t'lastName'\t'unknown_column1'\t'age'\t'Phone1''s location'\t'Phone1''s number'\t'location'\t'number'\r\n" +
         "'John'\t'Doe'\t'unknown_value1'\t'42'\t\t\t\r\n" +
         "'Jane'\t'Doe'\t\t'35'\t\t\t\r\n"
         ), m_tsv_message);
      
      
      assertEquals("CSV_SimpleTable_TSV", root.getClassName());
      
      List recordList = (List)root.getValue("Row");
      
      assertEquals(2, recordList.size());
      
      record = (TransferObject)recordList.get(0);
      assertEquals("John", record.getValue("firstName"));
      assertEquals("Doe",  record.getValue("lastName"));
      assertEquals(Primitive.createInteger(42), record.getValue("age"));
      assertFalse(record.hasValue("Phone1's number"));
      assertFalse(record.hasValue("number"));
      assertNull(record.getClassName());

      record = (TransferObject)recordList.get(1);
      assertEquals("Jane", record.getValue("firstName"));
      assertEquals("Doe",  record.getValue("lastName"));
      assertEquals(Primitive.createInteger(35), record.getValue("age"));
      assertNull(record.getClassName());
   }

   /**
    * Tests format string support in the CSV format adapter.
    */
   public void testFormatCSVWithFormatString()
   {
      StringWriter outWriter = new StringWriter();
      WriterOutput out = new WriterOutput(outWriter);
      TransferObject tobj = new TransferObject("CSV_FormatString", 1);
      TransferObject row = new TransferObject();
      Timestamp ts1 = Timestamp.valueOf("2010-03-03 18:30:00.123");
      List rows = new ArrayList(4);

      tobj.setValue("rows", rows);
      rows.add(row);
      row.setValue("normalDT", ts1);
      row.setValue("formatDT", ts1);
      row.setValue("formatBool", Boolean.FALSE);

      m_formatter.format(tobj, m_formatStringMessage, out);

      String sOutput = outWriter.toString();

      assertEquals("formatBool,normalDT,formatDT\n" +
         ",2010-03-03 23:30:00.123000000,20100303183000.123-0500\n",
         sOutput
      );

      tobj = m_parser.parse(new StringInput(sOutput), m_formatStringMessage);

      rows = (List)tobj.getValue("rows");
      row = (TransferObject)rows.get(0);

      assertEquals(ts1, row.getValue("normalDT"));
      assertEquals(ts1, row.getValue("formatDT"));
      assertEquals(Boolean.FALSE, row.getValue("formatBool"));
   }
}
