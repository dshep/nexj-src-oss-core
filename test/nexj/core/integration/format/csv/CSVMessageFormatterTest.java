// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.csv;

import java.io.IOException;
import java.io.StringWriter;
import java.util.List;
import java.util.ArrayList;

import junit.framework.TestCase;

import nexj.core.integration.IntegrationException;
import nexj.core.integration.MessageFormatter;
import nexj.core.integration.io.WriterOutput;
import nexj.core.meta.Repository;
import nexj.core.meta.integration.Format;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.PrimitiveMessagePart;
import nexj.core.meta.integration.format.csv.CSVMessagePartMapping;
import nexj.core.rpc.TransferObject;

import nexj.core.meta.Primitive;


public class CSVMessageFormatterTest extends TestCase
{   
   protected Format m_format;
   protected MessageFormatter m_formatter;
   protected Message m_simpleMessage;
   protected Message m_tsvMessage;
   protected Message m_noHeaderMessage;
   protected Message m_skippedColumnsMessage;
   protected Message m_skippedOptionalBeginningMessage;
   protected Message m_skippedRequiredBeginningMessage;
   protected Message m_noEscapeOrQuoteMessage;

   protected void setUp() throws Exception
   {
      m_format = Repository.getMetadata().getFormat("CSV");
      m_formatter = (MessageFormatter)m_format.getFormatter().getInstance(null);
      m_simpleMessage = Repository.getMetadata().getMessage("CSV_SimpleTable");
      m_tsvMessage = Repository.getMetadata().getMessage("CSV_SimpleTable_TSV");
      m_noHeaderMessage = Repository.getMetadata().getMessage("CSV_SimpleTable_NoHeader");
      m_skippedColumnsMessage = Repository.getMetadata().getMessage("CSV_SkippedColumns");
      m_skippedOptionalBeginningMessage = Repository.getMetadata().getMessage("CSV_SkippedOptionalBeginning");
      m_skippedRequiredBeginningMessage = Repository.getMetadata().getMessage("CSV_SkippedRequiredBeginning");
      m_noEscapeOrQuoteMessage = Repository.getMetadata().getMessage("CSV_SimpleTable_NoEscapeOrQuote");
   }
   
   public void testFormatOutputMessageWithSimpleTable() throws Exception
   {
      TransferObject root = new TransferObject("Table");
      TransferObject record1 = new TransferObject();
      record1.setValue("firstName", "Jo\\hn");
      record1.setValue("lastName", "Doe");
      record1.setValue("age", Primitive.createInteger(42));
      record1.setValue("balance", Primitive.toDecimal(new Integer(108)));
      
      TransferObject record2 = new TransferObject();
      record2.setValue("firstName", "#Jane");
      record2.setValue("lastName", "Doe");
      record2.setValue("age", Primitive.createInteger(37));
      
      TransferObject record3 = new TransferObject();
      record3.setValue("firstName", "Jack");
      record3.setValue("lastName", "L\"in");
      record3.setValue("age", null);  //produces blank field
      
      TransferObject record5 = new TransferObject();
      record5.setValue("firstName", "Jack");
      record5.setValue("lastName", "Lin");
      //age is missing, and there is no more data on line, so no blank field will be created.
      
      List recordList = new ArrayList(5);
      recordList.add(record1);
      recordList.add(record2);
      recordList.add(null);
      recordList.add(record3);
      recordList.add(record5);
      root.setValue("Row", recordList);
      
      //Format it
      StringWriter writer = new StringWriter();
      m_formatter.format(root, m_simpleMessage, new WriterOutput(writer));
      
      assertEquals("firstName,familyName,age,Phone1_location,Phone1_number,location,number,balance\n" +
         "Jo\\\\hn,Doe,42,,,,,108\n" +
         "\\#Jane,Doe,37\n\n" +  //two newlines, due to null row datum
         "Jack,L\"in,\n" +
         "Jack,Lin\n",
         writer.toString());
   }
   
   public void testFormatOutputMessageWithSingleRow() throws Exception
   {
      TransferObject root = new TransferObject("Table");
      TransferObject record1 = new TransferObject();
      record1.setValue("firstName", "John");
      record1.setValue("lastName", "Doe");
      record1.setValue("age", Primitive.createInteger(42));
      
      List recordList = new ArrayList(1);
      recordList.add(record1);
      root.setValue("Row", recordList);
      
      //Format it
      StringWriter writer = new StringWriter();
      m_formatter.format(root, m_simpleMessage, new WriterOutput(writer));
      
      assertEquals("firstName,familyName,age,Phone1_location,Phone1_number,location,number,balance\n" +
         "John,Doe,42\n",
         writer.toString());
   }

   /**
    * Tests that a zero-length collection yields zero rows of output.
    */
   public void testFormatOutputMessageWithZeroRows() throws Exception
   {
      TransferObject root = new TransferObject("Table");
      List recordList = new ArrayList(0);

      root.setValue("Row", recordList);

      //Format it
      StringWriter writer = new StringWriter();
      m_formatter.format(root, m_simpleMessage, new WriterOutput(writer));

      assertEquals("firstName,familyName,age,Phone1_location,Phone1_number,location,number,balance\n",
         writer.toString());
   }

   public void testFormatOutputMessageWithSimpleTableAndLogicalComponents() throws Exception
   {
      TransferObject root = new TransferObject("Table");
      TransferObject record1 = new TransferObject();
      record1.setValue("firstName", "John");
      record1.setValue("lastName", "Doe");
      record1.setValue("age", Primitive.createInteger(42));
      
      TransferObject record2 = new TransferObject();
      record2.setValue("firstName", ";Hui Mei");
      record2.setValue("lastName", "Zhang");
      record2.setValue("age", Primitive.createInteger(37));
      
      TransferObject record2_phone1 = new TransferObject();
      record2_phone1.setValue("location","cell");
      record2_phone1.setValue("number", "0932077773");
      record2.setValue("Phone1", record2_phone1);
      
      TransferObject record3 = new TransferObject();
      record3.setValue("firstName", "Jack");
      record3.setValue("lastName", "Lin");
      record3.setValue("age", Primitive.createInteger(26));
      
      TransferObject record3_phone2 = new TransferObject();
      record3_phone2.setValue("location","test");
      record3_phone2.setValue("number", "012345678");
      record3.setValue("Phone2", record3_phone2);
      
      List recordList = new ArrayList(2);
      recordList.add(record1);
      recordList.add(record2);
      recordList.add(record3);
      root.setValue("Row", recordList);
      
      //Format it
      StringWriter writer = new StringWriter();
      m_formatter.format(root, m_simpleMessage, new WriterOutput(writer));
      
      assertEquals("firstName,familyName,age,Phone1_location,Phone1_number,location,number,balance\n" +
         "John,Doe,42\n" +
         "\\;Hui Mei,Zhang,37,cell,0932077773\n" +
         "Jack,Lin,26,,,test,012345678\n",
         writer.toString());
   }
   
   
   public void testFormatOutputMessageWithTSVSettings() throws Exception
   {
      TransferObject root = new TransferObject("Table");
      TransferObject record1 = new TransferObject();
      record1.setValue("firstName", "John");
      record1.setValue("lastName", "Doe");
      record1.setValue("age", Primitive.createInteger(42));
      
      TransferObject record2 = new TransferObject();
      record2.setValue("firstName", "Jane");
      record2.setValue("lastName", "Doe");
      record2.setValue("age", Primitive.createInteger(37));
      
      TransferObject record3 = new TransferObject();
      record3.setValue("firstName", "Jack");
      record3.setValue("lastName", "Lin");
      record3.setValue("age", Primitive.createInteger(26));
      
      List recordList = new ArrayList(2);
      recordList.add(record1);
      recordList.add(record2);
      recordList.add(record3);
      root.setValue("Row", recordList);
      
      //Format it
      StringWriter writer = new StringWriter();
      m_formatter.format(root, m_tsvMessage, new WriterOutput(writer));
      
      assertEquals("'firstName'\t'lastName'\t'age'\t'Phone1''s location'\t'Phone1''s number'\t'location'\t'number'\r\n" +
         "'John'\t'Doe'\t42\r\n" +
         "'Jane'\t'Doe'\t37\r\n" +
         "'Jack'\t'Lin'\t26\r\n",
         writer.toString());
   }
   
   
   /**
    * Test that supplied data can be brought down to the very minimum required
    * for a row, with delimiters still working correctly.
    * 
    * Also verify that a required datum may be explicity set to null
    * to generate a blank required or optional field.
    * 
    * Verify that if the first field starts with a comment character, then it will
    * be quoted. Subsequent fields will not.
    */
   public void testFormatOutputMessageWithSimpleTableAndNoHeader() throws Exception
   {
      TransferObject root = new TransferObject("Table");
      TransferObject record1 = new TransferObject();
      record1.setValue("firstName", "John");
      record1.setValue("lastName", "Doe");
      record1.setValue("age", Primitive.createInteger(42));
      
      TransferObject record2 = new TransferObject();
      record2.setValue("firstName", ";Hui Mei");
      record2.setValue("lastName", ";Zhang");
      record2.setValue("age", Primitive.createInteger(37));
      record2.setValue("eventtime", Primitive.toTimestamp(new Long(1185513840000L)));
            
      TransferObject record3 = new TransferObject();
      record3.setValue("firstName", "Jack");
      record3.setValue("lastName", "#Lin");
      record3.setValue("age", Primitive.createInteger(26));
      
      TransferObject record4 = new TransferObject();
      record4.setValue("lastName", "Huo");
      record4.setValue("firstName", "Yuan Jia");
      
      TransferObject record5 = new TransferObject();
      record5.setValue("lastName", "Deng");
      
      TransferObject record6 = new TransferObject();
      record6.setValue("lastName", null);
      record6.setValue("firstName", "Jay");
      record6.setValue("age", Primitive.createInteger(29));
      record6.setValue("eventtime", null);
      
      
      List recordList = new ArrayList(6);
      recordList.add(record1);
      recordList.add(record2);
      recordList.add(record3);
      recordList.add(record4);
      recordList.add(record5);
      recordList.add(record6);
      root.setValue("Data", recordList);
      
      //Format it
      StringWriter writer = new StringWriter();
      m_formatter.format(root, m_noHeaderMessage, new WriterOutput(writer));
      
      assertEquals("Doe,John,42\r" +
         "\u001e;Zhang\u001e,;Hui Mei,37,2007-07-27 05:24:00.000000000\r" +
         "\u001e#Lin\u001e,Jack,26\r" +
         "Huo,Yuan Jia\r" +
         "Deng\r" +
         ",Jay,29,\r",
         writer.toString());
   }
   
   
   /**
    * Ensure appropriate exception is raised when an entire row of data is missing
    * and at least one of the fields is required.
    */
   public void testFormatOutputMessageRequiredMissing1() throws Exception
   {
      TransferObject root = new TransferObject("Table");
      TransferObject record1 = new TransferObject();
      record1.setValue("firstName", "John");
      record1.setValue("lastName", "Doe");
      record1.setValue("age", Primitive.createInteger(42));
      
      TransferObject record2 = new TransferObject();
      record2.setValue("firstName", "Hui Mei");
      record2.setValue("lastName", "Zhang");
      record2.setValue("age", Primitive.createInteger(37));
      record2.setValue("eventtime", Primitive.toTimestamp(new Long(1185513840000L)));
            
      TransferObject record3 = new TransferObject();
      record3.setValue("firstName", "Jack");
      record3.setValue("lastName", "Lin");
      record3.setValue("age", Primitive.createInteger(26));
      
      TransferObject record4 = new TransferObject();
      record4.setValue("lastName", "Huo");
      record4.setValue("firstName", "Yuan Jia");
      
      //This is the problem row: it is blank
      TransferObject record5 = new TransferObject();
      
      
      List recordList = new ArrayList(5);
      recordList.add(record1);
      recordList.add(record2);
      recordList.add(record3);
      recordList.add(record4);
      recordList.add(record5);
      root.setValue("Data", recordList);
      
      //Format it
      try
      {
         StringWriter writer = new StringWriter();
         
         m_formatter.format(root, m_noHeaderMessage, new WriterOutput(writer));
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
         assertEquals("CSV_SimpleTable_NoHeader Data lastName", realEx.getErrorArgs()[0]);         
      }
   }
   
   
   /**
    * Ensure an exception is raised when the datum for a required field is not
    * set, even if other data in the row are set.
    * 
    * Also ensure that a datum which is set to null does not
    * trigger the exception; only if the key for that datum is not
    * present in the TransferObject should the exception be raised (this is
    * actually tested in the test testFormatOutputMessageWithSimpleTableAndNoHeader)
    */
   public void testFormatOutputMessageRequiredMissing2() throws Exception
   {
      TransferObject root = new TransferObject("Table");
      TransferObject record1 = new TransferObject();
      record1.setValue("firstName", "John");
      record1.setValue("lastName", "Doe");
      record1.setValue("age", Primitive.createInteger(42));
      
      TransferObject record2 = new TransferObject();
      record2.setValue("firstName", "Hui Mei");
      record2.setValue("lastName", "Zhang");
      record2.setValue("age", Primitive.createInteger(37));
      record2.setValue("eventtime", Primitive.toTimestamp(new Long(1185513840000L)));
            
      TransferObject record3 = new TransferObject();
      record3.setValue("firstName", null);
      record3.setValue("lastName", null);   //not a problem
      record3.setValue("age", Primitive.createInteger(26));
      
      TransferObject record4 = new TransferObject();
      record4.setValue("lastName", null); //not a problem
      record4.setValue("firstName", "Yuan Jia");
      
      //problem is here: no lastName is set, even though it is required.
      TransferObject record5 = new TransferObject();
      record5.setValue("firstName", "Xiao Ping");
      
      
      
      List recordList = new ArrayList(5);
      recordList.add(record1);
      recordList.add(record2);
      recordList.add(record3);
      recordList.add(record4);
      recordList.add(record5);
      root.setValue("Data", recordList);
      
      //Format it
      try
      {
         StringWriter writer = new StringWriter();
         
         m_formatter.format(root, m_noHeaderMessage, new WriterOutput(writer));
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
         assertEquals("CSV_SimpleTable_NoHeader Data lastName", realEx.getErrorArgs()[0]);         
      }
   }
   
   
   /**
    * Ensure an exception is raised when the datum representing an entire row
    * is null, if there is at least one required field on the row.
    */
   public void testFormatOutputMessageRequiredMissing3() throws Exception
   {
      TransferObject root = new TransferObject("Table");
      TransferObject record1 = new TransferObject();
      record1.setValue("firstName", "John");
      record1.setValue("lastName", "Doe");
      record1.setValue("age", Primitive.createInteger(42));
      
      TransferObject record2 = new TransferObject();
      record2.setValue("firstName", "Hui Mei");
      record2.setValue("lastName", "Zhang");
      record2.setValue("age", Primitive.createInteger(37));
      record2.setValue("eventtime", Primitive.toTimestamp(new Long(1185513840000L)));
            
      TransferObject record3 = new TransferObject();
      record3.setValue("firstName", null);
      record3.setValue("lastName", null);   //not a problem
      record3.setValue("age", Primitive.createInteger(26));
      
      TransferObject record4 = new TransferObject();
      record4.setValue("lastName", null); //not a problem
      record4.setValue("firstName", "Yuan Jia");
      
      
      
      List recordList = new ArrayList(5);
      recordList.add(record1);
      recordList.add(record2);
      recordList.add(null);
      recordList.add(record3);
      recordList.add(record4);
      root.setValue("Data", recordList);
      
      //Format it
      try
      {
         StringWriter writer = new StringWriter();
         
         m_formatter.format(root, m_noHeaderMessage, new WriterOutput(writer));
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
         assertEquals("CSV_SimpleTable_NoHeader Data lastName", realEx.getErrorArgs()[0]);         
      }
   }
   
   
   public void testFormatSkippedColumns() throws Exception
   {
      TransferObject root = new TransferObject("Table");
      TransferObject record1 = new TransferObject();
      record1.setValue("firstName", "John");
      record1.setValue("lastName", "Doe");
      
      TransferObject record2 = new TransferObject();
      record2.setValue("firstName", "Jane");
      record2.setValue("lastName", "#Doe");
      
      TransferObject record3 = new TransferObject();
      record3.setValue("lastName", "Lin");
            
      List recordList = new ArrayList(3);
      recordList.add(record1);
      recordList.add(record2);
      recordList.add(record3);
      root.setValue("Data", recordList);
      
      //Format it
      StringWriter writer = new StringWriter();
      m_formatter.format(root, m_skippedColumnsMessage, new WriterOutput(writer));
      
      assertEquals("Doe,,,,John\r" +
         "\"#Doe\",,,,Jane\r" +
         "Lin\r",
         writer.toString());
   }
   
   
   public void testFormatSkippedOptionalBeginning() throws Exception
   {
      TransferObject root = new TransferObject("Table");
      TransferObject record1 = new TransferObject();
      record1.setValue("first", "realdata1");
      record1.setValue("second", "realdata2");
      
      TransferObject record2 = new TransferObject();
      record2.setValue("first", "nextrealdata1");
      record2.setValue("second", "nextrealdata2");
      
      TransferObject record3 = new TransferObject();
      record3.setValue("first", null);
      
      TransferObject record4 = new TransferObject();
            
      List recordList = new ArrayList(3);
      recordList.add(record1);
      recordList.add(record2);
      recordList.add(record4);
      recordList.add(record3);
      root.setValue("Data", recordList);
      
      //Format it
      StringWriter writer = new StringWriter();
      m_formatter.format(root, m_skippedOptionalBeginningMessage, new WriterOutput(writer));
      
      assertEquals(",,realdata1,,,realdata2\r" +
         ",,nextrealdata1,,,nextrealdata2\r" +
         "\r" +
         ",,\r",
         writer.toString());
   }
   
   
   public void testFormatSkippedRequiredBeginning() throws Exception
   {
      TransferObject root = new TransferObject("Table");
      TransferObject record1 = new TransferObject();
      record1.setValue("first", "realdata1");
      record1.setValue("second", "realdata2");
      
      TransferObject record2 = new TransferObject();
      record2.setValue("first", "nextrealdata1");
      record2.setValue("second", "nextrealdata2");
      
      TransferObject record3 = new TransferObject();
            
      List recordList = new ArrayList(3);
      recordList.add(record1);
      recordList.add(record2);
      recordList.add(record3);
      root.setValue("Data", recordList);
      
      //Format it
      try
      {
         StringWriter writer = new StringWriter();
         
         m_formatter.format(root, m_skippedRequiredBeginningMessage, new WriterOutput(writer));
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
    * Tests the ability to format a message that has no escape or quote.
    */
   public void testFormatOutputMessageWithNoEscapeOrQuote() throws Exception
   {
      TransferObject root = new TransferObject("Table");
      TransferObject record1 = new TransferObject();
      record1.setValue("firstName", "Jo\\hn");
      record1.setValue("lastName", "D,oe");
      record1.setValue("age", Primitive.createInteger(42));
      record1.setValue("balance", Primitive.toDecimal(new Integer(108)));

      TransferObject record2 = new TransferObject();
      record2.setValue("firstName", "#Ja,ne");
      record2.setValue("lastName", "Doe");
      record2.setValue("age", Primitive.createInteger(37));

      TransferObject record3 = new TransferObject();
      record3.setValue("firstName", "Jack");
      record3.setValue("lastName", "L\"in");
      record3.setValue("age", null);  //produces blank field

      TransferObject record5 = new TransferObject();
      record5.setValue("firstName", "Jack");
      record5.setValue("lastName", "Lin,");
      //age is missing, and there is no more data on line, so no blank field will be created.

      List recordList = new ArrayList(5);
      recordList.add(record1);
      recordList.add(record2);
      recordList.add(null);
      recordList.add(record3);
      recordList.add(record5);
      root.setValue("Row", recordList);

      //Format it
      StringWriter writer = new StringWriter();
      m_formatter.format(root, m_noEscapeOrQuoteMessage, new WriterOutput(writer));

      assertEquals("firstName,familyName,age,Phone1_location,Phone1_number,location,number,balance\n" +
         "Jo\\hn,D,oe,42,,,,,108\n" +
         "#Ja,ne,Doe,37\n\n" +  //comment char can't be escaped. Also, followed by null row.
         "Jack,L\"in,\n" +
         "Jack,Lin,\n",
         writer.toString());
   }


   /**
    * Test that the delimiter setting for each individual field can be used to override
    * the row delimiter.
    */
   public void testFormatNonUniformDelimiters() throws Exception
   {
      TransferObject root = new TransferObject("Table");
      TransferObject record1 = new TransferObject();
      record1.setValue("first", "realdata1");
      record1.setValue("second", "realdata2");
      record1.setValue("third", "realdata3");
      record1.setValue("fourth", "realdata4");
      
      TransferObject record2 = new TransferObject();
      record2.setValue("first", "a,a:a");
      record2.setValue("second", "b,b:b");
      record2.setValue("third", "c,c:c");
      record2.setValue("fourth", "d,d:d");
      
      TransferObject record3 = new TransferObject();
      record3.setValue("first", "a,a:a");
      record3.setValue("second", "b,b:b");
      record3.setValue("third", "c,c:c");

      
      List recordList = new ArrayList(3);
      recordList.add(record1);
      recordList.add(record2);
      recordList.add(record3);
      root.setValue("Data", recordList);
      
      //Format it
      StringWriter writer = new StringWriter();
      m_formatter.format(root,
         Repository.getMetadata().getMessage("CSV_NonUniformDelimiter"),
         new WriterOutput(writer));
      
      assertEquals("first:second:third:fourth\n" +
         "realdata1,realdata2:realdata3,\"realdata4\"\n" +
         "a\\,a:a,'b,b:b':c\\,c:c,\"d,d:d\"\n" +
         "a\\,a:a,'b,b:b':c\\,c:c\n",
         writer.toString());
   }
   
   
   /**
    * Test quoting behaviour when both quote character and escape character
    * are specified.
    */
   public void testFieldEscapedQuoting() throws Exception
   {
      TestCSVMessageFormatter format = new TestCSVMessageFormatter();
      CSVMessagePartMapping mapping = new CSVMessagePartMapping();
      
      mapping.setDelimiter(Character.valueOf(','));
      mapping.setQuote(Character.valueOf('\''));
      mapping.setEscape(Character.valueOf('_'));
      mapping.setQuoted(true);
            
      assertEquals("'Lorem __ipsum'", format.testFormatString(
         "Lorem _ipsum",
         mapping));
      
      assertEquals("'Lorem, ipsum?'", format.testFormatString(
         "Lorem, ipsum?",
         mapping));
      
      assertEquals("'Lorem _'Ipsum_''", format.testFormatString(
         "Lorem 'Ipsum'",
         mapping));
      
      assertEquals("'Lorem:\n_'Ipsum_''", format.testFormatString(
         "Lorem:\n'Ipsum'",
         mapping));
   }
   
   
   /**
    * Test quoting behaviour when only a quote character is specified,
    * and escape is disabled.
    */
   public void testFieldDoubleQuoting() throws Exception
   {
      TestCSVMessageFormatter format = new TestCSVMessageFormatter();
      CSVMessagePartMapping mapping = new CSVMessagePartMapping();
      
      mapping.setDelimiter(Character.valueOf(','));
      mapping.setQuote(Character.valueOf('\''));
      mapping.setEscape(null);  //no escape
      mapping.setQuoted(true);
            
      assertEquals("'Lorem ipsum!'", format.testFormatString(
         "Lorem ipsum!",
         mapping));
      
      assertEquals("'Lorem, ipsum?'", format.testFormatString(
         "Lorem, ipsum?",
         mapping));
      
      assertEquals("'Lorem ''Ipsum'''", format.testFormatString(
         "Lorem 'Ipsum'",
         mapping));
      
      assertEquals("'Lorem:\n''Ipsum'''", format.testFormatString(
         "Lorem:\n'Ipsum'",
         mapping));

      mapping.setQuoted(false);
      mapping.setOrdinal(1);

      assertEquals("'a,b'", format.testFormatString("a,b", mapping));
      assertEquals("a b", format.testFormatString("a b", mapping));
   }
   
   
   /**
    * Test quoting behaviour when only an escape character is
    * specified, and quote is disabled.
    */
   public void testFieldEscapeButNoQuoting() throws Exception
   {
      TestCSVMessageFormatter format = new TestCSVMessageFormatter();
      CSVMessagePartMapping mapping = new CSVMessagePartMapping();
      
      mapping.setMessagePart(new PrimitiveMessagePart("TestPart"));
      
      mapping.setDelimiter(Character.valueOf(','));
      mapping.setQuote(null);  //no quote
      mapping.setEscape(Character.valueOf('_'));
            
      assertEquals("Lorem __ipsum", format.testFormatString(
         "Lorem _ipsum",
         mapping));
      
      assertEquals("Lorem_, ipsum? dolor_, sit__ amet.", format.testFormatString(
         "Lorem, ipsum? dolor, sit_ amet.",
         mapping));
      
      assertEquals("Lorem_, ipsum?", format.testFormatString(
         "Lorem, ipsum?",
         mapping));
      
      assertEquals("Lorem 'Ipsum'", format.testFormatString(
         "Lorem 'Ipsum'",
         mapping));
      
      //Error: A newline character in data is an error when there
      //is no quote character to use.
      try {
         format.testFormatString("Lorem:\n'Ipsum'", mapping);
         fail();
      }
      catch (IntegrationException e)
      {
         assertEquals("err.integration.csv.fieldHasNewlineButCantQuote", e.getErrorCode());
         assertEquals("TestPart", e.getErrorArgs()[0]);
      }
   }


   /**
    * Test quoting behaviour when neither quote character nor escape
    * character are specified.
    */
   public void testFieldNoEscapeNoQuote() throws Exception
   {
      TestCSVMessageFormatter format = new TestCSVMessageFormatter();
      CSVMessagePartMapping mapping = new CSVMessagePartMapping();

      mapping.setDelimiter(Character.valueOf(','));
      mapping.setQuote(null);
      mapping.setEscape(null);

      assertEquals("Lorem, ipsum", format.testFormatString(
         "Lorem, ipsum",
         mapping));

      assertEquals("\"Lorem,\"ipsum?\"", format.testFormatString(
         "\"Lorem,\"ipsum?\"",
         mapping));

      assertEquals("Lorem\\Ipsum", format.testFormatString(
         "Lorem\\Ipsum",
         mapping));
   }


   /**
    * With both escape and quote characters specified, test that
    * only fields that need to be quoted are quoted.
    */
   public void testFieldMinimalQuoting() throws Exception
   {
      TestCSVMessageFormatter format = new TestCSVMessageFormatter();
      CSVMessagePartMapping mapping = new CSVMessagePartMapping();
      
      mapping.setDelimiter(Character.valueOf(','));
      mapping.setQuote(Character.valueOf('\''));
      mapping.setEscape(Character.valueOf('_'));
      

      assertEquals("Lorem __ipsum", format.testFormatString(
         "Lorem _ipsum",
         mapping));
      
      mapping.setEscape(null);
      
      assertEquals("Lorem _ipsum", format.testFormatString(
         "Lorem _ipsum",
         mapping));
      
      mapping.setEscape(Character.valueOf('_'));
      
      assertEquals("'Lorem ___'ipsum'", format.testFormatString(
         "Lorem _'ipsum",
         mapping));
      
      assertEquals("'Lorem __,ipsum'", format.testFormatString(
         "Lorem _,ipsum",
         mapping));
      
      assertEquals("'Lorem, ipsum?'", format.testFormatString(
         "Lorem, ipsum?",
         mapping));
      
      assertEquals("'Lorem _'Ipsum_''", format.testFormatString(
         "Lorem 'Ipsum'",
         mapping));
      
      assertEquals("'Lorem:\nIpsum'", format.testFormatString(
         "Lorem:\nIpsum",
         mapping));

      mapping.setOrdinal(1);

      assertEquals("'a,b'", format.testFormatString("a,b", mapping));
      assertEquals("a b", format.testFormatString("a b", mapping));
   }
   
   
   // inner classes
   
   /**
    * A test-only class used to expose the formatString method
    * of CSVMessageFormatter for white-box testing.
    */
   private class TestCSVMessageFormatter extends CSVMessageFormatter
   {
      public String testFormatString(String sToFormat, CSVMessagePartMapping mapping) throws IOException
      {
         StringWriter writer = new StringWriter();
         m_writer = writer;
         formatString(sToFormat, mapping);
         return writer.toString();
      }
   }
   
}
