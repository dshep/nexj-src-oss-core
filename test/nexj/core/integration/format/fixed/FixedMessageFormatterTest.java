// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.fixed;

import java.io.StringWriter;
import java.sql.Timestamp;
import java.util.List;
import java.util.ArrayList;
import java.util.Locale;
import java.util.TimeZone;

import junit.framework.TestCase;

import nexj.core.integration.IntegrationException;
import nexj.core.integration.MessageFormatter;
import nexj.core.integration.io.WriterOutput;
import nexj.core.meta.Repository;
import nexj.core.meta.integration.Format;
import nexj.core.meta.integration.Message;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.InvocationContext;


public class FixedMessageFormatterTest extends TestCase
{
   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   protected Format m_format;
   protected MessageFormatter m_formatter;
   protected Message m_oneFieldMessage;

   protected void setUp() throws Exception
   {
      m_context = (InvocationContext)Repository.getMetadata().getComponent("System.InvocationContext").getInstance(null);
      m_context.setLocale(Locale.ENGLISH);
      m_context.setTimeZone(TimeZone.getTimeZone("America/New_York"));

      m_format = Repository.getMetadata().getFormat("Fixed");
      m_formatter = (MessageFormatter)m_format.getFormatter().getInstance(m_context);
      m_oneFieldMessage = Repository.getMetadata().getMessage("Fixed_OneField");
   }

   public void testFormatOutputSingleFieldsMessage() throws Exception
   {
      TransferObject root = new TransferObject("Table");
      TransferObject record1 = new TransferObject();
      record1.setValue("field_1", "John");

      TransferObject record2 = new TransferObject();
      record2.setValue("field_1", "#Jane");

      TransferObject record3 = new TransferObject();
      record3.setValue("field_1", "Jack");

      TransferObject record4 = new TransferObject();
      record4.setValue("field_1", "Jack");

      List recordList = new ArrayList(5);
      recordList.add(record1);
      recordList.add(record2);
      recordList.add(record3);
      recordList.add(record4);
      root.setValue("record", recordList);

      //Format it
      StringWriter writer = new StringWriter();
      m_formatter.format(root, m_oneFieldMessage, new WriterOutput(writer));

      assertEquals("John      " +
         "#Jane     " +
         "Jack      " +
         "Jack      ",
         writer.toString());
   }

   public void testFormatOutputMessageWithSingleRow() throws Exception
   {
      TransferObject root = new TransferObject("Table");
      TransferObject record1 = new TransferObject();
      record1.setValue("field_1", "JohnnyB");

      List recordList = new ArrayList(5);
      recordList.add(record1);
      root.setValue("record", recordList);

      //Format it
      StringWriter writer = new StringWriter();
      m_formatter.format(root, m_oneFieldMessage, new WriterOutput(writer));

      assertEquals("JohnnyB   ", writer.toString());
   }

   /**
    * Tests that a zero-length collection yields zero rows of output.
    */
   public void testFormatOutputMessageWithZeroRows() throws Exception
   {
      TransferObject root = new TransferObject("Table");
      List recordList = new ArrayList(0);

      root.setValue("record", recordList);

      //Format it
      StringWriter writer = new StringWriter();
      m_formatter.format(root, m_oneFieldMessage, new WriterOutput(writer));

      assertEquals("", writer.toString());
   }

   public void testFormatOutputSingleFieldsWithPrefixMessage() throws Exception
   {
      TransferObject root = new TransferObject("Table");
      TransferObject record1 = new TransferObject();
      record1.setValue("field_1", "John");

      TransferObject record2 = new TransferObject();
      record2.setValue("field_1", "#Jane");

      TransferObject record3 = new TransferObject();
      record3.setValue("field_1", "Jack");

      TransferObject record4 = new TransferObject();
      record4.setValue("field_1", "Jack");

      List recordList = new ArrayList(5);
      recordList.add(record1);
      recordList.add(record2);
      recordList.add(record3);
      recordList.add(record4);
      root.setValue("record", recordList);

      //Format it
      StringWriter writer = new StringWriter();
      m_formatter.format(root, Repository.getMetadata().getMessage("Fixed_OneFieldWithPrefix"), new WriterOutput(writer));

      assertEquals("<<John    " +
         "<<#Jane   " +
         "<<Jack    " +
         "<<Jack    ",
         writer.toString());
   }

   public void testFormatOutputSingleFieldsWithPrefixAlignRightMessage() throws Exception
   {
      TransferObject root = new TransferObject("Table");
      TransferObject record1 = new TransferObject();
      record1.setValue("field_1", "John");

      TransferObject record2 = new TransferObject();
      record2.setValue("field_1", "");

      TransferObject record3 = new TransferObject();
      record3.setValue("field_1", "X");

      TransferObject record4 = new TransferObject();
      record4.setValue("field_1", "Joe Blow Frat");

      List recordList = new ArrayList(5);
      recordList.add(record1);
      recordList.add(record2);
      recordList.add(record3);
      recordList.add(record4);
      root.setValue("record", recordList);

      //Format it
      StringWriter writer = new StringWriter();
      m_formatter.format(root, Repository.getMetadata().getMessage("Fixed_OneFieldWithPrefixAlignRight"), new WriterOutput(writer));

      assertEquals("<<         John" +
         "<<             " +
         "<<            X" +
         "<<Joe Blow Frat",
         writer.toString());
   }

   public void testFormatOutputSingleFieldsWithPrefixAndSuffixMessage() throws Exception
   {
      TransferObject root = new TransferObject("Table");
      TransferObject record1 = new TransferObject();
      record1.setValue("field_1", "John");

      TransferObject record2 = new TransferObject();
      record2.setValue("field_1", "#Jane");

      TransferObject record3 = new TransferObject();
      record3.setValue("field_1", "X");

      TransferObject record4 = new TransferObject();
      record4.setValue("field_1", "");

      List recordList = new ArrayList(5);
      recordList.add(record1);
      recordList.add(record2);
      recordList.add(record3);
      recordList.add(record4);
      root.setValue("record", recordList);

      //Format it
      StringWriter writer = new StringWriter();
      m_formatter.format(root, Repository.getMetadata().getMessage("Fixed_OneFieldWithPrefixAndSuffix"), new WriterOutput(writer));

      assertEquals("<<John >>>" +
         "<<#Jane>>>" +
         "<<X    >>>" +
         "<<     >>>",
         writer.toString());
   }

   public void testFormatOutputInheritedMessage() throws Exception
   {
      TransferObject root = new TransferObject("Table");
      TransferObject record1 = new TransferObject();
      record1.setValue("field_1", "Apple");
      record1.setValue("field_2", "Apple");

      TransferObject record2 = new TransferObject();
      record2.setValue("field_1", "Pear");
      record2.setValue("field_2", "Pear");

      List recordList = new ArrayList(3);
      recordList.add(record1);
      recordList.add(record2);
      root.setValue("records", recordList);

      //Format it
      StringWriter writer = new StringWriter();
      m_formatter.format(root, Repository.getMetadata().getMessage("Fixed_TwoFieldsDerived"), new WriterOutput(writer));

      assertEquals("Apple     Apple*****" +
         "Pear      Pear******",
         writer.toString());
   }

   public void testFormatOutputMessageWithTimestamps() throws Exception
   {
      TransferObject root = new TransferObject("Table");

      Timestamp ts1 = Timestamp.valueOf("2010-03-03 18:30:00.123");
      Timestamp ts2 = Timestamp.valueOf("2011-03-13 19:31:11.123");
      Timestamp ts3 = Timestamp.valueOf("2012-03-23 20:42:42.421");

      TransferObject record1 = new TransferObject();
      record1.setValue("ts_1", ts1);
      record1.setValue("ts_2", ts1);
      record1.setValue("ts_3", ts1);
      record1.setValue("ts_4", ts1);
      record1.setValue("ts_5", ts1);

      TransferObject record2 = new TransferObject();
      record2.setValue("ts_1", ts2);
      record2.setValue("ts_2", ts2);
      record2.setValue("ts_3", ts2);
      record2.setValue("ts_4", ts2);
      record2.setValue("ts_5", ts2);

      TransferObject record3 = new TransferObject();
      record3.setValue("ts_1", ts3);
      record3.setValue("ts_2", ts3);
      record3.setValue("ts_3", ts3);
      record3.setValue("ts_4", ts3);
      record3.setValue("ts_5", ts3);


      List recordList = new ArrayList(2);
      recordList.add(record1);
      recordList.add(record2);
      recordList.add(record3);
      root.setValue("record", recordList);

      //Format it
      StringWriter writer = new StringWriter();
      m_formatter.format(root, Repository.getMetadata().getMessage("Fixed_Timestamps"), new WriterOutput(writer));

      assertEquals(
         //record 1
         "2010-03-03 23:30:00.123000000 " +  // width=30
         "20100303183000.123-0500       " +  // width=30 format="yyyyMMddHHmmss.SSSZ"
         "            20100303183000.123" +  // width=30 alignment="right" format="yyyyMMddHHmmss.SSS"
         "((        20100303183000.123))" +  // width=30 alignment="right" format="yyyyMMddHHmmss.SSS" prefix="((" suffix="))"
         "[[[[02010.March.03 AD 06:30:00 PM__" +  // width=35 format="yyyyy.MMMMM.dd GGG hh:mm:ss aaa" padding="_" prefix="[[[["

         //record 2
         "2011-03-13 23:31:11.123000000 " +  // width=30
         "20110313193111.123-0400       " +  // width=30 format="yyyyMMddHHmmss.SSSZ"
         "            20110313193111.123" +  // width=30 alignment="right" format="yyyyMMddHHmmss.SSS"
         "((        20110313193111.123))" +  // width=30 alignment="right" format="yyyyMMddHHmmss.SSS" prefix="((" suffix="))"
         "[[[[02011.March.13 AD 07:31:11 PM__" +  // width=35 format="yyyyy.MMMMM.dd GGG hh:mm:ss aaa" padding="_" prefix="[[[["

         //record 3
         "2012-03-24 00:42:42.421000000 " +  // width=30
         "20120323204242.421-0400       " +  // width=30 format="yyyyMMddHHmmss.SSSZ"
         "            20120323204242.421" +  // width=30 alignment="right" format="yyyyMMddHHmmss.SSS"
         "((        20120323204242.421))" +  // width=30 alignment="right" format="yyyyMMddHHmmss.SSS" prefix="((" suffix="))"
         "[[[[02012.March.23 AD 08:42:42 PM__"    // width=35 format="yyyyy.MMMMM.dd GGG hh:mm:ss aaa" padding="_" prefix="[[[["

         , writer.toString());
   }

   public void testFormatOutputRequiredFields() throws Exception
   {
      String sMessageName = "Fixed_RequiredFields";

      TransferObject root = new TransferObject("Table");
      TransferObject record1 = new TransferObject();
      Timestamp ts1 = Timestamp.valueOf("2010-03-03 18:30:00.123");
      Timestamp ts2 = Timestamp.valueOf("2011-03-13 19:31:11.123");
      Timestamp ts3 = Timestamp.valueOf("2012-03-23 20:42:42.421");

      record1.setValue("field_1", "John");
      record1.setValue("field_2", ts1);

      TransferObject record2 = new TransferObject();
      record2.setValue("field_1", "#Jane");
      record2.setValue("field_2", ts2);

      TransferObject record3 = new TransferObject();
      record3.setValue("field_1", "X");
      record3.setValue("field_2", ts3);

      TransferObject record4 = new TransferObject();
      record4.setValue("field_1", "");      // leaving field_2 blank

      TransferObject record5 = new TransferObject();
      record5.setValue("field_2", "");      // leaving field_1 blank

      List recordList = new ArrayList(5);
      recordList.add(record1);
      recordList.add(record2);
      recordList.add(record3);

      root.setValue("record", recordList);

      //Format it
      StringWriter writer = new StringWriter();
      m_formatter.format(root, Repository.getMetadata().getMessage(sMessageName), new WriterOutput(writer));

      assertEquals(
         "John______|2010-03-03 23:30:00.123000000" +
         "#Jane_____|2011-03-13 23:31:11.123000000" +
         "X_________|2012-03-24 00:42:42.421000000",
         writer.toString());

      // force a failure on field 2
      try
      {
         recordList.add(record4);
         m_formatter.format(root, Repository.getMetadata().getMessage(sMessageName), new WriterOutput(writer));
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.minPartCount", ex.getErrorCode());
         assertEquals(sMessageName + " record field_2", ex.getErrorArgs()[0]);
      }

      // force a failure on field 1
      try
      {
         recordList.remove(record4);
         recordList.add(record5);
         m_formatter.format(root, Repository.getMetadata().getMessage(sMessageName), new WriterOutput(writer));
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.minPartCount", ex.getErrorCode());
         assertEquals(sMessageName + " record field_1", ex.getErrorArgs()[0]);
      }
}
}
