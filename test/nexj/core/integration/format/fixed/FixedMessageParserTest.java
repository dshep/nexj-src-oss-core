// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.fixed;


import java.io.StringWriter;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.Locale;
import java.util.TimeZone;

import junit.framework.TestCase;

import nexj.core.integration.IntegrationException;
import nexj.core.integration.MessageFormatter;
import nexj.core.integration.MessageParser;
import nexj.core.integration.io.StringInput;
import nexj.core.integration.io.WriterOutput;
import nexj.core.meta.Primitive;
import nexj.core.meta.Repository;
import nexj.core.meta.integration.Format;
import nexj.core.meta.integration.Message;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.InvocationContext;
import nexj.core.util.Logger;
import nexj.core.util.PagedArrayList;

public class FixedMessageParserTest extends TestCase
{
   // associations

   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   protected Format m_format;
   protected MessageParser m_parser;
   protected MessageFormatter m_formatter;
   protected Message m_one_field;

   protected void setUp() throws Exception
   {
      m_context = (InvocationContext)Repository.getMetadata().getComponent("System.InvocationContext").getInstance(null);
      m_context.setLocale(Locale.ENGLISH);
      m_context.setTimeZone(TimeZone.getTimeZone("America/New_York"));

      m_format = Repository.getMetadata().getFormat("Fixed");
      m_parser = (MessageParser)m_format.getParser().getInstance(m_context);
      m_formatter = (MessageFormatter)m_format.getFormatter().getInstance(null);
   }

   /**
    * Tests basic parser operation of a single fielded message, including blank
    * padding and last-record handling.
    */
   public void testParseOneField() throws Exception
   {
      String sMessageName = "Fixed_OneField";

      String sRec1 = "r1 f1";
      String sRec2 = "__r2--f1__";
      String sRec3 = "(r3 f1)";
      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         sRec1 + "     " +   // record 1, blank padded
         sRec2 +             // record 2, exact width
         sRec3               // record 3, not a full width, not blank padded
      ), Repository.getMetadata().getMessage(sMessageName));

      assertEquals(sMessageName, root.getClassName());

      List recordList = (List)root.getValue("record");

      assertEquals(3, recordList.size());

      record = (TransferObject)recordList.get(0);
      assertEquals(sRec1, record.getValue("field_1"));
      assertNull(record.getClassName());

      record = (TransferObject)recordList.get(1);
      assertEquals(sRec2, record.getValue("field_1"));
      assertNull(record.getClassName());

      record = (TransferObject)recordList.get(2);
      assertEquals(sRec3, record.getValue("field_1"));
      assertNull(record.getClassName());
   }

   /**
    * Tests basic parser operation of a single fielded message that uses tab
    * character for padding.
    */
   public void testParseOneFieldTabPad() throws Exception
   {
      String sMessageName = "Fixed_OneFieldTabPadding";

      String sRec1 = "r1 f1";
      String sRec2 = "__r2--f1__";
      String sRec3 = "(r3 f1)";
      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         sRec1 + "\t\t\t\t\t" +   // record 1, blank padded
         sRec2 +             // record 2, exact width
         sRec3               // record 3, not a full width, not blank padded
      ), Repository.getMetadata().getMessage(sMessageName));

      assertEquals(sMessageName, root.getClassName());

      List recordList = (List)root.getValue("record");

      assertEquals(3, recordList.size());

      record = (TransferObject)recordList.get(0);
      assertEquals(sRec1, record.getValue("field_1"));
      assertNull(record.getClassName());

      record = (TransferObject)recordList.get(1);
      assertEquals(sRec2, record.getValue("field_1"));
      assertNull(record.getClassName());

      record = (TransferObject)recordList.get(2);
      assertEquals(sRec3, record.getValue("field_1"));
      assertNull(record.getClassName());
   }

   /**
    * Tests basic parser operation of a single fielded message that uses tab
    * character for padding.
    */
   public void testParseOneFieldU0020Pad() throws Exception
   {
      String sMessageName = "Fixed_OneFieldU0020Padding";

      String sRec1 = "r1 f1";
      String sRec2 = "__r2--f1__";
      String sRec3 = "(r3 f1)";

      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         sRec1 + "\u0020\u0020\u0020\u0020\u0020" +   // record 1, blank padded
         sRec2 +             // record 2, exact width
         sRec3               // record 3, not a full width, not blank padded
      ), Repository.getMetadata().getMessage(sMessageName));

      assertEquals(sMessageName, root.getClassName());

      List recordList = (List)root.getValue("record");

      assertEquals(3, recordList.size());

      record = (TransferObject)recordList.get(0);
      assertEquals(sRec1, record.getValue("field_1"));
      assertNull(record.getClassName());

      record = (TransferObject)recordList.get(1);
      assertEquals(sRec2, record.getValue("field_1"));
      assertNull(record.getClassName());

      record = (TransferObject)recordList.get(2);
      assertEquals(sRec3, record.getValue("field_1"));
      assertNull(record.getClassName());
   }

   /**
    * Tests basic parser operation of a single fielded message, including blank
    * padding and last-record handling, where the record also has a prefix and
    * suffix.
    */
   public void testParseOneFieldMessagePrefix() throws Exception
   {
      String sMessageName = "Fixed_OneField_MsgPrefix";

      String sRec1 = "r1 f1";
      String sRec2 = "__r2--f1__";
      String sRec3 = "(r3 f1)";
      String sMsgPrefix = "***|";

      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         sMsgPrefix +
         sRec1 + "     " +  // record 1, blank padded
         sRec2 +            // record 2, exact width
         sRec3 + "   "      // record 3, not a full width, must be blank padded
      ), Repository.getMetadata().getMessage(sMessageName));

      assertEquals(sMessageName, root.getClassName());

      List recordList = (List)root.getValue("record");

      assertEquals(3, recordList.size());

      record = (TransferObject)recordList.get(0);
      assertEquals(sRec1, record.getValue("field_1"));
      assertNull(record.getClassName());

      record = (TransferObject)recordList.get(1);
      assertEquals(sRec2, record.getValue("field_1"));
      assertNull(record.getClassName());

      record = (TransferObject)recordList.get(2);
      assertEquals(sRec3, record.getValue("field_1"));
      assertNull(record.getClassName());
   }

   /**
    * Tests basic parser operation of a single fielded message, including blank
    * padding and last-record handling, where the record also has a prefix and
    * suffix.
    */
   public void testParseOneFieldRecordPreSuf() throws Exception
   {
      String sMessageName = "Fixed_OneField_RecPreSuf";

      String sRec1 = "r1 f1";
      String sRec2 = "__r2--f1__";
      String sRec3 = "(r3 f1)";
      String sRecPrefix = "<(<";
      String sRecSuffix = ">)>";

      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         sRecPrefix + sRec1 + "     " + sRecSuffix +  // record 1, blank padded
         sRecPrefix + sRec2 + sRecSuffix +            // record 2, exact width
         sRecPrefix + sRec3 + "   " + sRecSuffix      // record 3, not a full width, must be blank padded
      ), Repository.getMetadata().getMessage(sMessageName));

      assertEquals(sMessageName, root.getClassName());

      List recordList = (List)root.getValue("record");

      assertEquals(3, recordList.size());

      record = (TransferObject)recordList.get(0);
      assertEquals(sRec1, record.getValue("field_1"));
      assertNull(record.getClassName());

      record = (TransferObject)recordList.get(1);
      assertEquals(sRec2, record.getValue("field_1"));
      assertNull(record.getClassName());

      record = (TransferObject)recordList.get(2);
      assertEquals(sRec3, record.getValue("field_1"));
      assertNull(record.getClassName());
   }

   /**
    * Tests basic parser operation of a single fielded message, including blank
    * padding and last-record handling, where the record also has a prefix and
    * suffix.
    */
   public void testParseOneFieldRecordSuffix() throws Exception
   {
      String sMessageName = "Fixed_OneField_RecSuf";

      String sRec1 = "r1 f1";
      String sRec2 = "__r2--f1__";
      String sRec3 = "(r3 f1)";
      String sRecSuffix = "^^^^^";

      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         sRec1 + "     " + sRecSuffix +  // record 1, blank padded
         sRec2 + sRecSuffix +            // record 2, exact width
         sRec3 + "   " + sRecSuffix      // record 3, not a full width, must be blank padded
      ), Repository.getMetadata().getMessage(sMessageName));

      assertEquals(sMessageName, root.getClassName());

      List recordList = (List)root.getValue("record");

      assertEquals(3, recordList.size());

      record = (TransferObject)recordList.get(0);
      assertEquals(sRec1, record.getValue("field_1"));
      assertNull(record.getClassName());

      record = (TransferObject)recordList.get(1);
      assertEquals(sRec2, record.getValue("field_1"));
      assertNull(record.getClassName());

      record = (TransferObject)recordList.get(2);
      assertEquals(sRec3, record.getValue("field_1"));
      assertNull(record.getClassName());
   }

   /**
    * Tests basic parser operation of fields aligned right, including blank
    * padding and last-record handling.
    */
   public void testParseOneFieldAlignRight() throws Exception
   {
      String sMessageName = "Fixed_OneFieldAlignRight";

      String sRec1 = "r1 f1";
      String sRec2 = "__r2--f1__";
      String sRec3 = "(r3 f1)";
      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         "     " + sRec1 +    // record 1, blank padded
         sRec2 +             // record 2, exact width
         " " + sRec3               // record 3, not a full width, not blank padded
      ), Repository.getMetadata().getMessage(sMessageName));

      assertEquals(sMessageName, root.getClassName());

      List recordList = (List)root.getValue("record");

      assertEquals(3, recordList.size());

      record = (TransferObject)recordList.get(0);
      assertEquals(sRec1, record.getValue("field_1"));
      assertNull(record.getClassName());

      record = (TransferObject)recordList.get(1);
      assertEquals(sRec2, record.getValue("field_1"));
      assertNull(record.getClassName());

      record = (TransferObject)recordList.get(2);
      assertEquals(sRec3, record.getValue("field_1"));
      assertNull(record.getClassName());
   }

   /**
    * Tests basic parser operation, one field, one record.
    */
   public void testParseOneFieldOneRecord() throws Exception
   {
      String sMessageName = "Fixed_OneField";

      String sRec1 = "r1 f1";
      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         sRec1               // record 1, not a full width, not blank padded
      ), Repository.getMetadata().getMessage(sMessageName));

      assertEquals(sMessageName, root.getClassName());

      List recordList = (List)root.getValue("record");

      assertEquals(1, recordList.size());

      record = (TransferObject)recordList.get(0);
      assertEquals(sRec1, record.getValue("field_1"));
      assertNull(record.getClassName());
   }

   /**
    * Tests basic parser operation of fields with a prefix, including blank
    * padding and last-record handling.
    */
   public void testParseOneFieldWithPrefix() throws Exception
   {
      String sMessageName = "Fixed_OneFieldWithPrefix";

      String sPrefix = "!!";

      String sRec1 = "r1 f1";     // field length is 8 (10 - prefix_length)
      String sRec2 = "_r2--f1_";
      String sRec3 = "(r3f1)";

      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         sPrefix + "   " + sRec1 +   // record 1, blank padded
         sPrefix + sRec2 +             // record 2, exact width
         sPrefix + sRec3               // record 3, not a full width, not blank padded
      ), Repository.getMetadata().getMessage(sMessageName));

      assertEquals(sMessageName, root.getClassName());

      List recordList = (List)root.getValue("record");

      assertEquals(3, recordList.size());

      record = (TransferObject)recordList.get(0);
      assertEquals(sRec1, record.getValue("field_1"));
      assertNull(record.getClassName());

      record = (TransferObject)recordList.get(1);
      assertEquals(sRec2, record.getValue("field_1"));
      assertNull(record.getClassName());

      record = (TransferObject)recordList.get(2);
      assertEquals(sRec3, record.getValue("field_1"));
      assertNull(record.getClassName());
   }


   /**
    * Tests basic parser operation of fields with a prefix, including blank
    * padding and last-record handling.
    */
   public void testParseOneFieldWithPrefixAlignRight() throws Exception
   {
      String sMessageName = "Fixed_OneFieldWithPrefixAlignRight";

      String sPrefix = "!!";

      String sRec1 = "r1 f1";         // field length is 13 (15 - prefix_length)
      String sRec2 = "_r2-------f1_";
      String sRec3 = "r1 f1";
      String sRec4 = "(r3f1)";

      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         sPrefix + "        " + sRec1 +  // record 1, blank padded on right
         sPrefix + sRec2 +               // record 2, exact width
         sPrefix + "        " + sRec3 +  // record 3, blank padded on right
         sPrefix + sRec4                 // record 4, not a full width, not blank padded
      ), Repository.getMetadata().getMessage(sMessageName));

      assertEquals(sMessageName, root.getClassName());

      List recordList = (List)root.getValue("record");

      assertEquals(4, recordList.size());

      record = (TransferObject)recordList.get(0);
      assertEquals(sRec1, record.getValue("field_1"));
      assertNull(record.getClassName());

      record = (TransferObject)recordList.get(1);
      assertEquals(sRec2, record.getValue("field_1"));
      assertNull(record.getClassName());

      record = (TransferObject)recordList.get(2);
      assertEquals(sRec3, record.getValue("field_1"));
      assertNull(record.getClassName());
   }


   /**
    * Tests basic parser operation of fields with a suffix, including blank
    * padding and last-record handling.
    */
   public void testParseOneFieldWithSuffix() throws Exception
   {
      String sMessageName = "Fixed_OneFieldWithSuffix";
      String sRec1 = "r1 f1";     // field length is 7 (10 - suffix_length)
      String sRec2 = "r2---f1";
      String sRec3 = "(r3f1)";
      String sSuffix = ">>>";

      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         sRec1 + "  " + sSuffix  + // record 1, blank padded
         sRec2 + sSuffix +         // record 2, exact width
         sRec3 + sSuffix           // record 3, no padding, no suffix
      ), Repository.getMetadata().getMessage(sMessageName));

      assertEquals(sMessageName, root.getClassName());

      List recordList = (List)root.getValue("record");

      assertEquals(3, recordList.size());

      record = (TransferObject)recordList.get(0);
      assertEquals(sRec1, record.getValue("field_1"));
      assertNull(record.getClassName());

      record = (TransferObject)recordList.get(1);
      assertEquals(sRec2, record.getValue("field_1"));
      assertNull(record.getClassName());

      record = (TransferObject)recordList.get(2);
      assertEquals(sRec3, record.getValue("field_1"));
      assertNull(record.getClassName());
   }


   /**
    * Tests basic parser operation of fields with a prefix and a suffix,
    * including blank padding and last-record handling.
    */
   public void testParseOneFieldWithPrefixAndSuffix() throws Exception
   {
      String sMessageName = "Fixed_OneFieldWithPrefixAndSuffix";

      String sPrefix = "<<";
      String sSuffix = ">>>";

      String sRec1 = "r1f1";     // field length is 5 (10 - prefix_length - suffix_length)
      String sRec2 = "_r2f1";
      String sRec3 = "r3f1";

      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         sPrefix + sRec1 + " " + sSuffix +   // record 1, blank padded
         sPrefix + sRec2 + sSuffix +         // record 2, exact width
         sPrefix + sRec3 + sSuffix           // record 3, no padding, no suffix
      ), Repository.getMetadata().getMessage(sMessageName));

      assertEquals(sMessageName, root.getClassName());

      List recordList = (List)root.getValue("record");

      assertEquals(3, recordList.size());

      record = (TransferObject)recordList.get(0);
      assertEquals(sRec1, record.getValue("field_1"));
      assertNull(record.getClassName());

      record = (TransferObject)recordList.get(1);
      assertEquals(sRec2, record.getValue("field_1"));
      assertNull(record.getClassName());

      record = (TransferObject)recordList.get(2);
      assertEquals(sRec3, record.getValue("field_1"));
      assertNull(record.getClassName());
   }


   /**
    * Tests basic parser operation of each primitive type of field.
    */
   public void testParseEveryPrimitiveType() throws Exception
   {
      String sMessageName = "Fixed_EveryPrimitiveType";

      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         "__r1--f1__" +       // field 1, string
         "1234567890" +       // field 2, integer
         "2468246.48" +       // field 3, double
         "false" + "     " +  // field 4, boolean, blank padded
         "2010-03-19 15:26:37.000000000"  // field 5, timestamp
      ), Repository.getMetadata().getMessage(sMessageName));

      assertEquals(sMessageName, root.getClassName());

      List recordList = (List)root.getValue("record");

      assertEquals(1, recordList.size());

      record = (TransferObject)recordList.get(0);
      assertEquals("__r1--f1__", record.getValue("fld_String"));
      assertEquals(Primitive.createInteger(1234567890), record.getValue("fld_Integer"));
      assertEquals(Primitive.createDouble(2468246.48), record.getValue("fld_Double"));
      assertFalse(Primitive.toBoolean(record.getValue("fld_Boolean")).booleanValue());
      assertEquals(Primitive.toTimestamp(new Long(1269012397000L)), record.getValue("fld_Timestamp"));
      assertNull(record.getClassName());
   }

   /**
    * Tests basic parser operation of each primitive type of field, including
    * blank padding.
    */
   public void testParseEveryPrimitiveType_WithPadding() throws Exception
   {
      String sMessageName = "Fixed_EveryPrimitiveType";

      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         "r1--f1" + "    " +     // field 1, string
         "123456" + "    " +     // field 2, integer
         "246.48" + "    " +     // field 3, double
         "true" + "      " +     // field 4, boolean, blank padded
         "2010-03-19 15:26:37.000000000"  // field 5, timestamp
      ), Repository.getMetadata().getMessage(sMessageName));

      assertEquals(sMessageName, root.getClassName());

      List recordList = (List)root.getValue("record");

      assertEquals(1, recordList.size());

      record = (TransferObject)recordList.get(0);
      assertEquals("r1--f1", record.getValue("fld_String"));
      assertEquals(Primitive.createInteger(123456), record.getValue("fld_Integer"));
      assertEquals(Primitive.createDouble(246.48), record.getValue("fld_Double"));
      assertTrue(Primitive.toBoolean(record.getValue("fld_Boolean")).booleanValue());
      assertEquals(Primitive.toTimestamp(new Long(1269012397000L)), record.getValue("fld_Timestamp"));
      assertNull(record.getClassName());
   }

   /**
    * Tests parser operation of each primitive type of field including a record prefix and suffix.
    */
   public void testParseEveryPrimitiveType_RecordPrefixSuffix() throws Exception
   {
      String sMessageName = "Fixed_EveryPrimitiveType_RecPreSuf";

      String sRecPrefix = "<(<";
      String sRecSuffix = ">)>";

      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         sRecPrefix +
         "__r1--f1__" +       // field 1, string
         "1234567890" +       // field 2, integer
         "1357135.79" +       // field 3, double
         "false" + "     " +  // field 4, boolean, blank padded
         "2010-03-19 15:26:37.000000000 "  // field 5, timestamp
         + sRecSuffix
         + sRecPrefix +
         "__r2--f1__" +       // field 1, string
         "1222222222" +       // field 2, integer
         "2468246.48" +       // field 3, double
         "true" + "      " +  // field 4, boolean, blank padded
         "2010-03-19 15:26:37.000000000 "  // field 5, timestamp
         + sRecSuffix
      ), Repository.getMetadata().getMessage(sMessageName));

      assertEquals(sMessageName, root.getClassName());

      List recordList = (List)root.getValue("record");

      assertEquals(2, recordList.size());

      record = (TransferObject)recordList.get(0);
      assertEquals("__r1--f1__", record.getValue("fld_String"));
      assertEquals(Primitive.createInteger(1234567890), record.getValue("fld_Integer"));
      assertEquals(Primitive.createDouble(1357135.79), record.getValue("fld_Double"));
      assertFalse(Primitive.toBoolean(record.getValue("fld_Boolean")).booleanValue());
      assertEquals(Primitive.toTimestamp(new Long(1269012397000L)), record.getValue("fld_Timestamp"));
      assertNull(record.getClassName());

      record = (TransferObject)recordList.get(1);
      assertEquals("__r2--f1__", record.getValue("fld_String"));
      assertEquals(Primitive.createInteger(1222222222), record.getValue("fld_Integer"));
      assertEquals(Primitive.createDouble(2468246.48), record.getValue("fld_Double"));
      assertTrue(Primitive.toBoolean(record.getValue("fld_Boolean")).booleanValue());
      assertEquals(Primitive.toTimestamp(new Long(1269012397000L)), record.getValue("fld_Timestamp"));
      assertNull(record.getClassName());
   }

   /**
    * Tests basic parser operation of each primitive type of field, including
    * blank padding.
    */
   public void testParseTimestamps() throws Exception
   {
      String sMessageName = "Fixed_Timestamps";

      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         "2010-03-19 15:26:37.000000000" + " " + // width=30
         "20110319102639.000-0500" + "       " + // format="yyyyMMddHHmmss.SSSZ" width=30
         "            " + "20120319112643.000" + // alignment="right" format="yyyyMMddHHmmss.SSS" width=30
         "((        20130319112637.000))" +      // alignment="right" format="yyyyMMddHHmmss.SSS" prefix="((" suffix="))" width="30"
         "[[[[__02014.March.19 AD 11:26:37 AM____"   // format="yyyyy.MMMMM.dd GGG hh:mm:ss aaa" padding="_" prefix="[[[[" width="35"
      ), Repository.getMetadata().getMessage(sMessageName));

      assertEquals(sMessageName, root.getClassName());

      List recordList = (List)root.getValue("record");

      assertEquals(1, recordList.size());

      record = (TransferObject)recordList.get(0);
      assertEquals(Primitive.toTimestamp(new Long(1269012397000L)), record.getValue("ts_1"));
      assertEquals(Primitive.toTimestamp(new Long(1300548399000L)), record.getValue("ts_2"));
      assertEquals(Primitive.toTimestamp(new Long(1332170803000L)), record.getValue("ts_3"));
      assertEquals(Primitive.toTimestamp(new Long(1363706797000L)), record.getValue("ts_4"));
      assertEquals(Primitive.toTimestamp(new Long(1395242797000L)), record.getValue("ts_5"));
      assertNull(record.getClassName());
   }

   /**
    * Tests parser operation using paging.  First formats NUM_MESSAGES records,
    * then parses them again printing out one TO for every MOD_NUM
    */
   public void testParseWithPaging() throws Exception
   {
      String sMessageName = "Fixed_RootPaging";

      String sRecName = "foo:";

      // Create a TO with NUM_MESSAGES records
      int NUM_MESSAGES = 10579;
      int DIVISOR = 1000;
      int MODULO = 765;

      List formatRecList = new ArrayList(NUM_MESSAGES);

      for(int i=0; i < NUM_MESSAGES; i++)
      {
         TransferObject rec = new TransferObject();

         rec.setValue("name", sRecName + i);
         rec.setValue("recNum", new Long(i));
         rec.setValue("created", new Timestamp(Calendar.getInstance().getTimeInMillis()));

         formatRecList.add(rec);
      }

      TransferObject formatRoot = new TransferObject("Table");

      formatRoot.setValue("record", formatRecList);

      assertEquals(NUM_MESSAGES, formatRecList.size());

      // Format the TO
      StringWriter writer = new StringWriter();
      Message msg = Repository.getMetadata().getMessage(sMessageName);

      m_formatter.format(formatRoot, msg, new WriterOutput(writer));

      Logger logger = Logger.getLogger("FixedMessageParserTest");

      String s = writer.toString();

      logger.info("size of formatted string is (" + s.length() + ") chars");

      // Parse the formatted string
      TransferObject parseRoot = m_parser.parse(new StringInput(s), msg);

      // Test it
      assertEquals(sMessageName, parseRoot.getClassName());

      List parseRecList = (List)parseRoot.getValue("record");

      assertTrue(parseRecList instanceof PagedArrayList);

      assertEquals(NUM_MESSAGES, parseRecList.size());

      for(int i=0; i < parseRecList.size(); i++)
      {
         TransferObject record = (TransferObject)formatRecList.get(i);
         assertEquals(sRecName + i, record.getValue("name"));
         assertEquals(record.getValue("recNum"), new Long(i));
         if (i % DIVISOR == MODULO)
         {
            logger.info("record: <<" + record.toString() + ">>");
         }
      }

      ((PagedArrayList)parseRecList).dispose();
   }

   /**
    * Tests parsing operation of optional fields.
    */
   public void testParseOptionalFields() throws Exception
   {
      String sMessageName = "Fixed_OptionalFields";

      String sR1F1 = "oneoneoneo";
      String sR1F2 = "|2010-03-19 15:26:37.000000000";
      String sEmptyF1 = "__________";     // all padding chars
      String sR2F2 = "|2012-03-19 15:26:43.000000000";
      String sEmptyF2 = "|^^^^^^^^^^^^^^^^^^^^^^^^^^^^^";

      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         sR1F1 + sR1F2 +      // record 1
         sEmptyF1 + sR2F2 +   // record 2
         sEmptyF1 + sEmptyF2  // record 3
      ), Repository.getMetadata().getMessage(sMessageName));

      assertEquals(sMessageName, root.getClassName());

      List recordList = (List)root.getValue("record");

      assertEquals(3, recordList.size());

      record = (TransferObject)recordList.get(0);
      assertEquals(sR1F1, record.getValue("field_1"));
      assertEquals(Primitive.toTimestamp(new Long(1269012397000L)), record.getValue("field_2"));

      record = (TransferObject)recordList.get(1);
      assertNull(record.getValue("field_1"));
      assertEquals(Primitive.toTimestamp(new Long(1332170803000L)), record.getValue("field_2"));

      record = (TransferObject)recordList.get(2);
      assertNull(record.getValue("field_1"));
      assertNull(record.getValue("field_2"));
   }

   /**
    * Tests basic parser operation of a single fielded message, including blank
    * padding and last-record handling.
    */
   public void testParseRequiredFields() throws Exception
   {
      String sMessageName = "Fixed_RequiredFields";

      String sR1F1 = "oneoneoneo";
      String sR1F2 = "|2010-03-19 15:26:37.000000000";
      String sEmptyF1 = "__________";     // all padding chars
      String sR2F2 = "|2012-03-19 15:26:43.000000000";
      String sEmptyF2 = "|^^^^^^^^^^^^^^^^^^^^^^^^^^^^^";

      TransferObject record;
      TransferObject root = m_parser.parse(new StringInput(
         sR1F1 + sR1F2 +       // record 1
         sR1F1 + sR2F2         // record 2
      ), Repository.getMetadata().getMessage(sMessageName));

      assertEquals(sMessageName, root.getClassName());

      List recordList = (List)root.getValue("record");

      assertEquals(2, recordList.size());

      record = (TransferObject)recordList.get(0);
      assertEquals(sR1F1, record.getValue("field_1"));
      assertEquals(Primitive.toTimestamp(new Long(1269012397000L)), record.getValue("field_2"));

      record = (TransferObject)recordList.get(1);
      assertEquals(sR1F1, record.getValue("field_1"));
      assertEquals(Primitive.toTimestamp(new Long(1332170803000L)), record.getValue("field_2"));

      //
      // Try some failure cases
      //
      try
      {
         m_parser.parse(new StringInput(
            sR1F1 + sR1F2 +       // record 1
            sEmptyF1 + sR2F2 +    // record 2, fails requiredness on field_1
            sEmptyF1 + sEmptyF2 + // record 3
            sR1F1 + sR2F2         // record 4
         ), Repository.getMetadata().getMessage(sMessageName));
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.parse.noDataForRequiredPart", ex.getErrorCode());
         assertEquals(sMessageName + " record field_1", ex.getErrorArgs()[0]);
         assertEquals(2, ((Integer)(ex.getErrorArgs()[1])).intValue());    // record 2 fails
      }

      try
      {
         m_parser.parse(new StringInput(
            sR1F1 + sR1F2 +    // record 1
            sR1F1 + sR2F2 +    // record 2
            sR1F1 + sEmptyF2 + // record 3, fails requiredness on field_2
            sR1F1 + sR2F2      // record 4
         ), Repository.getMetadata().getMessage(sMessageName));
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.parse.noDataForRequiredPart", ex.getErrorCode());
         assertEquals(sMessageName + " record field_2", ex.getErrorArgs()[0]);
         assertEquals(3, ((Integer)(ex.getErrorArgs()[1])).intValue());    // record 3 fails
      }
   }
}
