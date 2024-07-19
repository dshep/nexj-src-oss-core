package nexj.core.integration.format.json;

import java.io.StringReader;
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
import nexj.core.integration.io.ReaderInput;
import nexj.core.integration.io.WriterOutput;
import nexj.core.meta.Primitive;
import nexj.core.meta.Repository;
import nexj.core.meta.integration.CompositeMessagePart;
import nexj.core.meta.integration.Format;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessageTable;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.InvocationContext;
import nexj.core.util.Binary;
import nexj.core.util.PropertyIterator;

public class JSONMessageTest extends TestCase
{
   protected InvocationContext m_context;
   protected Format m_format;
   protected MessageFormatter m_formatter;
   protected Message m_literal;
   protected MessageParser m_parser;
   protected StringWriter m_writer;
   protected WriterOutput m_output;

   /**
    * Sets up variables for all test cases
    * @see junit.framework.TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      m_context = (InvocationContext)Repository.getMetadata().getComponent("System.InvocationContext").getInstance(null);

      m_context.setLocale(Locale.ENGLISH);
      m_context.setTimeZone(TimeZone.getTimeZone("America/New_York"));

      m_format = Repository.getMetadata().getFormat("JSON");
      m_formatter = (MessageFormatter)m_format.getFormatter().getInstance(m_context);
      m_parser = (MessageParser)m_format.getParser().getInstance(m_context);

      m_writer = new StringWriter();
      m_output = new WriterOutput(m_writer);
   }

   /**
    * Test formating and parsing of message value data types.
    */
   public void testDataTypes()
   {
      // Test of Primitive types
      m_literal = Repository.getMetadata().getMessage("JSON_Primitives");

      TransferObject tobj = new TransferObject();

      tobj.setValue("key1", "string");
      tobj.setValue("key2", new Boolean(true));
      tobj.setValue("key3", Primitive.createInteger(123));
      tobj.setValue("key4", Primitive.createFloat(Float.MAX_VALUE));
      tobj.setValue("key5", Primitive.createDouble(Double.MAX_VALUE));
      tobj.setValue("key6", Primitive.createLong(Long.MAX_VALUE));
      tobj.setValue("key7", Timestamp.valueOf("2010-03-03 18:30:00.123"));
      tobj.setValue("key8", Primitive.toDecimal(Primitive.createDouble(Double.MAX_VALUE)));
      tobj.setValue("key9", new Binary(new byte[]{1, 2, 3}));
      tobj.setValue("key10", "string2");

      m_formatter.format(tobj, m_literal, getWriterOutput());
      assertEquals("{" +
                      "\"key1\":\"string\"," +
                      "\"key2\":true," +
                      "\"key3\":123," +
                      "\"key4\":3.4028235E38," +
                      "\"key5\":1.7976931348623157E308," +
                      "\"key6\":" + Primitive.createLong(Long.MAX_VALUE) + "," +
                      "\"key7\":1267659000123," +
                      "\"key8\":1.7976931348623157E308," +
                      "\"key9\":\"010203\"," +
                      "\"key10\":\"string2\"" +
                   "}", m_writer.toString());
      checkEquals(tobj, m_parser.parse(getReaderInput(), m_literal));

      // Test ANY data type
      m_literal = Repository.getMetadata().getMessage("JSON_PrimitivesAny");

      tobj.setValue("key1", "string");
      m_formatter.format(tobj, m_literal, getWriterOutput());
      assertEquals("string", m_parser.parse(getReaderInput(), m_literal).getValue("key1"));

      tobj.setValue("key1", Boolean.TRUE);
      m_formatter.format(tobj, m_literal, getWriterOutput());
      assertEquals(Boolean.TRUE, m_parser.parse(getReaderInput(), m_literal).getValue("key1"));

      tobj.setValue("key1", Primitive.createInteger(123));
      m_formatter.format(tobj, m_literal, getWriterOutput());
      assertEquals(Primitive.createInteger(123), m_parser.parse(getReaderInput(), m_literal).getValue("key1"));

      tobj.setValue("key1", Primitive.createFloat(Float.MAX_VALUE));
      m_formatter.format(tobj, m_literal, getWriterOutput());
      assertEquals(Float.MAX_VALUE, ((Number)m_parser.parse(getReaderInput(), m_literal).getValue("key1")).floatValue(), 0);

      tobj.setValue("key1", Primitive.createDouble(Double.MAX_VALUE));
      m_formatter.format(tobj, m_literal, getWriterOutput());
      assertEquals(Double.MAX_VALUE, ((Number)m_parser.parse(getReaderInput(), m_literal).getValue("key1")).doubleValue(), 0);

      tobj.setValue("key1", Primitive.createLong(Long.MAX_VALUE));
      m_formatter.format(tobj, m_literal, getWriterOutput());
      assertEquals(Primitive.createLong(Long.MAX_VALUE), m_parser.parse(getReaderInput(), m_literal).getValue("key1"));

      tobj.setValue("key1", new BigDecimal(Double.MAX_VALUE));
      m_formatter.format(tobj, m_literal, getWriterOutput());
      assertEquals(Double.MAX_VALUE, ((Double)m_parser.parse(getReaderInput(), m_literal).getValue("key1")).doubleValue(),
         Double.MAX_VALUE * 1E-15);

      // Timestamp and Binary cannot be unmarshalled unambiguously 
      tobj.setValue("key1", Timestamp.valueOf("2010-03-03 18:30:00.123"));
      m_formatter.format(tobj, m_literal, getWriterOutput());
      assertEquals(1267659000123L, ((Number)m_parser.parse(getReaderInput(), m_literal).getValue("key1")).longValue());

      tobj.setValue("key1", new Binary(new byte[]{1, 2, 3}));
      m_formatter.format(tobj, m_literal, getWriterOutput());
      assertEquals(new Binary(new byte[]{1, 2, 3}).toString(), m_parser.parse(getReaderInput(), m_literal).getValue("key1"));
   }

   /**
    * Test formatting and parsing of formatted message values.
    */
   public void testPrimitiveFormatting()
   {
      m_literal = Repository.getMetadata().getMessage("JSON_ValueFormats");

      TransferObject tobj = new TransferObject();

      tobj.setValue("timestamp", Timestamp.valueOf("2010-03-03 18:30:00.123"));
      tobj.setValue("decimal", new BigDecimal("3.14159265358979323846264"));
      tobj.setValue("double", Primitive.createDouble(12345.95813));
      tobj.setValue("float", Primitive.createFloat(123456.95813f));
      tobj.setValue("integer", Primitive.createInteger(-5));
      tobj.setValue("long", new Long(Long.MAX_VALUE));
      tobj.setValue("boolean", new Boolean(true));

      m_formatter.format(tobj, m_literal, getWriterOutput());
      assertEquals("{" +
                      "\"timestamp\":\"20100303183000.123-0500\"," +
                      "\"decimal\":3.1415926536," +
                      "\"double\":\"12,345.96\"," +
                      "\"float\":\"123,456.96\"," +
                      "\"integer\":\"(5)\"," +
                      "\"long\":\"9223372036854775807\"," +
                      "\"boolean\":\"T\"" +
                   "}", m_writer.toString());
      tobj = m_parser.parse(getReaderInput(), m_literal);

      assertEquals(Timestamp.valueOf("2010-03-03 18:30:00.123"), tobj.getValue("timestamp"));
      assertEquals(new BigDecimal("3.1415926536"), tobj.getValue("decimal"));
      assertEquals(Primitive.createDouble(12345.96), tobj.getValue("double"));
      assertEquals(Primitive.createFloat(123456.96f), tobj.getValue("float"));
      assertEquals(Primitive.createInteger(-5), tobj.getValue("integer"));
      assertEquals(Primitive.createLong(Long.MAX_VALUE), tobj.getValue("long"));
      assertEquals(Boolean.TRUE, tobj.getValue("boolean"));
   }

   /**
    * Test formatting and parsing of primitive subtypes and value names.
    */
   public void testPrimitiveSubtypesAndNames()
   {
      m_literal = Repository.getMetadata().getMessage("JSON_ValueSubtypes_Names");

      Binary binary = new Binary(new byte[]{1, 2, 3});
      Timestamp timestamp = Timestamp.valueOf("2010-03-03 18:30:00.123");
      TransferObject tobj = new TransferObject();

      tobj.setValue("hex", binary);
      tobj.setValue("base64", binary);
      tobj.setValue("date", timestamp);
      tobj.setValue("time", timestamp);
      tobj.setValue("datetime", timestamp);

      m_formatter.format(tobj, m_literal, getWriterOutput());
      tobj = m_parser.parse(getReaderInput(), m_literal);
      m_formatter.format(tobj, m_literal, getWriterOutput());

      assertEquals("{" +
                      "\"hex\":\"010203\"," +
                      "\"base64\":\"AQID\"," +
                      "\"date\":\"2010-03-03Z\"," +
                      "\"time\":\"23:30:00.123Z\"," +
                      "\"datetime\":\"2010-03-03T23:30:00.123Z\"" +
                   "}", m_writer.toString());
   }

   /**
    * Test formatting and parsing of message format CompositeJSONMessagePartMapping.SUBTYPE_ROOT.
    */
   public void testMessageSubTypeRoot()
   {
      TransferObject tobj = new TransferObject();
      TransferObject rootPrimitive = new TransferObject();
      TransferObject object = new TransferObject();
      TransferObject rootArray = new TransferObject();
      Integer value = Primitive.createInteger(1);
      List values = new ArrayList();

      values.add(Primitive.createInteger(1));
      values.add(Primitive.createInteger(2));
      values.add(Primitive.createInteger(3));

      rootPrimitive.setValue("primitive", value);
      object.setValue("key", value);
      rootArray.setValue("values", values);

      TransferObject message = new TransferObject();

      message.setValue("rootPrimitive", rootPrimitive);
      message.setValue("object", object);
      message.setValue("rootArray", rootArray);

      List messages = new ArrayList();

      messages.add(message);

      tobj.setValue("messages", messages);

      m_literal = Repository.getMetadata().getMessage("JSON_SubType_Root");

      m_formatter.format(tobj, m_literal, getWriterOutput());
      assertEquals("[" +
                     "{" +
                        "\"rootPrimitive\":1," +
                        "\"object\":{\"key\":1}," +
                        "\"rootArray\":[1,2,3]" +
                     "}" +
                   "]", m_writer.toString());
      checkEquals(tobj, m_parser.parse(getReaderInput(), m_literal));
   }

   /**
    * Test message with format set to CompositeJSONMessagePartMapping.SUBTYPE_ARRAY.
    */
   public void testMessageSubTypeArray()
   {
      m_literal = Repository.getMetadata().getMessage("JSON_SubType_Array");

      TransferObject root = new TransferObject();
      TransferObject value = new TransferObject();
      TransferObject level1Left = new TransferObject();
      TransferObject level1Right = new TransferObject();
      TransferObject level2Left = new TransferObject();
      TransferObject level2Right = new TransferObject();

      value.setValue("name", "left of left");
      value.setValue("level", Primitive.createInteger(3));
      level2Left.setValue("value", value);

      value = new TransferObject();

      value.setValue("name", "right of left");
      value.setValue("level", Primitive.createInteger(3));
      level2Right.setValue("value", value);

      value = new TransferObject();

      value.setValue("name", "left of root");
      value.setValue("level", Primitive.createInteger(2));
      level1Left.setValue("value", value);
      level1Left.setValue("left", level2Left);
      level1Left.setValue("right", level2Right);

      value = new TransferObject();

      value.setValue("name", "right of root");
      value.setValue("level", Primitive.createInteger(2));
      level1Right.setValue("value", value);

      value = new TransferObject();

      value.setValue("name", "root");
      value.setValue("level", Primitive.createInteger(1));
      root.setValue("value", value);
      root.setValue("left", level1Left);
      root.setValue("right", level1Right);

      m_formatter.format(root, m_literal, getWriterOutput());
      assertEquals("[" +
                      "{\"name\":\"root\",\"level\":1}," +
                      "[" +
                         "{\"name\":\"left of root\",\"level\":2}," +
                         "[" +
                            "{\"name\":\"left of left\",\"level\":3}," +
                            "null," +
                            "null" +
                         "]," +
                         "[" +
                            "{\"name\":\"right of left\",\"level\":3}," +
                            "null," +
                            "null" +
                         "]" +
                      "]," +
                      "[" +
                         "{\"name\":\"right of root\",\"level\":2}," +
                         "null," +
                         "null" +
                      "]" +
                   "]", m_writer.toString());
      checkEquals(root, m_parser.parse(getReaderInput(), m_literal));
   }

   /**
    * Test message table parsing.
    */
   public void testParseInputMessageTable()
   {
      MessageTable table = new MessageTable();

      Message msg1 = Repository.getMetadata().getMessage("JSON_Primitives");
      Message msg2 = Repository.getMetadata().getMessage("JSON_Values");

      table.addMessage(msg1);
      table.addMessage(msg2);

      m_parser.initializeMessageTable(table);

     TransferObject tobj = m_parser.parse(new ReaderInput(
              new StringReader("{\"key1\":\"values\"," +
                                  "\"key3\":[\"one\",\"two\"]," +
                                  "\"array\":[" +
                                                 "[{\"name\":\"one\",\"level\":1},null,null]," +
                                                 "[{\"name\":\"two\",\"level\":1},null,null]" +
                                               "]" +
                               "}")), table);

     assertEquals("values", tobj.getValue("key1"));
     assert(tobj.getValue("key3") instanceof List);

     List list = (List)tobj.getValue("key3");

     assertEquals("one", list.get(0));
     assertEquals("two", list.get(1));

     tobj = m_parser.parse(new ReaderInput(new StringReader("{\"key1\":\"primitives\",\"key3\":1}")), table);

     assertEquals("primitives", tobj.getValue("key1"));
     assertEquals(Primitive.createInteger(1), tobj.getValue("key3"));
   }

   /**
    * Test formatter exceptions
    */
   public void testFormatterExceptions()
   {
      // Test Missing Part
      m_literal = Repository.getMetadata().getMessage("JSON_Messages");

      try
      {
         m_formatter.format(new TransferObject(), m_literal, getWriterOutput());
         fail();
      }
      catch (IntegrationException e)
      {
         assertEquals("err.integration.missingPart", e.getErrorCode());
         assertEquals(m_literal.getRoot().getPart(1).getFullPath(), e.getErrorArgs()[0]);
      }

      // Test expecting transfer object but found a different object
      TransferObject msg = new TransferObject();

      msg.setValue("message1", "message");

      try
      {
         m_formatter.format(msg, m_literal, getWriterOutput());
         fail();
      }
      catch (IntegrationException e)
      {
         assertEquals("err.integration.format", e.getErrorCode());
      }

      // Test part required but value is null
      msg.setValue("message1", new TransferObject());
      msg.setValue("message2", null);

      try
      {
         m_formatter.format(msg, m_literal, getWriterOutput());
         fail();
      }
      catch (IntegrationException e)
      {
         assertEquals("err.integration.missingPart", e.getErrorCode());
         assertEquals(m_literal.getRoot().getPart(1).getFullPath(), e.getErrorArgs()[0]);
      }

      // Test invalid cast to list in transfer object
      msg.setValue("message2", new TransferObject());
      msg.setValue("message3", new TransferObject());

      try
      {
         m_formatter.format(msg, m_literal, getWriterOutput());
         fail();
      }
      catch (IntegrationException e)
      {
         assertEquals("err.integration.format", e.getErrorCode());
      }

      // Test composite list smaller than minimum count
      // minimum for this message is 2
      List list = new ArrayList();

      list.add(null);
      msg.setValue("message1", new TransferObject());
      msg.setValue("message2", new TransferObject());
      msg.setValue("message3", list);

      try
      {
         m_formatter.format(msg, m_literal, getWriterOutput());
         fail();
      }
      catch (IntegrationException e)
      {
         assertEquals("err.integration.minPartCount", e.getErrorCode());
         assertEquals(m_literal.getRoot().getPart(2).getFullPath(), e.getErrorArgs()[0]);
      }

      // Test composite list greater than maximum count
      // maximum for this message is 5,
      list.add(null);
      list.add(null);
      list.add(null);
      list.add(null);
      list.add(null);
      list.add(null);
      msg.setValue("message3", list);

      try
      {
         m_formatter.format(msg, m_literal, getWriterOutput());
         fail();
      }
      catch (IntegrationException e)
      {
         assertEquals("err.integration.maxPartCount", e.getErrorCode());
         assertEquals(m_literal.getRoot().getPart(2).getFullPath(), e.getErrorArgs()[0]);
      }

      // Test class cast exception of list for primitive collection
      m_literal = Repository.getMetadata().getMessage("JSON_Values");
      msg.setValue("key1", new String("val1"));
      msg.setValue("key3", new TransferObject());

      try
      {
         m_formatter.format(msg, m_literal, getWriterOutput());
         fail();
      }
      catch (IntegrationException e)
      {
         assertEquals("err.integration.format", e.getErrorCode());
      }

      // Test primitive list smaller than minimum possible
      msg.setValue("key1", new String("val1"));

      list = new ArrayList();

      list.add(null);
      msg.setValue("key3", list);

      try
      {
         m_formatter.format(msg, m_literal, getWriterOutput());
         fail();
      }
      catch (IntegrationException e)
      {
         assertEquals("err.integration.minPartCount", e.getErrorCode());
         assertEquals(m_literal.getRoot().getPart(2).getFullPath(), e.getErrorArgs()[0]);
      }

      // Test primitive list greater than maximum possible
      list.add(null); // maximum for this message is 5, we will add 6
      list.add(null);
      list.add(null);
      list.add(null);
      list.add(null);
      msg.setValue("key3", list);

      try
      {
         m_formatter.format(msg, m_literal, getWriterOutput());
         fail();
      }
      catch (IntegrationException e)
      {
         assertEquals("err.integration.maxPartCount", e.getErrorCode());
         assertEquals(m_literal.getRoot().getPart(2).getFullPath(), e.getErrorArgs()[0]);
      }

      // Test invalid primitive part type ordinal
      msg.setValue("key1", new Integer(5));

      list = new ArrayList();

      list.add(new String("val1"));
      msg.setValue("key3", list);

      try
      {
         m_formatter.format(msg, m_literal, getWriterOutput());
         fail();
      }
      catch (IntegrationException e)
      {
         assertEquals("err.integration.messageTypeMismatch", e.getErrorCode());
         assertEquals(m_literal.getRoot().getPart(0).getFullPath(), e.getErrorArgs()[0]);
         assertEquals(Primitive.STRING, e.getErrorArgs()[1]);
         assertEquals(Integer.class.getName(), e.getErrorArgs()[2]);
      }
   }

   /**
    * Test parser exceptions
    */
   public void testParserExceptions()
   {
      // Test Missing Part
      m_literal = Repository.getMetadata().getMessage("JSON_Messages");

      try
      {
         m_parser.parse(getReaderInput("{\"message1\":{\"value1\":\"abc\"}}"), m_literal);
         fail();
      }
      catch (IntegrationException e)
      {
         assertEquals("err.integration.missingPart", e.getErrorCode());
         assertEquals(m_literal.getRoot().getPart(1).getFullPath(), e.getErrorArgs()[0]);
      }

      // Test duplicate key
      try
      {
         m_parser.parse(getReaderInput("{\"message1\":{\"value1\":\"abc\",\"value1\":\"abc\"}}"), m_literal);
         fail();
      }
      catch (IntegrationException e)
      {
         assertEquals("err.integration.json.parse.duplicateKey", e.getErrorCode());
         assertEquals("value1", e.getErrorArgs()[0]);
         assertEquals(((CompositeMessagePart)m_literal.getRoot().getPart(0)).getPart(0).getFullPath(), e.getErrorArgs()[1]);
      }

      // Test invalid key
      try
      {
         m_parser.parse(getReaderInput("{\"message1\":{\"value2\":\"abc\"}}"), m_literal);
         fail();
      }
      catch (IntegrationException e)
      {
         assertEquals("err.integration.json.parse.invalidKey", e.getErrorCode());
         assertEquals("value2", e.getErrorArgs()[0]);
         assertEquals(m_literal.getRoot().getPart(0).getFullPath(), e.getErrorArgs()[1]);
      }

      // Test expecting transfer object but found something else
      try
      {
         m_parser.parse(getReaderInput("{\"message1\":\"message\"}"), m_literal);
         fail();
      }
      catch (IntegrationException e)
      {
         assertEquals("err.integration.json.parse.unexpectedToken", e.getErrorCode());
         assertEquals(m_literal.getRoot().getPart(0).getFullPath(), e.getErrorArgs()[0]);
      }

      // Test part required but value is null
      try
      {
         m_parser.parse(getReaderInput("{\"message2\":null}"), m_literal);
         fail();
      }
      catch (IntegrationException e)
      {
         assertEquals("err.integration.json.parse.requiredNull", e.getErrorCode());
         assertEquals(m_literal.getRoot().getPart(1).getFullPath(), e.getErrorArgs()[0]);
      }

      // Test expected list but found something else
      try
      {
         m_parser.parse(getReaderInput("{\"message3\": {\"value3\": 123}}"), m_literal);
         fail();
      }
      catch (IntegrationException e)
      {
         assertEquals("err.integration.json.parse.unexpectedToken", e.getErrorCode());
         assertEquals(m_literal.getRoot().getPart(2).getFullPath(), e.getErrorArgs()[0]);
      }

      // Test composite list less than minimum count
      try
      {
         m_parser.parse(getReaderInput("{\"message3\":[null]}"), m_literal);
         fail();
      }
      catch (IntegrationException e)
      {
         assertEquals("err.integration.minPartCount", e.getErrorCode());
         assertEquals(m_literal.getRoot().getPart(2).getFullPath(), e.getErrorArgs()[0]);
      }

      // Test composite list greater than maximum count
      try
      {
         m_parser.parse(getReaderInput("{\"message3\":[null,null,null,null,null,null]}"), m_literal);
         fail();
      }
      catch (IntegrationException e)
      {
         assertEquals("err.integration.maxPartCount", e.getErrorCode());
         assertEquals(m_literal.getRoot().getPart(2).getFullPath(), e.getErrorArgs()[0]);
      }

      // Test class cast exception of list for primitive collection
      m_literal = Repository.getMetadata().getMessage("JSON_Values");

      try
      {
         m_parser.parse(getReaderInput("{\"key3\":\"abc\"}"), m_literal);
         fail();
      }
      catch (IntegrationException e)
      {
         assertEquals("err.integration.json.parse.unexpectedToken", e.getErrorCode());
         assertEquals(m_literal.getRoot().getPart(2).getFullPath(), e.getErrorArgs()[0]);
      }

      // Test primitive list smaller than minimum possible
      try
      {
         m_parser.parse(getReaderInput("{\"key3\":[\"abc\"]}"), m_literal);
         fail();
      }
      catch (IntegrationException e)
      {
         assertEquals("err.integration.minPartCount", e.getErrorCode());
         assertEquals(m_literal.getRoot().getPart(2).getFullPath(), e.getErrorArgs()[0]);
      }

      // Test primitive list greater than maximum possible
      try
      {
         m_parser.parse(getReaderInput("{\"key3\":[\"1\",\"2\",\"3\",\"4\",\"5\",\"6\"]}"), m_literal);
         fail();
      }
      catch (IntegrationException e)
      {
         assertEquals("err.integration.maxPartCount", e.getErrorCode());
         assertEquals(m_literal.getRoot().getPart(2).getFullPath(), e.getErrorArgs()[0]);
      }

      // Test invalid primitive part type ordinal
      try
      {
         m_parser.parse(getReaderInput("{\"key1\":123}"), m_literal);
         fail();
      }
      catch (IntegrationException e)
      {
         assertEquals("err.integration.json.parse.typeMismatch", e.getErrorCode());

         assertEquals(m_literal.getRoot().getPart(0).getFullPath(), e.getErrorArgs()[0]);
         assertEquals(Primitive.STRING, e.getErrorArgs()[1]);
      }

      // Test invalid root sub type mode
      m_literal = Repository.getMetadata().getMessage("JSON_SubType_Root");

      try
      {
         m_parser.parse(getReaderInput("{\"messages\":[{\"rootPrimitive\":1,\"object\":{\"key\":1},\"rootArray\":[1,2,3]}]}"), m_literal);
         fail();
      }
      catch (IntegrationException e)
      {
         assertEquals("err.integration.json.parse.unexpectedToken", e.getErrorCode());
         assertEquals(m_literal.getRoot().getPart(0).getFullPath(), e.getErrorArgs()[0]);
      }

      m_literal = Repository.getMetadata().getMessage("JSON_Primitives");

      // Binary invalid hex
      try
      {
         m_parser.parse(getReaderInput("{\"key9\":\"!\""), m_literal);
         fail();
      }
      catch (IntegrationException e)
      {
         assertEquals("err.integration.json.parse.hex", e.getErrorCode());
         assertEquals(m_literal.getRoot().getPart(8).getFullPath(), e.getErrorArgs()[0]);
      }

      // Binary expected string
      try
      {
         m_parser.parse(getReaderInput("{\"key9\":123"), m_literal);
         fail();
      }
      catch (IntegrationException e)
      {
         assertEquals("err.integration.json.parse.binary", e.getErrorCode());
         assertEquals(m_literal.getRoot().getPart(8).getFullPath(), e.getErrorArgs()[0]);
      }

      // Timestamp invalid object
      try
      {
         m_parser.parse(getReaderInput("{\"key7\":\"2011-10-10 11:30:34\""), m_literal);
         fail();
      }
      catch (IntegrationException e)
      {
         assertEquals("err.integration.json.parse.timestamp", e.getErrorCode());
         assertEquals(m_literal.getRoot().getPart(6).getFullPath(), e.getErrorArgs()[0]);
      }

      Message msg1 = Repository.getMetadata().getMessage("JSON_Values");
      Message msg2 = Repository.getMetadata().getMessage("JSON_Primitives");
      Message msg3 = Repository.getMetadata().getMessage("JSON_ValueFormats");

      // Message table conflicting messages
      try
      {
         MessageTable table = new MessageTable();

         table.addMessage(msg1);
         table.addMessage(msg2);
         table.addMessage(msg3);

         m_parser.initializeMessageTable(table);
         fail();
      }
      catch (IntegrationException e)
      {
         assertEquals("err.integration.json.parse.messageTableConflict", e.getErrorCode());
         assertEquals(msg1, e.getErrorArgs()[0]);
         assertEquals(msg3, e.getErrorArgs()[1]);
      }

      MessageTable table = new MessageTable();

      table.addMessage(msg1);
      table.addMessage(msg2);

      m_parser.initializeMessageTable(table);

      // Expected JSON Object
      try
      {
         m_parser.parse(new ReaderInput( new StringReader("\"object\"")), table);
         fail();
      }
      catch (IntegrationException e)
      {
         assertEquals("err.integration.json.parse.tableObject", e.getErrorCode());
      }

      try
      {
         m_parser.parse(new ReaderInput( new StringReader("[\"object\"]")), table);
         fail();
      }
      catch (IntegrationException e)
      {
         assertEquals("err.integration.json.parse.tableObject", e.getErrorCode());
      }

      // Message not found
      try
      {
         m_parser.parse(new ReaderInput( new StringReader("{\"key2\":\"values\"}")), table);
         fail();
      }
      catch (IntegrationException e)
      {
         assertEquals("err.integration.json.parse.invalidObject", e.getErrorCode());
         assertEquals(table.getParserTable(), e.getErrorArgs()[0]);
      }
   }

   /**
    * Checks where a transfer object is equal to another.
    * @param expected The expected transfer object.
    * @param actual The actual transfer object.
    */
   protected void checkEquals(TransferObject expected, TransferObject actual)
   {
      PropertyIterator itr = expected.getIterator();

      while(itr.hasNext())
      {
         String sName = (String)itr.next();
         Object obj = itr.getValue();

         if (obj instanceof TransferObject)
         {
            checkEquals((TransferObject)obj, (TransferObject)actual.getValue(sName));
         }
         else if (obj instanceof List)
         {
            List listTobj = (List)obj;
            List listMsg = (List)(actual.getValue(sName));

            assertEquals(listTobj.size(), listMsg.size());

            int nCount = listTobj.size();

            for (int i = 0; i < nCount; i++)
            {
               obj = listTobj.get(i);

               if (obj instanceof TransferObject)
               {
                  checkEquals((TransferObject)obj, (TransferObject)listMsg.get(i));
               }
               else
               {
                  assertEquals(obj, listMsg.get(i));
               }
            }
         }
         else if (obj instanceof Number)
         {
            checkEquals((Number)obj, (Number)actual.getValue(sName));
         }
         else
         {
            assertEquals(obj, actual.getValue(sName));
         }
      }
   }

   /**
    * Checks where a number is equal to another.
    * @param expected The expected number.
    * @param actual The actual number.
    */
   public void checkEquals(Number expected, Number actual)
   {
      if (expected.getClass() == actual.getClass())
      {
         assertEquals(expected, actual);

         return;
      }

      assertEquals(expected.doubleValue(), actual.doubleValue(),
         1E-15 * Math.abs(expected.doubleValue()));
   }

   /**
    * @return A reader input.
    */
   protected ReaderInput getReaderInput()
   {
      return new ReaderInput(new StringReader(m_writer.toString()));
   }

   /**
    * @param sJSON The string with which to initialize the reader.
    * @return A reader input.
    */
   protected ReaderInput getReaderInput(String sJSON)
   {
      return new ReaderInput(new StringReader(sJSON));
   }

   /**
    * @return A writer output.
    */
   protected WriterOutput getWriterOutput()
   {
      m_writer.getBuffer().setLength(0);

      return m_output;
   }
}