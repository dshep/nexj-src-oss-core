// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.xml;

import java.io.ByteArrayInputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.TimeZone;

import junit.framework.TestCase;

import nexj.core.integration.Input;
import nexj.core.integration.IntegrationException;
import nexj.core.integration.MessageFormatter;
import nexj.core.integration.MessageParser;
import nexj.core.integration.Output;
import nexj.core.integration.io.ReaderInput;
import nexj.core.integration.io.StringInput;
import nexj.core.integration.io.WriterOutput;
import nexj.core.meta.Metadata;
import nexj.core.meta.Primitive;
import nexj.core.meta.Repository;
import nexj.core.meta.integration.Format;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessageTable;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.InvocationContext;
import nexj.core.util.Binary;
import nexj.core.util.IOUtil;
import nexj.core.util.URLUtil;
import nexj.core.util.XMLUtil;

public class XMLMessageParserTest extends TestCase
{
   // constants
   
   /**
    * A Patient message in XML format.
    */
   protected final static String PATIENT = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" +
      "\n<a:Patient xmlns:a=\"urn:com.nexjsystems:ns:test:a\" xmlns:b=\"urn:com.nexjsystems:ns:test:b\" " +
      "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"urn:com.nexjsystems:ns:test:a http://www.nexjsystems.com/nexj/integration.xsd urn:com.nexjsystems:ns:test:b http://www.nexjsystems.com/nexj/b.xsd\">" +
      "<player><firstName>My1</firstName><lastName>Test</lastName><guid>AQIDBAUGBwgJEBESExQVFg==</guid>" +
      "<id type=\"OHIP\" b:id=\"111222333\"><moose><peterson ok=\"true\"/>qqq<wildlife/></moose>zzz</id></player></a:Patient>";

   /**
    * A Patient message in XML format, blank first name.
    */
   protected final static String PATIENT_BLANK_FIRST_NAME = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" +
      "\n<a:Patient xmlns:a=\"urn:com.nexjsystems:ns:test:a\" xmlns:b=\"urn:com.nexjsystems:ns:test:b\" " +
      "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"urn:com.nexjsystems:ns:test:a http://www.nexjsystems.com/nexj/integration.xsd\">" +
      "<player><firstName></firstName><lastName>Test</lastName><guid>AQIDBAUGBwgJEBESExQVFg==</guid>" +
      "<id type=\"OHIP\" b:id=\"111222333\"><moose><peterson ok=\"true\"/>qqq<wildlife/></moose>zzz</id></player></a:Patient>";

   /**
    * A Patient message in XML format, empty first name.
    */
   protected final static String PATIENT_EMPTY_FIRST_NAME = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" +
      "\n<a:Patient xmlns:a=\"urn:com.nexjsystems:ns:test:a\" xmlns:b=\"urn:com.nexjsystems:ns:test:b\" " +
      "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"urn:com.nexjsystems:ns:test:a http://www.nexjsystems.com/nexj/integration.xsd\">" +
      "<player><firstName/><lastName>Test</lastName><guid>AQIDBAUGBwgJEBESExQVFg==</guid>" +
      "<id type=\"OHIP\" b:id=\"111222333\"><moose><peterson ok=\"true\"/>qqq<wildlife/></moose>zzz</id></player></a:Patient>";

   /**
    * A Patient message in XML format, no first name.
    */
   protected final static String PATIENT_NO_FIRST_NAME = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" +
      "\n<a:Patient xmlns:a=\"urn:com.nexjsystems:ns:test:a\" xmlns:b=\"urn:com.nexjsystems:ns:test:b\" " +
      "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"urn:com.nexjsystems:ns:test:a http://www.nexjsystems.com/nexj/integration.xsd\">" +
      "<player><lastName>Test</lastName><guid>AQIDBAUGBwgJEBESExQVFg==</guid>" +
      "<id type=\"OHIP\" b:id=\"111222333\"><moose><peterson ok=\"true\"/>qqq<wildlife/></moose>zzz</id></player></a:Patient>";

   /**
    * A Patient message in a SOAP envelope.
    */
   protected final static String PATIENT_SOAP = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
      "<soap:Envelope xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
      "<soap:Body><a:Patient xmlns:a=\"urn:com.nexjsystems:ns:test:a\" xmlns:b=\"urn:com.nexjsystems:ns:test:b\" " +
      "xmlns:wscoor=\"http://docs.oasis-open.org/ws-tx/wscoor/2006/06\" xmlns:m=\"http://travelcompany.example.org/reservation\">" +
      "<player><firstName>My1</firstName><lastName>Test</lastName><guid>AQIDBAUGBwgJEBESExQVFg==</guid>" +
      "<id type=\"OHIP\" b:id=\"111222333\"/></player></a:Patient></soap:Body></soap:Envelope>";

   /**
    * A Patient message in a SOAP 1.2 envelope
    */
   protected final static String PATIENT_SOAP12 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
      "<soap12:Envelope xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\" " +
      "xmlns:soap12=\"http://www.w3.org/2003/05/soap-envelope\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
      "<soap12:Body><a:Patient xmlns:a=\"urn:com.nexjsystems:ns:test:a\" xmlns:b=\"urn:com.nexjsystems:ns:test:b\" " +
      "xmlns:wscoor=\"http://docs.oasis-open.org/ws-tx/wscoor/2006/06\" xmlns:m=\"http://travelcompany.example.org/reservation\">" +
      "<player><firstName>My1</firstName><lastName>Test</lastName><guid>AQIDBAUGBwgJEBESExQVFg==</guid>" +
      "<id type=\"OHIP\" b:id=\"111222333\"/></player></a:Patient></soap12:Body></soap12:Envelope>";

   protected final static String SOAP_FAULT = "<soap:Envelope xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\" " +
      "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">     " +
      "<soap:Body> <soap:Fault> " +
      "<faultcode>123</faultcode>  \n" +
      "<faultstring>This is a minor SOAP fault</faultstring>    " +
      "<faultactor>not-me</faultactor>   " +
      "<detail><a>    <b>   </b>  </a></detail>   " +
      "</soap:Fault> </soap:Body></soap:Envelope>";

   /**
    * A Patient message in a SOAP envelope with SOAP headers.
    */
   protected final static String PATIENT_SOAP_HEADERS = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
      "<soap:Envelope" +
      " xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\"" +
      " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<soap:Header><m:reservation" +
         " xmlns:a=\"urn:com.nexjsystems:ns:test:a\" xmlns:b=\"urn:com.nexjsystems:ns:test:b\"" +
         " xmlns:wscoor=\"http://docs.oasis-open.org/ws-tx/wscoor/2006/06\" xmlns:m=\"http://travelcompany.example.org/reservation\"" +
         " soap:mustUnderstand=\"1\">" +
         "<m:reference>uuid:093a2da1-q345-739r-ba5d-pqff98fe8j7d</m:reference></m:reservation>" +
         "<wscoor:CoordinationContext" +
         " xmlns:a=\"urn:com.nexjsystems:ns:test:a\" xmlns:b=\"urn:com.nexjsystems:ns:test:b\"" +
         " xmlns:wscoor=\"http://docs.oasis-open.org/ws-tx/wscoor/2006/06\" xmlns:m=\"http://travelcompany.example.org/reservation\"" +
         " soap:mustUnderstand=\"1\">" +
         "<Expires>3000</Expires></wscoor:CoordinationContext>" +
         "</soap:Header>" +
         "<soap:Body><a:Patient" +
         " xmlns:a=\"urn:com.nexjsystems:ns:test:a\" xmlns:b=\"urn:com.nexjsystems:ns:test:b\"" +
         " xmlns:wscoor=\"http://docs.oasis-open.org/ws-tx/wscoor/2006/06\" xmlns:m=\"http://travelcompany.example.org/reservation\"" +
         "><player><firstName>My1</firstName>" +
         "<lastName>Test</lastName><title>\u4e2d\u6587</title></player>" +
         "</a:Patient></soap:Body>" +
      "</soap:Envelope>";

   /**
    * Another SOAP message with headers.
    */
   protected final static String SOAP_MESSAGE2 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
      "<soap:Envelope" +
         " xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\"" +
         " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"" +   
      "><soap:Header>" +
         "<wscoor:CoordinationContext" +
         " xmlns:x=\"http://www.nexj.com/ns/example/x\"" +
         " xmlns:wscoor=\"http://docs.oasis-open.org/ws-tx/wscoor/2006/06\"" +
         " xmlns:wsa=\"http://schemas.xmlsoap.org/ws/2004/08/addressing\"" +
         " soap:mustUnderstand=\"1\">" +
         "<Expires>3000</Expires></wscoor:CoordinationContext>" +
         "<wsa:Action" +
         " xmlns:x=\"http://www.nexj.com/ns/example/x\"" +
         " xmlns:wscoor=\"http://docs.oasis-open.org/ws-tx/wscoor/2006/06\"" +
         " xmlns:wsa=\"http://schemas.xmlsoap.org/ws/2004/08/addressing\"" +
         ">abc123</wsa:Action>" +
      "</soap:Header><soap:Body>" +
         "<x:Record" +
         " xmlns:x=\"http://www.nexj.com/ns/example/x\"" +
         " xmlns:wscoor=\"http://docs.oasis-open.org/ws-tx/wscoor/2006/06\"" +
         " xmlns:wsa=\"http://schemas.xmlsoap.org/ws/2004/08/addressing\"" +
         "><description>Test</description>" +
         "<data>AAAB</data></x:Record>" +
      "</soap:Body></soap:Envelope>";

   /**
    * Test for parsing collection of polymorphic message parts.
    */
   protected final static String POLYMORPHIC_COLLECTION_TEST = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" +
         "<Test xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"><part xsi:type=\"ChildType\">" +
         "<x>x</x><y>y</y></part><part><x>x</x></part></Test>";

   /**
    * Test for single aggregation.
    */
   protected final static String SINGLE_AGGREGATION_TEST = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" +
         "<single><A></A></single>";

   // associations

   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   protected Format m_format;
   protected MessageParser m_parser;
   protected MessageFormatter m_formatter;
   protected Message m_patient;
   protected Message m_patientSOAP;
   protected Message m_xds;

   /**
    * A message that maps to SOAP fault.
    */
   protected Message m_faultMessage;

   /**
    * The patient message as a SOAP message with headers.
    */
   protected Message m_patientHeadersSOAP;

   /**
    * A second SOAP message that uses a message reference to
    * its SOAP header block.
    */
   protected Message m_SOAPMessage2;

   /**
    * A SOAP message whose SOAP header block is a primitive message part.
    */
   protected Message m_SOAPPrimitiveHeader;

   /**
    * The patient message with a nillable "lastName".
    */
   protected Message m_patientNillable;

   /**
    * Message with nillable sub message part.
    */
   protected Message m_nillableSubMessage;

   /**
    * Message with nillable ref sub message part.
    */
   protected Message m_nillableRefSubMessage;

   /**
    * Message with nillable ref sub message part 2.
    */
   protected Message m_nillableRefSubMessage2;

   /**
    * A message for testing format string support.
    */
   protected Message m_formatStringMessage;

   /**
    * Message for polymorphic collection test.
    */
   protected Message m_polymorphicCollectionMessage;

   /**
    * Message for single aggregation test.
    */
   protected Message m_singleAggregationMessage;

   // operations

   protected void setUp() throws Exception
   {
      m_context = (InvocationContext)Repository.getMetadata().getComponent("System.InvocationContext").getInstance(null);
      m_context.setLocale(Locale.ENGLISH);
      m_context.setTimeZone(TimeZone.getTimeZone("America/New_York"));

      Metadata metadata = m_context.getMetadata();

      m_format = metadata.getFormat("XML");
      m_parser = (MessageParser)m_format.getParser().getInstance(m_context);
      m_formatter = (MessageFormatter)m_format.getFormatter().getInstance(m_context);
      m_patient = metadata.getMessage("XML_PatientDemographics");
      m_patientSOAP = metadata.getMessage("SOAP_PatientDemographics");
      m_xds = metadata.getMessage("XDS_SubmitObjects");
      m_patientHeadersSOAP = metadata.getMessage("SOAP_Headers_PatientDemographics");
      m_SOAPMessage2 = metadata.getMessage("SOAP_Message2");
      m_SOAPPrimitiveHeader = metadata.getMessage("SOAP_PrimitiveHeader");
      m_patientNillable = metadata.getMessage("XML_PatientDemographicsNillable");
      m_nillableSubMessage = metadata.getMessage("XML_NillableSubMessage");
      m_nillableRefSubMessage = metadata.getMessage("XML_NillableRefSubMessage");
      m_nillableRefSubMessage2 = metadata.getMessage("XML_NillableRefSubMessage2");
      m_faultMessage = metadata.getMessage("SOAP11Fault");
      m_formatStringMessage = metadata.getMessage("XML_FormatString");
      m_polymorphicCollectionMessage = metadata.getMessage("PolymorphicCollectionTest");
      m_singleAggregationMessage = metadata.getMessage("SingleAggregation");
   }

   public void testParseInputMessage() throws Exception
   {
      TransferObject tobj = m_parser.parse(new StringInput(PATIENT), m_patient);
      TransferObject player = (TransferObject)tobj.getValue("player");

      assertEquals("My1", player.getValue("firstName"));
      assertEquals("Test", player.getValue("lastName"));

      TransferObject id = (TransferObject)((List)player.getValue("id")).get(0);

      assertEquals("XML_PatientDemographics", tobj.getClassName());
      assertEquals("OHIP", id.getValue("type"));
      assertEquals("111222333", id.getValue("id"));

      StringWriter writer = new StringWriter();

      m_formatter.format(tobj, m_patient, new WriterOutput(writer));

      assertEquals("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<a:Patient xmlns:a=\"urn:com.nexjsystems:ns:test:a\" " +
         "xmlns:b=\"urn:com.nexjsystems:ns:test:b\" " +
         "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"urn:com.nexjsystems:ns:test:a http://www.nexjsystems.com/nexj/integration.xsd urn:com.nexjsystems:ns:test:b http://www.nexjsystems.com/nexj/b.xsd\">" +
         "<player><firstName>My1</firstName><lastName>Test</lastName><guid>AQIDBAUGBwgJEBESExQVFg==</guid>" +
         "<id type=\"OHIP\" b:id=\"111222333\"/></player></a:Patient>", writer.toString());

      writer = new StringWriter();
      tobj.setClassName(null);
      m_formatter.format(tobj, m_patientSOAP, new WriterOutput(writer));
      assertEquals(PATIENT_SOAP, writer.toString());


      tobj = m_parser.parse(new StringInput(PATIENT_BLANK_FIRST_NAME), m_patient);
      player = (TransferObject)tobj.getValue("player");

      assertNull(player.getValue("firstName"));
      assertEquals("Test", player.getValue("lastName"));


      tobj = m_parser.parse(new StringInput(PATIENT_EMPTY_FIRST_NAME), m_patient);
      player = (TransferObject)tobj.getValue("player");

      assertNull(player.getValue("firstName"));
      assertEquals("Test", player.getValue("lastName"));


      tobj = m_parser.parse(new StringInput(PATIENT_NO_FIRST_NAME), m_patient);
      player = (TransferObject)tobj.getValue("player");

      assertFalse(player.hasValue("firstName"));
      assertEquals("Test", player.getValue("lastName"));


      tobj = m_parser.parse(new StringInput(PATIENT_SOAP), m_patientSOAP);
      writer = new StringWriter();
      m_formatter.format(tobj, m_patientSOAP, new WriterOutput(writer));
      assertEquals(PATIENT_SOAP, writer.toString());

      Reader reader = null;

      try
      {
         reader = new InputStreamReader(URLUtil.openResource(XMLMessageParserTest.class, "XDS_SubmitObjectsRequest.xml"), XMLUtil.ENCODING);
         tobj = m_parser.parse(new ReaderInput(reader), m_xds);
      }
      finally
      {
         IOUtil.close(reader);
      }

      assertEquals("XDS_SubmitObjects", tobj.getClassName());
      assertEquals("0123 456 789 KT^^^&OHIP&ISO", ((TransferObject)((List)((TransferObject)
         ((TransferObject)tobj.getValue("LeafRegistryObjectList")).getValue("ExtrinsicObject"))
         .getValue("ExternalIdentifier")).get(0)).getValue("value"));

      try
      {
         m_parser.parse(new StringInput(SOAP_FAULT), m_patientSOAP);
         fail("Expected SOAPFaultException");
      }
      catch (SOAPFaultException e)
      {
         assertEquals("123", e.getFaultCode());
         assertEquals("This is a minor SOAP fault", e.getFaultString());
         assertEquals("not-me", e.getFaultActor());
      }

      // Try parsing a message that is mapped to SOAP Fault
      tobj = m_parser.parse(new StringInput(SOAP_FAULT), m_faultMessage);
      assertEquals(m_faultMessage.getName(), tobj.getClassName());
      assertEquals("123", tobj.getValue("code"));
      assertEquals("This is a minor SOAP fault", tobj.getValue("msg"));
      assertEquals("not-me", tobj.getValue("actor"));
      assertEquals("<a>    <b>   </b>  </a>", tobj.getValue("detail"));

      m_parser.parse(new StringInput(POLYMORPHIC_COLLECTION_TEST), m_polymorphicCollectionMessage);
   }

   public void testParseInputMessageTable()
   {
      MessageTable table = new MessageTable();

      table.addMessage(m_patient);
      table.addMessage(m_xds);

      m_parser.initializeMessageTable(table);

      TransferObject tobj = m_parser.parse(new StringInput(PATIENT), table);
      TransferObject player = (TransferObject)tobj.getValue("player");

      assertEquals("My1", player.getValue("firstName"));
      assertEquals("Test", player.getValue("lastName"));

      TransferObject id = (TransferObject)((List)player.getValue("id")).get(0);

      assertEquals("XML_PatientDemographics", tobj.getClassName());
      assertEquals("OHIP", id.getValue("type"));
      assertEquals("111222333", id.getValue("id"));

      table = new MessageTable();
      table.addMessage(m_patientSOAP);

      m_parser.initializeMessageTable(table);

      tobj = m_parser.parse(new StringInput(PATIENT_SOAP), table);
      player = (TransferObject)tobj.getValue("player");

      assertEquals("My1", player.getValue("firstName"));
      assertEquals("Test", player.getValue("lastName"));

      id = (TransferObject)((List)player.getValue("id")).get(0);

      assertEquals("SOAP_PatientDemographics", tobj.getClassName());
      assertEquals("OHIP", id.getValue("type"));
      assertEquals("111222333", id.getValue("id"));

      try
      {
         m_parser.parse(new StringInput(SOAP_FAULT), table);
         fail("Expected SOAPFaultException");
      }
      catch (SOAPFaultException e)
      {
         assertEquals("123", e.getFaultCode());
         assertEquals("This is a minor SOAP fault", e.getFaultString());
         assertEquals("not-me", e.getFaultActor());
      }

      // Try parsing a message that is mapped to SOAP Fault
      table.addMessage(m_faultMessage);
      m_parser.initializeMessageTable(table);
      tobj = m_parser.parse(new StringInput(SOAP_FAULT), table);
      assertEquals(m_faultMessage.getName(), tobj.getClassName());
      assertEquals("123", tobj.getValue("code"));
      assertEquals("This is a minor SOAP fault", tobj.getValue("msg"));
      assertEquals("not-me", tobj.getValue("actor"));
      assertEquals("<a>    <b>   </b>  </a>", tobj.getValue("detail"));
   }

   /**
    * Sanity check for formatting a message that contains SOAP headers.
    */
   public void testFormatSOAPHeaders() throws Exception
   {
      TransferObject message = new TransferObject();
      TransferObject player = new TransferObject(4);
      TransferObject headers = new TransferObject(2);
      TransferObject reservationHeader = new TransferObject();
      TransferObject contextHeader = new TransferObject();

      player.setValue("firstName", "My1");
      player.setValue("lastName", "Test");
      player.setValue("title", "\u4e2d\u6587");
      message.setValue("player", player);

      reservationHeader.setValue("role", "http://www.w3.org/2003/05/soap-envelope/role/next");
      reservationHeader.setValue("mustUnderstand", Boolean.TRUE);
      reservationHeader.setValue("reference", "uuid:093a2da1-q345-739r-ba5d-pqff98fe8j7d");

      contextHeader.setValue("mustUnderstand", Boolean.TRUE);
      contextHeader.setValue("Expires", Primitive.createLong(3000L));

      headers.setValue("reservation", reservationHeader);
      headers.setValue("context", contextHeader);
      message.setValue("player", player);
      message.setValue("headers", headers);

      StringWriter writer = new StringWriter();

      m_formatter.format(message, m_patientHeadersSOAP, new WriterOutput(writer));
      assertEquals(PATIENT_SOAP_HEADERS, writer.toString());

      // Format the second SOAP message (has reference to entire header block)
      TransferObject header;

      message = new TransferObject(3);
      headers = new TransferObject(2);

      message.setValue("description", "Test");
      message.setValue("data", Binary.parse("000001"));
      message.setValue("headers", headers);

      header = new TransferObject(2);
      header.setValue("mustUnderstand", Boolean.TRUE);
      header.setValue("Expires", Primitive.createLong(3000L));
      headers.setValue("context", header);

      header = new TransferObject(1);
      header.setValue("value", "abc123");
      headers.setValue("action", header);

      writer = new StringWriter();
      m_formatter.format(message, m_SOAPMessage2, new WriterOutput(writer));
      assertEquals(SOAP_MESSAGE2, writer.toString());
   }

   /**
    * Sanity check for parsing a message that contains SOAP headers.
    */
   public void testParseSOAPHeaders() throws Exception
   {
      TransferObject message = m_parser.parse(new StringInput(PATIENT_SOAP_HEADERS), m_patientHeadersSOAP);

      verifyPatientSOAPMessage(message);

      // Parse the second SOAP message (has reference to entire header block)
      message = m_parser.parse(new StringInput(SOAP_MESSAGE2), m_SOAPMessage2);
      verifySOAPMessage2(message);

      // Parse data with headers to a message with no headers
      TransferObject tobj = m_parser.parse(new StringInput(PATIENT_SOAP_HEADERS), m_patientSOAP);
      TransferObject player = (TransferObject)tobj.getValue("player");

      assertEquals("My1", (String)player.getValue("firstName"));
      assertEquals("Test", (String)player.getValue("lastName"));
      assertEquals("\u4e2d\u6587", (String)player.getValue("title"));

      assertEquals(1, tobj.getValueCount());

      // Parse data with headers to a message where header message part is primitive
      message = m_parser.parse(new StringInput(SOAP_MESSAGE2), m_SOAPPrimitiveHeader);
      assertEquals("SOAP_PrimitiveHeader", message.getClassName());
      assertEquals("Test", message.getValue("description"));
      assertEquals(Binary.parse("000001"), message.getValue("data"));
      assertEquals(2, message.getValueCount());
   }

   /**
    * Tests parsing SOAP messages 
    */
   public void testParseSOAPHeadersMessageTable() throws Exception
   {
      TransferObject message;
      MessageTable table = new MessageTable();

      table.addMessage(m_patientHeadersSOAP);
      table.addMessage(m_SOAPMessage2);

      m_parser.initializeMessageTable(table);
      message = m_parser.parse(new StringInput(PATIENT_SOAP_HEADERS), table);
      verifyPatientSOAPMessage(message);

      // Parse with data corresponding to the second message
      message = m_parser.parse(new StringInput(SOAP_MESSAGE2), table);
      verifySOAPMessage2(message);

      // Ensure determination of correct message, even if prologue is very long
      int nDataCount = 10000;
      String sLongStart = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<soap:Envelope " +
            "xmlns:wsa=\"http://schemas.xmlsoap.org/ws/2004/08/addressing\"" +
            " xmlns:x=\"http://www.nexj.com/ns/example/x\"" +
            " xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\"" +
            " xmlns:wscoor=\"http://docs.oasis-open.org/ws-tx/wscoor/2006/06\"" +
            " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"" +
         "><soap:Header>" +
            "<wscoor:CoordinationContext soap:mustUnderstand=\"1\">" +
            "<Expires>3000</Expires></wscoor:CoordinationContext>" +
            "<wsa:Action>abc123</wsa:Action>";
      String sLongEnd = "</soap:Header><soap:Body>" +
            "<x:Record><description>Test</description>" +
            "<data>AAAB</data></x:Record>" +
         "</soap:Body></soap:Envelope>";
      StringBuilder buf = new StringBuilder(sLongStart.length() + nDataCount + sLongEnd.length());

      buf.append(sLongStart);

      while (nDataCount-- > 0)
      {
         buf.append(' ');
      }

      buf.append(sLongEnd);

      // This arrangement allows mark() to be limited--using a StringReader, for example,
      // mark() has no limit.
      Input input = new ReaderInput(new InputStreamReader(
         new ByteArrayInputStream(buf.toString().getBytes("utf-8")),
         "utf-8"));

      message = m_parser.parse(input, table);
      verifySOAPMessage2(message);
   }

   /**
    * @param message The parse-tree to verify.
    */
   protected void verifyPatientSOAPMessage(TransferObject message) throws Exception
   {
      assertTrue(message.hasValue("headers"));

      TransferObject headers = (TransferObject)message.getValue("headers");
      TransferObject player = (TransferObject)message.getValue("player");

      assertEquals("My1", (String)player.getValue("firstName"));
      assertEquals("Test", (String)player.getValue("lastName"));
      assertEquals("\u4e2d\u6587", (String)player.getValue("title"));

      TransferObject header = (TransferObject)headers.getValue("reservation");

      assertEquals(Boolean.TRUE, header.getValue("mustUnderstand"));
      assertEquals("uuid:093a2da1-q345-739r-ba5d-pqff98fe8j7d", (String)header.getValue("reference"));

      header = (TransferObject)headers.getValue("context");
      assertEquals(Boolean.TRUE, header.getValue("mustUnderstand"));
      assertEquals(Primitive.createLong(3000L), header.getValue("Expires"));
   }

   /**
    * @param message The parse-tree to verify.
    */
   protected void verifySOAPMessage2(TransferObject message) throws Exception
   {
      assertTrue(message.hasValue("headers"));

      TransferObject header;
      TransferObject headers = (TransferObject)message.getValue("headers");

      assertEquals("SOAP_Message2", message.getClassName());
      assertEquals("Test", message.getValue("description"));
      assertEquals(Binary.parse("000001"), message.getValue("data"));

      header = (TransferObject)headers.getValue("context");
      assertEquals(Boolean.TRUE, header.getValue("mustUnderstand"));
      assertEquals(Primitive.createLong(3000L), header.getValue("Expires"));

      header = (TransferObject)headers.getValue("action");
      assertEquals("abc123", (String)header.getValue("value"));
   }

   /**
    * Tests that the formatter produces correct output for element mappings
    * marked nillable.
    */
   public void testFormatNillable() throws Exception
   {
      StringWriter writer;
      TransferObject message = new TransferObject(1);
      TransferObject player = new TransferObject(9);
      TransferObject id = new TransferObject(2);
      TransferObject telcom, telcom2, telcom3;
      ArrayList idList = new ArrayList(1);

      player.setValue("firstName", "My1");
      player.setValue("guid", Binary.parse("01020304050607080910111213141516"));
      id.setValue("type", "OHIP");
      id.setValue("id", "111222333");
      idList.add(id);
      player.setValue("id", idList);
      message.setValue("player", player);

      writer = new StringWriter();

      /*
       * These first tests are characterization tests. They demonstrate the
       * operation of the XML message formatter *before* xsi:nillable support
       * was added.
       */

      // Required element not in TransferObject
      try
      {
         m_formatter.format(message, m_patient, new WriterOutput(writer));
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         IntegrationException inner = (IntegrationException)ex.getCause();

         assertEquals("err.integration.minPartCount", inner.getErrorCode());
      }


      // Required element null in TransferObject
      player.setValue("lastName", null);
      writer = new StringWriter();

      try
      {
         m_formatter.format(message, m_patient, new WriterOutput(writer));
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         IntegrationException inner = (IntegrationException)ex.getCause();

         assertEquals("err.integration.minPartCount", inner.getErrorCode());
      }


      // Required element present in TransferObject
      player.setValue("lastName", "Test");
      writer = new StringWriter();
      m_formatter.format(message, m_patient, new WriterOutput(writer));
      assertEquals("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<a:Patient xmlns:a=\"urn:com.nexjsystems:ns:test:a\" " +
         "xmlns:b=\"urn:com.nexjsystems:ns:test:b\" " +
         "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"urn:com.nexjsystems:ns:test:a http://www.nexjsystems.com/nexj/integration.xsd urn:com.nexjsystems:ns:test:b http://www.nexjsystems.com/nexj/b.xsd\">" +
         "<player><firstName>My1</firstName><lastName>Test</lastName><guid>AQIDBAUGBwgJEBESExQVFg==</guid>" +
         "<id type=\"OHIP\" b:id=\"111222333\"/></player></a:Patient>", writer.toString());


      // Required element present, Optional element ""
      player.setValue("firstName", "");
      writer = new StringWriter();
      m_formatter.format(message, m_patient, new WriterOutput(writer));
      assertEquals("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<a:Patient xmlns:a=\"urn:com.nexjsystems:ns:test:a\" " +
         "xmlns:b=\"urn:com.nexjsystems:ns:test:b\" " +
         "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"urn:com.nexjsystems:ns:test:a http://www.nexjsystems.com/nexj/integration.xsd urn:com.nexjsystems:ns:test:b http://www.nexjsystems.com/nexj/b.xsd\">" +
         "<player><firstName></firstName><lastName>Test</lastName><guid>AQIDBAUGBwgJEBESExQVFg==</guid>" +
         "<id type=\"OHIP\" b:id=\"111222333\"/></player></a:Patient>", writer.toString());

      // Required element present, Optional element null
      player.setValue("firstName", null);
      writer = new StringWriter();
      m_formatter.format(message, m_patient, new WriterOutput(writer));
      assertEquals("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<a:Patient xmlns:a=\"urn:com.nexjsystems:ns:test:a\" " +
         "xmlns:b=\"urn:com.nexjsystems:ns:test:b\" " +
         "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"urn:com.nexjsystems:ns:test:a http://www.nexjsystems.com/nexj/integration.xsd urn:com.nexjsystems:ns:test:b http://www.nexjsystems.com/nexj/b.xsd\">" +
         "<player><lastName>Test</lastName><guid>AQIDBAUGBwgJEBESExQVFg==</guid>" +
         "<id type=\"OHIP\" b:id=\"111222333\"/></player></a:Patient>", writer.toString());


      // Required element present, Optional element not present
      player.removeValue("firstName");
      writer = new StringWriter();
      m_formatter.format(message, m_patient, new WriterOutput(writer));
      assertEquals("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<a:Patient xmlns:a=\"urn:com.nexjsystems:ns:test:a\" " +
         "xmlns:b=\"urn:com.nexjsystems:ns:test:b\" " +
         "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"urn:com.nexjsystems:ns:test:a http://www.nexjsystems.com/nexj/integration.xsd urn:com.nexjsystems:ns:test:b http://www.nexjsystems.com/nexj/b.xsd\">" +
         "<player><lastName>Test</lastName><guid>AQIDBAUGBwgJEBESExQVFg==</guid>" +
         "<id type=\"OHIP\" b:id=\"111222333\"/></player></a:Patient>", writer.toString());


      /*
       * Tests of formatting an element marked nillable.
       */

      player.removeValue("lastName");
      player.setValue("firstName", "My1");
      player.removeValue("title");

      // NILLABLE: Optional Nillable element not present
      writer = new StringWriter();
      m_formatter.format(message, m_patientNillable, new WriterOutput(writer));
      assertEquals("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<a:Patient xmlns:a=\"urn:com.nexjsystems:ns:test:a\" " +
         "xmlns:b=\"urn:com.nexjsystems:ns:test:b\" " +
         "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"urn:com.nexjsystems:ns:test:a http://www.nexjsystems.com/nexj/integration.xsd urn:com.nexjsystems:ns:test:b http://www.nexjsystems.com/nexj/b.xsd\">" +
         "<player><firstName>My1</firstName><title xsi:nil=\"1\"/><guid>AQIDBAUGBwgJEBESExQVFg==</guid>" +
         "<id type=\"OHIP\" b:id=\"111222333\"/><telcom xsi:nil=\"1\"/><telcom2 xsi:nil=\"1\"/></player></a:Patient>", writer.toString());


      // NILLABLE: Optional Nillable element set to null
      player.setValue("title", null);
      writer = new StringWriter();
      m_formatter.format(message, m_patientNillable, new WriterOutput(writer));
      assertEquals("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<a:Patient xmlns:a=\"urn:com.nexjsystems:ns:test:a\" " +
         "xmlns:b=\"urn:com.nexjsystems:ns:test:b\" " +
         "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"urn:com.nexjsystems:ns:test:a http://www.nexjsystems.com/nexj/integration.xsd urn:com.nexjsystems:ns:test:b http://www.nexjsystems.com/nexj/b.xsd\">" +
         "<player><firstName>My1</firstName><title xsi:nil=\"1\"/><guid>AQIDBAUGBwgJEBESExQVFg==</guid>" +
         "<id type=\"OHIP\" b:id=\"111222333\"/><telcom xsi:nil=\"1\"/><telcom2 xsi:nil=\"1\"/></player></a:Patient>", writer.toString());


      // NILLABLE: Optional Nillable element set
      player.setValue("title", "Count");
      writer = new StringWriter();
      m_formatter.format(message, m_patientNillable, new WriterOutput(writer));
      assertEquals("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<a:Patient xmlns:a=\"urn:com.nexjsystems:ns:test:a\" " +
         "xmlns:b=\"urn:com.nexjsystems:ns:test:b\" " +
         "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"urn:com.nexjsystems:ns:test:a http://www.nexjsystems.com/nexj/integration.xsd urn:com.nexjsystems:ns:test:b http://www.nexjsystems.com/nexj/b.xsd\">" +
         "<player><firstName>My1</firstName><title>Count</title><guid>AQIDBAUGBwgJEBESExQVFg==</guid>" +
         "<id type=\"OHIP\" b:id=\"111222333\"/><telcom xsi:nil=\"1\"/><telcom2 xsi:nil=\"1\"/></player></a:Patient>", writer.toString());
      player.removeValue("Count");


      // NILLABLE: Composite messages, one with nil mapped, the other one nillable.
      telcom = new TransferObject(2);
      telcom.setValue("useCode", "HOME");
      telcom.setValue("address", "adr1");
      player.setValue("telcom", telcom);
      telcom2 = new TransferObject(2);
      telcom2.setValue("useCode", "WORK");
      telcom2.setValue("address", "adr2");
      player.setValue("telcom2", telcom2);
      telcom3 = new TransferObject(2);
      telcom3.setValue("useCode", "CELL");
      telcom3.setValue("address", "adr3");
      player.setValue("telcom3", telcom3);
      writer = new StringWriter();
      m_formatter.format(message, m_patientNillable, new WriterOutput(writer));
      assertEquals("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<a:Patient xmlns:a=\"urn:com.nexjsystems:ns:test:a\" " +
         "xmlns:b=\"urn:com.nexjsystems:ns:test:b\" " +
         "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"urn:com.nexjsystems:ns:test:a http://www.nexjsystems.com/nexj/integration.xsd urn:com.nexjsystems:ns:test:b http://www.nexjsystems.com/nexj/b.xsd\">" +
         "<player><firstName>My1</firstName><title>Count</title><guid>AQIDBAUGBwgJEBESExQVFg==</guid>" +
         "<id type=\"OHIP\" b:id=\"111222333\"/>" +
         "<telcom useCode=\"HOME\">adr1</telcom>" +
         "<telcom2 useCode=\"WORK\">adr2</telcom2>" +
         "<telcom3 useCode=\"CELL\">adr3</telcom3>" +
         "</player></a:Patient>", writer.toString());


      // NILLABLE: Composite messages, one with nil mapped, the other one nillable. Values null.
      telcom.setValue("address", null);
      telcom2.setValue("address", null);
      telcom3.setValue("address", null);
      writer = new StringWriter();
      m_formatter.format(message, m_patientNillable, new WriterOutput(writer));
      assertEquals("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<a:Patient xmlns:a=\"urn:com.nexjsystems:ns:test:a\" " +
         "xmlns:b=\"urn:com.nexjsystems:ns:test:b\" " +
         "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"urn:com.nexjsystems:ns:test:a http://www.nexjsystems.com/nexj/integration.xsd urn:com.nexjsystems:ns:test:b http://www.nexjsystems.com/nexj/b.xsd\">" +
         "<player><firstName>My1</firstName><title>Count</title><guid>AQIDBAUGBwgJEBESExQVFg==</guid>" +
         "<id type=\"OHIP\" b:id=\"111222333\"/>" +
         "<telcom useCode=\"HOME\" xsi:nil=\"1\"/>" +
         "<telcom2 useCode=\"WORK\" xsi:nil=\"1\"/>" +
         "<telcom3 useCode=\"CELL\"/>" +
         "</player></a:Patient>", writer.toString());


      // NILLABLE: Composite messages, values not present
      telcom.removeValue("address");
      telcom2.removeValue("address");
      telcom3.removeValue("address");
      writer = new StringWriter();
      m_formatter.format(message, m_patientNillable, new WriterOutput(writer));
      assertEquals("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<a:Patient xmlns:a=\"urn:com.nexjsystems:ns:test:a\" " +
         "xmlns:b=\"urn:com.nexjsystems:ns:test:b\" " +
         "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"urn:com.nexjsystems:ns:test:a http://www.nexjsystems.com/nexj/integration.xsd urn:com.nexjsystems:ns:test:b http://www.nexjsystems.com/nexj/b.xsd\">" +
         "<player><firstName>My1</firstName><title>Count</title><guid>AQIDBAUGBwgJEBESExQVFg==</guid>" +
         "<id type=\"OHIP\" b:id=\"111222333\"/>" +
         "<telcom useCode=\"HOME\" xsi:nil=\"1\"/>" +
         "<telcom2 useCode=\"WORK\" xsi:nil=\"1\"/>" +
         "<telcom3 useCode=\"CELL\"/>" +
         "</player></a:Patient>", writer.toString());


      // NILLABLE: Null composite message forces nil attribute
      player.setValue("telcom", null);
      player.setValue("telcom2", null);
      player.setValue("telcom3", null);
      writer = new StringWriter();
      m_formatter.format(message, m_patientNillable, new WriterOutput(writer));
      assertEquals("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<a:Patient xmlns:a=\"urn:com.nexjsystems:ns:test:a\" " +
         "xmlns:b=\"urn:com.nexjsystems:ns:test:b\" " +
         "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"urn:com.nexjsystems:ns:test:a http://www.nexjsystems.com/nexj/integration.xsd urn:com.nexjsystems:ns:test:b http://www.nexjsystems.com/nexj/b.xsd\">" +
         "<player><firstName>My1</firstName><title>Count</title><guid>AQIDBAUGBwgJEBESExQVFg==</guid>" +
         "<id type=\"OHIP\" b:id=\"111222333\"/>" +
         "<telcom xsi:nil=\"1\"/>" +
         "<telcom2 xsi:nil=\"1\"/>" +
         "<telcom3/>" +
         "</player></a:Patient>", writer.toString());


      // NILLABLE: Composite messages dropped when not present
      player.removeValue("telcom");
      player.removeValue("telcom2");
      player.removeValue("telcom3");
      writer = new StringWriter();
      m_formatter.format(message, m_patientNillable, new WriterOutput(writer));
      assertEquals("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<a:Patient xmlns:a=\"urn:com.nexjsystems:ns:test:a\" " +
         "xmlns:b=\"urn:com.nexjsystems:ns:test:b\" " +
         "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"urn:com.nexjsystems:ns:test:a http://www.nexjsystems.com/nexj/integration.xsd urn:com.nexjsystems:ns:test:b http://www.nexjsystems.com/nexj/b.xsd\">" +
         "<player><firstName>My1</firstName><title>Count</title><guid>AQIDBAUGBwgJEBESExQVFg==</guid>" +
         "<id type=\"OHIP\" b:id=\"111222333\"/>" +
         "<telcom xsi:nil=\"1\"/>" +
         "<telcom2 xsi:nil=\"1\"/>" +
         "</player></a:Patient>", writer.toString());
   }

   /**
    * Tests that the parser can handle XML Schema nillable elements.
    */
   public void testParseNillable() throws Exception
   {
      TransferObject message, player, telcom;
      String sMessage;

      // Required part missing
      sMessage = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<a:Patient xmlns:a=\"urn:com.nexjsystems:ns:test:a\" " +
         "xmlns:b=\"urn:com.nexjsystems:ns:test:b\" " +
         "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"urn:com.nexjsystems:ns:test:a http://www.nexjsystems.com/nexj/integration.xsd urn:com.nexjsystems:ns:test:b http://www.nexjsystems.com/nexj/b.xsd\">" +
         "<player><firstName>My1</firstName><guid>AQIDBAUGBwgJEBESExQVFg==</guid>" +
         "<id type=\"OHIP\" b:id=\"111222333\"/></player></a:Patient>";

      try
      {
         message = m_parser.parse(new StringInput(sMessage), m_patient);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.minPartCount", ex.getErrorCode());
      }


      // Required part present but empty
      sMessage = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<a:Patient xmlns:a=\"urn:com.nexjsystems:ns:test:a\" " +
         "xmlns:b=\"urn:com.nexjsystems:ns:test:b\" " +
         "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"urn:com.nexjsystems:ns:test:a http://www.nexjsystems.com/nexj/integration.xsd urn:com.nexjsystems:ns:test:b http://www.nexjsystems.com/nexj/b.xsd\">" +
         "<player><firstName>My1</firstName><lastName/><guid>AQIDBAUGBwgJEBESExQVFg==</guid>" +
         "<id type=\"OHIP\" b:id=\"111222333\"/></player></a:Patient>";

      try
      {
         message = m_parser.parse(new StringInput(sMessage), m_patient);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.minPartCount", ex.getErrorCode());
      }


      // Required part present
      sMessage = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<a:Patient xmlns:a=\"urn:com.nexjsystems:ns:test:a\" " +
         "xmlns:b=\"urn:com.nexjsystems:ns:test:b\" " +
         "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"urn:com.nexjsystems:ns:test:a http://www.nexjsystems.com/nexj/integration.xsd urn:com.nexjsystems:ns:test:b http://www.nexjsystems.com/nexj/b.xsd\">" +
         "<player><firstName>My1</firstName><lastName>Test</lastName><guid>AQIDBAUGBwgJEBESExQVFg==</guid>" +
         "<id type=\"OHIP\" b:id=\"111222333\"/></player></a:Patient>";
      message = m_parser.parse(new StringInput(sMessage), m_patient);
      player = (TransferObject)message.getValue("player");
      assertEquals("Test", player.getValue("lastName"));
      assertFalse(player.hasValue("title"));


      // Required part present with nil (part is NOT nillable)
      sMessage = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<a:Patient xmlns:a=\"urn:com.nexjsystems:ns:test:a\" " +
         "xmlns:b=\"urn:com.nexjsystems:ns:test:b\" " +
         "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"urn:com.nexjsystems:ns:test:a http://www.nexjsystems.com/nexj/integration.xsd urn:com.nexjsystems:ns:test:b http://www.nexjsystems.com/nexj/b.xsd\">" +
         "<player><firstName>My1</firstName><lastName xsi:nil=\"true\">Test</lastName><guid>AQIDBAUGBwgJEBESExQVFg==</guid>" +
         "<id type=\"OHIP\" b:id=\"111222333\"/></player></a:Patient>";

      try
      {
         message = m_parser.parse(new StringInput(sMessage), m_patient);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.xml.messageSyntax", ex.getErrorCode());
      }


      // Required part empty with nil (part is NOT nillable)
      sMessage = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<a:Patient xmlns:a=\"urn:com.nexjsystems:ns:test:a\" " +
         "xmlns:b=\"urn:com.nexjsystems:ns:test:b\" " +
         "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"urn:com.nexjsystems:ns:test:a http://www.nexjsystems.com/nexj/integration.xsd urn:com.nexjsystems:ns:test:b http://www.nexjsystems.com/nexj/b.xsd\">" +
         "<player><firstName>My1</firstName><lastName xsi:nil=\"true\"/><guid>AQIDBAUGBwgJEBESExQVFg==</guid>" +
         "<id type=\"OHIP\" b:id=\"111222333\"/></player></a:Patient>";

      try
      {
         message = m_parser.parse(new StringInput(sMessage), m_patient);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.xml.messageSyntax", ex.getErrorCode());
      }


      /*
       * Tests of parsing a nillable element.
       */

      // NILLABLE: Optional part empty with nil
      sMessage = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<a:Patient xmlns:a=\"urn:com.nexjsystems:ns:test:a\" " +
         "xmlns:b=\"urn:com.nexjsystems:ns:test:b\" " +
         "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"urn:com.nexjsystems:ns:test:a http://www.nexjsystems.com/nexj/integration.xsd urn:com.nexjsystems:ns:test:b http://www.nexjsystems.com/nexj/b.xsd\">" +
         "<player><firstName>My1</firstName><title xsi:nil=\"true\"/><guid>AQIDBAUGBwgJEBESExQVFg==</guid>" +
         "<id type=\"OHIP\" b:id=\"111222333\"/></player></a:Patient>";
      message = m_parser.parse(new StringInput(sMessage), m_patientNillable);
      player = (TransferObject)message.getValue("player");
      assertNull(player.getValue("title"));


      // NILLABLE: Optional part has text contents with nil
      sMessage = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<a:Patient xmlns:a=\"urn:com.nexjsystems:ns:test:a\" " +
         "xmlns:b=\"urn:com.nexjsystems:ns:test:b\" " +
         "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"urn:com.nexjsystems:ns:test:a http://www.nexjsystems.com/nexj/integration.xsd urn:com.nexjsystems:ns:test:b http://www.nexjsystems.com/nexj/b.xsd\">" +
         "<player><firstName>My1</firstName><title xsi:nil=\"true\">Test</title><guid>AQIDBAUGBwgJEBESExQVFg==</guid>" +
         "<id type=\"OHIP\" b:id=\"111222333\"/></player></a:Patient>";

      try
      {
         message = m_parser.parse(new StringInput(sMessage), m_patientNillable);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.xml.nilElementNotEmpty", ex.getErrorCode());
      }


      // NILLABLE: Optional part has element contents with nil
      sMessage = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<a:Patient xmlns:a=\"urn:com.nexjsystems:ns:test:a\" " +
         "xmlns:b=\"urn:com.nexjsystems:ns:test:b\" " +
         "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"urn:com.nexjsystems:ns:test:a http://www.nexjsystems.com/nexj/integration.xsd urn:com.nexjsystems:ns:test:b http://www.nexjsystems.com/nexj/b.xsd\">" +
         "<player><firstName>My1</firstName><title xsi:nil=\"true\"><efg/></title><guid>AQIDBAUGBwgJEBESExQVFg==</guid>" +
         "<id type=\"OHIP\" b:id=\"111222333\"/></player></a:Patient>";

      try
      {
         message = m_parser.parse(new StringInput(sMessage), m_patientNillable);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.xml.nilElementNotEmpty", ex.getErrorCode());
      }


      // NILLABLE: Attributes allowed when nil
      sMessage = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<a:Patient xmlns:a=\"urn:com.nexjsystems:ns:test:a\" " +
         "xmlns:b=\"urn:com.nexjsystems:ns:test:b\" " +
         "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"urn:com.nexjsystems:ns:test:a http://www.nexjsystems.com/nexj/integration.xsd urn:com.nexjsystems:ns:test:b http://www.nexjsystems.com/nexj/b.xsd\">" +
         "<player><firstName>My1</firstName><title xsi:nil=\"true\"/><guid>AQIDBAUGBwgJEBESExQVFg==</guid>" +
         "<id type=\"OHIP\" b:id=\"111222333\"/>" +
         "<telcom useCode=\"T1\" xsi:nil=\"true\"/><telcom2 useCode=\"T2\" xsi:nil=\"true\"/>" +
         "</player></a:Patient>";
      message = m_parser.parse(new StringInput(sMessage), m_patientNillable);
      player = (TransferObject)message.getValue("player");
      assertNull(player.getValue("title"));
      telcom = (TransferObject)player.getValue("telcom");
      assertNull(telcom.getValue("address"));
      assertEquals("T1", telcom.getValue("useCode"));
      assertEquals(Boolean.TRUE, telcom.getValue("isNil"));

      telcom = (TransferObject)player.getValue("telcom2");
      assertNull(telcom.getValue("address"));
      assertEquals("T2", telcom.getValue("useCode"));


      // NILLABLE: Composite part has attribute and is empty (not nil)
      sMessage = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<a:Patient xmlns:a=\"urn:com.nexjsystems:ns:test:a\" " +
         "xmlns:b=\"urn:com.nexjsystems:ns:test:b\" " +
         "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"urn:com.nexjsystems:ns:test:a http://www.nexjsystems.com/nexj/integration.xsd urn:com.nexjsystems:ns:test:b http://www.nexjsystems.com/nexj/b.xsd\">" +
         "<player><firstName>My1</firstName><title>Test</title><guid>AQIDBAUGBwgJEBESExQVFg==</guid>" +
         "<id type=\"OHIP\" b:id=\"111222333\"/>" +
         "<telcom useCode=\"T1\"/><telcom2 useCode=\"T2\"/>" +
         "</player></a:Patient>";
      message = m_parser.parse(new StringInput(sMessage), m_patientNillable);
      player = (TransferObject)message.getValue("player");
      assertEquals("Test", player.getValue("title"));
      telcom = (TransferObject)player.getValue("telcom");
      assertNull(telcom.getValue("address"));
      assertEquals("T1", telcom.getValue("useCode"));
      assertFalse(telcom.hasValue("isNil"));

      telcom = (TransferObject)player.getValue("telcom2");
      assertNull(telcom.getValue("address"));
      assertEquals("T2", telcom.getValue("useCode"));


      // NILLABLE: Composite message part nil, but has text content
      sMessage = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<a:Patient xmlns:a=\"urn:com.nexjsystems:ns:test:a\" " +
         "xmlns:b=\"urn:com.nexjsystems:ns:test:b\" " +
         "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"urn:com.nexjsystems:ns:test:a http://www.nexjsystems.com/nexj/integration.xsd urn:com.nexjsystems:ns:test:b http://www.nexjsystems.com/nexj/b.xsd\">" +
         "<player><firstName>My1</firstName><title xsi:nil=\"true\"/><guid>AQIDBAUGBwgJEBESExQVFg==</guid>" +
         "<id type=\"OHIP\" b:id=\"111222333\"/>" +
         "<telcom2 useCode=\"T2\" xsi:nil=\"true\">addr2</telcom2>" +
         "</player></a:Patient>";

      try
      {
         message = m_parser.parse(new StringInput(sMessage), m_patientNillable);
         fail("IntegrationException Expected");
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.xml.nilElementNotEmpty", ex.getErrorCode());
      }


      // NILLABLE: Composite message part nil, but has element content
      sMessage = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<a:Patient xmlns:a=\"urn:com.nexjsystems:ns:test:a\" " +
         "xmlns:b=\"urn:com.nexjsystems:ns:test:b\" " +
         "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"urn:com.nexjsystems:ns:test:a http://www.nexjsystems.com/nexj/integration.xsd urn:com.nexjsystems:ns:test:b http://www.nexjsystems.com/nexj/b.xsd\">" +
         "<player><firstName>My1</firstName><title xsi:nil=\"true\"/><guid>AQIDBAUGBwgJEBESExQVFg==</guid>" +
         "<id type=\"OHIP\" b:id=\"111222333\"/>" +
         "<telcom2 useCode=\"T2\" xsi:nil=\"true\"><sub/></telcom2>" +
         "</player></a:Patient>";

      try
      {
         message = m_parser.parse(new StringInput(sMessage), m_patientNillable);
         fail("IntegrationException Expected");
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.xml.nilElementNotEmpty", ex.getErrorCode());
      }
   }

   /**
    * Tests additional cases for a nillable composite message part.
    */
   public void testParseCompositeMessagePartNil() throws Exception
   {
      TransferObject message, cpart;

      message = m_parser.parse(new StringInput(
         "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<t:outer xmlns:t=\"http://www.nexj.com/schema/test/one\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<a>t1</a><b>t2</b>" +
         "</t:outer>"
         ), m_nillableSubMessage);
      assertEquals("t1", message.getValue("a"));
      assertEquals("t2", message.getValue("b"));
      assertFalse(message.hasValue("c"));


      message = m_parser.parse(new StringInput(
         "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<t:outer xmlns:t=\"http://www.nexj.com/schema/test/one\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<a>t1</a><b>t2</b><c z=\"a1\" xsi:nil=\"true\"/>" +
         "</t:outer>"
         ), m_nillableSubMessage);
      assertEquals("t1", message.getValue("a"));
      assertEquals("t2", message.getValue("b"));
      assertEquals("a1", ((TransferObject)message.getValue("c")).getValue("z"));



      message = m_parser.parse(new StringInput(
         "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<t:outer xmlns:t=\"http://www.nexj.com/schema/test/one\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<a>t1</a><b xsi:nil=\"true\"/><c z=\"a1\" xsi:nil=\"true\"/>" +
         "</t:outer>"
         ), m_nillableSubMessage);
      assertEquals("t1", message.getValue("a"));
      assertNull(message.getValue("b"));
      assertEquals("a1", ((TransferObject)message.getValue("c")).getValue("z"));



      message = m_parser.parse(new StringInput(
         "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<soap12:Envelope xmlns:soap12=\"http://www.w3.org/2003/05/soap-envelope\" xmlns:x" +
         "si=\"http://www.w3.org/2001/XMLSchema-instance\"><soap12:Body><ns2:A xmlns:ns2=\"urn:hl7-org:v3\"><ns2:B xsi:nil=\"1\"/></ns2:A>" +
         "</soap12:Body></soap12:Envelope>"), m_nillableRefSubMessage);
      assertNull(message.getValue("B"));



      message = new TransferObject("XML_NillableRefSubMessage2");

      StringWriter outWriter = new StringWriter();
      Output out = new WriterOutput(outWriter);

      m_formatter.format(message, m_nillableRefSubMessage2, out);
      message = m_parser.parse(new StringInput(outWriter.toString()), m_nillableRefSubMessage2);



      message = m_parser.parse(new StringInput(
         "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<t:outer xmlns:t=\"http://www.nexj.com/schema/test/one\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<a>t1</a><b>t2</b><c z=\"a1\"><x>x1</x></c>" +
         "</t:outer>"
         ), m_nillableSubMessage);
      assertEquals("t1", message.getValue("a"));
      assertEquals("t2", message.getValue("b"));
      assertTrue(message.hasValue("c"));
      cpart = (TransferObject)message.getValue("c");
      assertEquals("x1", cpart.getValue("x"));
      assertEquals("a1", cpart.getValue("z"));


      try
      {
         message = m_parser.parse(new StringInput(
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
            "<t:outer xmlns:t=\"http://www.nexj.com/schema/test/one\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
            "<a>t1</a><b>t2</b><c z=\"a1\"/>" +
            "</t:outer>"
            ), m_nillableSubMessage);
         fail("IntegrationException expected");
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.minPartCount", ex.getErrorCode());
         assertEquals("XML_NillableSubMessage c x", ex.getErrorArgs()[0]);
      }


      try
      {
         message = m_parser.parse(new StringInput(
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
            "<t:outer xmlns:t=\"http://www.nexj.com/schema/test/one\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
            "<a>t1</a><b>t2</b><c/>" +
            "</t:outer>"
            ), m_nillableSubMessage);
         fail("IntegrationException expected");
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.minPartCount", ex.getErrorCode());
         assertEquals("XML_NillableSubMessage c x", ex.getErrorArgs()[0]);
      }
   }


   /**
    * Tests that all of the character content from an element, regardless
    * of how it is interspersed with child elements, goes into the
    * value-mapped message part.
    */
   public void testValuePartGetsAllCharacterContent() throws Exception
   {
      Message message = m_context.getMetadata().getMessage("XML_ValuePart");
      TransferObject tobj;

      tobj = m_parser.parse(new StringInput(
         "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<outer attr=\"tst\">" +
         "<a>123</a>" +
         "Part1" +
         "<a>456</a>" +
         "Part2" +
         "</outer>"), message);

      assertEquals("tst", tobj.getValue("attr"));
      assertEquals("Part1Part2", tobj.getValue("data"));

      List elementList = (List)tobj.getValue("a");

      assertEquals(2, elementList.size());
      assertEquals("123", elementList.get(0));
      assertEquals("456", elementList.get(1));
   }

   /**
    * Tests optional value-mapped part when its element is empty.
    */
   public void testEmptyValuePart()
   {
      Message message = m_context.getMetadata().getMessage("XML_ValuePart");
      TransferObject tobj;

      tobj = m_parser.parse(new StringInput(
         "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<outer attr=\"tst\">" +
         "</outer>"), message);

      assertEquals("tst", tobj.getValue("attr"));
      assertNull(tobj.getValue("data"));
   }

   /**
    * Tests required value-mapped part when its element is empty.
    */
   public void testEmptyRequiredValuePart()
   {
      Message message = m_context.getMetadata().getMessage("XML_ValuePartReq");

      try
      {
         m_parser.parse(new StringInput(
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
            "<outer attr=\"tst\">" +
            "</outer>"), message);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.minPartCount", ex.getErrorCode());
         assertEquals("XML_ValuePartReq data", ex.getErrorArgs()[0]);
      }
   }

   /**
    * Tests format string support in the XML format adapter.
    */
   public void testFormatXMLWithFormatString()
   {
      StringWriter outWriter = new StringWriter();
      WriterOutput out = new WriterOutput(outWriter);
      TransferObject tobj = new TransferObject("XML_FormatString", 11);
      Timestamp ts1 = Timestamp.valueOf("2010-03-03 18:30:00.123");
      List col1 = new ArrayList(4);
      BigDecimal dec = new BigDecimal("3.14159265358979323846264");

      tobj.setValue("normalDT", ts1);
      tobj.setValue("formatDT", ts1);
      tobj.setValue("formatLocaleDT", ts1);
      tobj.setValue("normalInteger", col1);
      tobj.setValue("formatInteger", col1);
      tobj.setValue("normalDouble", col1);
      tobj.setValue("formatDouble", col1);
      tobj.setValue("normalBool", Boolean.TRUE);
      tobj.setValue("formatBool", Boolean.TRUE);
      tobj.setValue("normalDecimal", dec);
      tobj.setValue("formatDecimal", dec);

      col1.add(Primitive.createInteger(4));
      col1.add(Primitive.createInteger(-5));
      col1.add(Primitive.createDouble(112.95813));
      col1.add(Primitive.createDouble(-42.424242));

      m_formatter.format(tobj, m_formatStringMessage, out);

      String sOutput = outWriter.toString();

      assertEquals("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<format>" +
         "<normalDT>2010-03-03T23:30:00.123Z</normalDT>" +
         "<formatDT>20100303183000.123-0500</formatDT>" +
         "<formatLocaleDT>Wednesday, March 3, 2010</formatLocaleDT>" +
         "<normalInteger>4</normalInteger>" +
         "<normalInteger>-5</normalInteger>" +
         "<normalInteger>112</normalInteger>" +
         "<normalInteger>-42</normalInteger>" +
         "<formatInteger>4</formatInteger>" +
         "<formatInteger>(5)</formatInteger>" +
         "<formatInteger>112</formatInteger>" +
         "<formatInteger>(42)</formatInteger>" +
         "<normalDouble>4.0</normalDouble>" +
         "<normalDouble>-5.0</normalDouble>" +
         "<normalDouble>112.95813</normalDouble>" +
         "<normalDouble>-42.424242</normalDouble>" +
         "<formatDouble>4.00</formatDouble>" +
         "<formatDouble>(5.00)</formatDouble>" +
         "<formatDouble>112.96</formatDouble>" +
         "<formatDouble>(42.42)</formatDouble>" +
         "<normalBool>1</normalBool>" +
         "<formatBool>Yes</formatBool>" +
         "<normalDecimal>3.14159265358979323846264</normalDecimal>" +
         "<formatDecimal>3.14159265358979324</formatDecimal>" +
         "</format>",
         sOutput
      );

      tobj = m_parser.parse(new StringInput(sOutput), m_formatStringMessage);

      assertEquals(ts1, tobj.getValue("normalDT"));
      assertEquals(ts1, tobj.getValue("formatDT"));
      assertEquals(Timestamp.valueOf("2010-03-03 00:00:00.000"), tobj.getValue("formatLocaleDT"));
      assertEquals(Boolean.TRUE, tobj.getValue("normalBool"));
      assertEquals(Boolean.TRUE, tobj.getValue("formatBool"));
      assertEquals(dec, tobj.getValue("normalDecimal"));
      assertEquals(new BigDecimal("3.14159265358979324"), tobj.getValue("formatDecimal"));

      col1 = (List)tobj.getValue("normalInteger");
      assertEquals(Primitive.createInteger(4), col1.get(0));
      assertEquals(Primitive.createInteger(-5), col1.get(1));
      assertEquals(Primitive.createInteger(112), col1.get(2));
      assertEquals(Primitive.createInteger(-42), col1.get(3));

      col1 = (List)tobj.getValue("formatInteger");
      assertEquals(Primitive.createInteger(4), col1.get(0));
      assertEquals(Primitive.createInteger(-5), col1.get(1));
      assertEquals(Primitive.createInteger(112), col1.get(2));
      assertEquals(Primitive.createInteger(-42), col1.get(3));

      col1 = (List)tobj.getValue("normalDouble");
      assertEquals(Primitive.createDouble(4), col1.get(0));
      assertEquals(Primitive.createDouble(-5), col1.get(1));
      assertEquals(Primitive.createDouble(112.95813), col1.get(2));
      assertEquals(Primitive.createDouble(-42.424242), col1.get(3));

      col1 = (List)tobj.getValue("formatDouble");
      assertEquals(Primitive.createDouble(4), col1.get(0));
      assertEquals(Primitive.createDouble(-5), col1.get(1));
      assertEquals(Primitive.createDouble(112.96), col1.get(2));
      assertEquals(Primitive.createDouble(-42.42), col1.get(3));
   }

   /**
    * Message with root node reference to another message, using a different envelope than the referent.
    */
   public void testFormatMessageWithRootRef()
   {
      Message message = m_context.getMetadata().getMessage("SOAP12_PatientDemographics");
      TransferObject root = new TransferObject();
      TransferObject tobj;
      List list;
      StringWriter writer = new StringWriter();

      root.setClassName("SOAP12_PatientDemographics");
      tobj = new TransferObject(4);
      root.setValue("player", tobj);
      tobj.setValue("firstName", "My1");
      tobj.setValue("lastName", "Test");
      tobj.setValue("guid", Binary.parse("01020304050607080910111213141516"));
      list = new ArrayList(1);
      tobj.setValue("id", list);
      tobj = new TransferObject(2);
      list.add(tobj);
      tobj.setValue("type", "OHIP");
      tobj.setValue("id", "111222333");

      m_formatter.format(root, message, new WriterOutput(writer));
      assertEquals(PATIENT_SOAP12, writer.toString());
   }

   /**
    * Message with root node reference to another message, using a different envelope than the referent.
    */
   public void testParseMessageWithRootRef() throws Exception
   {
      Message message = m_context.getMetadata().getMessage("SOAP12_PatientDemographics");
      TransferObject root = m_parser.parse(new StringInput(PATIENT_SOAP12), message);

      assertFalse(root.hasValue("headers"));

      TransferObject player = (TransferObject)root.getValue("player");

      assertEquals("My1", player.getValue("firstName"));
      assertEquals("Test", player.getValue("lastName"));

      TransferObject id = (TransferObject)((List)player.getValue("id")).get(0);

      assertEquals("SOAP12_PatientDemographics", root.getClassName());
      assertEquals("OHIP", id.getValue("type"));
      assertEquals("111222333", id.getValue("id"));
   }

   /**
    * Tests SOAP 1.2 fault parsing.
    */
   public void testSOAP12Fault() throws Exception
   {
      Message message = m_context.getMetadata().getMessage("SOAP12Fault");
      TransferObject root = new TransferObject("SOAP12Fault", 5);
      TransferObject tobj;
      List collection;
      StringWriter writer;

      root.setValue("node", "urn:nexj:test:node1");
      root.setValue("role", "urn:nexj:test:role1");
      tobj = new TransferObject(2);
      tobj.setValue("value", "soap12:Sender");
      root.setValue("code", tobj);
      tobj.setValue("subcode", new TransferObject(1));
      tobj = (TransferObject)tobj.getValue("subcode");
      tobj.setValue("value", "urn:nexj:test:subcode");
      tobj = new TransferObject(1);
      root.setValue("reason", tobj);
      collection = new ArrayList(1);
      tobj.setValue("text", collection);
      tobj = new TransferObject(2);
      collection.add(tobj);
      tobj.setValue("lang", "en");
      tobj.setValue("value", "English error message");
      tobj = new TransferObject(2);
      collection.add(tobj);
      tobj.setValue("lang", "fr");
      tobj.setValue("value", "French error message");
      root.setValue("detail", "<p>Application-specific <b>detail</b> goes here.</p>");

      writer = new StringWriter();
      m_formatter.format(root, message, new WriterOutput(writer));

      try
      {
         m_parser.parse(new ReaderInput(new StringReader(writer.toString())), m_context.getMetadata().getMessage("SOAP12_PatientDemographics"));
         fail("Expected exception");
      }
      catch (SOAPFaultException ex)
      {
         assertEquals("urn:nexj:test:node1", ((SOAP12FaultException)ex).getNode());
         assertEquals("urn:nexj:test:role1", ((SOAP12FaultException)ex).getRole());
         assertEquals("English error message", ((SOAP12FaultException)ex).getReason("en"));
         assertEquals("French error message", ((SOAP12FaultException)ex).getReason("fr"));
         assertEquals("soap12:Sender", ((SOAP12FaultException)ex).getFaultCode());

         List subcodeList = ((SOAP12FaultException)ex).getFaultSubcodeList();

         assertEquals(1, subcodeList.size());
         assertEquals("urn:nexj:test:subcode", subcodeList.get(0));
      }
   }

   /**
    * Test single aggregation.
    */
   public void testSingleAggregation() throws Exception
   {
      m_formatter.format(m_parser.parse(new StringInput(SINGLE_AGGREGATION_TEST), m_singleAggregationMessage),
            m_singleAggregationMessage, new WriterOutput(new StringWriter()));
   }

   /**
    * Test message inheritance parsing.
    */
   public void testInheritanceParsing()
   {
      Message msg = m_context.getMetadata().getMessage("XML_Inheritance_Test");

      TransferObject tobj = m_parser.parse(new StringInput(
         "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" +
         "<soap12:Envelope xmlns:soap12=\"http://www.w3.org/2003/05/soap-envelope\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
            "<soap12:Body>" +
               "<ns2:T xmlns:ns2=\"urn:hl7-org:v3\" testValue=\"t1\">" +
                  "<ns2:value baseValue=\"base\" value=\"100\" xsi:type=\"ns2:B\"/>" +
               "</ns2:T>" +
            "</soap12:Body>" +
         "</soap12:Envelope>"), msg);

      TransferObject valueTobj = (TransferObject)tobj.getValue("value");

      assertEquals("base", valueTobj.getValue("baseValue"));
      assertEquals(Primitive.createInteger(100), valueTobj.getValue("value"));
   }
}
