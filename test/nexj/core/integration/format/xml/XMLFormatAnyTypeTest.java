// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.xml;

import java.io.StringWriter;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import nexj.core.integration.IntegrationException;
import nexj.core.integration.MessageFormatter;
import nexj.core.integration.MessageParser;
import nexj.core.integration.io.StringInput;
import nexj.core.integration.io.WriterOutput;
import nexj.core.meta.MetadataLookupException;
import nexj.core.meta.Primitive;
import nexj.core.meta.Repository;
import nexj.core.meta.integration.Format;
import nexj.core.meta.integration.Message;
import nexj.core.rpc.TransferObject;
import nexj.core.rpc.xml.XML;
import nexj.core.runtime.InvocationContext;
import nexj.core.util.Binary;

import junit.framework.TestCase;

/**
 * Tests the xsd:anyType support in the XML parser and formatter.
 */
public class XMLFormatAnyTypeTest extends TestCase
{
   // associations

   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   /**
    * The XML message formatter.
    */
   protected MessageFormatter m_formatter;

   /**
    * The XML message parser.
    */
   protected MessageParser m_parser;

   /**
    * The XMLAnyTest message metadata.
    */
   protected Message m_message;

   // operations

   public void setUp()
   {
      m_context = (InvocationContext)Repository.getMetadata().getComponent("System.InvocationContext").getInstance(null);

      Format format = m_context.getMetadata().getFormat("XML");

      m_parser = (MessageParser)format.getParser().getInstance(m_context);
      m_formatter = (MessageFormatter)format.getFormatter().getInstance(m_context);
      m_message = m_context.getMetadata().getMessage("XMLAnyTest");
   }

   /**
    * Tests that a String gets formatted without quoting when put in a part of type ANY.
    */
   public void testFormatStringValue()
   {
      String sMarkup = "<p>This is some <b>marked-up</b> text.</p>";
      String sQuotedMarkup = "&lt;p&gt;This is some &lt;b&gt;marked-up&lt;/b&gt; text.&lt;/p&gt;";
      TransferObject msg = new TransferObject(m_message.getName(), 2);
      TransferObject valueElement = new TransferObject(2);

      valueElement.setValue("attribute", sMarkup);
      valueElement.setValue("value", sMarkup);
      msg.setValue("anyTypeElementComp", valueElement);
      msg.setValue("anyTypeElement", sMarkup);

      StringWriter writer = new StringWriter();
      WriterOutput out = new WriterOutput(writer);

      m_formatter.format(msg, m_message, out);

      assertEquals("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<XMLAnyTest xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"" +
         " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"><anyTypeElement>" +
         sMarkup +
         "</anyTypeElement><anyTypeElementComp attribute=\"" + sQuotedMarkup + "\">" +
         sMarkup +
         "</anyTypeElementComp></XMLAnyTest>", writer.toString());
   }

   /**
    * Tests that a TO gets formatted when put in a part of type ANY.
    */
   public void testFormatMessageValue()
   {
      String sMarkup = "<internationalPrice currency=\"CNY\">150</internationalPrice>";
      TransferObject msg = new TransferObject(m_message.getName(), 4);
      TransferObject subMsg = new TransferObject("XML_InternationalPrice", 2);
      TransferObject valueElement = new TransferObject(2);
      TransferObject valueElementWithBeforeAndAfter = new TransferObject(4);

      subMsg.setValue("currency", "CNY");
      subMsg.setValue("price", Primitive.createInteger(150));
      msg.setValue("anyTypeElement", subMsg);
      msg.setValue("anyTypeElementComp", valueElement);
      msg.setValue("anyInterfaceElement", subMsg);
      msg.setValue("anyInterfaceElementComp", valueElementWithBeforeAndAfter);
      valueElement.setValue("attribute", "Message disallowed");
      valueElement.setValue("value", subMsg);
      valueElementWithBeforeAndAfter.setValue("attribute", "Message disallowed");
      valueElementWithBeforeAndAfter.setValue("value", subMsg);
      valueElementWithBeforeAndAfter.setValue("before", "pre");
      valueElementWithBeforeAndAfter.setValue("after", "post");

      StringWriter writer = new StringWriter();
      WriterOutput out = new WriterOutput(writer);

      m_formatter.format(msg, m_message, out);

      assertEquals("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<XMLAnyTest xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"" +
         " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"><anyTypeElement>" +
         sMarkup +
         "</anyTypeElement>" +
         "<anyInterfaceElement>" +
         sMarkup +
         "</anyInterfaceElement>" +
         "<anyTypeElementComp attribute=\"Message disallowed\">" +
         sMarkup +
         "</anyTypeElementComp>" +
         "<anyInterfaceElementComp attribute=\"Message disallowed\">" +
         "<before>pre</before>" +
         sMarkup +
         "<after>post</after>" +
         "</anyInterfaceElementComp></XMLAnyTest>", writer.toString());
   }

   /**
    * Tests that a collection of TOs gets formatted when put in a part of type ANY.
    */
   public void testFormatCollectionValue()
   {
      String sMarkup = "<internationalPrice currency=\"CNY\">150</internationalPrice>";
      String sMarkup2 = "<a:Patient xmlns:a=\"urn:com.nexjsystems:ns:test:a\" " +
         "xmlns:b=\"urn:com.nexjsystems:ns:test:b\" " +
         "xsi:schemaLocation=\"urn:com.nexjsystems:ns:test:a " +
         "http://www.nexjsystems.com/nexj/integration.xsd " +
         "urn:com.nexjsystems:ns:test:b " +
         "http://www.nexjsystems.com/nexj/b.xsd\">" +
         "<player><lastName>Hu</lastName></player></a:Patient>";
      TransferObject msg = new TransferObject(m_message.getName(), 2);
      TransferObject subMsg = new TransferObject("XML_InternationalPrice", 2);
      TransferObject player = new TransferObject(1);
      TransferObject subMsg2 = new TransferObject("XML_PatientDemographics", 1);
      TransferObject valueElement = new TransferObject(1);
      ArrayList collection = new ArrayList(4);

      subMsg.setValue("currency", "CNY");
      subMsg.setValue("price", Primitive.createInteger(150));
      subMsg2.setValue("player", player);
      player.setValue("lastName", "Hu");
      collection.add(subMsg);
      collection.add(subMsg2);
      collection.add(subMsg);
      collection.add(subMsg);
      msg.setValue("anyTypeElementCollection", collection);
      msg.setValue("anyTypeElementCompValueCollection", valueElement);
      valueElement.setValue("value", collection);

      StringWriter writer = new StringWriter();
      WriterOutput out = new WriterOutput(writer);

      m_formatter.format(msg, m_message, out);

      assertEquals("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<XMLAnyTest xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"" +
         " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"><anyTypeElementCollection>" +
         sMarkup + "</anyTypeElementCollection><anyTypeElementCollection>" +
         sMarkup2 + "</anyTypeElementCollection><anyTypeElementCollection>" +
         sMarkup + "</anyTypeElementCollection><anyTypeElementCollection>" +
         sMarkup +
         "</anyTypeElementCollection><anyTypeElementCompValueCollection>" +
         sMarkup + sMarkup2 + sMarkup + sMarkup +
         "</anyTypeElementCompValueCollection></XMLAnyTest>", writer.toString());

      ArrayList collection2 = new ArrayList(2);

      collection2.add(valueElement);
      collection2.add(valueElement);
      msg.setValue("anyTypeElementCompCollection", collection2);
      msg.removeValue("anyTypeElementCompValueCollection");

      writer = new StringWriter();
      out = new WriterOutput(writer);

      m_formatter.format(msg, m_message, out);

      assertEquals("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<XMLAnyTest xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"" +
         " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"><anyTypeElementCollection>" +
         sMarkup + "</anyTypeElementCollection><anyTypeElementCollection>" +
         sMarkup2 + "</anyTypeElementCollection><anyTypeElementCollection>" +
         sMarkup + "</anyTypeElementCollection><anyTypeElementCollection>" +
         sMarkup +
         "</anyTypeElementCollection><anyTypeElementCompCollection>" +
         sMarkup + sMarkup2 + sMarkup + sMarkup +
         "</anyTypeElementCompCollection><anyTypeElementCompCollection>" +
         sMarkup + sMarkup2 + sMarkup + sMarkup +
         "</anyTypeElementCompCollection></XMLAnyTest>", writer.toString());
   }

   /**
    * Tests formatting a collection of ANY-type primitives.
    */
   public void testFormatTypedCollectionValue()
   {
      TransferObject msg = new TransferObject(m_message.getName(), 1);
      ArrayList collection = new ArrayList(5);

      collection.add("5 < 4");
      collection.add(Primitive.createInteger(42));
      collection.add(Binary.parse("000000"));
      collection.add(new Timestamp(86400L*1000L));
      collection.add(null);
      msg.setValue("anyTypeElementCollectionTyped", collection);

      StringWriter writer = new StringWriter();
      WriterOutput out = new WriterOutput(writer);

      m_formatter.format(msg, m_message, out);

      assertEquals(XML.HEADER +
         "<XMLAnyTest xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"" +
         " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<anyTypeElementCollectionTyped xsi:type=\"xsd:string\">5 &lt; 4</anyTypeElementCollectionTyped>" +
         "<anyTypeElementCollectionTyped xsi:type=\"xsd:integer\">42</anyTypeElementCollectionTyped>" +
         "<anyTypeElementCollectionTyped xsi:type=\"xsd:base64Binary\">AAAA</anyTypeElementCollectionTyped>" +
         "<anyTypeElementCollectionTyped xsi:type=\"xsd:dateTime\">1970-01-02T00:00:00.000Z</anyTypeElementCollectionTyped>" +
         "<anyTypeElementCollectionTyped xsi:nil=\"1\"/>" +
         "</XMLAnyTest>", writer.toString());
   }

   /**
    * Tests that the XSD and XSI namespaces get added when formatting type: any subtype: xsi.
    * Also tests that one can specify a well-known namespace (e.g. "soap") on a part, without specifying a URI.
    */
   public void testFormatSingleElementAny()
   {
      Message message = m_context.getMetadata().getMessage("XML_SingleElementAny");
      TransferObject root = new TransferObject(message.getName(), 1);

      root.setValue("value", "test");

      StringWriter writer = new StringWriter();
      WriterOutput out = new WriterOutput(writer);

      m_formatter.format(root, message, out);

      assertEquals(XML.HEADER +
         "<root" +
         " xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"" +
         " xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\"" +
         " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<value xsi:type=\"xsd:string\">test</value>" +
         "</root>", writer.toString());
   }

   /**
    * Tests that the XML corresponding to a part of type ANY gets placed directly
    * into a String.
    */
   public void testParseToString()
   {
      String sMarkup = "<p a=\"b\">This is some <b>marked-up</b> text.</p>";
      String sQuotedMarkup = "&lt;p a=&quot;b&quot;&gt;This is some &lt;b&gt;marked-up&lt;/b&gt; text.&lt;/p&gt;";
      String sMessage = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<XMLAnyTest> <anyTypeElement>" +
         " abc " + sMarkup + " 123 " +
         "</anyTypeElement> " +
         "<anyTypeElementCollection> " +
         sMarkup + sMarkup +
         "</anyTypeElementCollection> " +
         "<anyTypeElementComp attribute=\"" + sQuotedMarkup + "\"> " +
         "<before>pre</before>" +
         " abc " + sMarkup +
         "abc<after>post</after>123" +
         "</anyTypeElementComp> " +
         "<anyTypeElementCompCollection>" +
         sMarkup +
         "</anyTypeElementCompCollection> " +
         "<anyTypeElementCompCollection attribute=\"" + sQuotedMarkup + "\">   " +
         sMarkup +
         "   </anyTypeElementCompCollection></XMLAnyTest>";

      TransferObject msg = m_parser.parse(new StringInput(sMessage), m_message);
      List list, subList;

      assertEquals(" abc " + sMarkup + " 123 ", msg.getValue("anyTypeElement"));
      list = (List)msg.getValue("anyTypeElementCollection");
      assertEquals(1, list.size());
      assertEquals(" " + sMarkup + sMarkup, list.get(0));

      TransferObject subMsg = (TransferObject)msg.getValue("anyTypeElementComp");

      assertEquals(sMarkup, subMsg.getValue("attribute"));
      assertEquals(" <before>pre</before> abc " + sMarkup + "abc<after>post</after>123", subMsg.getValue("value"));

      list = (List)msg.getValue("anyTypeElementCompCollection");

      assertEquals(2, list.size());
      subMsg = (TransferObject)list.get(0);
      subList = (List)subMsg.getValue("value");
      assertEquals(sMarkup, subList.get(0));
      subMsg = (TransferObject)list.get(1);
      subList = (List)subMsg.getValue("value");
      assertEquals("   " + sMarkup + "   ", subList.get(0));
      assertEquals(sMarkup, subMsg.getValue("attribute"));
   }

   /**
    * Tests that when a part of type ANY has a interface specified, the sub-message is parsed
    * to one of the messages in the interface.
    */
   public void testParseToMessage()
   {
      String sMarkup = "<internationalPrice currency=\"CNY\">150</internationalPrice>";
      String sQuotedMarkup = "&lt;internationalPrice currency=&quot;CNY&quot;&gt;150&lt;/internationalPrice&gt;";
      String sMessage = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<XMLAnyTest> <anyInterfaceElement> " +
         sMarkup +
         "</anyInterfaceElement> <anyInterfaceElementCollection>" +
         sMarkup +
         "</anyInterfaceElementCollection> <anyInterfaceElementCollection> " +
         sMarkup +
         "</anyInterfaceElementCollection> " +
         "<anyInterfaceElementComp attribute=\"" + sQuotedMarkup + "\"> " +
         " <before>pre</before> " +
         sMarkup +
         " <after>post</after> " +
         " <subInterface>   " + sMarkup + " </subInterface> " +
         "</anyInterfaceElementComp> </XMLAnyTest>";

      TransferObject msg = m_parser.parse(new StringInput(sMessage), m_message);
      TransferObject subMsg = (TransferObject)msg.getValue("anyInterfaceElement");
      List list;

      assertEquals("XML_InternationalPrice", subMsg.getClassName());
      assertEquals("CNY", subMsg.getValue("currency"));
      assertEquals(15000, ((BigDecimal)subMsg.getValue("price")).multiply(new BigDecimal(100)).intValueExact());

      list = (List)msg.getValue("anyInterfaceElementCollection");
      assertEquals(2, list.size());
      subMsg = (TransferObject)list.get(0);
      assertEquals("XML_InternationalPrice", subMsg.getClassName());
      assertEquals("CNY", subMsg.getValue("currency"));
      assertEquals(15000, ((BigDecimal)subMsg.getValue("price")).multiply(new BigDecimal(100)).intValueExact());
      subMsg = (TransferObject)list.get(1);
      assertEquals("XML_InternationalPrice", subMsg.getClassName());
      assertEquals("CNY", subMsg.getValue("currency"));
      assertEquals(15000, ((BigDecimal)subMsg.getValue("price")).multiply(new BigDecimal(100)).intValueExact());
      

      msg = (TransferObject)msg.getValue("anyInterfaceElementComp");
      assertEquals(sMarkup, msg.getValue("attribute"));
      assertEquals("pre", msg.getValue("before"));
      assertEquals("post", msg.getValue("after"));

      subMsg = (TransferObject)msg.getValue("value");
      assertEquals("XML_InternationalPrice", subMsg.getClassName());
      assertEquals("CNY", subMsg.getValue("currency"));
      assertEquals(15000, ((BigDecimal)subMsg.getValue("price")).multiply(new BigDecimal(100)).intValueExact());

      subMsg = (TransferObject)msg.getValue("subInterface");
      assertEquals("XML_InternationalPrice", subMsg.getClassName());
      assertEquals("CNY", subMsg.getValue("currency"));
      assertEquals(15000, ((BigDecimal)subMsg.getValue("price")).multiply(new BigDecimal(100)).intValueExact());
   }

   /**
    * Tests that when a part of type ANY has an interface specified, multiple sub-messages may be
    * parsed an put in a collection, if the part is a collection.
    */
   public void testParseToCollection()
   {
      String sMarkup = "<internationalPrice currency=\"CNY\">150</internationalPrice>";
      String sMarkup2 = "<internationalPrice currency=\"GBP\">233</internationalPrice>";
      String sQuotedMarkup = "&lt;internationalPrice currency=&quot;CNY&quot;&gt;150&lt;/internationalPrice&gt;";
      String sMessage = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<XMLAnyTest> " +
         "<anyInterfaceElementCompCollection attribute=\"" + sQuotedMarkup + "\"> " +
         sMarkup +
         "</anyInterfaceElementCompCollection> <anyInterfaceElementCompCollection attribute=\"" + sQuotedMarkup + "\">" +
         " <before>pre</before> " +
         sMarkup + " " + sMarkup2 +
         " <after>post</after> " +
         "</anyInterfaceElementCompCollection> </XMLAnyTest>";

      TransferObject msg = m_parser.parse(new StringInput(sMessage), m_message);
      TransferObject subMsg;
      List list, subList;

      list = (List)msg.getValue("anyInterfaceElementCompCollection");
      assertEquals(2, list.size());

      msg = (TransferObject)list.get(0);
      assertEquals(sMarkup, msg.getValue("attribute"));

      subList = (List)msg.getValue("value");
      assertEquals(1, subList.size());
      subMsg = (TransferObject)subList.get(0);
      assertEquals("XML_InternationalPrice", subMsg.getClassName());
      assertEquals("CNY", subMsg.getValue("currency"));
      assertEquals(15000, ((BigDecimal)subMsg.getValue("price")).multiply(new BigDecimal(100)).intValueExact());

      msg = (TransferObject)list.get(1);
      assertEquals(sMarkup, msg.getValue("attribute"));
      assertEquals("pre", msg.getValue("before"));
      assertEquals("post", msg.getValue("after"));

      subList = (List)msg.getValue("value");
      assertEquals(2, subList.size());
      subMsg = (TransferObject)subList.get(0);
      assertEquals("XML_InternationalPrice", subMsg.getClassName());
      assertEquals("CNY", subMsg.getValue("currency"));
      assertEquals(15000, ((BigDecimal)subMsg.getValue("price")).multiply(new BigDecimal(100)).intValueExact());
      subMsg = (TransferObject)subList.get(1);
      assertEquals("XML_InternationalPrice", subMsg.getClassName());
      assertEquals("GBP", subMsg.getValue("currency"));
      assertEquals(23300, ((BigDecimal)subMsg.getValue("price")).multiply(new BigDecimal(100)).intValueExact());
   }

   /**
    * Tests parsing a collection of ANY-type primitives.
    */
   public void testParseTypedCollection()
   {
      String sMessage = XML.HEADER +
         "<XMLAnyTest xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"" +
         " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<anyTypeElementCollectionTyped xsi:type=\"xsd:string\">5 &lt; 4</anyTypeElementCollectionTyped>" +
         "<anyTypeElementCollectionTyped xsi:type=\"xsd:integer\">42</anyTypeElementCollectionTyped>" +
         "<anyTypeElementCollectionTyped xsi:type=\"xsd:base64Binary\">AAAA</anyTypeElementCollectionTyped>" +
         "<anyTypeElementCollectionTyped xsi:type=\"xsd:dateTime\">1970-01-02T00:00:00.000Z</anyTypeElementCollectionTyped>" +
         "<anyTypeElementCollectionTyped xsi:nil=\"1\"/>" +
         "<anyTypeElementCollectionTyped xsi:type=\"xsd:integer\"/>" +
         "<anyTypeElementCollectionTyped/>" +
         "</XMLAnyTest>";

      TransferObject msg = m_parser.parse(new StringInput(sMessage), m_message);
      List list = (List)msg.getValue("anyTypeElementCollectionTyped");

      assertEquals(7, list.size());

      assertEquals("5 < 4", list.get(0));
      assertEquals(Primitive.createInteger(42), list.get(1));
      assertEquals(Binary.parse("000000"), list.get(2));
      assertEquals(new Timestamp(86400L*1000L), list.get(3));
      assertNull(list.get(4));
      assertNull(list.get(5));
      assertNull(list.get(6));
   }

   /**
    * Tests that a suitable exception is thrown when the any-type part interface lookup fails.
    */
   public void testParseUnknownMessage()
   {
      // Primitive element contents are an unknown message
      String sMessage = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<XMLAnyTest> <anyInterfaceElement>" +
         " <unknown> 123</unknown> " +
         "</anyInterfaceElement> </XMLAnyTest>";

      try
      {
         m_parser.parse(new StringInput(sMessage), m_message);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.xml.interfaceElementLookup", ex.getErrorCode());
         assertEquals("unknown", ex.getErrorArgs()[1]);
         assertEquals("AnyTypeTest", ex.getErrorArgs()[2]);
      }

      // Value-mapped part is unknown
      sMessage = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<XMLAnyTest> <anyInterfaceElementComp>" +
         " <before>pre</before> " +
         " <unknown>123</unknown> " +
         " <after>post</after> " +
         "</anyInterfaceElementComp> </XMLAnyTest>";

      try
      {
         m_parser.parse(new StringInput(sMessage), m_message);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.xml.invalidElement", ex.getErrorCode());
         assertEquals("unknown", ex.getErrorArgs()[1]);
         assertEquals("XMLAnyTest anyInterfaceElementComp", ex.getErrorArgs()[2]);
      }
   }

   /**
    * Tests that an exception is thrown when an unknown message is passed to the formatter.
    */
   public void testFormatUnknownMessage()
   {
      TransferObject msg = new TransferObject(m_message.getName(), 1);
      TransferObject subMsg = new TransferObject("UnknownMessage", 1);

      subMsg.setValue("abc", "123");
      msg.setValue("anyTypeElement", subMsg);

      StringWriter writer = new StringWriter();
      WriterOutput out = new WriterOutput(writer);

      try
      {
         m_formatter.format(msg, m_message, out);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
      }
   }

   /**
    * Tests that the part counts are validated.
    */
   public void testParseErrorOnMultipleOccurrences()
   {
      String sMarkup = "<internationalPrice currency=\"CNY\">150</internationalPrice>";
      String sMessage = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<XMLAnyTest> <anyInterfaceElement> " +
         sMarkup +
         "</anyInterfaceElement> <anyInterfaceElement>" +
         sMarkup +
         "</anyInterfaceElement> </XMLAnyTest>";

      try
      {
         m_parser.parse(new StringInput(sMessage), m_message);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.xml.invalidElement", ex.getErrorCode());
         assertEquals("anyInterfaceElement", ex.getErrorArgs()[1]);
         assertEquals("XMLAnyTest", ex.getErrorArgs()[2]);
      }


      // There can be at most 1 of "anyTypeElementComp"
      sMessage = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<XMLAnyTest> <anyTypeElementComp attribute=\"123\"> " +
         sMarkup +
         "</anyTypeElementComp> <anyTypeElementComp attribute=\"456\">" +
         sMarkup +
         "</anyTypeElementComp> </XMLAnyTest>";

      try
      {
         m_parser.parse(new StringInput(sMessage), m_message);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.xml.invalidElement", ex.getErrorCode());
         assertEquals("anyTypeElementComp", ex.getErrorArgs()[1]);
         assertEquals("XMLAnyTest", ex.getErrorArgs()[2]);
      }


      // There can be at most 3 of "anyInterfaceElementCompCollection"
      String sItem = "<anyInterfaceElementCompCollection> <before>pre</before>" + sMarkup + "</anyInterfaceElementCompCollection>";

      sMessage = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<XMLAnyTest> " +
         sItem + sItem + sItem + sItem +
         "</XMLAnyTest>";

      try
      {
         m_parser.parse(new StringInput(sMessage), m_message);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.maxPartCount", ex.getErrorCode());
         assertEquals("XMLAnyTest anyInterfaceElementCompCollection", ex.getErrorArgs()[0]);
      }


      // There can be at most 2 of the value part.
      sMessage = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<XMLAnyTest> <anyInterfaceElementCompCollection>" +
         sMarkup + sMarkup + sMarkup + sMarkup +
         "</anyInterfaceElementCompCollection> </XMLAnyTest>";

      try
      {
         m_parser.parse(new StringInput(sMessage), m_message);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.maxPartCount", ex.getErrorCode());
         assertEquals("XMLAnyTest anyInterfaceElementCompCollection value", ex.getErrorArgs()[0]);
      }


      // The <before> element is missing
      sMessage = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<XMLAnyTest> <anyInterfaceElementComp>" +
         sMarkup +
         "</anyInterfaceElementComp> </XMLAnyTest>";

      try
      {
         m_parser.parse(new StringInput(sMessage), m_message);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.minPartCount", ex.getErrorCode());
         assertEquals("XMLAnyTest anyInterfaceElementComp before", ex.getErrorArgs()[0]);
      }
   }

   /**
    * Tests an any-type element-mapped part when its element is empty.
    */
   public void testParseMissingOptionalElement()
   {
      String sMessage;
      TransferObject tobj;

      // No content for anyType/string element
      sMessage = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<XMLAnyTest> <anyTypeElement></anyTypeElement> </XMLAnyTest>";

      tobj = m_parser.parse(new StringInput(sMessage), m_message);

      assertTrue(tobj.hasValue("anyTypeElement"));
      tobj = (TransferObject)tobj.getValue("anyTypeElement");
      assertNull(tobj);


      // No content for anyType/interface element
      sMessage = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<XMLAnyTest> <anyInterfaceElement></anyInterfaceElement> </XMLAnyTest>";

      tobj = m_parser.parse(new StringInput(sMessage), m_message);

      assertTrue(tobj.hasValue("anyInterfaceElement"));
      tobj = (TransferObject)tobj.getValue("anyInterfaceElement");
      assertNull(tobj);
   }

   /**
    * Tests a required any-type element-mapped part when its element is empty.
    */
   public void testParseMissingRequiredElement()
   {
      String sMessage;

      // No content for anyType/string element
      sMessage = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<XMLAnyTest> <requiredAnyTypeElement><anyTypeElement></anyTypeElement></requiredAnyTypeElement> </XMLAnyTest>";

      try
      {
         m_parser.parse(new StringInput(sMessage), m_message);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.minPartCount", ex.getErrorCode());
         assertEquals("XMLAnyTest requiredAnyTypeElement anyTypeElement", ex.getErrorArgs()[0]);
      }
   }

   /**
    * Tests an any-type value-mapped part when its element is empty.
    */
   public void testParseMissingOptionalValuePart()
   {
      List list;
      String sMessage;
      TransferObject tobj;

      // No content for anyType/string value part
      sMessage = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<XMLAnyTest> <anyTypeElementComp attribute=\"abc\"></anyTypeElementComp> </XMLAnyTest>";

      tobj = m_parser.parse(new StringInput(sMessage), m_message);

      tobj = (TransferObject)tobj.getValue("anyTypeElementComp");
      assertEquals("abc", tobj.getValue("attribute"));
      assertNull(tobj.getValue("value"));


      // No content for anyType/string value collection part
      sMessage = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<XMLAnyTest> <anyTypeElementCompValueCollection attribute=\"abc\"></anyTypeElementCompValueCollection> </XMLAnyTest>";

      tobj = m_parser.parse(new StringInput(sMessage), m_message);

      tobj = (TransferObject)tobj.getValue("anyTypeElementCompValueCollection");
      assertEquals("abc", tobj.getValue("attribute"));
      assertNull(tobj.getValue("value"));


      // No content for anyType/interface value part
      sMessage = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<XMLAnyTest> <anyInterfaceElementCompCollection attribute=\"abc\"></anyInterfaceElementCompCollection> </XMLAnyTest>";

      tobj = m_parser.parse(new StringInput(sMessage), m_message);

      list = (List)tobj.getValue("anyInterfaceElementCompCollection");
      assertEquals(1, list.size());
      tobj = (TransferObject)list.get(0);
      assertEquals("abc", tobj.getValue("attribute"));
      assertNull(tobj.getValue("value"));
   }

   /**
    * Tests a required any-type value-mapped part when its element is empty.
    */
   public void testParseMissingRequiredValuePart()
   {
      String sMessage;

      // No content for anyType/string value part
      sMessage = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<XMLAnyTest> <anyTypeElementCompReqValue attribute=\"abc\"></anyTypeElementCompReqValue> </XMLAnyTest>";

      try
      {
         m_parser.parse(new StringInput(sMessage), m_message);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.minPartCount", ex.getErrorCode());
         assertEquals("XMLAnyTest anyTypeElementCompReqValue value", ex.getErrorArgs()[0]);
      }


      // No content for anyType/interface value part
      sMessage = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
         "<XMLAnyTest> <anyInterfaceElementCompCollectionReqValue attribute=\"abc\"></anyInterfaceElementCompCollectionReqValue> </XMLAnyTest>";

      try
      {
         m_parser.parse(new StringInput(sMessage), m_message);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.minPartCount", ex.getErrorCode());
         assertEquals("XMLAnyTest anyInterfaceElementCompCollectionReqValue value", ex.getErrorArgs()[0]);
      }
   }

   /**
    * Formats a message that has an any-type part with an interface. Tests that a
    * suitable error is thrown when the message that is formatted is not in the
    * interface response table.
    */
   public void testFormatMessageNotInInterface()
   {
      TransferObject msg = new TransferObject(m_message.getName(), 2);
      TransferObject subMsg = new TransferObject("XML_PatientId", 2);
      TransferObject valueElement = new TransferObject(3);

      subMsg.setValue("type", "patient");
      subMsg.setValue("id", "the_id");
      msg.setValue("anyInterfaceElement", subMsg);
      msg.setValue("anyInterfaceElementComp", valueElement);
      valueElement.setValue("attribute", "Message disallowed");
      valueElement.setValue("value", subMsg);
      valueElement.setValue("before", "pre");

      StringWriter writer = new StringWriter();
      WriterOutput out = new WriterOutput(writer);

      try
      {
         m_formatter.format(msg, m_message, out);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         ex = (IntegrationException)ex.getCause();

         assertEquals("err.integration.xml.unknownMessage", ex.getErrorCode());
         assertEquals("XML_PatientId", ex.getErrorArgs()[0]);
         assertEquals("XMLAnyTest anyInterfaceElement", ex.getErrorArgs()[1]);
      }

      // Test a value part with message missing from interface
      msg.removeValue("anyInterfaceElement");

      try
      {
         m_formatter.format(msg, m_message, out);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         ex = (IntegrationException)ex.getCause();

         assertEquals("err.integration.xml.unknownMessage", ex.getErrorCode());
         assertEquals("XML_PatientId", ex.getErrorArgs()[0]);
         assertEquals("XMLAnyTest anyInterfaceElementComp value", ex.getErrorArgs()[1]);
      }
   }

   /**
    * Formats a message that has an any-type part with no interface. Tests that a
    * suitable error is thrown when the message that is formatted is not in the
    * repository.
    */
   public void testFormatMessageNotInRepository()
   {
      TransferObject msg = new TransferObject(m_message.getName(), 1);
      TransferObject subMsg = new TransferObject("This message not in repository", 1);

      subMsg.setValue("x", "y");
      msg.setValue("anyTypeElement", subMsg);

      StringWriter writer = new StringWriter();
      WriterOutput out = new WriterOutput(writer);

      try
      {
         m_formatter.format(msg, m_message, out);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException e1)
      {
         e1 = (IntegrationException)e1.getCause();

         assertEquals("err.integration.xml.unknownMessage", e1.getErrorCode());
         assertEquals("This message not in repository", e1.getErrorArgs()[0]);
         assertEquals("XMLAnyTest anyTypeElement", e1.getErrorArgs()[1]);

         MetadataLookupException e2 = (MetadataLookupException)e1.getCause();

         assertEquals("err.meta.messageLookup", e2.getErrorCode());
         assertEquals("This message not in repository", e2.getErrorArgs()[0]);
      }
   }
}
