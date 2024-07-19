// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.xml;

import java.io.StringWriter;
import java.math.BigDecimal;

import nexj.core.integration.IntegrationException;
import nexj.core.integration.MessageFormatter;
import nexj.core.integration.MessageParser;
import nexj.core.integration.io.StringInput;
import nexj.core.integration.io.WriterOutput;
import nexj.core.meta.Metadata;
import nexj.core.meta.MetadataException;
import nexj.core.meta.Primitive;
import nexj.core.meta.Repository;
import nexj.core.meta.integration.Format;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessageTable;
import nexj.core.meta.integration.format.xml.RootXMLMessagePartMapping;
import nexj.core.rpc.TransferObject;
import nexj.core.rpc.xml.XML;
import nexj.core.runtime.InvocationContext;
import nexj.core.util.Binary;
import nexj.core.util.XMLParserException;

import junit.framework.TestCase;

/**
 * Tests message inheritance support in the XML formatter and parser.
 */
public class XMLInheritanceTest extends TestCase
{
   // constants

   /**
    * The XML document header that begins every XML message.
    */
   protected final static String XML_HEADER = XML.HEADER;

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
    * Message metadata that refers to the parent.
    */
   protected Message m_referrerMessage;

   // operations

   public void setUp()
   {
      m_context = (InvocationContext)Repository.getMetadata().getComponent("System.InvocationContext").getInstance(null);

      Format format = m_context.getMetadata().getFormat("XML");

      m_parser = (MessageParser)format.getParser().getInstance(m_context);
      m_formatter = (MessageFormatter)format.getFormatter().getInstance(m_context);
      m_referrerMessage = m_context.getMetadata().getMessage("XML_Inherit_Parent_Referrer");
   }

   /**
    * Formats a reference with the parent message.
    */
   public void testFormatParent()
   {
      StringWriter writer = new StringWriter();
      WriterOutput out = new WriterOutput(writer);
      TransferObject root = new TransferObject("XML_Inherit_Parent_Referrer", 2);
      TransferObject polyRoot = new TransferObject(2);

      polyRoot.setClassName("XML_Inherit_Parent");
      polyRoot.setValue("a", "Avalue");
      polyRoot.setValue("b", Primitive.createInteger(17));

      root.setValue("name", "123");
      root.setValue("ref", polyRoot);
      m_formatter.format(root, m_referrerMessage, out);

      assertEquals(XML_HEADER +
         "<referrer xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<name>123</name>" +
         "<reference>" +
            "<a>Avalue</a>" +
            "<b>17</b>" +
         "</reference>" +
         "</referrer>", writer.toString());
   }

   /**
    * Parses a reference with the parent message.
    */
   public void testParseParent()
   {
      StringInput in = new StringInput(XML_HEADER +
         "<referrer xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<name>123</name>" +
         "<reference>" +
            "<a>Avalue</a>" +
            "<b>17</b>" +
         "</reference>" +
         "</referrer>");
      TransferObject root = m_parser.parse(in, m_referrerMessage);
      TransferObject ref;

      assertEquals("XML_Inherit_Parent_Referrer", root.getClassName());
      assertEquals("123", root.getValue("name"));
      ref = (TransferObject)root.getValue("ref");
      assertEquals("XML_Inherit_Parent", ref.getClassName());
      assertEquals("Avalue", ref.getValue("a"));
      assertEquals(Primitive.createInteger(17), ref.getValue("b"));
   }

   /**
    * Formats a reference with a child message.
    */
   public void testFormatChild1()
   {
      StringWriter writer = new StringWriter();
      WriterOutput out = new WriterOutput(writer);
      TransferObject root = new TransferObject("XML_Inherit_Parent_Referrer", 2);
      TransferObject polyRoot = new TransferObject(2);

      polyRoot.setClassName("XML_Inherit_Child1");
      polyRoot.setValue("b", Primitive.createInteger(23));
      polyRoot.setValue("c1", Primitive.createInteger(31));

      root.setValue("name", "123");
      root.setValue("ref", polyRoot);
      m_formatter.format(root, m_referrerMessage, out);

      assertEquals(XML_HEADER +
         "<referrer xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<name>123</name>" +
         "<reference xsi:type=\"Child1Type\">" +
            "<b>23</b>" +
            "<c1>31</c1>" +
         "</reference>" +
         "</referrer>", writer.toString());
   }

   /**
    * Parses a reference with a child message.
    */
   public void testParseChild1()
   {
      StringInput in = new StringInput(XML_HEADER +
         "<referrer xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<name>123</name>" +
         "<reference xsi:type=\"Child1Type\">" +
            "<b>23</b>" +
            "<c1>31</c1>" +
         "</reference>" +
         "</referrer>");
      TransferObject root = m_parser.parse(in, m_referrerMessage);
      TransferObject ref;

      assertEquals("XML_Inherit_Parent_Referrer", root.getClassName());
      assertEquals("123", root.getValue("name"));
      ref = (TransferObject)root.getValue("ref");
      assertEquals("XML_Inherit_Child1", ref.getClassName());
      assertEquals(Primitive.createInteger(23), ref.getValue("b"));
      assertEquals(Primitive.createInteger(31), ref.getValue("c1"));
   }

   /**
    * Formats a reference with a child's child message.
    */
   public void testFormatChild1Child()
   {
      StringWriter writer = new StringWriter();
      WriterOutput out = new WriterOutput(writer);
      TransferObject root = new TransferObject("XML_Inherit_Parent_Referrer", 2);
      TransferObject polyRoot = new TransferObject(3);

      polyRoot.setClassName("XML_Inherit_Child1_Child");
      polyRoot.setValue("b", Primitive.createInteger(23));
      polyRoot.setValue("c1", Primitive.createInteger(31));
      polyRoot.setValue("c1c", "C1Cvalue");

      root.setValue("name", "123");
      root.setValue("ref", polyRoot);
      m_formatter.format(root, m_referrerMessage, out);

      assertEquals(XML_HEADER +
         "<referrer xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<name>123</name>" +
         "<reference xsi:type=\"Child1ChildType\">" +
            "<b>23</b>" +
            "<c1>31</c1>" +
            "<c1c>C1Cvalue</c1c>" +
         "</reference>" +
         "</referrer>", writer.toString());
   }

   /**
    * Parses a reference with a child's child message.
    */
   public void testParseChild1Child()
   {
      StringInput in = new StringInput(XML_HEADER +
         "<referrer xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<name>123</name>" +
         "<reference xsi:type=\"Child1ChildType\">" +
            "<b>23</b>" +
            "<c1>31</c1>" +
            "<c1c>C1Cvalue</c1c>" +
         "</reference>" +
         "</referrer>");
      TransferObject root = m_parser.parse(in, m_referrerMessage);
      TransferObject ref;

      assertEquals("XML_Inherit_Parent_Referrer", root.getClassName());
      assertEquals("123", root.getValue("name"));
      ref = (TransferObject)root.getValue("ref");
      assertEquals("XML_Inherit_Child1_Child", ref.getClassName());
      assertEquals(Primitive.createInteger(23), ref.getValue("b"));
      assertEquals(Primitive.createInteger(31), ref.getValue("c1"));
      assertEquals("C1Cvalue", ref.getValue("c1c"));
   }

   /**
    * Formats a reference to the child message.
    * Reference is not polymorphic, so no xsi:type should be generated.
    */
   public void testFormatChild1_Child1Referrer()
   {
      Message msg = m_context.getMetadata().getMessage("XML_Inherit_Child1_Referrer");
      StringWriter writer = new StringWriter();
      WriterOutput out = new WriterOutput(writer);
      TransferObject root = new TransferObject("XML_Inherit_Child1_Referrer", 3);
      TransferObject polyRoot = new TransferObject(2);

      polyRoot.setClassName("XML_Inherit_Child1");
      polyRoot.setValue("b", Primitive.createInteger(23));
      polyRoot.setValue("c1", Primitive.createInteger(31));

      root.setValue("name", "123");
      root.setValue("ref", polyRoot);
      root.setValue("refType", "test");
      m_formatter.format(root, msg, out);

      assertEquals(XML_HEADER +
         "<childReferrer>" +
         "<name>123</name>" +
         "<child1>" +
            "<b>23</b>" +
            "<c1>31</c1>" +
         "</child1>" +
         "<refType>test</refType>" +
         "</childReferrer>", writer.toString());
   }

   /**
    * Parses a reference to the child message.
    */
   public void testParseChild1_Child1Referrer()
   {
      Message msg = m_context.getMetadata().getMessage("XML_Inherit_Child1_Referrer");
      StringInput in = new StringInput(XML_HEADER +
         "<childReferrer>" +
         "<name>123</name>" +
         "<child1>" +
            "<b>23</b>" +
            "<c1>31</c1>" +
         "</child1>" +
         "<refType>test</refType>" +
         "</childReferrer>");
      TransferObject root = m_parser.parse(in, msg);
      TransferObject ref;

      assertEquals("XML_Inherit_Child1_Referrer", root.getClassName());
      assertEquals("123", root.getValue("name"));
      ref = (TransferObject)root.getValue("ref");
      assertEquals("XML_Inherit_Child1", ref.getClassName());
      assertEquals(Primitive.createInteger(23), ref.getValue("b"));
      assertEquals(Primitive.createInteger(31), ref.getValue("c1"));
   }

   /**
    * Formats a message where the TO class is a sub-message of the message
    * passed to the formatter.
    * 
    * Formatter gets message: XML_Inherit_Parent_Reference
    * Formatter gets TO for: XML_Inherit_Child1_Referrer
    */
   public void testFormatPolymorphicRootMessage()
   {
      StringWriter writer = new StringWriter();
      WriterOutput out = new WriterOutput(writer);
      TransferObject root = new TransferObject("XML_Inherit_Child1_Referrer", 3);
      TransferObject polyRoot = new TransferObject(2);

      polyRoot.setClassName("XML_Inherit_Child1");
      polyRoot.setValue("b", Primitive.createInteger(23));
      polyRoot.setValue("c1", Primitive.createInteger(31));

      root.setValue("name", "123");
      root.setValue("ref", polyRoot);
      root.setValue("refType", "test");
      m_formatter.format(root, m_referrerMessage, out);

      assertEquals(XML_HEADER +
         "<referrer xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"ChildReferrerType\">" +
         "<name>123</name>" +
         "<child1>" +
            "<b>23</b>" +
            "<c1>31</c1>" +
         "</child1>" +
         "<refType>test</refType>" +
         "</referrer>", writer.toString());
   }

   /**
    * Parses a message with xsi:type specified on the root node.
    * 
    * Parser told to parse message: XML_Inherit_Parent_Reference
    * Parser given this message data: XML_Inherit_Child1_Referrer
    */
   public void testParsePolymorphicRootMessage()
   {
      StringInput in = new StringInput(XML_HEADER +
         "<referrer xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"ChildReferrerType\">" +
         "<name>123</name>" +
         "<child1>" +
            "<b>23</b>" +
            "<c1>31</c1>" +
         "</child1>" +
         "<refType>test</refType>" +
         "</referrer>");
      TransferObject root = m_parser.parse(in, m_referrerMessage);
      TransferObject ref;

      assertEquals("XML_Inherit_Child1_Referrer", root.getClassName());
      assertEquals("123", root.getValue("name"));
      ref = (TransferObject)root.getValue("ref");
      assertEquals("XML_Inherit_Child1", ref.getClassName());
      assertEquals(Primitive.createInteger(23), ref.getValue("b"));
      assertEquals(Primitive.createInteger(31), ref.getValue("c1"));
   }

   /**
    * Formats a reference to the child message with the child's child message.
    * Should fail because reference is not polymorphic.
    */
   public void testFormatChild1Child_Child1Referrer()
   {
      StringWriter writer = new StringWriter();
      WriterOutput out = new WriterOutput(writer);
      TransferObject root = new TransferObject("XML_Inherit_Child1_Referrer", 3);
      TransferObject polyRoot = new TransferObject(3);

      polyRoot.setClassName("XML_Inherit_Child1_Child");
      polyRoot.setValue("b", Primitive.createInteger(23));
      polyRoot.setValue("c1", Primitive.createInteger(31));
      polyRoot.setValue("c1c", "C1Cvalue");

      root.setValue("name", "123");
      root.setValue("ref", polyRoot);
      root.setValue("refType", "test");

      try
      {
         m_formatter.format(root, m_referrerMessage, out);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         IntegrationException cause = (IntegrationException)ex.getCause();

         assertEquals("err.integration.messageTypeMismatch", cause.getErrorCode());
         assertEquals("XML_Inherit_Child1_Referrer ref", cause.getErrorArgs()[0]);
         assertEquals("XML_Inherit_Child1", cause.getErrorArgs()[1]);
         assertEquals("XML_Inherit_Child1_Child", cause.getErrorArgs()[2]);
      }
   }

   /**
    * Parses a reference to the child message with the child's child message.
    * Should fail because reference is not polymorphic.
    */
   public void testParseChild1Child_Child1Referrer()
   {
      StringInput in = new StringInput(XML_HEADER +
         "<referrer xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"ChildReferrerType\">" +
         "<name>123</name>" +
         "<child1 xsi:type=\"Child1ChildType\">" + // Not allowed--not polymorphic
            "<b>23</b>" +
            "<c1>31</c1>" +
            "<c1c>C1Cvalue</c1c>" +
         "</child1>" +
         "<refType>type</refType>" +
         "</referrer>");

      try
      {
         m_parser.parse(in, m_referrerMessage);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.messageTypeMismatch", ex.getErrorCode());
         assertEquals("XML_Inherit_Child1_Referrer ref", ex.getErrorArgs()[0]);
         assertEquals("XML_Inherit_Child1", ex.getErrorArgs()[1]);
         assertEquals("XML_Inherit_Child1_Child", ex.getErrorArgs()[2]);
      }
   }

   /**
    * Tests directly formatting a non-polymorphic message with one of its sub-messages.
    */
   public void testFormatNonPolymorphicMessage()
   {
      Message msgChild1 = m_context.getMetadata().getMessage("XML_Inherit_Child1");
      StringWriter writer = new StringWriter();
      WriterOutput out = new WriterOutput(writer);
      TransferObject root = new TransferObject(3);

      root.setClassName("XML_Inherit_Child1_Child");
      root.setValue("b", Primitive.createInteger(23));
      root.setValue("c1", Primitive.createInteger(31));
      root.setValue("c1c", "C1Cvalue");

      try
      {
         m_formatter.format(root, msgChild1, out);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         IntegrationException cause = (IntegrationException)ex.getCause();

         assertEquals("err.integration.messageTypeMismatch", cause.getErrorCode());
         assertEquals("XML_Inherit_Child1", cause.getErrorArgs()[0]);
         assertEquals("XML_Inherit_Child1", cause.getErrorArgs()[1]);
         assertEquals("XML_Inherit_Child1_Child", cause.getErrorArgs()[2]);
      }
   }

   /**
    * Tests directly parsing to a non-polymorphic message one of its sub-messages.
    */
   public void testParseNonPolymorphicMessage()
   {
      Message msgChild1 = m_context.getMetadata().getMessage("XML_Inherit_Child1");
      StringInput in = new StringInput(XML_HEADER +
         "<child1 xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"Child1ChildType\">" +
            "<b>23</b>" +
            "<c1>31</c1>" +
            "<c1c>C1Cvalue</c1c>" +
         "</child1>");

      try
      {
         m_parser.parse(in, msgChild1);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.messageTypeMismatch", ex.getErrorCode());
         assertEquals("XML_Inherit_Child1_Child", ex.getErrorArgs()[0]);
         assertEquals("XML_Inherit_Child1", ex.getErrorArgs()[1]);
         assertEquals("XML_Inherit_Child1_Child", ex.getErrorArgs()[2]);
      }

      // Parse Child1 directly, this time passing Child1 data. Specify xsi:type (unnecessary).
      in = new StringInput(XML_HEADER +
         "<child1 xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"Child1Type\">" +
            "<b>23</b>" +
            "<c1>31</c1>" +
         "</child1>");

      TransferObject root = m_parser.parse(in, msgChild1);

      assertEquals("XML_Inherit_Child1", root.getClassName());
      assertEquals(23, ((Integer)root.getValue("b")).intValue());
      assertEquals(31, ((Integer)root.getValue("c1")).intValue());
   }

   /**
    * Formats a reference with a child message.
    */
   public void testFormatChild2()
   {
      StringWriter writer = new StringWriter();
      WriterOutput out = new WriterOutput(writer);
      TransferObject root = new TransferObject("XML_Inherit_Parent_Referrer", 2);
      TransferObject polyRoot = new TransferObject(2);

      polyRoot.setClassName("XML_Inherit_Child2");
      polyRoot.setValue("a", "Avalue");
      polyRoot.setValue("b", "Bvalue");

      root.setValue("name", "123");
      root.setValue("ref", polyRoot);
      m_formatter.format(root, m_referrerMessage, out);

      assertEquals(XML_HEADER +
         "<referrer xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<name>123</name>" +
         "<reference xsi:type=\"Child2Type\">" +
            "<a>Avalue</a>" +
            "<b>Bvalue</b>" +
         "</reference>" +
         "</referrer>", writer.toString());
   }

   /**
    * Parses a reference with a child message.
    */
   public void testParseChild2()
   {
      StringInput in = new StringInput(XML_HEADER +
         "<referrer xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<name>123</name>" +
         "<reference xsi:type=\"Child2Type\">" +
            "<a>Avalue</a>" +
            "<b>Bvalue</b>" +
         "</reference>" +
         "</referrer>");
      TransferObject root = m_parser.parse(in, m_referrerMessage);
      TransferObject ref;

      assertEquals("XML_Inherit_Parent_Referrer", root.getClassName());
      assertEquals("123", root.getValue("name"));
      ref = (TransferObject)root.getValue("ref");
      assertEquals("XML_Inherit_Child2", ref.getClassName());
      assertEquals("Avalue", ref.getValue("a"));
      assertEquals("Bvalue", ref.getValue("b"));
   }

   /**
    * Formats a reference with an unknown message.
    */
   public void testFormatUnknown()
   {
      StringWriter writer = new StringWriter();
      WriterOutput out = new WriterOutput(writer);
      TransferObject root = new TransferObject("XML_Inherit_Parent_Referrer", 2);
      TransferObject polyRoot = new TransferObject(2);

      polyRoot.setClassName("XML_Inherit_Unknown");
      polyRoot.setValue("a", "Avalue");
      polyRoot.setValue("b", Primitive.createInteger(17));

      root.setValue("name", "123");
      root.setValue("ref", polyRoot);

      try
      {
         m_formatter.format(root, m_referrerMessage, out);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         MetadataException cause = (MetadataException)ex.getCause();

         assertEquals("err.meta.messageLookup", cause.getErrorCode());
      }
   }

   /**
    * Parses a reference with an unknown message.
    */
   public void testParseUnknown()
   {
      StringInput in = new StringInput(XML_HEADER +
         "<referrer xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<name>123</name>" +
         "<reference xsi:type=\"UnknownType\">" +
            "<a>Avalue</a>" +
            "<b>Bvalue</b>" +
         "</reference>" +
         "</referrer>");

      try
      {
         m_parser.parse(in, m_referrerMessage);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.xml.unknownType", ex.getErrorCode());
         assertEquals("UnknownType", ex.getErrorArgs()[0]);
         assertEquals("XML_Inherit_Parent_Referrer ref", ex.getErrorArgs()[1]);
      }
   }

   /**
    * Tests formatting an anyType element with an interface.
    * 
    * Parent: root element is "parent"
    * Child1: inherits from "Parent", root element is "child1"
    * Child2: inherits from "Parent", root element is "child2"
    * Child3: inherits from "Parent", root element is "parent"
    */
   public void testFormatAnyTypeInterface()
   {
      Message message = m_context.getMetadata().getMessage("XMLAnyTest");
      TransferObject root = new TransferObject("XMLAnyTest", 1);
      TransferObject any = new TransferObject(2);
      StringWriter writer;
      WriterOutput out;

      root.setValue("anyInterfaceInheritanceElement", any);

      // Format child1
      any.setClassName("XML_Inherit_Child1");
      any.setValue("b", Primitive.createInteger(23));
      any.setValue("c1", Primitive.createInteger(31));

      writer = new StringWriter();
      out = new WriterOutput(writer);
      m_formatter.format(root, message, out);

      assertEquals(XML_HEADER +
         "<XMLAnyTest xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"" +
         " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
            "<anyInterfaceInheritanceElement>" +
               "<child1>" +
                  "<b>23</b>" +
                  "<c1>31</c1>" +
               "</child1>" +
            "</anyInterfaceInheritanceElement>" +
         "</XMLAnyTest>", writer.toString());

      // Format child2
      any.removeAllValues();
      any.setClassName("XML_Inherit_Child2");
      any.setValue("a", "Avalue");
      any.setValue("b", "Bvalue");

      writer = new StringWriter();
      out = new WriterOutput(writer);
      m_formatter.format(root, message, out);

      assertEquals(XML_HEADER +
         "<XMLAnyTest xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"" +
         " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
            "<anyInterfaceInheritanceElement>" +
               "<child2>" +
                  "<a>Avalue</a>" +
                  "<b>Bvalue</b>" +
               "</child2>" +
            "</anyInterfaceInheritanceElement>" +
         "</XMLAnyTest>", writer.toString());

      // Format parent
      any.removeAllValues();
      any.setClassName("XML_Inherit_Parent");
      any.setValue("a", "Avalue");
      any.setValue("b", Primitive.createInteger(11));

      writer = new StringWriter();
      out = new WriterOutput(writer);
      m_formatter.format(root, message, out);

      assertEquals(XML_HEADER +
         "<XMLAnyTest xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"" +
         " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
            "<anyInterfaceInheritanceElement>" +
               "<parent>" +
                  "<a>Avalue</a>" +
                  "<b>11</b>" +
               "</parent>" +
            "</anyInterfaceInheritanceElement>" +
         "</XMLAnyTest>", writer.toString());

      // Format parent/child3
      any.removeAllValues();
      any.setClassName("XML_Inherit_Child3");
      any.setValue("a", "Avalue");
      any.setValue("b", "Bvalue");

      writer = new StringWriter();
      out = new WriterOutput(writer);
      m_formatter.format(root, message, out);

      assertEquals(XML_HEADER +
         "<XMLAnyTest xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"" +
         " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
            "<anyInterfaceInheritanceElement>" +
               "<parent xsi:type=\"Child3Type\">" +
                  "<a>Avalue</a>" +
                  "<b>Bvalue</b>" +
               "</parent>" +
            "</anyInterfaceInheritanceElement>" +
         "</XMLAnyTest>", writer.toString());
   }

   /**
    * Tests parsing an anyType element with an interface.
    * 
    * Parent: root element is "parent"
    * Child1: inherits from "Parent", root element is "child1"
    * Child2: inherits from "Parent", root element is "child2"
    * Child3: inherits from "Parent", root element is "parent"
    */
   public void testParseAnyTypeInterface()
   {
      TransferObject root;
      Message message = m_context.getMetadata().getMessage("XMLAnyTest");

      // Parse child1
      root = m_parser.parse(new StringInput(
         "<XMLAnyTest>" +
            "<anyInterfaceInheritanceElement>" +
               "<child1>" +
                  "<b>23</b>" +
                  "<c1>31</c1>" +
               "</child1>" +
            "</anyInterfaceInheritanceElement>" +
         "</XMLAnyTest>"), message);

      assertEquals("XMLAnyTest", root.getClassName());
      root = (TransferObject)root.getValue("anyInterfaceInheritanceElement");
      assertEquals("XML_Inherit_Child1", root.getClassName());
      assertEquals(Primitive.createInteger(23), root.getValue("b"));
      assertEquals(Primitive.createInteger(31), root.getValue("c1"));


      // Parse child2
      root = m_parser.parse(new StringInput(
         "<XMLAnyTest>" +
            "<anyInterfaceInheritanceElement>" +
               "<child2>" +
                  "<a>Avalue</a>" +
                  "<b>Bvalue</b>" +
               "</child2>" +
            "</anyInterfaceInheritanceElement>" +
         "</XMLAnyTest>"), message);

      assertEquals("XMLAnyTest", root.getClassName());
      root = (TransferObject)root.getValue("anyInterfaceInheritanceElement");
      assertEquals("XML_Inherit_Child2", root.getClassName());
      assertEquals("Avalue", root.getValue("a"));
      assertEquals("Bvalue", root.getValue("b"));

      // Cannot parse child3 on its own. (it has same root element name as its parent)

      // Parse parent
      root = m_parser.parse(new StringInput(
         "<XMLAnyTest>" +
            "<anyInterfaceInheritanceElement>" +
               "<parent>" +
                  "<a>Avalue</a>" +
                  "<b>11</b>" +
               "</parent>" +
            "</anyInterfaceInheritanceElement>" +
         "</XMLAnyTest>"), message);

      assertEquals("XMLAnyTest", root.getClassName());
      root = (TransferObject)root.getValue("anyInterfaceInheritanceElement");
      assertEquals("XML_Inherit_Parent", root.getClassName());
      assertEquals("Avalue", root.getValue("a"));
      assertEquals(Primitive.createInteger(11), root.getValue("b"));


      // Parse parent/child1
      root = m_parser.parse(new StringInput(
         "<XMLAnyTest>" +
            "<anyInterfaceInheritanceElement>" +
               "<parent xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"Child1Type\">" +
                  "<b>23</b>" +
                  "<c1>31</c1>" +
               "</parent>" +
            "</anyInterfaceInheritanceElement>" +
         "</XMLAnyTest>"), message);

      assertEquals("XMLAnyTest", root.getClassName());
      root = (TransferObject)root.getValue("anyInterfaceInheritanceElement");
      assertEquals("XML_Inherit_Child1", root.getClassName());
      assertEquals(Primitive.createInteger(23), root.getValue("b"));
      assertEquals(Primitive.createInteger(31), root.getValue("c1"));


      // Parse parent/child2
      root = m_parser.parse(new StringInput(
         "<XMLAnyTest>" +
            "<anyInterfaceInheritanceElement>" +
               "<parent xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"Child2Type\">" +
                  "<a>Avalue</a>" +
                  "<b>Bvalue</b>" +
               "</parent>" +
            "</anyInterfaceInheritanceElement>" +
         "</XMLAnyTest>"), message);

      assertEquals("XMLAnyTest", root.getClassName());
      root = (TransferObject)root.getValue("anyInterfaceInheritanceElement");
      assertEquals("XML_Inherit_Child2", root.getClassName());
      assertEquals("Avalue", root.getValue("a"));
      assertEquals("Bvalue", root.getValue("b"));


      // Parse parent/child3
      root = m_parser.parse(new StringInput(
         "<XMLAnyTest>" +
            "<anyInterfaceInheritanceElement>" +
               "<parent xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"Child3Type\">" +
                  "<a>Avalue</a>" +
                  "<b>Bvalue</b>" +
               "</parent>" +
            "</anyInterfaceInheritanceElement>" +
         "</XMLAnyTest>"), message);

      assertEquals("XMLAnyTest", root.getClassName());
      root = (TransferObject)root.getValue("anyInterfaceInheritanceElement");
      assertEquals("XML_Inherit_Child3", root.getClassName());
      assertEquals("Avalue", root.getValue("a"));
      assertEquals("Bvalue", root.getValue("b"));
   }

   /**
    * Formats a reference with a message that is not a sub-message of the
    * message specified on the reference.
    */
   public void testFormatNonSubMessage()
   {
      StringWriter writer = new StringWriter();
      WriterOutput out = new WriterOutput(writer);
      TransferObject root = new TransferObject("XML_Inherit_Parent_Referrer", 2);
      TransferObject polyRoot = new TransferObject(2);

      polyRoot.setClassName("XML_Inherit_Parent_Parent");
      polyRoot.setValue("a", "Avalue");
      polyRoot.setValue("b", Primitive.createInteger(17));

      root.setValue("name", "123");
      root.setValue("ref", polyRoot);

      try
      {
         m_formatter.format(root, m_referrerMessage, out);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         IntegrationException cause = (IntegrationException)ex.getCause();

         assertEquals("err.integration.messageTypeMismatch", cause.getErrorCode());
         assertEquals("XML_Inherit_Parent_Referrer ref", cause.getErrorArgs()[0]);
         assertEquals("XML_Inherit_Parent", cause.getErrorArgs()[1]);
         assertEquals("XML_Inherit_Parent_Parent", cause.getErrorArgs()[2]);
      }


      // Format the pure message
      Message msgParent = m_context.getMetadata().getMessage("XML_Inherit_Parent");

      root = polyRoot;
      writer = new StringWriter();
      out = new WriterOutput(writer);

      try
      {
         m_formatter.format(root, msgParent, out);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         IntegrationException cause = (IntegrationException)ex.getCause();

         assertEquals("err.integration.messageTypeMismatch", cause.getErrorCode());
         assertEquals("XML_Inherit_Parent", cause.getErrorArgs()[0]);
         assertEquals("XML_Inherit_Parent", cause.getErrorArgs()[1]);
         assertEquals("XML_Inherit_Parent_Parent", cause.getErrorArgs()[2]);
      }

      // Format the abstract message
      msgParent = m_context.getMetadata().getMessage("XML_Inherit_Parent_Parent");
      writer = new StringWriter();
      out = new WriterOutput(writer);

      try
      {
         m_formatter.format(root, msgParent, out);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         IntegrationException cause = (IntegrationException)ex.getCause();

         assertEquals("err.integration.abstractMessage", cause.getErrorCode());
         assertEquals("XML_Inherit_Parent_Parent", cause.getErrorArgs()[0]);
         assertEquals("XML_Inherit_Parent_Parent", cause.getErrorArgs()[1]);
      }
   }

   /**
    * Parses a reference with a message that is not a sub-message of the
    * message specified on the reference.
    */
   public void testParseNonSubMessage()
   {
      StringInput in = new StringInput(XML_HEADER +
         "<referrer xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<name>123</name>" +
         "<reference xsi:type=\"ParentParentType\">" +
            "<a>Avalue</a>" +
            "<b>Bvalue</b>" +
         "</reference>" +
         "</referrer>");

      try
      {
         m_parser.parse(in, m_referrerMessage);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.messageTypeMismatch", ex.getErrorCode());
         assertEquals("XML_Inherit_Parent_Referrer ref", ex.getErrorArgs()[0]);
         assertEquals("XML_Inherit_Parent", ex.getErrorArgs()[1]);
         assertEquals("XML_Inherit_Parent_Parent", ex.getErrorArgs()[2]);
      }

      // Parse the abstract message
      in = new StringInput(XML_HEADER +
         "<parentparent>" +
            "<a>Avalue</a>" +
            "<b>Bvalue</b>" +
         "</parentparent>");

      try
      {
         m_parser.parse(in, m_context.getMetadata().getMessage("XML_Inherit_Parent_Parent"));
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.abstractMessage", ex.getErrorCode());
         assertEquals("XML_Inherit_Parent_Parent", ex.getErrorArgs()[0]);
         assertEquals("XML_Inherit_Parent_Parent", ex.getErrorArgs()[1]);
      }
   }

   /**
    * Tests that SOAP headers may be modified by sub-messages.
    */
   public void testFormatSOAPHeader()
   {
      Message parentMessage = m_context.getMetadata().getMessage("XML_Inherit_SOAP_Header_Parent");
      Message childMessage = m_context.getMetadata().getMessage("XML_Inherit_SOAP_Header_Child");
      StringWriter writer = new StringWriter();
      WriterOutput out = new WriterOutput(writer);
      TransferObject root = new TransferObject("XML_Inherit_SOAP_Header_Parent", 3);
      TransferObject headerRoot = new TransferObject(2);

      headerRoot.setValue("data", Primitive.createInteger(11));

      root.setValue("a", "test");
      root.setValue("header", headerRoot);
      m_formatter.format(root, parentMessage, out);

      assertEquals(XML_HEADER +
         "<soap:Envelope" +
            " xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\"" +
            " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<soap:Header>" +
            "<data>11</data>" +
         "</soap:Header>" +
         "<soap:Body>" +
            "<parent " +
               "xmlns:wscoor=\"http://docs.oasis-open.org/ws-tx/wscoor/2006/06\" " +
               "xmlns:wsa=\"http://schemas.xmlsoap.org/ws/2004/08/addressing\">" +
               "<a>test</a>" +
            "</parent>" +
         "</soap:Body>" +
         "</soap:Envelope>", writer.toString());


      // Format child TO using child Message metadata
      root.setClassName("XML_Inherit_SOAP_Header_Child");
      headerRoot.setValue("data2", "abc");
      root.setValue("b", "Bvalue");

      writer = new StringWriter();
      out = new WriterOutput(writer);
      m_formatter.format(root, childMessage, out);

      assertEquals(XML_HEADER +
         "<soap:Envelope" +
            " xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\"" +
            " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<soap:Header>" +
            "<data>11</data>" +
            "<data2>abc</data2>" +
         "</soap:Header>" +
         "<soap:Body>" +
            "<child>" +
               "<a>test</a>" +
               "<b>Bvalue</b>" +
            "</child>" +
         "</soap:Body>" +
         "</soap:Envelope>", writer.toString());


      // Format child TO using parent Message metadata
      root.setClassName("XML_Inherit_SOAP_Header_Child");
      headerRoot.setValue("data2", "abc");
      root.setValue("b", "Bvalue");

      writer = new StringWriter();
      out = new WriterOutput(writer);
      m_formatter.format(root, parentMessage, out);

      assertEquals(XML_HEADER +
         "<soap:Envelope" +
            " xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\"" +
            " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<soap:Header>" +
            "<data>11</data>" +
            "<data2>abc</data2>" +
         "</soap:Header>" +
         "<soap:Body>" +
            "<parent xsi:type=\"ChildType\">" +
               "<a>test</a>" +
               "<b>Bvalue</b>" +
            "</parent>" +
         "</soap:Body>" +
         "</soap:Envelope>", writer.toString());
   }

   /**
    * Tests that SOAP headers may be modified by sub-messages.
    */
   public void testParseSOAPHeader()
   {
      Message parentMessage = m_context.getMetadata().getMessage("XML_Inherit_SOAP_Header_Parent");
      Message childMessage = m_context.getMetadata().getMessage("XML_Inherit_SOAP_Header_Child");
      StringInput in = new StringInput(XML_HEADER +
         "<soap:Envelope" +
            " xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\"" +
            " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<soap:Header>" +
            "<data>11</data>" +
         "</soap:Header>" +
         "<soap:Body>" +
            "<parent>" +
               "<a>test</a>" +
            "</parent>" +
         "</soap:Body>" +
         "</soap:Envelope>");
      TransferObject root = m_parser.parse(in, parentMessage);
      TransferObject headerRoot;

      assertEquals("XML_Inherit_SOAP_Header_Parent", root.getClassName());
      assertEquals("test", root.getValue("a"));
      headerRoot = (TransferObject)root.getValue("header");
      assertEquals(Primitive.createInteger(11), headerRoot.getValue("data"));


      // Parse child data using child Metadata
      in = new StringInput(XML_HEADER +
         "<soap:Envelope" +
            " xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\"" +
            " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<soap:Header>" +
            "<data>11</data>" +
            "<data2>abc</data2>" +
         "</soap:Header>" +
         "<soap:Body>" +
            "<child>" +
               "<a>test</a>" +
               "<b>Bvalue</b>" +
            "</child>" +
         "</soap:Body>" +
         "</soap:Envelope>");
      root = m_parser.parse(in, childMessage);

      assertEquals("XML_Inherit_SOAP_Header_Child", root.getClassName());
      assertEquals("test", root.getValue("a"));
      assertEquals("Bvalue", root.getValue("b"));
      headerRoot = (TransferObject)root.getValue("header");
      assertEquals(Primitive.createInteger(11), headerRoot.getValue("data"));
      assertEquals("abc", headerRoot.getValue("data2"));


      // Parse child data using parent Metadata
      in = new StringInput(XML_HEADER +
         "<soap:Envelope" +
            " xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\"" +
            " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<soap:Header>" +
            "<data>13</data>" +
            "<data2>abc2</data2>" +
         "</soap:Header>" +
         "<soap:Body>" +
            "<parent xsi:type=\"ChildType\">" +
               "<a>test2</a>" +
               "<b>Bvalue2</b>" +
            "</parent>" +
         "</soap:Body>" +
         "</soap:Envelope>");
      root = m_parser.parse(in, parentMessage);

      assertEquals("XML_Inherit_SOAP_Header_Child", root.getClassName());
      assertEquals("test2", root.getValue("a"));
      assertEquals("Bvalue2", root.getValue("b"));
      headerRoot = (TransferObject)root.getValue("header");
      assertEquals(Primitive.createInteger(13), headerRoot.getValue("data"));
      assertEquals("abc2", headerRoot.getValue("data2"));
   }

   /**
    * Test that SOAP headers may be replaced in a sub-message by a reference.
    */
   public void testFormatSOAPHeaderReference()
   {
      Message parentMessage = m_context.getMetadata().getMessage("XML_Inherit_SOAP_Header_Parent");
      Message childMessage = m_context.getMetadata().getMessage("XML_Inherit_SOAP_Header_Child_Header_Ref");
      StringWriter writer = new StringWriter();
      WriterOutput out = new WriterOutput(writer);
      TransferObject root = new TransferObject("XML_Inherit_SOAP_Header_Parent", 3);
      TransferObject headerRoot = new TransferObject(2);
      TransferObject actionHeader = new TransferObject(3);

      actionHeader.setValue("role", "Hamlet");
      actionHeader.setValue("mustUnderstand", Boolean.TRUE);
      actionHeader.setValue("value", "Adieu, adieu, adieu; remember me!");
      headerRoot.setValue("action", actionHeader);
      root.setValue("a", "test");
      root.setValue("header", headerRoot);


      // Format child TO using child Message metadata
      root.setClassName("XML_Inherit_SOAP_Header_Child_Header_Ref");
      headerRoot.setValue("data", "abc");  // should not appear
      root.setValue("c", "Cvalue");

      writer = new StringWriter();
      out = new WriterOutput(writer);
      m_formatter.format(root, childMessage, out);

      assertEquals(XML_HEADER +
         "<soap:Envelope" +
            " xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\"" +
            " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<soap:Header>" +
            "<wsa:Action" +
               " xmlns:wscoor=\"http://docs.oasis-open.org/ws-tx/wscoor/2006/06\"" +
               " xmlns:wsa=\"http://schemas.xmlsoap.org/ws/2004/08/addressing\"" +
               " soap:role=\"Hamlet\" soap:mustUnderstand=\"1\">" +
               "Adieu, adieu, adieu; remember me!" +
            "</wsa:Action>" +
         "</soap:Header>" +
         "<soap:Body>" +
            "<child2" +
               " xmlns:wscoor=\"http://docs.oasis-open.org/ws-tx/wscoor/2006/06\"" +
               " xmlns:wsa=\"http://schemas.xmlsoap.org/ws/2004/08/addressing\"" +
               ">" +
               "<a>test</a>" +
               "<c>Cvalue</c>" +
            "</child2>" +
         "</soap:Body>" +
         "</soap:Envelope>", writer.toString());


      // Format child TO using parent Message metadata
      writer = new StringWriter();
      out = new WriterOutput(writer);
      m_formatter.format(root, parentMessage, out);

      assertEquals(XML_HEADER +
         "<soap:Envelope" +
            " xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\"" +
            " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<soap:Header>" +
            "<wsa:Action" +
               " xmlns:wscoor=\"http://docs.oasis-open.org/ws-tx/wscoor/2006/06\"" +
               " xmlns:wsa=\"http://schemas.xmlsoap.org/ws/2004/08/addressing\"" +
               " soap:role=\"Hamlet\" soap:mustUnderstand=\"1\">" +
               "Adieu, adieu, adieu; remember me!" +
            "</wsa:Action>" +
         "</soap:Header>" +
         "<soap:Body>" +
            "<parent" +
               " xmlns:wscoor=\"http://docs.oasis-open.org/ws-tx/wscoor/2006/06\"" +
               " xmlns:wsa=\"http://schemas.xmlsoap.org/ws/2004/08/addressing\"" +
               " xsi:type=\"ChildHeaderRefType\"" +
               ">" +
               "<a>test</a>" +
               "<c>Cvalue</c>" +
            "</parent>" +
         "</soap:Body>" +
         "</soap:Envelope>", writer.toString());
   }

   /**
    * Test that SOAP headers may be replaced in a sub-message by a reference.
    */
   public void testParseSOAPHeaderReference()
   {
      Message parentMessage = m_context.getMetadata().getMessage("XML_Inherit_SOAP_Header_Parent");
      Message childMessage = m_context.getMetadata().getMessage("XML_Inherit_SOAP_Header_Child_Header_Ref");
      StringInput in;
      TransferObject root;
      TransferObject headerRoot, actionHeader;

      // Parse child data using child Metadata
      in = new StringInput(XML_HEADER +
         "<soap:Envelope" +
         " xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\"" +
         " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<soap:Header>" +
            "<wsa:Action" +
               " xmlns:wscoor=\"http://docs.oasis-open.org/ws-tx/wscoor/2006/06\"" +
               " xmlns:wsa=\"http://schemas.xmlsoap.org/ws/2004/08/addressing\"" +
               " soap:role=\"Hamlet\" soap:mustUnderstand=\"1\">" +
               "Adieu, adieu, adieu; remember me!" +
            "</wsa:Action>" +
         "</soap:Header>" +
         "<soap:Body>" +
            "<child2" +
               " xmlns:wscoor=\"http://docs.oasis-open.org/ws-tx/wscoor/2006/06\"" +
               " xmlns:wsa=\"http://schemas.xmlsoap.org/ws/2004/08/addressing\"" +
               ">" +
               "<a>test</a>" +
               "<c>Cvalue</c>" +
            "</child2>" +
         "</soap:Body>" +
         "</soap:Envelope>");
      root = m_parser.parse(in, childMessage);

      assertEquals("XML_Inherit_SOAP_Header_Child_Header_Ref", root.getClassName());
      assertEquals("test", root.getValue("a"));
      assertEquals("Cvalue", root.getValue("c"));
      headerRoot = (TransferObject)root.getValue("header");
      actionHeader = (TransferObject)headerRoot.getValue("action");
      assertEquals("Hamlet", actionHeader.getValue("role"));
      assertEquals(Boolean.TRUE, actionHeader.getValue("mustUnderstand"));
      assertEquals("Adieu, adieu, adieu; remember me!", actionHeader.getValue("value"));

      // Parse child data using parent Metadata
      in = new StringInput(XML_HEADER +
         "<soap:Envelope" +
         " xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\"" +
         " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<soap:Header>" +
            "<wsa:Action" +
               " xmlns:wscoor=\"http://docs.oasis-open.org/ws-tx/wscoor/2006/06\"" +
               " xmlns:wsa=\"http://schemas.xmlsoap.org/ws/2004/08/addressing\"" +
               " soap:role=\"Hamlet2\" soap:mustUnderstand=\"1\">" +
               "Adieu, adieu, adieu; remember me!" +
            "</wsa:Action>" +
         "</soap:Header>" +
         "<soap:Body>" +
            "<parent" +
               " xmlns:wscoor=\"http://docs.oasis-open.org/ws-tx/wscoor/2006/06\"" +
               " xmlns:wsa=\"http://schemas.xmlsoap.org/ws/2004/08/addressing\"" +
               " xsi:type=\"ChildHeaderRefType\"" +
               ">" +
               "<a>test2</a>" +
               "<c>Cvalue2</c>" +
            "</parent>" +
         "</soap:Body>" +
         "</soap:Envelope>");
      root = m_parser.parse(in, parentMessage);

      assertEquals("XML_Inherit_SOAP_Header_Child_Header_Ref", root.getClassName());
      assertEquals("test2", root.getValue("a"));
      assertEquals("Cvalue2", root.getValue("c"));
      headerRoot = (TransferObject)root.getValue("header");
      actionHeader = (TransferObject)headerRoot.getValue("action");
      assertEquals("Hamlet2", actionHeader.getValue("role"));
      assertEquals(Boolean.TRUE, actionHeader.getValue("mustUnderstand"));
      assertEquals("Adieu, adieu, adieu; remember me!", actionHeader.getValue("value"));
   }

   /**
    * Test formatting a sub-message that doesn't override the SOAP headers.
    */
   public void testFormatSOAPHeaderSameHeader()
   {
      Message parentMessage = m_context.getMetadata().getMessage("XML_Inherit_SOAP_Header_Parent");
      Message childMessage = m_context.getMetadata().getMessage("XML_Inherit_SOAP_Header_Child_SameHeader");
      StringWriter writer = new StringWriter();
      WriterOutput out = new WriterOutput(writer);
      TransferObject root = new TransferObject("XML_Inherit_SOAP_Header_Child_SameHeader", 3);
      TransferObject headerRoot = new TransferObject(1);

      headerRoot.setValue("data", Primitive.createInteger(11));
      root.setValue("a", "test");
      root.setValue("header", headerRoot);
      root.setValue("d", "Dvalue");

      // Format child TO using child Message metadata
      writer = new StringWriter();
      out = new WriterOutput(writer);
      m_formatter.format(root, childMessage, out);

      assertEquals(XML_HEADER +
         "<soap:Envelope" +
            " xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\"" +
            " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<soap:Header>" +
            "<data>11</data>" +
         "</soap:Header>" +
         "<soap:Body>" +
            "<child3>" +
               "<a>test</a>" +
               "<d>Dvalue</d>" +
            "</child3>" +
         "</soap:Body>" +
         "</soap:Envelope>", writer.toString());


      // Format child TO using parent Message metadata
      writer = new StringWriter();
      out = new WriterOutput(writer);
      m_formatter.format(root, parentMessage, out);

      assertEquals(XML_HEADER +
         "<soap:Envelope" +
            " xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\"" +
            " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<soap:Header>" +
            "<data>11</data>" +
         "</soap:Header>" +
         "<soap:Body>" +
            "<parent xsi:type=\"ChildSameHeaderType\">" +
               "<a>test</a>" +
               "<d>Dvalue</d>" +
            "</parent>" +
         "</soap:Body>" +
         "</soap:Envelope>", writer.toString());
   }

   /**
    * Test parsing a sub-message that doesn't override the SOAP headers.
    */
   public void testParseSOAPHeaderSameHeader()
   {
      Message parentMessage = m_context.getMetadata().getMessage("XML_Inherit_SOAP_Header_Parent");
      Message childMessage = m_context.getMetadata().getMessage("XML_Inherit_SOAP_Header_Child_SameHeader");
      StringInput in;
      TransferObject root;
      TransferObject headerRoot;

      // Parse child data using child Metadata
      in = new StringInput(XML_HEADER +
         "<soap:Envelope" +
         " xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\"" +
         " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<soap:Header>" +
            "<data>11</data>" +
         "</soap:Header>" +
         "<soap:Body>" +
            "<child3>" +
               "<a>test</a>" +
               "<d>Dvalue</d>" +
            "</child3>" +
         "</soap:Body>" +
         "</soap:Envelope>");
      root = m_parser.parse(in, childMessage);

      assertEquals("XML_Inherit_SOAP_Header_Child_SameHeader", root.getClassName());
      assertEquals("test", root.getValue("a"));
      assertEquals("Dvalue", root.getValue("d"));
      headerRoot = (TransferObject)root.getValue("header");
      assertEquals(Primitive.createInteger(11), headerRoot.getValue("data"));

      // Parse child data using parent Metadata
      in = new StringInput(XML_HEADER +
         "<soap:Envelope" +
         " xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\"" +
         " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<soap:Header>" +
            "<data>7</data>" +
         "</soap:Header>" +
         "<soap:Body>" +
            "<parent xsi:type=\"ChildSameHeaderType\">" +
               "<a>test2</a>" +
               "<d>Dvalue2</d>" +
            "</parent>" +
         "</soap:Body>" +
         "</soap:Envelope>");
      root = m_parser.parse(in, parentMessage);

      assertEquals("XML_Inherit_SOAP_Header_Child_SameHeader", root.getClassName());
      assertEquals("test2", root.getValue("a"));
      assertEquals("Dvalue2", root.getValue("d"));
      headerRoot = (TransferObject)root.getValue("header");
      assertEquals(Primitive.createInteger(7), headerRoot.getValue("data"));
   }

   /**
    * Test formatting messages and sub-messages with value parts.
    */
   public void testFormatValueParts() throws Exception
   {
      StringWriter writer = new StringWriter();
      WriterOutput out = new WriterOutput(writer);
      TransferObject root = new TransferObject("XML_Inherit_Parent_Referrer", 2);
      TransferObject ref = new TransferObject(3);
      TransferObject anyMsg = new TransferObject(2);

      // Parent - hex value part
      ref.setClassName("XML_Inherit_Parent");
      ref.setValue("a", "Avalue");
      ref.setValue("b", Primitive.createInteger(17));
      ref.setValue("value", Binary.fromBase64("MTIz"));

      root.setValue("name", "123");
      root.setValue("ref", ref);
      m_formatter.format(root, m_referrerMessage, out);

      assertEquals(XML_HEADER +
         "<referrer xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<name>123</name>" +
         "<reference>" +
            "<a>Avalue</a>" +
            "<b>17</b>" +
            "313233" +
         "</reference>" +
         "</referrer>", writer.toString());

      // Child2 - base64 value part
      writer = new StringWriter();
      out = new WriterOutput(writer);
      ref.setClassName("XML_Inherit_Child2");
      ref.setValue("value", Binary.parse("313233"));
      ref.setValue("b", "Bvalue");
      m_formatter.format(root, m_referrerMessage, out);

      assertEquals(XML_HEADER +
         "<referrer xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<name>123</name>" +
         "<reference xsi:type=\"Child2Type\">" +
            "<a>Avalue</a>" +
            "<b>Bvalue</b>" +
            "MTIz" +
         "</reference>" +
         "</referrer>", writer.toString());

      // Child4 - xsd:anyType value part (with interface)
      writer = new StringWriter();
      out = new WriterOutput(writer);
      ref.setClassName("XML_Inherit_Child4");
      ref.setValue("value", anyMsg);
      ref.setValue("b", "Bvalue");
      anyMsg.setClassName("XML_InternationalPrice");
      anyMsg.setValue("currency", "CNY");
      anyMsg.setValue("price", new BigDecimal("8.00"));
      m_formatter.format(root, m_referrerMessage, out);

      assertEquals(XML_HEADER +
         "<referrer xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<name>123</name>" +
         "<reference xsi:type=\"Child4Type\">" +
            "<a>Avalue</a>" +
            "<b>Bvalue</b>" +
            "<internationalPrice currency=\"CNY\">8.00</internationalPrice>" +
         "</reference>" +
         "</referrer>", writer.toString());

      // Child4Child - xsd:anyType value part (without interface)
      writer = new StringWriter();
      out = new WriterOutput(writer);
      ref.setClassName("XML_Inherit_Child4_Child");
      ref.removeValue("a");
      ref.removeValue("b");
      ref.setValue("value", "<a>Avalue</a><b>Bvalue</b><internationalPrice currency=\"CNY\">8.00</internationalPrice>");
      m_formatter.format(root, m_referrerMessage, out);

      assertEquals(XML_HEADER +
         "<referrer xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<name>123</name>" +
         "<reference xsi:type=\"Child4ChildType\">" +
            "<a>Avalue</a>" +
            "<b>Bvalue</b>" +
            "<internationalPrice currency=\"CNY\">8.00</internationalPrice>" +
         "</reference>" +
         "</referrer>", writer.toString());
   }

   /**
    * Test parsing messages and sub-messages with value parts.
    */
   public void testParseValueParts() throws Exception
   {
      // Parent - hex value part
      StringInput in = new StringInput(XML_HEADER +
         "<referrer xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<name>123</name>" +
         "<reference>" +
            "<a>Avalue</a>" +
            "<b>17</b>" +
            "313233" +
         "</reference>" +
         "</referrer>");
      TransferObject root = m_parser.parse(in, m_referrerMessage);
      TransferObject ref;

      assertEquals("XML_Inherit_Parent_Referrer", root.getClassName());
      assertEquals("123", root.getValue("name"));
      ref = (TransferObject)root.getValue("ref");
      assertEquals("XML_Inherit_Parent", ref.getClassName());
      assertEquals("Avalue", ref.getValue("a"));
      assertEquals(Primitive.createInteger(17), ref.getValue("b"));
      assertEquals(Binary.fromBase64("MTIz"), ref.getValue("value"));

      // Child2 - base64 value part
      in = new StringInput(XML_HEADER +
         "<referrer xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<name>123</name>" +
         "<reference xsi:type=\"Child2Type\">" +
            "<a>Avalue</a>" +
            "<b>Bvalue</b>" +
            "MTIz" +
         "</reference>" +
         "</referrer>");
      root = m_parser.parse(in, m_referrerMessage);

      assertEquals("XML_Inherit_Parent_Referrer", root.getClassName());
      assertEquals("123", root.getValue("name"));
      ref = (TransferObject)root.getValue("ref");
      assertEquals("XML_Inherit_Child2", ref.getClassName());
      assertEquals("Avalue", ref.getValue("a"));
      assertEquals("Bvalue", ref.getValue("b"));
      assertEquals(Binary.parse("313233"), ref.getValue("value"));

      // Child4 - xsd:anyType value part (with interface)
      in = new StringInput(XML_HEADER +
         "<referrer xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<name>123</name>" +
         "<reference xsi:type=\"Child4Type\">" +
            "<a>Avalue</a>" +
            "<b>Bvalue</b>" +
            "<internationalPrice currency=\"CNY\">8.00</internationalPrice>" +
         "</reference>" +
         "</referrer>");
      root = m_parser.parse(in, m_referrerMessage);

      assertEquals("XML_Inherit_Parent_Referrer", root.getClassName());
      assertEquals("123", root.getValue("name"));
      ref = (TransferObject)root.getValue("ref");
      assertEquals("XML_Inherit_Child4", ref.getClassName());
      assertEquals("Avalue", ref.getValue("a"));
      assertEquals("Bvalue", ref.getValue("b"));
      ref = (TransferObject)ref.getValue("value");
      assertEquals("XML_InternationalPrice", ref.getClassName());
      assertEquals("CNY", ref.getValue("currency"));
      assertEquals(800, ((BigDecimal)ref.getValue("price")).multiply(new BigDecimal(100)).intValueExact());

      // Child4Child - xsd:anyType value part (without interface)
      in = new StringInput(XML_HEADER +
         "<referrer xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<name>123</name>" +
         "<reference xsi:type=\"Child4ChildType\">" +
            "<a>Avalue</a>" +
            "<b>Bvalue</b>" +
            "<internationalPrice currency=\"CNY\">8.00</internationalPrice>" +
         "</reference>" +
         "</referrer>");
      root = m_parser.parse(in, m_referrerMessage);

      assertEquals("XML_Inherit_Parent_Referrer", root.getClassName());
      assertEquals("123", root.getValue("name"));
      ref = (TransferObject)root.getValue("ref");
      assertEquals("XML_Inherit_Child4_Child", ref.getClassName());
      assertEquals("<a>Avalue</a><b>Bvalue</b><internationalPrice currency=\"CNY\">8.00</internationalPrice>", ref.getValue("value"));
   }

   /**
    * Tests parsing using a message table.
    * 
    * Parent: root element is "parent"
    * Child1: inherits from "Parent", root element is "child1"
    * Child2: inherits from "Parent", root element is "child2"
    * Child3: inherits from "Parent", root element is "parent"
    * 
    * Both Child3 and Parent cannot be in the message table together,
    * but they do not have to be: Parent can be in the table and xsi:type
    * can be used to "cast" it to Child3.
    */
   public void testParseMessageTableSiblings()
   {
      TransferObject root;
      Message msg1 = m_context.getMetadata().getMessage("XML_Inherit_Child1");
      Message msg2 = m_context.getMetadata().getMessage("XML_Inherit_Child2");
      Message msg3 = m_context.getMetadata().getMessage("XML_Inherit_Child3");
      Message msgp = m_context.getMetadata().getMessage("XML_Inherit_Parent");
      MessageTable table = new MessageTable();

      table.addMessage(msg1);
      table.addMessage(msg2);
      table.addMessage(msg3);
      table.addMessage(msgp);
      m_parser.initializeMessageTable(table);

      // Parse child1
      root = m_parser.parse(new StringInput(
         "<child1>" +
            "<b>23</b>" +
            "<c1>31</c1>" +
         "</child1>"), table);

      assertEquals("XML_Inherit_Child1", root.getClassName());
      assertEquals(Primitive.createInteger(23), root.getValue("b"));
      assertEquals(Primitive.createInteger(31), root.getValue("c1"));

      // Parse child2
      root = m_parser.parse(new StringInput(
         "<child2>" +
            "<a>Avalue</a>" +
            "<b>Bvalue</b>" +
         "</child2>"), table);

      assertEquals("XML_Inherit_Child2", root.getClassName());
      assertEquals("Avalue", root.getValue("a"));
      assertEquals("Bvalue", root.getValue("b"));

      // Cannot parse child3 on its own. (it has same root element name as its parent)

      // Parse parent
      root = m_parser.parse(new StringInput(
         "<parent>" +
            "<a>Avalue</a>" +
            "<b>11</b>" +
         "</parent>"), table);

      assertEquals("XML_Inherit_Parent", root.getClassName());
      assertEquals("Avalue", root.getValue("a"));
      assertEquals(Primitive.createInteger(11), root.getValue("b"));

      // Parse parent/child1
      root = m_parser.parse(new StringInput(
         "<parent xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"Child1Type\">" +
            "<b>23</b>" +
            "<c1>31</c1>" +
         "</parent>"), table);

      assertEquals("XML_Inherit_Child1", root.getClassName());
      assertEquals(Primitive.createInteger(23), root.getValue("b"));
      assertEquals(Primitive.createInteger(31), root.getValue("c1"));

      // Parse parent/child2
      root = m_parser.parse(new StringInput(
         "<parent xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"Child2Type\">" +
            "<a>Avalue</a>" +
            "<b>Bvalue</b>" +
         "</parent>"), table);

      assertEquals("XML_Inherit_Child2", root.getClassName());
      assertEquals("Avalue", root.getValue("a"));
      assertEquals("Bvalue", root.getValue("b"));

      // Parse parent/child3
      root = m_parser.parse(new StringInput(
         "<parent xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"Child3Type\">" +
            "<a>Avalue</a>" +
            "<b>Bvalue</b>" +
         "</parent>"), table);

      assertEquals("XML_Inherit_Child3", root.getClassName());
      assertEquals("Avalue", root.getValue("a"));
      assertEquals("Bvalue", root.getValue("b"));
   }

   /**
    * Tests parsing of a message that is missing a reference whose min count is 1.
    */
   public void testParseMissingReference()
   {
      StringInput in = new StringInput(XML_HEADER +
         "<referrer xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<name>123</name>" +
         "</referrer>");

      try
      {
         m_parser.parse(in, m_referrerMessage);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.minPartCount", ex.getErrorCode());
         assertEquals("XML_Inherit_Parent_Referrer ref", ex.getErrorArgs()[0]);
      }
   }

   /**
    * Test that parser enforces sequential aggregation on a message, and
    * that it allows random aggregation on that message's sub-message.
    */
   public void testParseParentUsesSequentialAggregation()
   {
      StringInput in = new StringInput(XML_HEADER +
         "<referrer xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<name>123</name>" +
         "<reference xsi:type=\"ParentType\">" +
            "<b>17</b>" +
            "<a>Avalue</a>" +
         "</reference>" +
         "</referrer>");

      try
      {
         m_parser.parse(in, m_referrerMessage);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.xml.invalidElement", ex.getErrorCode());
         assertEquals("a", ex.getErrorArgs()[1]);
      }

      // Test random aggregation on a sub-message
      in = new StringInput(XML_HEADER +
         "<referrer xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
         "<name>123</name>" +
         "<reference xsi:type=\"Child1Type\">" +
            "<c1>31</c1>" +
            "<b>23</b>" +
         "</reference>" +
         "</referrer>");
      TransferObject root = m_parser.parse(in, m_referrerMessage);
      TransferObject ref;

      assertEquals("XML_Inherit_Parent_Referrer", root.getClassName());
      assertEquals("123", root.getValue("name"));
      ref = (TransferObject)root.getValue("ref");
      assertEquals("XML_Inherit_Child1", ref.getClassName());
      assertEquals(Primitive.createInteger(23), ref.getValue("b"));
      assertEquals(Primitive.createInteger(31), ref.getValue("c1"));
   }

   /**
    * Tests that changing the document root element name to the name
    * of a sub-message element is not sufficient for polymorphism: an
    * xsi:type must be supplied.
    * 
    * This test exists to enforce the message inheritance specification.
    */
   public void testParseDocRootNameDoesNotFunctionLikeXSIType()
   {
      Message msgChild1 = m_context.getMetadata().getMessage("XML_Inherit_Child1");
      Message msgParent = m_context.getMetadata().getMessage("XML_Inherit_Parent");
      StringInput in;

      // Child1 is not polymorphic.
      in = new StringInput(XML_HEADER +
         "<child1child>" +
            "<b>23</b>" +
            "<c1>31</c1>" +
            "<c1c>C1Cvalue</c1c>" +
         "</child1child>");

      try
      {
         m_parser.parse(in, msgChild1);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.xml.invalidDocRoot", ex.getErrorCode());
         assertEquals("child1child", ex.getErrorArgs()[1]);
      }

      // Parent is polymorphic--but still xsi:type is needed.
      in = new StringInput(XML_HEADER +
         "<child1>" +
            "<b>23</b>" +
            "<c1>31</c1>" +
         "</child1>");

      try
      {
         m_parser.parse(in, msgParent);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         assertEquals("err.integration.xml.invalidDocRoot", ex.getErrorCode());
         assertEquals("child1", ex.getErrorArgs()[1]);
      }
   }

   /**
    * Tests parsing of messages with schemas and inheritance relationships.
    */
   public void testParseSchemaInherited()
   {
      Message msg1 = m_context.getMetadata().getMessage("XML_Inherit_Schema1");
      Message msg2 = m_context.getMetadata().getMessage("XML_Inherit_Schema2");
      Message msg3 = m_context.getMetadata().getMessage("XML_Inherit_Schema3");
      StringInput in;
      TransferObject root;

      // Valid root message
      in = new StringInput(
         "<ns:schema1" +
            " xmlns:ns=\"urn:com.nexjsystems:ns:test:inherit:schema1\"" +
            " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
            "<a>Hi</a>" +
         "</ns:schema1>");

      root = m_parser.parse(in, msg1);
      assertEquals("XML_Inherit_Schema1", root.getClassName());
      assertEquals("Hi", root.getValue("a"));

      // Invalid root message
      in = new StringInput(
         "<ns:schema1" +
            " xmlns:ns=\"urn:com.nexjsystems:ns:test:inherit:schema1\"" +
            " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
            "<a>Hi</a>" +
            "<b>there!</b>" +
         "</ns:schema1>");

      try
      {
         m_parser.parse(in, msg1);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
      }

      // Valid sub-message (parsed directly)
      in = new StringInput(
         "<ns:schema2" +
            " xmlns:ns=\"urn:com.nexjsystems:ns:test:inherit:schema1\"" +
            " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
            "<a>Hi</a>" +
            "<b>there!</b>" +
         "</ns:schema2>");

      root = m_parser.parse(in, msg2);
      assertEquals("XML_Inherit_Schema2", root.getClassName());
      assertEquals("Hi", root.getValue("a"));
      assertEquals("there!", root.getValue("b"));

      // Invalid sub-message (parsed directly)
      in = new StringInput(
         "<ns:schema2" +
            " xmlns:ns=\"urn:com.nexjsystems:ns:test:inherit:schema1\"" +
            " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
            "<a>Hi</a>" +
            "<b>there!</b>" +
            "<c>bye</c>" +
         "</ns:schema2>");

      try
      {
         m_parser.parse(in, msg2);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         XMLParserException cause = (XMLParserException)ex.getCause();

         assertEquals("err.xml.parser", cause.getErrorCode());
      }

      // Valid sub-message (parsed through inheritance)
      in = new StringInput(
         "<ns:schema1 xsi:type=\"ns:Schema2Type\"" +
            " xmlns:ns=\"urn:com.nexjsystems:ns:test:inherit:schema1\"" +
            " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
            "<a>2Hi</a>" +
            "<b>2there!</b>" +
         "</ns:schema1>");

      root = m_parser.parse(in, msg1);
      assertEquals("XML_Inherit_Schema2", root.getClassName());
      assertEquals("2Hi", root.getValue("a"));
      assertEquals("2there!", root.getValue("b"));

      // Invalid sub-message (parsed through inheritance)
      in = new StringInput(
         "<ns:schema1 xsi:type=\"ns:Schema2Type\"" +
            " xmlns:ns=\"urn:com.nexjsystems:ns:test:inherit:schema1\"" +
            " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
            "<a>Hi</a>" +
            "<b>there!</b>" +
            "<c>bye</c>" +
         "</ns:schema1>");

      try
      {
         m_parser.parse(in, msg1);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         XMLParserException cause = (XMLParserException)ex.getCause();

         assertEquals("err.xml.parser", cause.getErrorCode());
      }

      // Valid sub-sub-message in different schema (direct)
      in = new StringInput(
         "<ns3:schema3" +
            " xmlns:ns=\"urn:com.nexjsystems:ns:test:inherit:schema1\"" +
            " xmlns:ns3=\"urn:com.nexjsystems:ns:test:inherit:schema3\"" +
            " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
            "<a>3Hi</a>" +
            "<b>3there!</b>" +
            "<cReal>bye</cReal>" +
         "</ns3:schema3>");

      root = m_parser.parse(in, msg3);
      assertEquals("XML_Inherit_Schema3", root.getClassName());
      assertEquals("3Hi", root.getValue("a"));
      assertEquals("3there!", root.getValue("b"));
      assertEquals("bye", root.getValue("cReal"));

      // Invalid sub-sub-message in different schema (direct)
      in = new StringInput(
         "<ns3:schema3" +
            " xmlns:ns=\"urn:com.nexjsystems:ns:test:inherit:schema1\"" +
            " xmlns:ns3=\"urn:com.nexjsystems:ns:test:inherit:schema3\"" +
            " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
            "<a>3Hi</a>" +
            "<b>3there!</b>" +
            "<cReal>bye</cReal>" +
            "<x>bad</x>" +
         "</ns3:schema3>");

      try
      {
         m_parser.parse(in, msg3);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         XMLParserException cause = (XMLParserException)ex.getCause();

         assertEquals("err.xml.parser", cause.getErrorCode());
      }

      // Valid sub-sub-message in different schema (inheritance)
      in = new StringInput(
         "<ns:schema1 xsi:type=\"ns3:Schema3Type\"" +
            " xmlns:ns=\"urn:com.nexjsystems:ns:test:inherit:schema1\"" +
            " xmlns:ns3=\"urn:com.nexjsystems:ns:test:inherit:schema3\"" +
            " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
            "<a>4Hi</a>" +
            "<b>4there!</b>" +
            "<cReal>2bye</cReal>" +
         "</ns:schema1>");

      root = m_parser.parse(in, msg1);
      assertEquals("XML_Inherit_Schema3", root.getClassName());
      assertEquals("4Hi", root.getValue("a"));
      assertEquals("4there!", root.getValue("b"));
      assertEquals("2bye", root.getValue("cReal"));

      // Invalid sub-sub-message in different schema (inheritance)
      in = new StringInput(
         "<ns:schema1 xsi:type=\"ns3:Schema3Type\"" +
            " xmlns:ns=\"urn:com.nexjsystems:ns:test:inherit:schema1\"" +
            " xmlns:ns3=\"urn:com.nexjsystems:ns:test:inherit:schema3\"" +
            " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
            "<a>4Hi</a>" +
            "<b>4there!</b>" +
            "<cReal>2bye</cReal>" +
            "<x>bad</x>" +
         "</ns:schema1>");

      try
      {
         m_parser.parse(in, msg1);
         fail("Expected IntegrationException");
      }
      catch (IntegrationException ex)
      {
         XMLParserException cause = (XMLParserException)ex.getCause();

         assertEquals("err.xml.parser", cause.getErrorCode());
      }
   }

   /**
    * Tests formatting of messages with schemas and inheritance relationships.
    */
   public void testFormatSchemaInherited()
   {
      Message msg1 = m_context.getMetadata().getMessage("XML_Inherit_Schema1");
      StringWriter writer;
      WriterOutput out;
      TransferObject root;

      // Format sub-message
      root = new TransferObject("XML_Inherit_Schema2", 2);
      root.setValue("a", "aValue");
      root.setValue("b", "bValue");
      out = new WriterOutput(writer = new StringWriter());
      m_formatter.format(root, msg1, out);

      assertEquals(XML_HEADER +
         "<is1:schema1" +
            " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"" +
            " xmlns:is1=\"urn:com.nexjsystems:ns:test:inherit:schema1\"" +
            " xsi:schemaLocation=\"urn:com.nexjsystems:ns:test:inherit:schema1 http://www.nexjsystems.com/ns/test/inherit/schema1.xsd\"" +
            " xsi:type=\"is1:Schema2Type\">" +
            "<a>aValue</a>" +
            "<b>bValue</b>" +
         "</is1:schema1>", writer.toString());

      // Format sub-sub-message
      root = new TransferObject("XML_Inherit_Schema3", 3);
      root.setValue("a", "aValue");
      root.setValue("b", "bValue");
      root.setValue("cReal", "cValue");
      out = new WriterOutput(writer = new StringWriter());
      m_formatter.format(root, msg1, out);

      assertEquals(XML_HEADER +
         "<is1:schema1" +
            " xmlns:is3=\"urn:com.nexjsystems:ns:test:inherit:schema3\"" +
            " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"" +
            " xmlns:is1=\"urn:com.nexjsystems:ns:test:inherit:schema1\"" +
            " xsi:schemaLocation=\"urn:com.nexjsystems:ns:test:inherit:schema3" +
               " http://www.nexjsystems.com/ns/test/inherit/schema3.xsd" +
               " urn:com.nexjsystems:ns:test:inherit:schema1" +
               " http://www.nexjsystems.com/ns/test/inherit/schema1.xsd\"" +
            " xsi:type=\"is3:Schema3Type\">" +
            "<a>aValue</a>" +
            "<b>bValue</b>" +
            "<cReal>cValue</cReal>" +
         "</is1:schema1>", writer.toString());
   }

   /**
    * Tests for namespaces being copied according to the following (recursive) rules:
    * 1. From referent to referrer.
    * 2. From derived to base if base.derivation is not final.
    * 3. From base to all derived.
    */
   public void testNamespaceCopying()
   {
      Metadata metadata = m_context.getMetadata();

      ((RootXMLMessagePartMapping)(metadata.getMessage(
            "XML_Namespace_Referrer").getRoot().getMapping())).getNamespace("xsd");
      ((RootXMLMessagePartMapping)(metadata.getMessage(
            "XML_Namespace_Derived_Referent").getRoot().getMapping())).getNamespace("xsd");
      ((RootXMLMessagePartMapping)(metadata.getMessage(
            "XML_Namespace_Derived_Referrer").getRoot().getMapping())).getNamespace("xsd");
      ((RootXMLMessagePartMapping)(metadata.getMessage(
            "XML_Namespace_Derived_Derived").getRoot().getMapping())).getNamespace("xsd");
      ((RootXMLMessagePartMapping)(metadata.getMessage(
            "XML_Namespace_Base_Referent").getRoot().getMapping())).getNamespace("xsd");
      ((RootXMLMessagePartMapping)(metadata.getMessage(
            "XML_Namespace_Base_Referrer").getRoot().getMapping())).getNamespace("xsd");
      ((RootXMLMessagePartMapping)(metadata.getMessage(
            "XML_Namespace_Base_Base").getRoot().getMapping())).getNamespace("xsd");
   }
}
