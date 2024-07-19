// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.xml;

import java.io.StringReader;
import java.io.StringWriter;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import junit.framework.TestCase;

import org.w3c.dom.Element;

import nexj.core.meta.MetadataLookupException;
import nexj.core.meta.Primitive;
import nexj.core.meta.Repository;
import nexj.core.meta.integration.CompositeMessagePart;
import nexj.core.meta.integration.CompositeMessagePartInstance;
import nexj.core.meta.integration.CompositeMessagePartRef;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessagePart;
import nexj.core.meta.integration.MessagePartMapping;
import nexj.core.meta.integration.PrimitiveMessagePart;
import nexj.core.meta.integration.format.xml.XSDMessageImporter;
import nexj.core.meta.xml.XMLMetadataExporter;
import nexj.core.util.XMLUtil;

public class XSDMessageImporterTest extends TestCase
{
   private Map m_messageRegistry;
   
   public void testDuplicateNames() throws Exception
   {
      Map msgRegistry = getMessageRegistry(XSDMessageImporterTest.class.getResource("duplicateelements.xsd"));

      Message msg = (Message)msgRegistry.get("validTime");
      StringWriter writer = new StringWriter();
      XMLMetadataExporter exporter = new XMLMetadataExporter(writer);
      exporter.exportMessage(msg);

      // Check result
      assertEquals("validTime", msg.getName());

      CompositeMessagePart comp = msg.getRoot();

      assertTrue(comp.getDescription().startsWith("An interval of time specifying"));
      assertEquals(CompositeMessagePart.RANDOM, comp.getAggregation());
      assertEquals(4, comp.getPartCount());

      assertEquals("low", comp.getPart(0).getName());
      assertEquals(Primitive.ANY_ORDINAL, ((PrimitiveMessagePart)comp.getPart(0)).getType().getOrdinal());

      assertEquals("width", comp.getPart(1).getName());
      assertEquals(Primitive.ANY_ORDINAL, ((PrimitiveMessagePart)comp.getPart(1)).getType().getOrdinal());

      assertEquals("high", comp.getPart(2).getName());
      assertEquals(Primitive.ANY_ORDINAL, ((PrimitiveMessagePart)comp.getPart(2)).getType().getOrdinal());

      assertEquals("center", comp.getPart(3).getName());
      assertEquals(Primitive.ANY_ORDINAL, ((PrimitiveMessagePart)comp.getPart(3)).getType().getOrdinal());
   }

   public void testComplexType() throws Exception
   {
      Map msgRegistry = getMessageRegistry(XSDMessageImporterTest.class.getResource("complextype.xsd"));
      Message msg = (Message)msgRegistry.get("validTime");
      StringWriter writer = new StringWriter();
      XMLMetadataExporter exporter = new XMLMetadataExporter(writer);
      exporter.exportMessage(msg);

      // Check result
      assertEquals("validTime", msg.getName());

      CompositeMessagePart comp = msg.getRoot();

      assertEquals(1, comp.getPartCount());
      comp = (CompositeMessagePart)comp.getPart(0);
      assertEquals("elemwithtypereferral", comp.getName());
      assertTrue(comp instanceof CompositeMessagePartRef);
      comp = ((CompositeMessagePartRef)comp).getRefPart();

      msg = (Message)msgRegistry.get("IVL_TS");
      assertNotNull(msg);
      assertSame(comp, msg.getRoot());
      assertEquals(1, comp.getPartCount());
      assertEquals("low", comp.getPart(0).getName());
      assertEquals(Primitive.ANY_ORDINAL, ((PrimitiveMessagePart)comp.getPart(0)).getType().getOrdinal());

      // Test export
      writer = new StringWriter();
      exporter = new XMLMetadataExporter(writer);
      exporter.exportMessage(msg);
   }

   public void testDuplicateRootElements() throws Exception
   {
      Map msgRegistry = getMessageRegistry(XSDMessageImporterTest.class.getResource("duplicaterootelements.xsd"));

      Message msg = (Message)msgRegistry.get("PostalAddress");
      StringWriter writer = new StringWriter();
      XMLMetadataExporter exporter = new XMLMetadataExporter(writer);
      exporter.exportMessage(msg);

      Element root = XMLUtil.parse(new StringReader(writer.toString())).getDocumentElement();
      assertEquals("Address", root.getAttribute("ref"));

      // Check result
      CompositeMessagePart comp = msg.getRoot();

      assertTrue(comp instanceof CompositeMessagePartRef);

      msg = (Message)msgRegistry.get("Address");
      assertNotNull(msg);

      comp = msg.getRoot();
      assertTrue(comp instanceof CompositeMessagePartInstance);
      assertEquals(1, comp.getPartCount());
      assertEquals("addr", comp.getPart(0).getName());
      assertTrue(comp.getPart(0) instanceof CompositeMessagePartRef);
      assertSame(comp, ((CompositeMessagePartRef)comp.getPart(0)).getRefPart());


      // Test export
      writer = new StringWriter();
      exporter = new XMLMetadataExporter(writer);
      exporter.exportMessage(msg);

      //System.err.println(XMLUtil.formatXML(writer.toString()));
   }

   public void testChoiceElement() throws Exception
   {
      Map msgRegistry = getMessageRegistry(XSDMessageImporterTest.class.getResource("choiceelement.xsd"));
      Message msg = (Message)msgRegistry.get("account");
      
      assertEquals(msg.getRoot().getAggregation(), CompositeMessagePart.SINGLE);
      msg = (Message)msgRegistry.get("aaaParent");
      
      CompositeMessagePart cPart = (CompositeMessagePart)msg.getRoot().getPart("aaaChild");
      
      assertEquals(3, cPart.getPart("areaCode").getMinCount());
      assertTrue(msg.getRoot().getAggregation() != CompositeMessagePart.SINGLE);
      msg = (Message)msgRegistry.get("phoneNumberA");
      assertTrue(msg.getRoot().getAggregation() != CompositeMessagePart.SINGLE);

      MessagePart p = msg.getRoot().getPart("phoneNumber");
      
      assertEquals(0, p.getMinCount());
      p = msg.getRoot().getPart("areaCode");
      assertEquals(0, p.getMinCount());
      p = msg.getRoot().getPart("number");
      assertEquals(0, p.getMinCount());

      msg = (Message)msgRegistry.get("sequencechoice");
      p = msg.getRoot().getPart("externalAct");
      assertEquals(0, p.getMinCount());

      msg = (Message)msgRegistry.get("unboundedChoice");
      p = msg.getRoot().getPart("th");
      assertEquals(0, p.getMinCount());

      msg = (Message)msgRegistry.get("wierdBoolean");
      p = msg.getRoot().getPart("ba");
      assertEquals(((XMLMessagePartMapping)p.getMapping()).getFormat(), "true;false");
   }

   public void testTable() throws Exception
   {
      Message tableMessage = getMessage("Table");     
      StringWriter writer = new StringWriter();
      XMLMetadataExporter exporter = new XMLMetadataExporter(writer);
      exporter.exportMessage(tableMessage);
      
      assertEquals(tableMessage.getRoot().getPartCount(), 2);
      CompositeMessagePart rowPart = (CompositeMessagePart)tableMessage.getRoot().getPart("Row");
      PrimitiveMessagePart namePart = (PrimitiveMessagePart)rowPart.getPart("name");
      assertEquals(namePart.getType(), Primitive.STRING);
      PrimitiveMessagePart timePart = (PrimitiveMessagePart)rowPart.getPart("time");
      assertEquals(timePart.getType(), Primitive.LONG);
      PrimitiveMessagePart booleanPart = (PrimitiveMessagePart)rowPart.getPart("required");
      assertEquals(booleanPart.getType(), Primitive.BOOLEAN);
      PrimitiveMessagePart enumPart = (PrimitiveMessagePart)rowPart.getPart("type");
      assertEquals(enumPart.getType(), Primitive.STRING);
      assertEquals( enumPart.getEnumerationCount(), 10);

      //System.err.println(XMLUtil.formatXML(writer.toString()));
      
      Message recursiveElement = getMessage("recursiveElement");
      
      MessagePart elem2 = recursiveElement.getRoot().getPart("recursiveElement2");
      
      assertTrue(elem2 instanceof CompositeMessagePartRef);
      
      CompositeMessagePartRef msgRef = (CompositeMessagePartRef)elem2;
      
      assertEquals("recursiveElement2", msgRef.getName());
      assertEquals(msgRef.getRefPart(), getMessage("recursiveElement2").getRoot());
      
      writer = new StringWriter();
      exporter = new XMLMetadataExporter(writer);
      exporter.exportMessage(recursiveElement);
   }

   public void testImportInclude() throws Exception
   {
      Map msgRegistry = getMessageRegistry(XSDMessageImporterTest.class.getResource("include.wsdl"));
      
      Message msg1 = (Message)msgRegistry.get("element1");
      Message msg2 = (Message)msgRegistry.get("element2");
      
      msg1.getRoot().getPart("attribute1");
      msg2.getRoot().getPart("attribute2");
   }
   
   public void testMultipleSchemaCase1() throws Exception
   {
      Map msgRegistry = getMessageRegistry(XSDMessageImporterTest.class.getResource("multipleschema1.wsdl"));

      Message msgA = (Message)msgRegistry.get("elementA");
      
      msgA.getRoot().getPart("booleanA");
      msgA.getRoot().getPart("stringA");
      
      Message msgB = (Message)msgRegistry.get("elementB");
      
      msgB.getRoot().getPart("booleanB");
      msgB.getRoot().getPart("stringB");
   }
   
   public void testMultipleSchemaCase2() throws Exception
   {
      Map msgRegistry = getMessageRegistry(XSDMessageImporterTest.class.getResource("multipleschema2.wsdl"));
      
      Message msgA = (Message)msgRegistry.get("elementA");
      
      msgA.getRoot().getPart("booleanA");
      msgA.getRoot().getPart("stringA");
      
      Message msgB = (Message)msgRegistry.get("elementB");
      
      assertTrue(msgB.getRoot() instanceof CompositeMessagePartRef);
      assertEquals(((CompositeMessagePartRef)msgB.getRoot()).getRefPart().getName(), "elementA");
   }
   
   public void testMultipleSchemaCase3() throws Exception
   {
      Map msgRegistry = getMessageRegistry(XSDMessageImporterTest.class.getResource("multipleschema3.wsdl"));
      
      Message msgA = (Message)msgRegistry.get("elementA");
      
      msgA.getRoot().getPart("booleanA");
      msgA.getRoot().getPart("stringA");
      
      Message msgB = (Message)msgRegistry.get("elementB");
      MessagePart childB = msgB.getRoot().getPart("childB");
      
      assertTrue(childB instanceof CompositeMessagePartRef);
      assertEquals(((CompositeMessagePartRef)childB).getRefPart().getName(), "elementA");
   }
   
   public void testMultipleSchemaCase4() throws Exception
   {
      Map msgRegistry = getMessageRegistry(XSDMessageImporterTest.class.getResource("multipleschema4.wsdl"));
      
      Message codeType = (Message)msgRegistry.get("CodeType");
      
      codeType.getRoot().getPart("code");
      codeType.getRoot().getPart("description");
      
      Message msgA = (Message)msgRegistry.get("elementA");
      
      MessagePart codeA = msgA.getRoot().getPart("codeA");
      
      assertTrue(codeA instanceof CompositeMessagePartRef);
      assertEquals(((CompositeMessagePartRef)codeA).getRefPart().getName(), "CodeType");
      
      msgA.getRoot().getPart("stringA");
      
      Message msgB = (Message)msgRegistry.get("elementB");
      MessagePart codeB = msgB.getRoot().getPart("codeB");
      MessagePart childB = msgB.getRoot().getPart("childB");
      
      assertTrue(codeB instanceof CompositeMessagePartRef);
      assertEquals(((CompositeMessagePartRef)codeB).getRefPart().getName(), "CodeType");
      
      assertTrue(childB instanceof CompositeMessagePartRef);
      assertEquals(((CompositeMessagePartRef)childB).getRefPart().getName(), "elementA");
   }
   
   public void testMultipleImportSameNamespace() throws Exception
   {
      Map msgRegistry = getMessageRegistry(XSDMessageImporterTest.class.getResource("multipleImport.xsd"));
      
      Message itemOne = (Message)msgRegistry.get("itemOne");
      Message itemTwo = (Message)msgRegistry.get("itemTwo");
      
      assertNotNull(itemOne);
      assertNotNull(itemTwo);
   }
      
   public void testMessageInheritance() throws Exception
   {
      Map msgRegistry = getMessageRegistry(XSDMessageImporterTest.class.getResource("inheritance.xsd"));
      
      Message myElement = (Message)msgRegistry.get("myElement");
      
      try
      {
         myElement.getRoot().getPart("baseElement");
         assertTrue(false);
      }
      catch (MetadataLookupException e)
      {
         assertNotNull(e);
      }
      
      Message myBaseType = (Message)msgRegistry.get("myBaseType");
      MessagePart part = myBaseType.getRoot().getPart("baseElement");
         
      assertTrue(part instanceof PrimitiveMessagePart);

      Message parent = (Message)msgRegistry.get("parent");
      MessagePart child = parent.getRoot().getPart("child");

      assertTrue(child instanceof CompositeMessagePartRef);
      
      Message childMsg = (Message)msgRegistry.get("parent_child");
      
      assertEquals(childMsg.getBaseMessage().getName(), "myBaseType");
   }

   public void testSequenceChoice() throws Exception
   {
      Map msgRegistry = getMessageRegistry(XSDMessageImporterTest.class.getResource("sequencechoice.xsd"));

      Message example = (Message)msgRegistry.get("sequencechoiceexample");

      assertEquals(CompositeMessagePart.RANDOM, example.getRoot().getAggregation());
   }

   public void testNillableMinOccurs() throws Exception
   {
      Map msgRegistry = getMessageRegistry(XSDMessageImporterTest.class.getResource("nillable.wsdl"));
      Message nillable = (Message)msgRegistry.get("nillable_example");

      MessagePart complexChild = nillable.getRoot().getPart("nillable_complex");
      MessagePartMapping complexChildMapping = complexChild.getMapping();

      assert(complexChild != null);
      assert(complexChild.getMinCount() == 0);
      assert(complexChildMapping != null);
      assert(complexChildMapping instanceof XMLMessagePartMapping);
      assert(((XMLMessagePartMapping)complexChildMapping).m_bNillable);

      MessagePart builtInChild = nillable.getRoot().getPart("nillable_built_in");
      MessagePartMapping builtInChildMapping = builtInChild.getMapping();

      assert(builtInChild != null);
      assert(builtInChild.getMinCount() == 0);
      assert(builtInChildMapping != null);
      assert(builtInChildMapping instanceof XMLMessagePartMapping);
      assert(((XMLMessagePartMapping)builtInChildMapping).m_bNillable);
   }

   static public Map getMessageRegistry(URL xsdURL) throws Exception
   {
      Message[] messages =  XSDMessageImporter.createMessageParts(xsdURL, Repository.getMetadata(), null, true);
      Map retVal = new HashMap();
      
      for (int i = 0; i < messages.length; ++i)
      {
         retVal.put(messages[i].getName(), messages[i]);
      }
      
      return retVal;
   }

   public Message getMessage(String name) throws Exception
   {
      if (m_messageRegistry == null)
      {
         m_messageRegistry = getMessageRegistry(XSDMessageImporterTest.class.getResource("test.xsd"));
      }

      return (Message)m_messageRegistry.get(name);
   }
   
   public void testAnnotationsToDescriptions() throws Exception
   {
      Map msgLookup = getMessageRegistry(XSDMessageImporterTest.class.getResource("annotations.xsd"));

      Message msg = (Message)msgLookup.get("PRPA_IN101103CA");
      
      assertNull(msg.getRoot().getDescription());

      Message msgBase = (Message)msgLookup.get("PRPA_IN101103CA_MCCI_MT002100CA_Message");
      MessagePart part = msgBase.getRoot().getPart("id");

      String sDescription = part.getDescription();
      
      assertTrue(sDescription.startsWith("A unique identifier for the message."));
      assertTrue(sDescription.endsWith("A root component is required or else the II value is NULL."));
      
      msg = (Message)msgLookup.get("II");
      
      assertNotNull(msg.getRoot().getPart("use").getDescription());
   }

   public void testNestedChoices() throws Exception
   {
      Map msgLookup = getMessageRegistry(XSDMessageImporterTest.class.getResource("nestedchoices.xsd"));

      Message msg = (Message)msgLookup.get("nestedGroups");

      assertEquals(msg.getRoot().getAggregation(), CompositeMessagePart.SEQUENTIAL);

      msg = (Message)msgLookup.get("sequenceChoice");
      
      assertEquals(msg.getRoot().getAggregation(), CompositeMessagePart.SEQUENTIAL);
   }
}
