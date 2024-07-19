// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.xml;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import junit.framework.TestCase;

import org.w3c.dom.Document;
import org.w3c.dom.Node;

import nexj.core.meta.Argument;
import nexj.core.meta.Event;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Primitive;
import nexj.core.util.XMLUtil;
import nexj.core.util.XMLWriter;

public class WSDLGeneratorTest extends TestCase
{
   private static ArrayList/*<Metaclass>*/ s_metaList = new ArrayList/*<Metaclass>*/();
   static
   {
      Metaclass meta = new Metaclass("Test");
      Event typed = new Event("typed");
      Event untyped = new Event("untyped");
      Argument arg0 = new Argument("arg0");
      Argument arg1 = new Argument("arg1");
      Argument result = new Argument(null);

      arg0.setType(Primitive.INTEGER);
      arg1.setType(Primitive.STRING);
      arg1.setCollection(true);
      result.setType(Primitive.ANY);
      result.setCollection(true);
      typed.addArgument(arg0);
      typed.addArgument(arg1);
      typed.setResult(result);
      typed.setVarArg(true);
      untyped.addArgument(new Argument("arg0"));
      untyped.setStatic(true);
      meta.addEvent(typed);
      meta.addEvent(untyped);
      s_metaList.add(meta);
   }

   public void testGenerator() throws IOException
   {
      StringWriter writer = new StringWriter();
      WSDLGenerator gen = new WSDLGenerator(XMLHTTPServer.SOAP_ACTION_DEFAULT);

      gen.setCompatible(false);
      writer.getBuffer().setLength(0);
      gen.generate(new XMLWriter(writer), null, "baseURI");
      validateDocument(XMLUtil.parse(new StringReader(writer.toString())), "baseURI", null);

      writer.getBuffer().setLength(0);
      gen.generate(new XMLWriter(writer), null, null);
      validateDocument(XMLUtil.parse(new StringReader(writer.toString())), null, null);

      writer.getBuffer().setLength(0);
      gen.generate(new XMLWriter(writer), s_metaList.iterator(), null);
      validateDocument(XMLUtil.parse(new StringReader(writer.toString())), null, s_metaList);
   }

   /**
    * validate document
    * @param doc document to validate
    * @param sURI the location of the web service
    * @param metaList A list of metaclasses defined in schema.
    */
   private void validateDocument(Document doc, String sURI, List/*<Metaclass>*/ metaList)
   {
      assertNotNull(doc.getFirstChild()); // there is a definition generated
      assertEquals(1, doc.getChildNodes().getLength()); // only one definitions section

      // Check attributes for outer node
      Node definitions = doc.getFirstChild();
      assertEquals("definitions", definitions.getNodeName());
      assertEquals(7, definitions.getAttributes().getLength());
      assertEquals("http://schemas.xmlsoap.org/wsdl/",
                   XMLUtil.getStringAttr(definitions, XML.XML_NS));
      assertEquals(XML.NS_URI_TNS,
                   XMLUtil.getStringAttr(definitions, XML.XML_NS + ':' + XML.TNS_NS));
      assertEquals("http://schemas.xmlsoap.org/soap/encoding/",
                   XMLUtil.getStringAttr(definitions, XML.XML_NS + ':' + XML.ENC_NS));
      assertEquals("http://schemas.xmlsoap.org/wsdl/",
                   XMLUtil.getStringAttr(definitions, XML.XML_NS + ':' + XML.NS_TAG_WSDL));
      assertEquals(XML.NS_URI_WSDL_SOAP,
                   XMLUtil.getStringAttr(definitions, XML.XML_NS + ':' + XML.NS_TAG_WSDL_SOAP));
      assertEquals("http://www.w3.org/2001/XMLSchema",
                   XMLUtil.getStringAttr(definitions, XML.XML_NS + ":" + XML.XSD_NS));
      assertEquals(XML.NS_URI_TNS, XMLUtil.getStringAttr(definitions, "targetNamespace"));

      int nTagCount = 10;

      for (int i = 0, nCount = (metaList == null) ? 0 : metaList.size(); i < nCount; ++i)
      {
         Metaclass metaclass = (Metaclass)metaList.get(i);

         validateEventBinding(definitions, metaclass);
         validateEventPortType(definitions, metaclass);
         nTagCount += 2; // binding section and porttype section

         for (int k = 0, nEventCount = metaclass.getEventCount(); k < nEventCount; ++k)
         {
            validateEventMessages(definitions, metaclass.getEvent(k));
            nTagCount += 2; // for request/response message
         }
      }

      assertEquals(nTagCount, definitions.getChildNodes().getLength());

      validateTypes(definitions, metaList); // check type section
      validateMessageRequest(definitions); // check invoke-request message section
      validateMessageResponse(definitions); // check invoke-response message section
      validateMessageChangeRequest(definitions); // check change-request message section
      validateMessageReadRequest(definitions); // check read-request message section
      validateMessageChangeResponse(definitions); // check event-response message section
      validatePortType(definitions); // check portType section
      validateBinding(definitions); // check binding section
      validateService(definitions, sURI, metaList); // check service section
   }

   /**
    * validate binding section
    * @param parent the parent node
    */
   private void validateBinding(Node parent)
   {
      Node binding = XMLUtil.findChildElement(parent, "binding");
      assertNotNull(binding);

      assertEquals(2, binding.getAttributes().getLength());
      assertEquals("GenericServer", XMLUtil.getStringAttr(binding, "name"));
      assertEquals(XML.TNS_NS + ":Server", XMLUtil.getStringAttr(binding, "type"));
      assertEquals(4, binding.getChildNodes().getLength());

      Node tmpNode = XMLUtil.findChildElement(binding, "binding");
      assertNotNull(tmpNode);
      assertEquals(3, tmpNode.getAttributes().getLength());
      assertEquals(XML.NS_URI_WSDL_SOAP, XMLUtil.getStringAttr(tmpNode, XML.XML_NS));
      assertEquals("document", XMLUtil.getStringAttr(tmpNode, "style"));
      assertEquals("http://schemas.xmlsoap.org/soap/http",
                   XMLUtil.getStringAttr(tmpNode, "transport"));
      assertFalse(tmpNode.hasChildNodes());

      tmpNode = XMLUtil.findChildElement(binding, "operation", "name", "invoke");
      assertNotNull(tmpNode);
      assertEquals(1, tmpNode.getAttributes().getLength());
      assertEquals(3, tmpNode.getChildNodes().getLength());
      
      Node tmpNode2 = XMLUtil.findChildElement(tmpNode, "operation");
      assertNotNull(tmpNode2);
      assertEquals(2, tmpNode2.getAttributes().getLength());
      assertEquals(XML.NS_URI_WSDL_SOAP, XMLUtil.getStringAttr(tmpNode2, XML.XML_NS));
      assertEquals("Server#invoke", XMLUtil.getStringAttr(tmpNode2, "soapAction"));
      assertFalse(tmpNode2.hasChildNodes());

      tmpNode2 = XMLUtil.findChildElement(tmpNode, "input");
      assertNotNull(tmpNode2);
      assertFalse(tmpNode2.hasAttributes());
      assertEquals(1, tmpNode2.getChildNodes().getLength());
      assertEquals("body", tmpNode2.getFirstChild().getNodeName());
      assertEquals(3, tmpNode2.getFirstChild().getAttributes().getLength());
      assertEquals(XML.NS_URI_WSDL_SOAP,
                   XMLUtil.getStringAttr(tmpNode2.getFirstChild(), XML.XML_NS));
      assertEquals("http://schemas.xmlsoap.org/soap/encoding/",
                   XMLUtil.getStringAttr(tmpNode2.getFirstChild(), "encodingStyle"));
      assertEquals("literal", XMLUtil.getStringAttr(tmpNode2.getFirstChild(), "use"));
      assertFalse(tmpNode2.getFirstChild().hasChildNodes());

      tmpNode2 = XMLUtil.findChildElement(tmpNode, "output");
      assertNotNull(tmpNode2);
      assertFalse(tmpNode2.hasAttributes());
      assertEquals(1, tmpNode2.getChildNodes().getLength());
      assertEquals("body", tmpNode2.getFirstChild().getNodeName());
      assertEquals(3, tmpNode2.getFirstChild().getAttributes().getLength());
      assertEquals(XML.NS_URI_WSDL_SOAP,
                   XMLUtil.getStringAttr(tmpNode2.getFirstChild(), XML.XML_NS));
      assertEquals("http://schemas.xmlsoap.org/soap/encoding/",
                   XMLUtil.getStringAttr(tmpNode2.getFirstChild(), "encodingStyle"));
      assertEquals("literal", XMLUtil.getStringAttr(tmpNode2.getFirstChild(), "use"));
      assertFalse(tmpNode2.getFirstChild().hasChildNodes());

      tmpNode = XMLUtil.findChildElement(binding, "operation", "name", "change");
      assertNotNull(tmpNode);
      assertEquals(1, tmpNode.getAttributes().getLength());
      assertEquals(3, tmpNode.getChildNodes().getLength());
      
      tmpNode2 = XMLUtil.findChildElement(tmpNode, "operation");
      assertNotNull(tmpNode2);
      assertEquals(2, tmpNode2.getAttributes().getLength());
      assertEquals(XML.NS_URI_WSDL_SOAP, XMLUtil.getStringAttr(tmpNode2, XML.XML_NS));
      assertEquals("Server#invoke", XMLUtil.getStringAttr(tmpNode2, "soapAction"));
      assertFalse(tmpNode2.hasChildNodes());

      tmpNode2 = XMLUtil.findChildElement(tmpNode, "input");
      assertNotNull(tmpNode2);
      assertFalse(tmpNode2.hasAttributes());
      assertEquals(1, tmpNode2.getChildNodes().getLength());
      assertEquals("body", tmpNode2.getFirstChild().getNodeName());
      assertEquals(3, tmpNode2.getFirstChild().getAttributes().getLength());
      assertEquals(XML.NS_URI_WSDL_SOAP,
                   XMLUtil.getStringAttr(tmpNode2.getFirstChild(), XML.XML_NS));
      assertEquals("http://schemas.xmlsoap.org/soap/encoding/",
                   XMLUtil.getStringAttr(tmpNode2.getFirstChild(), "encodingStyle"));
      assertEquals("literal", XMLUtil.getStringAttr(tmpNode2.getFirstChild(), "use"));
      assertFalse(tmpNode2.getFirstChild().hasChildNodes());

      tmpNode2 = XMLUtil.findChildElement(tmpNode, "output");
      assertNotNull(tmpNode2);
      assertFalse(tmpNode2.hasAttributes());
      assertEquals(1, tmpNode2.getChildNodes().getLength());
      assertEquals("body", tmpNode2.getFirstChild().getNodeName());
      assertEquals(3, tmpNode2.getFirstChild().getAttributes().getLength());
      assertEquals(XML.NS_URI_WSDL_SOAP,
                   XMLUtil.getStringAttr(tmpNode2.getFirstChild(), XML.XML_NS));
      assertEquals("http://schemas.xmlsoap.org/soap/encoding/",
                   XMLUtil.getStringAttr(tmpNode2.getFirstChild(), "encodingStyle"));
      assertEquals("literal", XMLUtil.getStringAttr(tmpNode2.getFirstChild(), "use"));
      assertFalse(tmpNode2.getFirstChild().hasChildNodes());

      tmpNode = XMLUtil.findChildElement(binding, "operation", "name", "read");
      assertNotNull(tmpNode);
      assertEquals(1, tmpNode.getAttributes().getLength());
      assertEquals(3, tmpNode.getChildNodes().getLength());
      
      tmpNode2 = XMLUtil.findChildElement(tmpNode, "operation");
      assertNotNull(tmpNode2);
      assertEquals(2, tmpNode2.getAttributes().getLength());
      assertEquals(XML.NS_URI_WSDL_SOAP, XMLUtil.getStringAttr(tmpNode2, XML.XML_NS));
      assertEquals("Server#invoke", XMLUtil.getStringAttr(tmpNode2, "soapAction"));
      assertFalse(tmpNode2.hasChildNodes());

      tmpNode2 = XMLUtil.findChildElement(tmpNode, "input");
      assertNotNull(tmpNode2);
      assertFalse(tmpNode2.hasAttributes());
      assertEquals(1, tmpNode2.getChildNodes().getLength());
      assertEquals("body", tmpNode2.getFirstChild().getNodeName());
      assertEquals(3, tmpNode2.getFirstChild().getAttributes().getLength());
      assertEquals(XML.NS_URI_WSDL_SOAP,
                   XMLUtil.getStringAttr(tmpNode2.getFirstChild(), XML.XML_NS));
      assertEquals("http://schemas.xmlsoap.org/soap/encoding/",
                   XMLUtil.getStringAttr(tmpNode2.getFirstChild(), "encodingStyle"));
      assertEquals("literal", XMLUtil.getStringAttr(tmpNode2.getFirstChild(), "use"));
      assertFalse(tmpNode2.getFirstChild().hasChildNodes());

      tmpNode2 = XMLUtil.findChildElement(tmpNode, "output");
      assertNotNull(tmpNode2);
      assertFalse(tmpNode2.hasAttributes());
      assertEquals(1, tmpNode2.getChildNodes().getLength());
      assertEquals("body", tmpNode2.getFirstChild().getNodeName());
      assertEquals(3, tmpNode2.getFirstChild().getAttributes().getLength());
      assertEquals(XML.NS_URI_WSDL_SOAP,
                   XMLUtil.getStringAttr(tmpNode2.getFirstChild(), XML.XML_NS));
      assertEquals("http://schemas.xmlsoap.org/soap/encoding/",
                   XMLUtil.getStringAttr(tmpNode2.getFirstChild(), "encodingStyle"));
      assertEquals("literal", XMLUtil.getStringAttr(tmpNode2.getFirstChild(), "use"));
      assertFalse(tmpNode2.getFirstChild().hasChildNodes());
   }

   /**
    * @param parent The parent node.
    * @param meta The metaclass to validate bindings for.
    */
   private void validateEventBinding(Node parent, Metaclass meta)
   {
      String sClass = meta.getName();
      Node binding = XMLUtil.findChildElementByName(
         parent, "binding", sClass + WSDLGenerator.METACLASS_NAME_DELIMITER + "Server");

      assertNotNull(binding);
      assertEquals(2, binding.getAttributes().getLength());
      assertEquals(XML.TNS_NS + ':' + sClass + WSDLGenerator.METACLASS_NAME_DELIMITER + "Server",
                   XMLUtil.getStringAttr(binding, "type"));
      assertEquals(1 + meta.getEventCount(), binding.getChildNodes().getLength());

      Node tmpNode = XMLUtil.findChildElement(binding, "binding");
      assertNotNull(tmpNode);
      assertEquals(3, tmpNode.getAttributes().getLength());
      assertEquals(XML.NS_URI_WSDL_SOAP, XMLUtil.getStringAttr(tmpNode, XML.XML_NS));
      assertEquals("document", XMLUtil.getStringAttr(tmpNode, "style"));
      assertEquals("http://schemas.xmlsoap.org/soap/http",
                   XMLUtil.getStringAttr(tmpNode, "transport"));
      assertFalse(tmpNode.hasChildNodes());

      for (int i = 0, nCount = meta.getEventCount(); i < nCount; ++i)
      {
         Event event = meta.getEvent(i);
         String sEvent = XSDGenerator.computeElementName(event);
         Node operation = XMLUtil.findChildElementByName(binding, "operation", sEvent);

         assertNotNull(operation);
         assertEquals(1, operation.getAttributes().getLength());
         assertEquals(3, operation.getChildNodes().getLength());

         tmpNode = XMLUtil.findChildElement(operation, "operation");
         assertNotNull(tmpNode);
         assertEquals(2, tmpNode.getAttributes().getLength());
         assertEquals(XML.NS_URI_WSDL_SOAP, XMLUtil.getStringAttr(tmpNode, XML.XML_NS));
         assertEquals("Server#invoke", XMLUtil.getStringAttr(tmpNode, "soapAction"));
         assertEquals(3, operation.getChildNodes().getLength());

         tmpNode = XMLUtil.findChildElement(operation, "input");
         assertNotNull(tmpNode);
         assertEquals(0, tmpNode.getAttributes().getLength());
         assertEquals(1, tmpNode.getChildNodes().getLength());
         assertEquals("body", tmpNode.getFirstChild().getNodeName());
         assertEquals(3, tmpNode.getFirstChild().getAttributes().getLength());
         assertEquals(XML.NS_URI_WSDL_SOAP,
                      XMLUtil.getStringAttr(tmpNode.getFirstChild(), XML.XML_NS));
         assertEquals("http://schemas.xmlsoap.org/soap/encoding/",
                      XMLUtil.getStringAttr(tmpNode.getFirstChild(), "encodingStyle"));
         assertEquals("literal", XMLUtil.getStringAttr(tmpNode.getFirstChild(), "use"));
         assertFalse(tmpNode.getFirstChild().hasChildNodes());

         tmpNode = XMLUtil.findChildElement(operation, "output");
         assertNotNull(tmpNode);
         assertEquals(0, tmpNode.getAttributes().getLength());
         assertEquals(1, tmpNode.getChildNodes().getLength());
         assertEquals("body", tmpNode.getFirstChild().getNodeName());
         assertEquals(3, tmpNode.getFirstChild().getAttributes().getLength());
         assertEquals(XML.NS_URI_WSDL_SOAP,
                      XMLUtil.getStringAttr(tmpNode.getFirstChild(), XML.XML_NS));
         assertEquals("http://schemas.xmlsoap.org/soap/encoding/",
                      XMLUtil.getStringAttr(tmpNode.getFirstChild(), "encodingStyle"));
         assertEquals("literal", XMLUtil.getStringAttr(tmpNode.getFirstChild(), "use"));
         assertFalse(tmpNode.getFirstChild().hasChildNodes());
      }
   }

   /**
    * @param parent The parent node.
    * @param event The event to validate messages for.
    */
   private void validateEventMessages(Node parent, Event event)
   {
      String sEvent = XSDGenerator.computeElementName(event);
      String sClass = event.getMetaclass().getName();
      Node node = XMLUtil.findChildElementByName(
         parent, "message", sClass + WSDLGenerator.METACLASS_NAME_DELIMITER + sEvent);

      assertNotNull(node);
      assertEquals(2, node.getAttributes().getLength());
      assertEquals(XML.NS_URI_TNS + '/' + sClass,
                   XMLUtil.getStringAttr(node, XML.XML_NS + ':' + sClass));
      assertEquals(1, node.getChildNodes().getLength());
      node = node.getFirstChild();
      assertEquals("part", node.getNodeName());
      assertEquals(2, node.getAttributes().getLength());
      assertEquals("parameters", XMLUtil.getStringAttr(node, "name"));
      assertEquals(sClass + ':' + sEvent, XMLUtil.getStringAttr(node, "element"));
      assertEquals(0, node.getChildNodes().getLength());

      sEvent += "_Response"; // the result message
      node = XMLUtil.findChildElementByName(
         parent, "message", sClass + WSDLGenerator.METACLASS_NAME_DELIMITER + sEvent);
      assertNotNull(node);
      assertEquals(2, node.getAttributes().getLength());
      assertEquals(XML.NS_URI_TNS + '/' + sClass,
                   XMLUtil.getStringAttr(node, XML.XML_NS + ':' + sClass));
      assertEquals(1, node.getChildNodes().getLength());
      node = node.getFirstChild();
      assertEquals("part", node.getNodeName());
      assertEquals(2, node.getAttributes().getLength());
      assertEquals("parameters", XMLUtil.getStringAttr(node, "name"));
      assertEquals(sClass + ':' + sEvent, XMLUtil.getStringAttr(node, "element"));
      assertEquals(0, node.getChildNodes().getLength());
   }

   /**
    * @param parent The parent node.
    * @param meta The metaclass to validate porttypes for.
    */
   private void validateEventPortType(Node parent, Metaclass meta)
   {
      String sClass = meta.getName();
      Node portType = XMLUtil.findChildElementByName(
         parent, "portType", sClass + WSDLGenerator.METACLASS_NAME_DELIMITER + "Server");

      assertNotNull(portType);
      assertEquals(1, portType.getAttributes().getLength());
      assertEquals(meta.getEventCount(), portType.getChildNodes().getLength());

      for (int i = 0, nCount = meta.getEventCount(); i < nCount; ++i)
      {
         Event event = meta.getEvent(i);
         String sEvent = XSDGenerator.computeElementName(event);
         String sType = XML.TNS_NS + ':' + sClass + WSDLGenerator.METACLASS_NAME_DELIMITER + sEvent;
         Node operation = XMLUtil.findChildElementByName(portType, "operation", sEvent);

         assertNotNull(operation);
         assertEquals(1, operation.getAttributes().getLength());
         assertEquals(2, operation.getChildNodes().getLength());

         Node node = XMLUtil.findChildElement(operation, "input");

         assertNotNull(node);
         assertEquals(1, node.getAttributes().getLength());
         assertEquals(sType, XMLUtil.getStringAttr(node, "message"));
         assertEquals(0, node.getChildNodes().getLength());

         node = XMLUtil.findChildElement(operation, "output");
         assertNotNull(node);
         assertEquals(1, node.getAttributes().getLength());
         assertEquals(sType + "_Response", XMLUtil.getStringAttr(node, "message"));
         assertEquals(0, node.getChildNodes().getLength());
      }
   }

   /**
    * validate message change-request section
    * @param parent the parent node
    */
   private void validateMessageChangeRequest(Node parent)
   {
      Node messageReq = XMLUtil.findChildElement(parent, "message", "name", "change-request");
      assertNotNull(messageReq);

      assertEquals(1, messageReq.getAttributes().getLength());
      assertEquals(1, messageReq.getChildNodes().getLength());

      assertEquals("part", messageReq.getFirstChild().getNodeName());
      assertEquals(2, messageReq.getFirstChild().getAttributes().getLength());
      assertEquals(XML.TNS_NS + ':' + XML.BASE_PREFIX + "Change-Request",
                   XMLUtil.getStringAttr(messageReq.getFirstChild(), "element"));
      assertEquals("parameters", XMLUtil.getStringAttr(messageReq.getFirstChild(), "name"));
      assertFalse(messageReq.getFirstChild().hasChildNodes());
   }

   /**
    * validate message change-response section
    * @param parent the parent node
    */
   private void validateMessageChangeResponse(Node parent)
   {
      Node messageReq = XMLUtil.findChildElement(parent, "message", "name", "change-response");
      assertNotNull(messageReq);

      assertEquals(1, messageReq.getAttributes().getLength());
      assertEquals(1, messageReq.getChildNodes().getLength());

      assertEquals("part", messageReq.getFirstChild().getNodeName());
      assertEquals(2, messageReq.getFirstChild().getAttributes().getLength());
      assertEquals(XML.TNS_NS + ':' + XML.BASE_PREFIX + "Change-Response",
                   XMLUtil.getStringAttr(messageReq.getFirstChild(), "element"));
      assertEquals("parameters", XMLUtil.getStringAttr(messageReq.getFirstChild(), "name"));
      assertFalse(messageReq.getFirstChild().hasChildNodes());
   }

   /**
    * validate message read-request section
    * @param parent the parent node
    */
   private void validateMessageReadRequest(Node parent)
   {
      Node messageReq = XMLUtil.findChildElement(parent, "message", "name", "read-request");
      assertNotNull(messageReq);

      assertEquals(1, messageReq.getAttributes().getLength());
      assertEquals(1, messageReq.getChildNodes().getLength());

      assertEquals("part", messageReq.getFirstChild().getNodeName());
      assertEquals(2, messageReq.getFirstChild().getAttributes().getLength());
      assertEquals(XML.TNS_NS + ':' + XML.BASE_PREFIX + "Read-Request",
                   XMLUtil.getStringAttr(messageReq.getFirstChild(), "element"));
      assertEquals("parameters", XMLUtil.getStringAttr(messageReq.getFirstChild(), "name"));
      assertFalse(messageReq.getFirstChild().hasChildNodes());
   }

   /**
    * validate message invoke-request section
    * @param parent the parent node
    */
   private void validateMessageRequest(Node parent)
   {
      Node messageReq = XMLUtil.findChildElement(parent, "message", "name", "invoke-request");
      assertNotNull(messageReq);

      assertEquals(1, messageReq.getAttributes().getLength());
      assertEquals(1, messageReq.getChildNodes().getLength());

      assertEquals("part", messageReq.getFirstChild().getNodeName());
      assertEquals(2, messageReq.getFirstChild().getAttributes().getLength());
      assertEquals(XML.TNS_NS + ':' + XML.BASE_PREFIX + "Request",
                   XMLUtil.getStringAttr(messageReq.getFirstChild(), "element"));
      assertEquals("request", XMLUtil.getStringAttr(messageReq.getFirstChild(), "name"));
      assertFalse(messageReq.getFirstChild().hasChildNodes());
   }

   /**
    * validate message invoke-response section
    * @param parent the parent node
    */
   private void validateMessageResponse(Node parent)
   {
      Node messageRsp = XMLUtil.findChildElement(parent, "message", "name", "invoke-response"); 
      assertNotNull(messageRsp);

      assertEquals(1, messageRsp.getAttributes().getLength());
      assertEquals(1, messageRsp.getChildNodes().getLength());

      assertEquals("part", messageRsp.getFirstChild().getNodeName());
      assertEquals(2, messageRsp.getFirstChild().getAttributes().getLength());
      assertEquals(XML.TNS_NS + ':' + XML.BASE_PREFIX + "Response",
                   XMLUtil.getStringAttr(messageRsp.getFirstChild(), "element"));
      assertEquals("response", XMLUtil.getStringAttr(messageRsp.getFirstChild(), "name"));
      assertFalse(messageRsp.getFirstChild().hasChildNodes());
   }

   /**
    * Validate the metaclass specific schema section.
    * @param schema The schema node to validate.
    * @param meta The metaclass to validate schema against.
    */
   private void validateMetaclassSchema(Node schema, Metaclass meta)
   {
      assertNotNull(schema);
      assertEquals("schema", schema.getNodeName());
      assertEquals(3, schema.getAttributes().getLength());
      assertEquals("http://www.w3.org/2001/XMLSchema", XMLUtil.getStringAttr(schema, XML.XML_NS));
      assertEquals("qualified", XMLUtil.getStringAttr(schema, "elementFormDefault"));
      assertEquals(XML.NS_URI_TNS + '/' + meta.getName(),
                   XMLUtil.getStringAttr(schema, "targetNamespace"));
      assertEquals(meta.getEventCount() * 2 + 1, schema.getChildNodes().getLength()); // +1 import

      Node node = XMLUtil.findChildElement(schema, "import");

      assertNotNull(node);
      assertEquals(1, node.getAttributes().getLength());
      assertEquals(XML.NS_URI_TNS, XMLUtil.getStringAttr(node, "namespace"));

      for (int i = 0, nCount = meta.getEventCount(); i < nCount; ++i)
      {
         Event event = meta.getEvent(i);
         String sEvent = XSDGenerator.computeElementName(event);
         Node element = XMLUtil.findChildElementByName(schema, sEvent); // request
         Node tmpNode;

         assertNotNull(element);
         assertEquals(1, element.getAttributes().getLength());
         assertEquals(1, element.getChildNodes().getLength());
         node = element.getFirstChild();
         assertEquals("complexType", node.getNodeName());
         assertEquals(0, node.getAttributes().getLength());
         assertEquals(1, node.getChildNodes().getLength());
         node = node.getFirstChild();
         assertEquals("sequence", node.getNodeName());
         assertEquals(0, node.getAttributes().getLength());

         if (event.isStatic()) // check "_instance" arg
         {
            assertEquals(event.getArgumentCount(), node.getChildNodes().getLength());
         }
         else
         {
            assertEquals(event.getArgumentCount() + 1, node.getChildNodes().getLength());
            tmpNode = XMLUtil.findChildElementByName(node, "_instance");
            assertNotNull(tmpNode);
            assertEquals(2, tmpNode.getAttributes().getLength());
            assertEquals(XML.TNS_NS + ':' + meta.getName(), XMLUtil.getStringAttr(tmpNode, "type"));
            assertEquals(0, tmpNode.getChildNodes().getLength());
         }

         for (int k = 0, nArgCount = event.getArgumentCount(); k < nArgCount; ++k) // for each arg
         {
            Argument arg = event.getArgument(k);
            QName type = XML.getQualifiedType(
               arg.getType(), event.isVarArg() && arg.isCollection() && k == nArgCount - 1);
            int nAttrCount = 2;

            tmpNode = XMLUtil.findChildElementByName(node, arg.getName());
            assertNotNull(tmpNode);
            assertEquals(type.getPrefix() + ':' + type.getLocalPart(),
                         XMLUtil.getStringAttr(tmpNode, "type"));

            if (!arg.isRequired())
            {
               assertEquals("0", XMLUtil.getStringAttr(tmpNode, "minOccurs"));
               ++nAttrCount;
            }

            if (arg.isCollection())
            {
               assertEquals("unbounded", XMLUtil.getStringAttr(tmpNode, "maxOccurs"));
               ++nAttrCount;
            }

            assertEquals(nAttrCount, tmpNode.getAttributes().getLength());
            assertEquals(0, tmpNode.getChildNodes().getLength());
         }

         Argument result = event.getResult();
         boolean bTyped = result != null;
         QName type = XML.getQualifiedType((bTyped) ? result.getType() : Primitive.ANY);
         int nAttrCount = 1;

         element = XMLUtil.findChildElementByName(schema, sEvent + "_Response"); // response
         assertNotNull(element);

         if (bTyped && result.isCollection() && !result.isRequired())
         {
            assertEquals("true", XMLUtil.getStringAttr(element, "nillable"));
            ++nAttrCount;
         }

         assertEquals(nAttrCount, element.getAttributes().getLength());
         assertEquals(1, element.getChildNodes().getLength());
         node = element.getFirstChild();
         assertEquals("complexType", node.getNodeName());
         assertEquals(0, node.getAttributes().getLength());
         assertEquals(1, node.getChildNodes().getLength());
         node = node.getFirstChild();
         assertEquals("sequence", node.getNodeName());
         assertEquals(0, node.getAttributes().getLength());
         assertEquals(1, node.getChildNodes().getLength());
         node = node.getFirstChild();
         nAttrCount = 2;
         assertEquals("element", node.getNodeName());
         assertEquals("item", XMLUtil.getStringAttr(node, "name"));
         assertEquals(type.getPrefix() + ':' + type.getLocalPart(),
                      XMLUtil.getStringAttr(node, "type"));

         if (!bTyped || result.isCollection())
         {
            assertEquals("true", XMLUtil.getStringAttr(node, "nillable"));
            ++nAttrCount;
         }

         if (bTyped && result.isCollection())
         {
            assertEquals("0", XMLUtil.getStringAttr(node, "minOccurs"));
            assertEquals("unbounded", XMLUtil.getStringAttr(node, "maxOccurs"));
            nAttrCount += 2;
         }

         assertEquals(nAttrCount, node.getAttributes().getLength());
         assertEquals(0, node.getChildNodes().getLength());
      }
   }

   /**
    * validate portType section
    * @param parent the parent node
    */
   private void validatePortType(Node parent)
   {
      Node portType = XMLUtil.findChildElement(parent, "portType");
      assertNotNull(portType);

      assertEquals(1, portType.getAttributes().getLength());
      assertEquals("Server", XMLUtil.getStringAttr(portType, "name"));
      assertEquals(3, portType.getChildNodes().getLength());

      Node tmpNode = XMLUtil.findChildElement(portType, "operation", "name", "invoke");
      assertNotNull(tmpNode);
      assertEquals(1, tmpNode.getAttributes().getLength());
      assertEquals(2, tmpNode.getChildNodes().getLength());

      Node tmpNode2 = XMLUtil.findChildElement(tmpNode, "input");
      assertNotNull(tmpNode2);
      assertEquals(1, tmpNode2.getAttributes().getLength());
      assertEquals(XML.TNS_NS + ":invoke-request", XMLUtil.getStringAttr(tmpNode2, "message"));
      assertFalse(tmpNode2.hasChildNodes());

      tmpNode2 = XMLUtil.findChildElement(tmpNode, "output");
      assertNotNull(tmpNode2);
      assertEquals(1, tmpNode2.getAttributes().getLength());
      assertEquals(XML.TNS_NS + ":invoke-response", XMLUtil.getStringAttr(tmpNode2, "message"));
      assertFalse(tmpNode2.hasChildNodes());

      tmpNode = XMLUtil.findChildElement(portType, "operation", "name", "change");
      assertNotNull(tmpNode);
      assertEquals(1, tmpNode.getAttributes().getLength());
      assertEquals(2, tmpNode.getChildNodes().getLength());

      tmpNode2 = XMLUtil.findChildElement(tmpNode, "input");
      assertNotNull(tmpNode2);
      assertEquals(1, tmpNode2.getAttributes().getLength());
      assertEquals(XML.TNS_NS + ":change-request", XMLUtil.getStringAttr(tmpNode2, "message"));
      assertFalse(tmpNode2.hasChildNodes());

      tmpNode2 = XMLUtil.findChildElement(tmpNode, "output");
      assertNotNull(tmpNode2);
      assertEquals(1, tmpNode2.getAttributes().getLength());
      assertEquals(XML.TNS_NS + ":change-response", XMLUtil.getStringAttr(tmpNode2, "message"));
      assertFalse(tmpNode2.hasChildNodes());

      tmpNode = XMLUtil.findChildElement(portType, "operation", "name", "read");
      assertNotNull(tmpNode);
      assertEquals(1, tmpNode.getAttributes().getLength());
      assertEquals(2, tmpNode.getChildNodes().getLength());

      tmpNode2 = XMLUtil.findChildElement(tmpNode, "input");
      assertNotNull(tmpNode2);
      assertEquals(1, tmpNode2.getAttributes().getLength());
      assertEquals(XML.TNS_NS + ":read-request", XMLUtil.getStringAttr(tmpNode2, "message"));
      assertFalse(tmpNode2.hasChildNodes());

      tmpNode2 = XMLUtil.findChildElement(tmpNode, "output");
      assertNotNull(tmpNode2);
      assertEquals(1, tmpNode2.getAttributes().getLength());
      assertEquals(XML.TNS_NS + ":read-response", XMLUtil.getStringAttr(tmpNode2, "message"));
      assertFalse(tmpNode2.hasChildNodes());
   }

   /**
    * validate service section
    * @param parent the parent node
    * @param sURI the location of the web service
    * @param metaList A list of metaclasses defined in schema.
    */
   private void validateService(Node parent, String sURI, List/*<Metaclass>*/ metaList)
   {
      Node service = XMLUtil.findChildElement(parent, "service");
      int nServiceCount = 1;

      assertNotNull(service);
      assertEquals(1, service.getAttributes().getLength());
      assertEquals("GenericServer", XMLUtil.getStringAttr(service, "name"));

      Node port = XMLUtil.findChildElementByName(service, "port", "Server");

      assertNotNull(port);
      assertEquals(2, port.getAttributes().getLength());
      assertEquals(XML.TNS_NS + ":GenericServer", XMLUtil.getStringAttr(port, "binding"));
      assertEquals(1, port.getChildNodes().getLength());
      assertEquals("address", port.getFirstChild().getNodeName());
      assertEquals(2, port.getFirstChild().getAttributes().getLength());
      assertEquals(XML.NS_URI_WSDL_SOAP, XMLUtil.getStringAttr(port.getFirstChild(), XML.XML_NS));
      assertEquals(sURI, XMLUtil.getStringAttr(port.getFirstChild(), "location"));
      assertEquals(0, port.getFirstChild().getChildNodes().getLength());

      for (int i = 0, nCount = (metaList == null) ? 0 : metaList.size(); i < nCount; ++i)
      {
         String sMeta = ((Metaclass)metaList.get(i)).getName();

         ++nServiceCount;
         port = XMLUtil.findChildElementByName(
            service, "port", sMeta + WSDLGenerator.METACLASS_NAME_DELIMITER + "Server");
         assertNotNull(port);
         assertEquals(2, port.getAttributes().getLength());
         assertEquals(XML.TNS_NS + ':' + sMeta + WSDLGenerator.METACLASS_NAME_DELIMITER + "Server",
                      XMLUtil.getStringAttr(port, "binding"));
         assertEquals(1, port.getChildNodes().getLength());
         assertEquals("address", port.getFirstChild().getNodeName());
         assertEquals(2, port.getFirstChild().getAttributes().getLength());
         assertEquals(XML.NS_URI_WSDL_SOAP,
                      XMLUtil.getStringAttr(port.getFirstChild(), XML.XML_NS));
         assertEquals(sURI, XMLUtil.getStringAttr(port.getFirstChild(), "location"));
         assertEquals(0, port.getFirstChild().getChildNodes().getLength());
      }

      assertEquals(nServiceCount, service.getChildNodes().getLength());
   }

   /**
    * Validate the TNS schema section.
    * @param schema The schema node to validate.
    */
   private void validateTNSSchema(Node schema)
   {
      assertNotNull(schema);
      assertEquals("schema", schema.getNodeName());
      assertEquals(3, schema.getAttributes().getLength());
      assertEquals("http://www.w3.org/2001/XMLSchema", XMLUtil.getStringAttr(schema, XML.XML_NS));
      assertEquals("qualified", XMLUtil.getStringAttr(schema, "elementFormDefault"));
      assertEquals(XML.NS_URI_TNS, XMLUtil.getStringAttr(schema, "targetNamespace"));

      Node tmpNode = XMLUtil.findChildElement(schema, "element", "name", XML.BASE_PREFIX + "Request");
      assertNotNull(tmpNode);
      assertEquals(2, tmpNode.getAttributes().getLength());
      assertEquals(XML.TNS_NS + ':' + XML.BASE_PREFIX + "Request",
                   XMLUtil.getStringAttr(tmpNode, "type"));
      assertFalse(tmpNode.hasChildNodes());

      tmpNode = XMLUtil.findChildElement(schema, "element", "name", XML.BASE_PREFIX + "Response");
      assertNotNull(tmpNode);
      assertEquals(2, tmpNode.getAttributes().getLength());
      assertEquals(XML.TNS_NS + ':' + XML.BASE_PREFIX + "Response",
                   XMLUtil.getStringAttr(tmpNode, "type"));
      assertFalse(tmpNode.hasChildNodes());

      tmpNode = XMLUtil.findChildElement(schema, "element", "name", XML.BASE_PREFIX + "Change-Request");
      assertNotNull(tmpNode);
      assertEquals(1, tmpNode.getAttributes().getLength());
      assertEquals(1, tmpNode.getChildNodes().getLength());
      assertEquals("complexType", tmpNode.getChildNodes().item(0).getNodeName());
      assertFalse(tmpNode.getChildNodes().item(0).hasAttributes());
      assertEquals(1, tmpNode.getChildNodes().item(0).getChildNodes().getLength());
      assertEquals("sequence",
                   tmpNode.getChildNodes().item(0).getChildNodes().item(0).getNodeName());
      assertFalse(tmpNode.getChildNodes().item(0).getChildNodes().item(0).hasAttributes());
      assertEquals(2,
                   tmpNode
                      .getChildNodes()
                         .item(0)
                            .getChildNodes()
                               .item(0)
                                  .getChildNodes()
                                     .getLength());
      
      Node tmpNode2 =
         XMLUtil.findChildElement(tmpNode.getChildNodes().item(0).getChildNodes().item(0),
                                  "element",
                                  "name",
                                  "objects");
      assertNotNull(tmpNode2);
      assertEquals(4, tmpNode2.getAttributes().getLength());
      assertEquals("0", XMLUtil.getStringAttr(tmpNode2, "minOccurs"));
      assertEquals("unbounded", XMLUtil.getStringAttr(tmpNode2, "maxOccurs"));
      assertEquals(XML.XSD_NS + ":anyType", XMLUtil.getStringAttr(tmpNode2, "type"));
      assertFalse(tmpNode2.hasChildNodes());

      tmpNode2 = XMLUtil.findChildElement(tmpNode.getChildNodes().item(0).getChildNodes().item(0),
                                          "element",
                                          "name",
                                          "attributes");
      assertNotNull(tmpNode2);
      assertEquals(2, tmpNode2.getAttributes().getLength());
      assertEquals(XML.XSD_NS + ":string", XMLUtil.getStringAttr(tmpNode2, "type"));
      assertFalse(tmpNode2.hasChildNodes());

      tmpNode = XMLUtil.findChildElement(schema, "element", "name", XML.BASE_PREFIX + "Read-Request");
      assertNotNull(tmpNode);
      assertEquals(1, tmpNode.getAttributes().getLength());
      assertEquals(1, tmpNode.getChildNodes().getLength());
      assertEquals("complexType", tmpNode.getChildNodes().item(0).getNodeName());
      assertFalse(tmpNode.getChildNodes().item(0).hasAttributes());
      assertEquals(1, tmpNode.getChildNodes().item(0).getChildNodes().getLength());
      assertEquals("sequence",
                   tmpNode.getChildNodes().item(0).getChildNodes().item(0).getNodeName());
      assertFalse(tmpNode.getChildNodes().item(0).getChildNodes().item(0).hasAttributes());
      assertEquals(6,
                   tmpNode
                      .getChildNodes()
                         .item(0)
                            .getChildNodes()
                               .item(0)
                                  .getChildNodes()
                                     .getLength());
      
      tmpNode2 = XMLUtil.findChildElement(tmpNode.getChildNodes().item(0).getChildNodes().item(0),
         "element",
         "name",
         "class");
      assertNotNull(tmpNode2);
      assertEquals(2, tmpNode2.getAttributes().getLength());
      assertEquals(XML.XSD_NS + ":string", XMLUtil.getStringAttr(tmpNode2, "type"));
      assertFalse(tmpNode2.hasChildNodes());

      tmpNode2 = XMLUtil.findChildElement(tmpNode.getChildNodes().item(0).getChildNodes().item(0),
                                          "element",
                                          "name",
                                          "attributes");
      assertNotNull(tmpNode2);
      assertEquals(2, tmpNode2.getAttributes().getLength());
      assertEquals(XML.XSD_NS + ":string", XMLUtil.getStringAttr(tmpNode2, "type"));
      assertFalse(tmpNode2.hasChildNodes());

      tmpNode2 = XMLUtil.findChildElement(tmpNode.getChildNodes().item(0).getChildNodes().item(0),
                                          "element",
                                          "name",
                                          "where");
      assertNotNull(tmpNode2);
      assertEquals(2, tmpNode2.getAttributes().getLength());
      assertEquals(XML.XSD_NS + ":string", XMLUtil.getStringAttr(tmpNode2, "type"));
      assertFalse(tmpNode2.hasChildNodes());

      tmpNode2 = XMLUtil.findChildElement(tmpNode.getChildNodes().item(0).getChildNodes().item(0),
                                          "element",
                                          "name",
                                          "orderBy");
      assertNotNull(tmpNode2);
      assertEquals(2, tmpNode2.getAttributes().getLength());
      assertEquals(XML.XSD_NS + ":string", XMLUtil.getStringAttr(tmpNode2, "type"));
      assertFalse(tmpNode2.hasChildNodes());

      tmpNode2 = XMLUtil.findChildElement(tmpNode.getChildNodes().item(0).getChildNodes().item(0),
                                          "element",
                                          "name",
                                          "count");
      assertNotNull(tmpNode2);
      assertEquals(2, tmpNode2.getAttributes().getLength());
      assertEquals(XML.XSD_NS + ":int", XMLUtil.getStringAttr(tmpNode2, "type"));
      assertFalse(tmpNode2.hasChildNodes());

      tmpNode2 = XMLUtil.findChildElement(tmpNode.getChildNodes().item(0).getChildNodes().item(0),
                                          "element",
                                          "name",
                                          "offset");
      assertNotNull(tmpNode2);
      assertEquals(2, tmpNode2.getAttributes().getLength());
      assertEquals(XML.XSD_NS + ":int", XMLUtil.getStringAttr(tmpNode2, "type"));
      assertFalse(tmpNode2.hasChildNodes());

      tmpNode = XMLUtil.findChildElement(schema, "element", "name", XML.BASE_PREFIX + "Change-Response");
      assertNotNull(tmpNode);
      assertEquals(2, tmpNode.getAttributes().getLength());
      assertEquals(XML.TNS_NS + ':' + XML.BASE_PREFIX + "Collection",
                   XMLUtil.getStringAttr(tmpNode, "type"));
      assertFalse(tmpNode.hasChildNodes());

      tmpNode = XMLUtil.findChildElement(schema, "element", "name", XML.BASE_PREFIX + "Read-Response");
      assertNotNull(tmpNode);
      assertEquals(2, tmpNode.getAttributes().getLength());
      assertEquals(XML.TNS_NS + ':' + XML.BASE_PREFIX + "Collection",
                   XMLUtil.getStringAttr(tmpNode, "type"));
      assertFalse(tmpNode.hasChildNodes());
   }

   /**
    * validate types section
    * @param parent the parent node
    * @param metaList A list of metaclasses defined in schema.
    */
   private void validateTypes(Node parent, List/*<Metaclass>*/ metaList)
   {
      Node types = XMLUtil.findChildElement(parent, "types");

      assertNotNull(types);
      assertFalse(types.hasAttributes());
      assertEquals(1 + ((metaList == null) ? 0 : metaList.size()), // an extra section per metaclass
                   types.getChildNodes().getLength());

      validateTNSSchema(XMLUtil.findChildElement(types, "targetNamespace", XML.NS_URI_TNS, false));

      for (int i = 0, nCount = (metaList == null) ? 0 : metaList.size(); i < nCount; ++i)
      {
         Metaclass meta = (Metaclass)metaList.get(i);
         Node schema = XMLUtil.findChildElement(
            types, "targetNamespace", XML.NS_URI_TNS + '/' + meta.getName(), false);

         validateMetaclassSchema(schema, meta);
      }
   }
}