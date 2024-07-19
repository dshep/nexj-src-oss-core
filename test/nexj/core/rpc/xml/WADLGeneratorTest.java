// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.xml;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.xml.namespace.QName;

import junit.framework.TestCase;

import org.w3c.dom.Document;
import org.w3c.dom.Node;

import nexj.core.meta.Argument;
import nexj.core.meta.Attribute;
import nexj.core.meta.Event;
import nexj.core.meta.Member;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Primitive;
import nexj.core.meta.Selector;
import nexj.core.meta.Type;
import nexj.core.util.XMLUtil;
import nexj.core.util.XMLWriter;

public class WADLGeneratorTest extends TestCase
{
   private static Metaclass s_metaclass; // the only class that should show up
   private static ArrayList/*<Metaclass>*/ s_metaList = new ArrayList/*<Metaclass>*/();
   static
   {
      Metaclass hidden = new Metaclass("TestMetaTypeHidden"); // this class should never show up
      Metaclass parent = new Metaclass("TestMetaTypeParent");
      Metaclass child = new Metaclass("TestMetaTypeChild");

      hidden.setVisibility(Metaclass.PROTECTED);

      parent.setVisibility(Metaclass.PROTECTED);
      parent.setBase(XMLSchemaGenerator.DEFAULT_BASE_TYPE);
      parent.setCaption("TestTitleParent");
      parent.setDescription("TestDescriptionParent");
      Attribute attr;
      parent.addAttribute(attr = new Attribute("TestElementPublicParent"));
      attr.setVisibility(Metaclass.PUBLIC);
      attr.setType(Primitive.STRING);
      attr.setCaption("TestTitleAttrParent");
      attr.setDescription("TestDescriptionAttrParent");
      parent.addAttribute(attr = new Attribute("TestElementProtectedParent"));
      attr.setVisibility(Metaclass.PROTECTED);
      attr.setType(Primitive.BINARY);
      Event event;
      parent.addEvent(event = new Event("TestElementPublicEvent"));
      event.setVisibility(Metaclass.PUBLIC);
      event.addArgument(new Argument("TestElementPublicEventArgument"));
      parent.addEvent(event = new Event("TestElementPublicEventStatic"));
      event.setVisibility(Metaclass.PUBLIC);
      event.setStatic(true);
      event.addArgument(new Argument("TestElementPublicEventArgumentStatic"));
      parent.addEvent(event = new Event("TestElementPublicEventTransaction"));
      event.setVisibility(Metaclass.PUBLIC);
      event.setTransactionMode(Event.TX_REQUIRED);
      event.addArgument(new Argument("TestElementPublicEventArgumentTransaction"));
      parent.addEvent(event = new Event("TestElementProtectedEvent"));
      event.setVisibility(Metaclass.PROTECTED);
      event.addArgument(new Argument("TestElementProtectedEventArgument"));

      child.setVisibility(Metaclass.PUBLIC);
      child.setBase(parent);
      child.setCaption("TestTitleChild");
      child.setDescription("TestDescriptionChild");
      child.addAttribute(attr = new Attribute("TestElementProtectedChild"));
      attr.setVisibility(Metaclass.PROTECTED);
      attr.setType(Primitive.TIMESTAMP);

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
      child.addEvent(typed);
      child.addEvent(untyped);

      s_metaclass = child;
      s_metaList.add(hidden);
      s_metaList.add(parent);
      s_metaList.add(child);
   }

   public void testGenerator() throws IOException
   {
      StringWriter writer = new StringWriter();
      WADLGenerator gen = new WADLGenerator();

      gen.setCompatible(false);
      writer.getBuffer().setLength(0);
      gen.generate(new XMLWriter(writer), s_metaList.iterator(), "baseURI");
      validateDocument(XMLUtil.parse(new StringReader(writer.toString())), "baseURI");

      writer.getBuffer().setLength(0);
      gen.generate(new XMLWriter(writer), s_metaList.iterator(), null);
      validateDocument(XMLUtil.parse(new StringReader(writer.toString())), null);
   }

   /**
    * validate document
    * @param doc document to validate
    * @param sURI the location of the web service
    */
   private void validateDocument(Document doc, String sURI)
   {
      assertNotNull(doc.getFirstChild()); // there is a definition generated
      assertEquals(1, doc.getChildNodes().getLength()); // only one definitions section

      // Check attributes for outer node
      Node definitions = doc.getFirstChild();
      assertEquals("application", definitions.getNodeName());
      assertEquals(5, definitions.getAttributes().getLength());
      assertEquals(XML.NS_URI_WADL, XMLUtil.getStringAttr(definitions, XML.XML_NS));
      assertEquals(XML.NS_URI_TNS,
                   XMLUtil.getStringAttr(definitions, XML.XML_NS + ':' + XML.TNS_NS));
      assertEquals(XML.NS_URI_WADL,XMLUtil.getStringAttr(definitions, XML.XML_NS + ":wadl"));
      assertEquals("http://www.w3.org/2001/XMLSchema",
                   XMLUtil.getStringAttr(definitions, XML.XML_NS + ':' + XML.XSD_NS));
      assertEquals(XML.NS_URI_TNS, XMLUtil.getStringAttr(definitions, "targetNamespace"));
      assertEquals(5, definitions.getChildNodes().getLength());

      validateGrammars(definitions); // check grammars section
      validateResources(definitions, sURI); // check resources section
      validateParameterAuthorization(definitions); // check parameter with "authorization" id
      validateMethodReadRequest(definitions); // check method with "read_request" id
      validateFaultException(definitions); // check fault with "exception" id
   }

   /**
    * Validate event invocation method definition.
    * @param method The method node to validate.
    * @param event The event to validate against.
    * @param sName The method name (not null).
    */
   private void validateEventMethod(Node method, Event event, String sName)
   {
      assertNotNull(method);
      assertEquals(1, method.getAttributes().getLength());
      assertEquals(sName, XMLUtil.getStringAttr(method, "name"));
      assertEquals(2, method.getChildNodes().getLength());

      Node request = XMLUtil.findChildElement(method, "request");

      assertNotNull(request);
      assertEquals(0, request.getAttributes().getLength());
      assertEquals(event.getArgumentCount(), request.getChildNodes().getLength());

      for (int i = 0, nArgCount = event.getArgumentCount(); i < nArgCount; ++i)
      {
         Argument arg = event.getArgument(i);
         Node param = XMLUtil.findChildElement(request, "param", "name", arg.getName());
         boolean bVarArg = event.isVarArg() && i == nArgCount - 1;
         QName type = XML.getQualifiedType(arg.getType(), bVarArg);
         int nAttrCount = 3;

         assertNotNull(param);
         assertEquals("query", XMLUtil.getStringAttr(param, "style"));
         assertEquals(type.getPrefix() + ':' + type.getLocalPart(),
                      XMLUtil.getStringAttr(param, "type"));

         if (arg.isRequired())
         {
            assertEquals("true", XMLUtil.getStringAttr(param, "required"));
            ++nAttrCount;
         }

         if (arg.isCollection())
         {
            assertEquals("true", XMLUtil.getStringAttr(param, "repeating"));
            ++nAttrCount;
         }

         assertEquals(nAttrCount, param.getAttributes().getLength());
         assertEquals(0, param.getChildNodes().getLength()); // test data does not have <doc/>
      }

      // below here validate response
      Node response = XMLUtil.findChildElement(method, "response");

      assertNotNull(response);
      assertEquals(0, response.getAttributes().getLength());
      assertEquals((event.getResult() == null) ? 1 : 2, response.getChildNodes().getLength());

      if (event.getResult() != null)
      {
         Node representation = XMLUtil.findChildElement(response, "representation");
         Type type = event.getResult().getType();

         assertNotNull(representation);
         assertEquals(2, representation.getAttributes().getLength());
         assertEquals("text/xml", XMLUtil.getStringAttr(representation, "mediaType"));
         assertEquals(0, representation.getChildNodes().getLength()); // test data lacks <doc/>

         QName qType = XML.getQualifiedType(type, event.getResult().isCollection());

         if (type instanceof Primitive)
         {
            assertEquals(XML.TNS_NS + ':' + XML.BASE_PREFIX + qType.getLocalPart(),
                         XMLUtil.getStringAttr(representation, "element"));
         }
         else
         {
            assertEquals(qType.getPrefix() + ':' + qType.getLocalPart(),
                         XMLUtil.getStringAttr(representation, "element"));
         }
      }

      Node fault = XMLUtil.findChildElement(response, "fault");

      assertNotNull(fault);
      assertEquals(1, fault.getAttributes().getLength());
      assertEquals("#exception", XMLUtil.getStringAttr(fault, "href"));
      assertEquals(0, fault.getChildNodes().getLength());
   }

   /**
    * Validate event resource definitions.
    * @param parent The parent node.
    * @param meta The metaclass events to validate against.
    * @param bStatic validate only static methods (false == validate only non-static methods).
    * @return Number of resource sections validated.
    */
   private int validateEventResources(Node parent, Metaclass meta, boolean bStatic)
   {
      int nCount = 0;

      for (Iterator/*<Selector>*/ selItr = meta.getSelectorIterator(); selItr.hasNext();)
      {
         Selector selector = (Selector)selItr.next();
         Node resource = null;
         int nChildCount = 1;

         for (Iterator/*<Member>*/ memItr = selector.getMemberIterator(); memItr.hasNext();)
         {
            Member member = (Member)memItr.next();

            if (!(member instanceof Event) || bStatic != member.isStatic())
            {
               continue;
            }

            Event event = (Event)member;

            if (resource == null) // first iteration for this selector, <resource> not found yet
            {
               resource = XMLUtil.findChildElement(parent, "resource", "path", member.getName());
               assertNotNull(resource);
               assertEquals(2, resource.getAttributes().getLength());
               assertEquals("text/xml", XMLUtil.getStringAttr(resource, "queryType"));

               Node param = XMLUtil.findChildElement(resource, "param");

               assertNotNull(param);
               assertEquals(1, param.getAttributes().getLength());
               assertEquals("#authorization", XMLUtil.getStringAttr(param, "href"));
               assertEquals(0, param.getChildNodes().getLength());
            }

            List/*<Node>*/ methodList = XMLUtil.findChildElements(resource, "method");
            boolean bFound = false;

            // find the correct method definition based on argument count
            for (int i = 0, nEventCount = methodList.size(); i < nEventCount; ++i)
            {
               Node method = (Node)methodList.get(i);
               Node request = XMLUtil.findChildElement(method, "request");

               if (request.getChildNodes().getLength() == event.getArgumentCount())
               {
                  validateEventMethod(method, event, "POST");
                  bFound = true;
                  break;
               }
            }

            assertTrue(bFound);
            ++nChildCount;
         }

         if (resource != null) // validate resource child count
         {
            assertEquals(nChildCount, resource.getChildNodes().getLength());
            ++nCount;
         }
      }

      return nCount;
   }

   /**
    * validate message invoke-request section
    * @param parent the parent node
    */
   private void validateFaultException(Node parent)
   {
      Node node = XMLUtil.findChildElement(parent, "fault", "id", "exception");
      assertNotNull(node);

      assertEquals(4, node.getAttributes().getLength());
      assertEquals("text/xml", XMLUtil.getStringAttr(node, "mediaType"));
      assertEquals(XML.TNS_NS + ':' + XML.BASE_PREFIX + "Exception",
                   XMLUtil.getStringAttr(node, "element"));
      assertEquals("500", XMLUtil.getStringAttr(node, "status"));
      assertFalse(node.hasChildNodes());
   }

   /**
    * validate grammars section
    * @param parent the parent node
    */
   private void validateGrammars(Node parent)
   {
      Node types = XMLUtil.findChildElement(parent, "grammars");
      assertNotNull(types);
      assertFalse(types.hasAttributes());
      assertEquals(1, types.getChildNodes().getLength()); // only one definitions section

      Node schema = types.getFirstChild();
      assertNotNull(schema);
      assertEquals("schema", schema.getNodeName());
      assertEquals(3, schema.getAttributes().getLength());
      assertEquals("http://www.w3.org/2001/XMLSchema", XMLUtil.getStringAttr(schema, XML.XML_NS));
      assertEquals("qualified", XMLUtil.getStringAttr(schema, "elementFormDefault"));
      assertEquals(XML.NS_URI_TNS, XMLUtil.getStringAttr(schema, "targetNamespace"));

      Node tmpNode = XMLUtil.findChildElement(schema, "element", "name", XML.BASE_PREFIX + "Exception");
      assertNotNull(tmpNode);
      assertEquals(2, tmpNode.getAttributes().getLength());
      assertEquals(XML.TNS_NS + ':' + XML.BASE_PREFIX + "Exception",
                   XMLUtil.getStringAttr(tmpNode, "type"));
      assertFalse(tmpNode.hasChildNodes());

      tmpNode = XMLUtil.findChildElement(schema, "element", "name", XML.BASE_PREFIX + "Request");
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
   }

   /**
    * validate parameter "Authorization"
    * @param parent the parent node
    */
   private void validateParameterAuthorization(Node parent)
   {
      Node node = XMLUtil.findChildElement(parent, "param", "id", "authorization");
      assertNotNull(node);

      assertEquals(3, node.getAttributes().getLength());
      assertEquals("Authorization", XMLUtil.getStringAttr(node, "name"));
      assertEquals("header", XMLUtil.getStringAttr(node, "style"));
      assertFalse(node.hasChildNodes());
   }

   /**
    * Validate metaclass resource definition.
    * @param resource The metaclass resource definition to validate.
    * @param meta The metaclass definition to validate against.
    */
   private void validateMetaclassResource(Node resource, Metaclass meta)
   {
      int nChildCount = 4;

      assertNotNull(resource);
      assertEquals(2, resource.getAttributes().getLength());
      assertEquals("text/xml", XMLUtil.getStringAttr(resource, "queryType"));

      Node param = XMLUtil.findChildElement(resource, "param");

      assertNotNull(param);
      assertEquals(1, param.getAttributes().getLength());
      assertEquals("#authorization", XMLUtil.getStringAttr(param, "href"));
      assertEquals(0, param.getChildNodes().getLength());

      Node method = XMLUtil.findChildElement(resource, "method", "href", "#read-request");

      assertNotNull(method);
      assertEquals(1, method.getAttributes().getLength());
      assertEquals(0, method.getChildNodes().getLength());

      method = XMLUtil.findChildElement(resource, "method", "name", "POST");
      assertNotNull(method);
      assertEquals(1, method.getAttributes().getLength());
      assertEquals(2, method.getChildNodes().getLength());

      Node tmpNode = XMLUtil.findChildElement(method, "request");

      assertNotNull(tmpNode);
      assertEquals(0, tmpNode.getAttributes().getLength());
      assertEquals(2, tmpNode.getChildNodes().getLength());
      assertEquals("param", tmpNode.getFirstChild().getNodeName());
      assertEquals("representation", tmpNode.getChildNodes().item(1).getNodeName());
      assertEquals(2, tmpNode.getChildNodes().item(1).getAttributes().getLength());
      assertEquals("text/xml", XMLUtil.getStringAttr(tmpNode.getChildNodes().item(1), "mediaType"));
      assertEquals(XML.TNS_NS + ":TestMetaTypeChild",
                   XMLUtil.getStringAttr(tmpNode.getChildNodes().item(1), "element"));
      assertEquals(0, tmpNode.getChildNodes().item(1).getChildNodes().getLength());

      tmpNode = XMLUtil.findChildElement(method, "response");
      assertNotNull(tmpNode);
      assertEquals(0, tmpNode.getAttributes().getLength());
      assertEquals(3, tmpNode.getChildNodes().getLength());

      Node tmpNode2 = XMLUtil.findChildElement(tmpNode, "representation");

      assertNotNull(tmpNode2);
      assertEquals(2, tmpNode2.getAttributes().getLength());
      assertEquals(XML.TNS_NS + ":TestMetaTypeChild", XMLUtil.getStringAttr(tmpNode2, "element"));
      assertEquals(0, tmpNode2.getChildNodes().getLength());

      tmpNode2 = XMLUtil.findChildElement(tmpNode, "fault");
      assertNotNull(tmpNode2);
      assertEquals(1, tmpNode2.getAttributes().getLength());
      assertEquals("#exception", XMLUtil.getStringAttr(tmpNode2, "href"));
      assertEquals(0, tmpNode2.getChildNodes().getLength());

      // validate all static event method definitions
      for (int i = 0, nCount = s_metaclass.getEventCount(); i < nCount; ++i)
      {
         Event event = s_metaclass.getEvent(i);

         if (event.isStatic())
         {
            String sEvent = XML.BASE_PREFIX + event.getName();

            validateEventMethod(
               XMLUtil.findChildElement(resource, "method", "name", sEvent), event, sEvent);
            ++nChildCount;
         }
      }

      nChildCount += validateEventResources(resource, s_metaclass, true); //validate event resources
      assertEquals(nChildCount, resource.getChildNodes().getLength());

      // below here validate non-static OID dependent definitions
      nChildCount = 6;
      resource = XMLUtil.findChildElement(resource, "resource", "path", "{oid}");
      assertNotNull(resource);
      assertEquals(2, resource.getAttributes().getLength());
      assertEquals("text/xml", XMLUtil.getStringAttr(resource, "queryType"));

      param = XMLUtil.findChildElement(resource, "param", "href", "#authorization");
      assertNotNull(param);
      assertEquals(1, param.getAttributes().getLength());
      assertEquals(0, param.getChildNodes().getLength());

      tmpNode = XMLUtil.findChildElement(resource, "param", "name", "oid");
      assertNotNull(tmpNode);
      assertEquals(3, tmpNode.getAttributes().getLength());
      assertEquals("template", XMLUtil.getStringAttr(tmpNode, "style"));
      assertEquals(XML.XSD_NS + ":hexBinary", XMLUtil.getStringAttr(tmpNode, "type"));
      assertEquals(0, tmpNode.getChildNodes().getLength());

      method = XMLUtil.findChildElement(resource, "method", "href", "#read-request");
      assertNotNull(method);
      assertEquals(1, method.getAttributes().getLength());
      assertEquals(0, method.getChildNodes().getLength());

      method = XMLUtil.findChildElement(resource, "method", "name", "POST");
      assertNotNull(method);
      assertEquals(1, method.getAttributes().getLength());
      assertEquals(2, method.getChildNodes().getLength());

      tmpNode = XMLUtil.findChildElement(method, "request");
      assertNotNull(tmpNode);
      assertFalse(tmpNode.hasAttributes());
      assertEquals(2, tmpNode.getChildNodes().getLength());
      assertEquals("param", tmpNode.getFirstChild().getNodeName());
      assertEquals("representation", tmpNode.getChildNodes().item(1).getNodeName());
      assertEquals(2, tmpNode.getChildNodes().item(1).getAttributes().getLength());
      assertEquals("text/xml", XMLUtil.getStringAttr(tmpNode.getChildNodes().item(1), "mediaType"));
      assertEquals(XML.TNS_NS + ":TestMetaTypeChild",
                   XMLUtil.getStringAttr(tmpNode.getChildNodes().item(1), "element"));
      assertEquals(0, tmpNode.getChildNodes().item(1).getChildNodes().getLength());

      tmpNode = XMLUtil.findChildElement(method, "response");
      assertNotNull(tmpNode);
      assertEquals(0, tmpNode.getAttributes().getLength());
      assertEquals(3, tmpNode.getChildNodes().getLength());

      tmpNode2 = XMLUtil.findChildElement(tmpNode, "representation");
      assertNotNull(tmpNode2);
      assertEquals(2, tmpNode2.getAttributes().getLength());
      assertEquals(XML.TNS_NS + ":TestMetaTypeChild", XMLUtil.getStringAttr(tmpNode2, "element"));
      assertEquals(0, tmpNode2.getChildNodes().getLength());

      tmpNode2 = XMLUtil.findChildElement(tmpNode, "fault");
      assertNotNull(tmpNode2);
      assertEquals(1, tmpNode2.getAttributes().getLength());
      assertEquals("#exception", XMLUtil.getStringAttr(tmpNode2, "href"));
      assertEquals(0, tmpNode2.getChildNodes().getLength());

      method = XMLUtil.findChildElement(resource, "method", "name", "PUT");
      assertNotNull(method);
      assertEquals(1, method.getAttributes().getLength());
      assertEquals(2, method.getChildNodes().getLength());

      tmpNode = XMLUtil.findChildElement(method, "request");
      assertNotNull(tmpNode);
      assertFalse(tmpNode.hasAttributes());
      assertEquals(2, tmpNode.getChildNodes().getLength());
      assertEquals("param", tmpNode.getFirstChild().getNodeName());
      assertEquals("representation", tmpNode.getChildNodes().item(1).getNodeName());
      assertEquals(2, tmpNode.getChildNodes().item(1).getAttributes().getLength());
      assertEquals("text/xml", XMLUtil.getStringAttr(tmpNode.getChildNodes().item(1), "mediaType"));
      assertEquals(XML.TNS_NS + ":TestMetaTypeChild",
                   XMLUtil.getStringAttr(tmpNode.getChildNodes().item(1), "element"));
      assertEquals(0, tmpNode.getChildNodes().item(1).getChildNodes().getLength());

      tmpNode = XMLUtil.findChildElement(method, "response");
      assertNotNull(tmpNode);
      assertEquals(0, tmpNode.getAttributes().getLength());
      assertEquals(2, tmpNode.getChildNodes().getLength());

      tmpNode2 = XMLUtil.findChildElement(tmpNode, "representation");
      assertNotNull(tmpNode2);
      assertEquals(2, tmpNode2.getAttributes().getLength());
      assertEquals(XML.TNS_NS + ":TestMetaTypeChild", XMLUtil.getStringAttr(tmpNode2, "element"));
      assertEquals(0, tmpNode2.getChildNodes().getLength());

      tmpNode2 = XMLUtil.findChildElement(tmpNode, "fault");
      assertNotNull(tmpNode2);
      assertEquals(1, tmpNode2.getAttributes().getLength());
      assertEquals("#exception", XMLUtil.getStringAttr(tmpNode2, "href"));
      assertEquals(0, tmpNode2.getChildNodes().getLength());

      // validate all non-static event method definitions
      for (int i = 0, nCount = s_metaclass.getEventCount(); i < nCount; ++i)
      {
         Event event = s_metaclass.getEvent(i);

         if (!event.isStatic())
         {
            String sEvent = XML.BASE_PREFIX + event.getName();

            validateEventMethod(
               XMLUtil.findChildElement(resource, "method", "name", sEvent), event, sEvent);
            ++nChildCount;
         }
      }

      nChildCount += validateEventResources(resource, s_metaclass, false);//validate event resources
      assertEquals(nChildCount, resource.getChildNodes().getLength());
   }

   /**
    * validate method read_request section
    * @param parent the parent node
    */
   private void validateMethodReadRequest(Node parent)
   {
      Node node = XMLUtil.findChildElement(parent, "method", "id", "read-request");
      assertNotNull(node);

      assertEquals(2, node.getAttributes().getLength());
      assertEquals("GET", XMLUtil.getStringAttr(node, "name"));
      assertEquals(2, node.getChildNodes().getLength());

      Node tmpNode = XMLUtil.findChildElement(node, "request");
      assertNotNull(tmpNode);
      assertFalse(tmpNode.hasAttributes());
      assertEquals(5, tmpNode.getChildNodes().getLength());

      Node tmpNode2 = XMLUtil.findChildElement(tmpNode, "param", "name", "attributes");
      assertNotNull(tmpNode2);
      assertEquals(2, tmpNode2.getAttributes().getLength());
      assertEquals("query", XMLUtil.getStringAttr(tmpNode2, "style"));
      assertFalse(tmpNode2.hasChildNodes());

      tmpNode2 = XMLUtil.findChildElement(tmpNode, "param", "name", "where");
      assertNotNull(tmpNode2);
      assertEquals(2, tmpNode2.getAttributes().getLength());
      assertEquals("query", XMLUtil.getStringAttr(tmpNode2, "style"));
      assertFalse(tmpNode2.hasChildNodes());

      tmpNode2 = XMLUtil.findChildElement(tmpNode, "param", "name", "orderBy");
      assertNotNull(tmpNode2);
      assertEquals(2, tmpNode2.getAttributes().getLength());
      assertEquals("query", XMLUtil.getStringAttr(tmpNode2, "style"));
      assertFalse(tmpNode2.hasChildNodes());

      tmpNode2 = XMLUtil.findChildElement(tmpNode, "param", "name", "count");
      assertNotNull(tmpNode2);
      assertEquals(4, tmpNode2.getAttributes().getLength());
      assertEquals("query", XMLUtil.getStringAttr(tmpNode2, "style"));
      assertEquals(XML.XSD_NS + ":int", XMLUtil.getStringAttr(tmpNode2, "type"));
      assertEquals("8", XMLUtil.getStringAttr(tmpNode2, "default"));
      assertFalse(tmpNode2.hasChildNodes());

      tmpNode2 = XMLUtil.findChildElement(tmpNode, "param", "name", "offset");
      assertNotNull(tmpNode2);
      assertEquals(3, tmpNode2.getAttributes().getLength());
      assertEquals("query", XMLUtil.getStringAttr(tmpNode2, "style"));
      assertEquals(XML.XSD_NS + ":int", XMLUtil.getStringAttr(tmpNode2, "type"));
      assertFalse(tmpNode2.hasChildNodes());

      tmpNode = XMLUtil.findChildElement(node, "response");
      assertNotNull(tmpNode);
      assertFalse(tmpNode.hasAttributes());
      assertEquals(2, tmpNode.getChildNodes().getLength());
      
      tmpNode2 = XMLUtil.findChildElement(tmpNode, "representation");
      assertNotNull(tmpNode2);
      assertEquals(2, tmpNode2.getAttributes().getLength());
      assertEquals(XML.TNS_NS + ':' + XML.BASE_PREFIX + "Collection",
                   XMLUtil.getStringAttr(tmpNode2, "element"));
      assertFalse(tmpNode2.hasChildNodes());

      tmpNode2 = XMLUtil.findChildElement(tmpNode, "fault");
      assertNotNull(tmpNode2);
      assertEquals(1, tmpNode2.getAttributes().getLength());
      assertEquals("#exception", XMLUtil.getStringAttr(tmpNode2, "href"));
      assertFalse(tmpNode2.hasChildNodes());
   }

   /**
    * validate resources section
    * @param parent the parent node
    * @param sURI the base of the web service
    */
   private void validateResources(Node parent, String sURI)
   {
      Node resources = XMLUtil.findChildElement(parent, "resources");
      assertNotNull(resources);
      assertEquals(1, resources.getAttributes().getLength());
      assertEquals(sURI, XMLUtil.getStringAttr(resources, "base"));

      // one section for each public metatype + "/"
      assertEquals(2, resources.getChildNodes().getLength());

      Node resource = XMLUtil.findChildElement(resources, "resource");
      assertNotNull(resource);

      // ensure examining element without the path
      assertNull(XMLUtil.getStringAttr(resource, "path"));

      assertEquals(1, resource.getAttributes().getLength());
      assertEquals("text/xml", XMLUtil.getStringAttr(resource, "queryType"));
      assertEquals(3, resource.getChildNodes().getLength());

      Node param = XMLUtil.findChildElement(resource, "param");
      assertNotNull(param);
      assertEquals(1, param.getAttributes().getLength());
      assertEquals("#authorization", XMLUtil.getStringAttr(param, "href"));
      assertFalse(param.hasChildNodes());

      Node method = XMLUtil.findChildElement(resource, "method", "name", "GET");
      assertNotNull(method);
      assertEquals(1, method.getAttributes().getLength());
      assertEquals(2, method.getChildNodes().getLength());
      
      Node tmpNode = XMLUtil.findChildElement(method, "request");
      assertNotNull(tmpNode);
      assertFalse(tmpNode.hasAttributes());
      assertEquals(1, tmpNode.getChildNodes().getLength());
      assertEquals("param", tmpNode.getFirstChild().getNodeName());
      assertEquals(3, tmpNode.getFirstChild().getAttributes().getLength());
      assertEquals("xsd", XMLUtil.getStringAttr(tmpNode.getFirstChild(), "name"));
      assertEquals("query", XMLUtil.getStringAttr(tmpNode.getFirstChild(), "style"));
      assertEquals(XML.XSD_NS + ":boolean", XMLUtil.getStringAttr(tmpNode.getFirstChild(), "type"));
      assertFalse(tmpNode.getFirstChild().hasChildNodes());

      tmpNode = XMLUtil.findChildElement(method, "response");
      assertNotNull(tmpNode);
      assertFalse(tmpNode.hasAttributes());
      assertEquals(1, tmpNode.getChildNodes().getLength());
      assertEquals("representation", tmpNode.getFirstChild().getNodeName());
      assertEquals(1, tmpNode.getFirstChild().getAttributes().getLength());
      assertEquals("application/vnd.sun.wadl+xml",
                   XMLUtil.getStringAttr(tmpNode.getFirstChild(), "mediaType"));
      assertFalse(tmpNode.getFirstChild().hasChildNodes());

      method = XMLUtil.findChildElement(resource, "method", "name", "POST");
      assertNotNull(method);
      assertEquals(1, method.getAttributes().getLength());
      assertEquals(2, method.getChildNodes().getLength());
      
      tmpNode = XMLUtil.findChildElement(method, "request");
      assertNotNull(tmpNode);
      assertFalse(tmpNode.hasAttributes());
      assertEquals(1, tmpNode.getChildNodes().getLength());
      assertEquals("representation", tmpNode.getFirstChild().getNodeName());
      assertEquals(2, tmpNode.getFirstChild().getAttributes().getLength());
      assertEquals("text/xml", XMLUtil.getStringAttr(tmpNode.getFirstChild(), "mediaType"));
      assertEquals(XML.TNS_NS + ':' + XML.BASE_PREFIX + "Request",
                   XMLUtil.getStringAttr(tmpNode.getFirstChild(), "element"));
      assertFalse(tmpNode.getFirstChild().hasChildNodes());

      tmpNode = XMLUtil.findChildElement(method, "response");
      assertNotNull(tmpNode);
      assertFalse(tmpNode.hasAttributes());
      assertEquals(2, tmpNode.getChildNodes().getLength());
      
      Node tmpNode2 = XMLUtil.findChildElement(tmpNode, "representation");
      assertNotNull(tmpNode2);
      assertEquals(2, tmpNode2.getAttributes().getLength());
      assertEquals("text/xml", XMLUtil.getStringAttr(tmpNode2, "mediaType"));
      assertEquals(XML.TNS_NS + ':' + XML.BASE_PREFIX + "Response",
                   XMLUtil.getStringAttr(tmpNode2, "element"));
      assertFalse(tmpNode2.hasChildNodes());

      tmpNode2 = XMLUtil.findChildElement(tmpNode, "fault");
      assertNotNull(tmpNode2);
      assertEquals(1, tmpNode2.getAttributes().getLength());
      assertEquals("#exception", XMLUtil.getStringAttr(tmpNode2, "href"));
      assertFalse(tmpNode2.hasChildNodes());

      validateMetaclassResource(
         XMLUtil.findChildElement(resources, "resource", "path", s_metaclass.getName()),
         s_metaclass);
   }
}