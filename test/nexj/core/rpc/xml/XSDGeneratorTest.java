// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.xml;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Iterator;

import javax.xml.namespace.QName;

import org.w3c.dom.Document;
import org.w3c.dom.Node;

import nexj.core.meta.Attribute;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Primitive;
import nexj.core.meta.Type;
import nexj.core.util.XMLUtil;
import nexj.core.util.XMLWriter;

import junit.framework.TestCase;

public class XSDGeneratorTest extends TestCase
{
   private static ArrayList/*<Metaclass>*/ s_metaList = new ArrayList/*<Metaclass>*/();
   static
   {
      Metaclass hidden = new Metaclass("TestMetaTypeHidden"); // this class should never show up
      Metaclass grandparent = new Metaclass("TestMetaTypeGrandparent");
      Metaclass parent = new Metaclass("TestMetaTypeParent");
      Metaclass child = new Metaclass("TestMetaTypeChild");
      Metaclass masked = new Metaclass("TestMetaTypeMasked");
      Attribute attr;

      hidden.setVisibility(Metaclass.PROTECTED);

      grandparent.setVisibility(Metaclass.PROTECTED);
      grandparent.setBase(XMLSchemaGenerator.DEFAULT_BASE_TYPE);
      grandparent.setCaption("TestTitleGrandparent");
      grandparent.setDescription("TestDescriptionGrandparent");
      grandparent.addAttribute(attr = new Attribute("TestElementQuaziCompatible"));
      attr.setVisibility(Metaclass.PUBLIC);
      attr.setType(Primitive.LONG);

      grandparent.addDerived(parent);
      parent.setVisibility(Metaclass.PROTECTED);
      parent.setCaption("TestTitleParent");
      parent.setDescription("TestDescriptionParent");
      parent.addAttribute(attr = new Attribute("TestElementIncompatibleParent"));
      attr.setVisibility(Metaclass.PUBLIC);
      attr.setType(Primitive.INTEGER);
      parent.addAttribute(attr = new Attribute("TestElementPublicParent"));
      attr.setCompatible(true);
      attr.setVisibility(Metaclass.PUBLIC);
      attr.setType(Primitive.STRING);
      attr.setCaption("TestTitleAttrParent");
      attr.setDescription("TestDescriptionAttrParent");
      parent.addAttribute(attr = new Attribute("TestElementProtectedParent"));
      attr.setCompatible(true);
      attr.setVisibility(Metaclass.PROTECTED);
      attr.setType(Primitive.BINARY);

      parent.addDerived(child);
      child.setVisibility(Metaclass.PUBLIC);
      child.setCaption("TestTitleChild");
      child.setDescription("TestDescriptionChild");
      child.addAttribute(attr = new Attribute("TestElementProtectedChild"));
      attr.setCompatible(true);
      attr.setVisibility(Metaclass.PROTECTED);
      attr.setType(Primitive.TIMESTAMP);
      child.addAttribute(attr = new Attribute("TestElementQuaziCompatible")); // from grandparent
      attr.setVisibility(Metaclass.PUBLIC);
      attr.setType(Primitive.LONG);
      attr.setCompatible(true);

      masked.setVisibility(Metaclass.PUBLIC);
      masked.addAttribute(attr = new Attribute("TestAttribute"));
      attr.setVisibility(Metaclass.PUBLIC);
      attr.setType(Primitive.DOUBLE);
      attr.setCompatible(true);

      hidden.resolveInheritance();
      grandparent.resolveInheritance();
      parent.resolveInheritance();
      child.resolveInheritance();

      s_metaList.add(hidden);
      s_metaList.add(grandparent);
      s_metaList.add(parent);
      s_metaList.add(child);
      s_metaList.add(masked);
   }

   public void testGenerator() throws IOException
   {
      StringWriter writer = new StringWriter();
      XSDGenerator gen = new XSDGenerator();

      gen.setCompatible(false);
      gen.setIncludeDocumentation(true);

      writer.getBuffer().setLength(0);
      gen.generate(new XMLWriter(writer), s_metaList.iterator(), "baseURI");
      validateDocument(XMLUtil.parse(new StringReader(writer.toString())), "baseURI", true, false);

      writer.getBuffer().setLength(0);
      gen.generate(new XMLWriter(writer), s_metaList.iterator(), null);
      validateDocument(XMLUtil.parse(new StringReader(writer.toString())), "", true, false);

      gen.setCompatible(true);

      writer.getBuffer().setLength(0);
      gen.generate(new XMLWriter(writer), s_metaList.iterator(), null);
      validateDocument(XMLUtil.parse(new StringReader(writer.toString())), "", false, false);

      gen.setMask(new String[]{"TestMetaTypeMasked"});
      writer.getBuffer().setLength(0);
      gen.generate(new XMLWriter(writer), s_metaList.iterator(), null);
      validateDocument(XMLUtil.parse(new StringReader(writer.toString())), "", false, true);
   }

   /**
    * validate document
    * @param doc document to validate
    * @param sURI the location of the web service
    * @param bAllMembers Include all members irrespective if they are compatible or not.
    */
   private void validateDocument(Document doc, String sURI, boolean bAllMembers, boolean bMasked)
   {
      Node tmpNode;
      Node annotation, complexContent, complexType, documentation, element, extension, sequence;
      XMLMetatype type;
      long nNumTypes = 0;

      assertNotNull(doc.getFirstChild()); // there is a definition generated
      assertEquals(1, doc.getChildNodes().getLength()); // only one definitions section

      // Check attributes for outer node
      Node schema = doc.getFirstChild();
      assertNotNull(schema);
      assertEquals("schema", schema.getNodeName());
      assertEquals(5, schema.getAttributes().getLength());
      assertEquals("http://www.w3.org/2001/XMLSchema",
                   schema.getAttributes().getNamedItem("xmlns").getNodeValue());
      assertEquals("http://www.w3.org/2001/XMLSchema",
                   schema.getAttributes().getNamedItem("xmlns:" + XML.XSD_NS).getNodeValue());
      assertEquals(XML.NS_URI_TNS, schema.getAttributes().getNamedItem("xmlns:ns").getNodeValue());
      assertEquals("qualified",
                   schema.getAttributes().getNamedItem("elementFormDefault").getNodeValue());
      assertEquals(XML.NS_URI_TNS,
                   schema.getAttributes().getNamedItem("targetNamespace").getNodeValue());

      // validate imports
      ++nNumTypes;
      tmpNode = schema.getFirstChild();
      assertNotNull(tmpNode);
      assertEquals("import", tmpNode.getNodeName());
      assertEquals(1, tmpNode.getAttributes().getLength());
      assertEquals(XML.NS_URI_XML, XMLUtil.getStringAttr(tmpNode, "namespace"));

      // validate hardcoded array types for XSD types
      for (int i = 0; i <= Primitive.MAX_COUNT; ++i)
      {
         Type pType = Primitive.get(i);
         QName qType = XML.getQualifiedType(pType);

         if (XML.XSD_URI != qType.getNamespaceURI())
         {
            continue; // only XSD types should have hardcoded array types generated
         }

         // validate hardcoded required array metatypes
         ++nNumTypes;
         tmpNode = XMLUtil.findChildElement(
            schema, "complexType", "name", qType.getLocalPart() + XML.ARRAY_SUFFIX);
         assertNotNull(tmpNode);
         assertEquals(1, tmpNode.getAttributes().getLength());
         assertEquals(1, tmpNode.getChildNodes().getLength());

         tmpNode = XMLUtil.findChildElement(tmpNode, "sequence");
         assertNotNull(tmpNode);
         assertEquals(0, tmpNode.getAttributes().getLength());
         assertEquals(1, tmpNode.getChildNodes().getLength());

         tmpNode = XMLUtil.findChildElement(tmpNode, "element");
         assertNotNull(tmpNode);
         assertEquals(5, tmpNode.getAttributes().getLength());
         assertEquals("item", XMLUtil.getStringAttr(tmpNode, "name"));
         assertEquals(XML.XSD_NS + ':' + qType.getLocalPart(),
                      XMLUtil.getStringAttr(tmpNode, "type"));
         assertEquals("0", XMLUtil.getStringAttr(tmpNode, "minOccurs"));
         assertEquals("unbounded", XMLUtil.getStringAttr(tmpNode, "maxOccurs"));
         assertEquals("true", XMLUtil.getStringAttr(tmpNode, "nillable"));
         assertEquals(0, tmpNode.getChildNodes().getLength());
      }

      // validate all custom metatypes are present
      for (Iterator/*<XMLMetatype>*/ itr = XSDGenerator.s_xmlMetatypeList.iterator();
           itr.hasNext();)
      {
         ++nNumTypes;
         type = (XMLMetatype)itr.next();
         tmpNode = XMLUtil.findChildElement(schema, "complexType", "name", type.getName());
         assertNotNull(tmpNode);
         assertEquals(type.getName(),
                      (type.m_bAbstract) ? 2 : 1,
                      tmpNode.getAttributes().getLength());

         if (type.m_bAbstract)
         {
            assertEquals(type.getName(), "true", XMLUtil.getStringAttr(tmpNode, "abstract"));
         }

         if (tmpNode.hasChildNodes())
         {
            assertEquals(type.getName(), 1, tmpNode.getChildNodes().getLength());

            // has to be either an extension or more elements
            assertTrue(XMLUtil.findChildElement(tmpNode, "complexContent") != null ||
                       XMLUtil.findChildElement(tmpNode, "sequence") != null);
         }
      }

      // validate masked test metatype
      ++nNumTypes;
      complexType = XMLUtil.findChildElement(schema, "complexType", "name", "TestMetaTypeMasked");
      assertNotNull(complexType);
      assertEquals(2, complexType.getChildNodes().getLength());

         annotation = XMLUtil.findChildElement(complexType, "annotation");
         assertNotNull(annotation);
         assertFalse(annotation.hasAttributes());
         assertEquals(1, annotation.getChildNodes().getLength());
            documentation = XMLUtil.findChildElement(annotation, "documentation");
            assertNotNull(documentation);
            assertEquals(1, documentation.getAttributes().getLength());
            assertEquals("en", XMLUtil.getStringAttr(documentation, "xml:lang"));
            assertEquals("Test Meta Type Masked", documentation.getTextContent());

         complexContent = XMLUtil.findChildElement(complexType, "complexContent");
         assertNotNull(complexContent);
         assertFalse(complexContent.hasAttributes());
         assertEquals(1, complexContent.getChildNodes().getLength());

            extension = XMLUtil.findChildElement(complexContent, "extension");
            assertNotNull(extension);
            assertEquals(1, extension.getAttributes().getLength());
            assertEquals("ns:" + XMLSchemaGenerator.DEFAULT_BASE_TYPE.getName(),
                         XMLUtil.getStringAttr(extension, "base"));
            assertEquals(1, extension.getChildNodes().getLength());

               sequence = XMLUtil.findChildElement(extension, "sequence");
               assertNotNull(sequence);
               assertFalse(sequence.hasAttributes());
               assertEquals(1, sequence.getChildNodes().getLength());

                  element = XMLUtil.findChildElement(sequence, "element", "name", "TestAttribute");
                  assertNotNull(element);
                  assertEquals(3, element.getAttributes().getLength());
                  assertEquals(XML.XSD_NS + ":double", XMLUtil.getStringAttr(element, "type"));
                  assertEquals("0", XMLUtil.getStringAttr(element, "minOccurs"));
                  assertEquals(1, element.getChildNodes().getLength());

                     annotation = XMLUtil.findChildElement(element, "annotation");
                     assertNotNull(annotation);
                     assertFalse(annotation.hasAttributes());
                     assertEquals(1, annotation.getChildNodes().getLength());

                        documentation = XMLUtil.findChildElement(annotation, "documentation");
                        assertNotNull(documentation);
                        assertEquals(1, documentation.getAttributes().getLength());
                        assertEquals("en", XMLUtil.getStringAttr(documentation, "xml:lang"));
                        assertEquals("TestAttribute", documentation.getTextContent());

      if (bMasked)
      {
         assertEquals(nNumTypes, schema.getChildNodes().getLength());

         return; // all other elements not part of mask
      }

      // validate grandparent metatype
      ++nNumTypes;
      complexType =
         XMLUtil.findChildElement(schema, "complexType", "name", "TestMetaTypeGrandparent");
      assertNotNull(complexType);
      assertEquals(2, complexType.getAttributes().getLength());
      assertEquals("true", XMLUtil.getStringAttr(complexType, "abstract"));
      assertEquals(2, complexType.getChildNodes().getLength());
         annotation = XMLUtil.findChildElement(complexType, "annotation");
         assertNotNull(annotation);
         assertFalse(annotation.hasAttributes());
         assertEquals(1, annotation.getChildNodes().getLength());
            documentation = XMLUtil.findChildElement(annotation, "documentation");
            assertNotNull(documentation);
            assertEquals(1, documentation.getAttributes().getLength());
            assertEquals("en", XMLUtil.getStringAttr(documentation, "xml:lang"));
            assertEquals("TestTitleGrandparent\nTestDescriptionGrandparent",
                         documentation.getTextContent());

         complexContent = XMLUtil.findChildElement(complexType, "complexContent");
         assertNotNull(complexContent);
         assertFalse(complexContent.hasAttributes());
         assertEquals(1, complexContent.getChildNodes().getLength());
            extension = XMLUtil.findChildElement(complexContent, "extension");
            assertNotNull(extension);
            assertEquals(1, extension.getAttributes().getLength());
            assertEquals("ns:" + XMLSchemaGenerator.DEFAULT_BASE_TYPE.getName(),
                         XMLUtil.getStringAttr(extension, "base"));
            assertEquals(1, extension.getChildNodes().getLength());
               sequence = XMLUtil.findChildElement(extension, "sequence");
               assertNotNull(sequence);
               assertFalse(sequence.hasAttributes());

               if (!bAllMembers)
               {
                  assertFalse(sequence.hasChildNodes());
               }
               else
               {
                  assertEquals(1, sequence.getChildNodes().getLength());
                     element = XMLUtil.findChildElement(sequence,
                                                        "element",
                                                        "name",
                                                        "TestElementQuaziCompatible");
                     assertNotNull(element);
                     assertEquals(3, element.getAttributes().getLength());
                     assertEquals(XML.XSD_NS + ":long", XMLUtil.getStringAttr(element, "type"));
                     assertEquals("0", XMLUtil.getStringAttr(element, "minOccurs"));
                     assertEquals(1, element.getChildNodes().getLength());
                        annotation = XMLUtil.findChildElement(element, "annotation");
                        assertNotNull(annotation);
                        assertFalse(annotation.hasAttributes());
                        assertEquals(1, annotation.getChildNodes().getLength());
                           documentation = XMLUtil.findChildElement(annotation, "documentation");
                           assertNotNull(documentation);
                           assertEquals(1, documentation.getAttributes().getLength());
                           assertEquals("en", XMLUtil.getStringAttr(documentation, "xml:lang"));
                           assertEquals("test element quazi compatible",
                                        documentation.getTextContent());
               }

      // validate parent test metatype
      ++nNumTypes;
      complexType = XMLUtil.findChildElement(schema, "complexType", "name", "TestMetaTypeParent");
      assertNotNull(complexType);
      assertEquals(2, complexType.getAttributes().getLength());
      assertEquals("true", XMLUtil.getStringAttr(complexType, "abstract"));
      assertEquals(2, complexType.getChildNodes().getLength());
         annotation = XMLUtil.findChildElement(complexType, "annotation");
         assertNotNull(annotation);
         assertFalse(annotation.hasAttributes());
         assertEquals(1, annotation.getChildNodes().getLength());
            documentation = XMLUtil.findChildElement(annotation, "documentation");
            assertNotNull(documentation);
            assertEquals(1, documentation.getAttributes().getLength());
            assertEquals("en", XMLUtil.getStringAttr(documentation, "xml:lang"));
            assertEquals("TestTitleParent\nTestDescriptionParent", documentation.getTextContent());

         complexContent = XMLUtil.findChildElement(complexType, "complexContent");
         assertNotNull(complexContent);
         assertFalse(complexContent.hasAttributes());
         assertEquals(1, complexContent.getChildNodes().getLength());
            extension = XMLUtil.findChildElement(complexContent, "extension");
            assertNotNull(extension);
            assertEquals(1, extension.getAttributes().getLength());
            assertEquals("ns:TestMetaTypeGrandparent",
                         XMLUtil.getStringAttr(extension, "base"));
            assertEquals(1, extension.getChildNodes().getLength());
               sequence = XMLUtil.findChildElement(extension, "sequence");
               assertNotNull(sequence);
               assertFalse(sequence.hasAttributes());
               assertEquals((bAllMembers) ? 2 : 1, sequence.getChildNodes().getLength());
                  element = XMLUtil.findChildElement(sequence,
                                                     "element",
                                                     "name",
                                                     "TestElementPublicParent");
                  assertNotNull(element);
                  assertEquals(3, element.getAttributes().getLength());
                  assertEquals(XML.XSD_NS + ":string", XMLUtil.getStringAttr(element, "type"));
                  assertEquals("0", XMLUtil.getStringAttr(element, "minOccurs"));
                  assertEquals(1, element.getChildNodes().getLength());
                     annotation = XMLUtil.findChildElement(element, "annotation");
                     assertNotNull(annotation);
                     assertFalse(annotation.hasAttributes());
                     assertEquals(1, annotation.getChildNodes().getLength());
                        documentation = XMLUtil.findChildElement(annotation, "documentation");
                        assertNotNull(documentation);
                        assertEquals(1, documentation.getAttributes().getLength());
                        assertEquals("en", XMLUtil.getStringAttr(documentation, "xml:lang"));
                        assertEquals("TestTitleAttrParent\nTestDescriptionAttrParent",
                                     documentation.getTextContent());

            if (bAllMembers) // if incompatible members should also have been generated
            {
               element = XMLUtil.findChildElement(sequence,
                                                  "element",
                                                  "name",
                                                  "TestElementIncompatibleParent");
               assertNotNull(element);
               assertEquals(3, element.getAttributes().getLength());
               assertEquals(XML.XSD_NS + ":int", XMLUtil.getStringAttr(element, "type"));
               assertEquals("0", XMLUtil.getStringAttr(element, "minOccurs"));
                  assertEquals(1, element.getChildNodes().getLength());
                     annotation = XMLUtil.findChildElement(element, "annotation");
                     assertNotNull(annotation);
                     assertFalse(annotation.hasAttributes());
                     assertEquals(1, annotation.getChildNodes().getLength());
                        documentation = XMLUtil.findChildElement(annotation, "documentation");
                        assertNotNull(documentation);
                        assertEquals(1, documentation.getAttributes().getLength());
                        assertEquals("en", XMLUtil.getStringAttr(documentation, "xml:lang"));
                        assertEquals("test element incompatible parent",
                                     documentation.getTextContent());
            }

      // validate child test metatype
      ++nNumTypes;
      complexType = XMLUtil.findChildElement(schema, "complexType", "name", "TestMetaTypeChild");
      assertNotNull(complexType);
      assertEquals(1, complexType.getAttributes().getLength());
      assertEquals(2, complexType.getChildNodes().getLength());
         annotation = XMLUtil.findChildElement(complexType, "annotation");
         assertNotNull(annotation);
         assertFalse(annotation.hasAttributes());
         assertEquals(1, annotation.getChildNodes().getLength());
            documentation = XMLUtil.findChildElement(annotation, "documentation");
            assertNotNull(documentation);
            assertEquals(1, documentation.getAttributes().getLength());
            assertEquals("en", XMLUtil.getStringAttr(documentation, "xml:lang"));
            assertEquals("TestTitleChild\nTestDescriptionChild", documentation.getTextContent());

         complexContent = XMLUtil.findChildElement(complexType, "complexContent");
         assertNotNull(complexContent);
         assertFalse(complexContent.hasAttributes());
         assertEquals(1, complexContent.getChildNodes().getLength());
            extension = XMLUtil.findChildElement(complexContent, "extension");
            assertNotNull(extension);
            assertEquals(1, extension.getAttributes().getLength());
            assertEquals("ns:TestMetaTypeParent", XMLUtil.getStringAttr(extension, "base"));
            assertEquals(1, extension.getChildNodes().getLength());
               sequence = XMLUtil.findChildElement(extension, "sequence");
               assertNotNull(sequence);
               assertFalse(sequence.hasAttributes());

               if (bAllMembers)
               {
                  assertFalse(sequence.hasChildNodes());
               }
               else
               {
                  assertEquals(1, sequence.getChildNodes().getLength());
                     element = XMLUtil.findChildElement(sequence,
                                                        "element",
                                                        "name",
                                                        "TestElementQuaziCompatible");
                     assertNotNull(element);
                     assertEquals(3, element.getAttributes().getLength());
                     assertEquals(XML.XSD_NS + ":long", XMLUtil.getStringAttr(element, "type"));
                     assertEquals("0", XMLUtil.getStringAttr(element, "minOccurs"));
                     assertEquals(1, element.getChildNodes().getLength());
                        annotation = XMLUtil.findChildElement(element, "annotation");
                        assertNotNull(annotation);
                        assertFalse(annotation.hasAttributes());
                        assertEquals(1, annotation.getChildNodes().getLength());
                           documentation = XMLUtil.findChildElement(annotation, "documentation");
                           assertNotNull(documentation);
                           assertEquals(1, documentation.getAttributes().getLength());
                           assertEquals("en", XMLUtil.getStringAttr(documentation, "xml:lang"));
                           assertEquals("test element quazi compatible",
                                        documentation.getTextContent());
               }

      assertEquals(nNumTypes, schema.getChildNodes().getLength());
   }
}