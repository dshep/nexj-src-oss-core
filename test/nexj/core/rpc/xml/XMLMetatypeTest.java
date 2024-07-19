// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.xml;

import java.io.IOException;
import java.io.StringWriter;

import javax.xml.namespace.QName;

import nexj.core.util.XMLWriter;

import junit.framework.TestCase;

public class XMLMetatypeTest extends TestCase
{
   public void testExtension() throws IOException
   {
      StringWriter writer;

      writer = new StringWriter();
      new XMLMetatype(XML.getTNSType("type"), new XMLMetatype.XMLMetatypeExtension("name", "ns"))
         .writeXSD(new XMLWriter(writer));
      assertEquals("<complexType name=\"type\"><complexContent><extension base=\"ns:name\"></ext" +
                   "ension></complexContent></complexType>",
                   writer.toString());

      writer = new StringWriter();
      new XMLMetatype(XML.getTNSType("type"), new XMLMetatype.XMLMetatypeExtension("nameEmpty", ""))
         .writeXSD(new XMLWriter(writer));
      assertEquals("<complexType name=\"type\"><complexContent><extension base=\"nameEmpty\"></e" +
                   "xtension></complexContent></complexType>",
                   writer.toString());

      writer = new StringWriter();
      new XMLMetatype(
         XML.getTNSType("type"), new XMLMetatype.XMLMetatypeExtension("nameNULL", null))
             .writeXSD(new XMLWriter(writer));
      assertEquals("<complexType name=\"type\"><complexContent><extension base=\"nameNULL\"></ex" +
                   "tension></complexContent></complexType>",
                   writer.toString());
   }

   public void testRestriction() throws IOException
   {
      StringWriter writer;

      writer = new StringWriter();
      new XMLMetatype(XML.getTNSType("type"), new XMLMetatype.XMLMetatypeRestriction("name", "ns"))
         .writeXSD(new XMLWriter(writer));
      assertEquals("<complexType name=\"type\"><complexContent><restriction base=\"ns:name\"></r" +
                   "estriction></complexContent></complexType>",
                   writer.toString());

      writer = new StringWriter();
      new XMLMetatype(
         XML.getTNSType("type"), new XMLMetatype.XMLMetatypeRestriction("nameEmpty", ""))
             .writeXSD(new XMLWriter(writer));
      assertEquals("<complexType name=\"type\"><complexContent><restriction base=\"nameEmpty\"><" +
                   "/restriction></complexContent></complexType>",
                   writer.toString());

      writer = new StringWriter();
      new XMLMetatype(
         XML.getTNSType("type"), new XMLMetatype.XMLMetatypeRestriction("nameNULL", null))
             .writeXSD(new XMLWriter(writer));
      assertEquals("<complexType name=\"type\"><complexContent><restriction base=\"nameNULL\"></" +
                   "restriction></complexContent></complexType>",
                   writer.toString());
      
      
   }

   public void testAbstract() throws IOException
   {
      StringWriter writer;

      writer = new StringWriter();
      new XMLMetatype(XML.getTNSType("type"), true).writeXSD(new XMLWriter(writer));
      assertEquals("<complexType name=\"type\" abstract=\"true\"></complexType>",
                   writer.toString());

      writer = new StringWriter();
      new XMLMetatype(XML.getTNSType("type"), null).writeXSD(new XMLWriter(writer));
      assertEquals("<complexType name=\"type\"></complexType>", writer.toString());

      writer = new StringWriter();
      new XMLMetatype(XML.getTNSType("type")).writeXSD(new XMLWriter(writer));
      assertEquals("<complexType name=\"type\"></complexType>", writer.toString());
   }

   public void testAttribute() throws IOException
   {
      XMLMetatype type = new XMLMetatype(XML.getTNSType("type"));

      type.addAttribute(new XMLMetatype.XMLMetatypeAttribute("name", "ns"));
      type.addAttribute(new XMLMetatype.XMLMetatypeAttribute("nameNULL", null));
      type.addAttribute(null);
      
      StringWriter writer = new StringWriter();
      type.writeXSD(new XMLWriter(writer));

      assertEquals("<complexType name=\"type\"><attribute ref=\"ns:name\"/><attribute ref=\"name" +
                   "NULL\"/></complexType>",
                   writer.toString());
   }

   public void testElement() throws IOException
   {
      XMLMetatype type = new XMLMetatype(XML.getTNSType("type"));

      type.addElement(new XMLMetatype.XMLMetatypeElement(
         "name1", new QName("", "type", "typeNS"), 1, 1, false));
      type.addElement(new XMLMetatype.XMLMetatypeElement(
         "name2", new QName("", "type", "typeNS"), 1, 1, true));
      type.addElement(new XMLMetatype.XMLMetatypeElement(
         "name3", new QName("", "type", "typeNS"), 0, 1, false));
      type.addElement(new XMLMetatype.XMLMetatypeElement(
         "name4", new QName("", "type", "typeNS"), 1, 2, false));
      type.addElement(new XMLMetatype.XMLMetatypeElement(
         "name5", new QName("", "type", "typeNS"), 1, -1, false));
      type.addElement(new XMLMetatype.XMLMetatypeElement("name6", new QName("type"), 1, 1, false));
      type.addElement(null);

      StringWriter writer = new StringWriter();
      type.writeXSD(new XMLWriter(writer));

      assertEquals("<complexType name=\"type\"><sequence><element name=\"name1\" type=\"typeNS:t" +
                   "ype\"/><element name=\"name2\" type=\"typeNS:type\" nillable=\"true\"/><elem" +
                   "ent name=\"name3\" type=\"typeNS:type\" minOccurs=\"0\"/><element name=\"nam" +
                   "e4\" type=\"typeNS:type\" maxOccurs=\"2\"/><element name=\"name5\" type=\"ty" +
                   "peNS:type\" maxOccurs=\"unbounded\"/><element name=\"name6\" type=\"type\"/>" +
                   "</sequence></complexType>",
                   writer.toString());
   }
}