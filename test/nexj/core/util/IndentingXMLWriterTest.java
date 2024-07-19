// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.StringWriter;

import nexj.core.util.IndentingXMLWriter;

import junit.framework.TestCase;

public class IndentingXMLWriterTest extends TestCase
{
   public void testInitialIndent() throws IOException
   {
      StringWriter buf = new StringWriter();
      IndentingXMLWriter out = new IndentingXMLWriter(buf, "\t");

      out.write("Before Start");
      out.startElement("tag");
      
      assertEquals("Before Start<tag>", buf.toString());
   }

   public void testNestedIndent() throws IOException
   {
      StringWriter buf = new StringWriter();
      IndentingXMLWriter writer = new IndentingXMLWriter(buf, "\t");

      writer.write("Before Start");
      writer.startElement("tag");
         writer.startElement("child1");
            writer.startElement("child2");
            writer.endElement("child2");
         writer.endElement("child1");
         writer.startElement("childA");

      assertEquals("Before Start<tag>" +
                   SysUtil.LINE_SEP +
                   "\t<child1>" + 
                   SysUtil.LINE_SEP + 
                   "\t\t<child2>" + 
                   SysUtil.LINE_SEP + 
                   "\t\t</child2>" + 
                   SysUtil.LINE_SEP + 
                   "\t</child1>" + 
                   SysUtil.LINE_SEP + 
                   "\t<childA>",
                   buf.toString());
   }
   
   public void testNestedIndentWithAttr() throws IOException
   {
      StringWriter buf = new StringWriter();
      IndentingXMLWriter writer = new IndentingXMLWriter(buf, "\t");

      writer.write("Before Start");
      writer.openElement("tag");
         writer.write("Attr");
      writer.closeElement();
         writer.openElement("child1");
            writer.write("Attr1");
         writer.closeElement();
            writer.openElement("child2");
               writer.write("Attr2");
            writer.closeElement();
            writer.endElement("child2");
         writer.endElement("child1");
         writer.startElement("childA");

      assertEquals("Before Start<tagAttr>" + 
                   SysUtil.LINE_SEP + 
                   "\t<child1Attr1>" + 
                   SysUtil.LINE_SEP + 
                   "\t\t<child2Attr2>" + 
                   SysUtil.LINE_SEP + 
                   "\t\t</child2>" + 
                   SysUtil.LINE_SEP + 
                   "\t</child1>" + 
                   SysUtil.LINE_SEP + 
                   "\t<childA>",
                   buf.toString());
   }

   public void testNestedInline() throws IOException
   {
      StringWriter buf = new StringWriter();
      IndentingXMLWriter writer = new IndentingXMLWriter(buf, "\t");

      writer.write("Before Start");
      writer.startElement("tag");
         writer.write("Inline");
      writer.endElement("tag");

      assertEquals("Before Start<tag>Inline</tag>", buf.toString());
   }
   
   public void testNestedInlineWithAttr() throws IOException
   {
      StringWriter buf = new StringWriter();
      IndentingXMLWriter writer = new IndentingXMLWriter(buf, "\t");

      writer.write("Before Start");
      writer.openElement("tag");
         writer.write("Attr");
      writer.closeElement();
         writer.write("Inline");
      writer.endElement("tag");

      assertEquals("Before Start<tagAttr>Inline</tag>", buf.toString());
   }
   
   public void testNestedEmpty() throws IOException
   {
      StringWriter buf = new StringWriter();
      IndentingXMLWriter writer = new IndentingXMLWriter(buf, "\t");

      writer.write("Before Start");
      writer.startElement("tag");
         writer.openElement("child1");
            writer.write("Attr");
         writer.closeEmptyElement();
         writer.startElement("child2");

      assertEquals("Before Start<tag>" +
                   SysUtil.LINE_SEP +
                   "\t<child1Attr/>" +
                   SysUtil.LINE_SEP +
                   "\t<child2>",
                   buf.toString());
   }
   
   public void testNestedNullIndent() throws IOException
   {
      StringWriter buf = new StringWriter();
      IndentingXMLWriter writer = new IndentingXMLWriter(buf, null);

      writer.write("Before Start");
      writer.startElement("tag");
         writer.startElement("child1");
            writer.startElement("child2");
            writer.endElement("child2");
         writer.endElement("child1");
         writer.startElement("childA");

      assertEquals("Before Start<tag>" +
                   SysUtil.LINE_SEP +
                   "<child1>" +
                   SysUtil.LINE_SEP +
                   "<child2>" +
                   SysUtil.LINE_SEP +
                   "</child2>" +
                   SysUtil.LINE_SEP +
                   "</child1>" +
                   SysUtil.LINE_SEP +
                   "<childA>",
                   buf.toString());
   }
   
   public void testNestedEmptyIndent() throws IOException
   {
      StringWriter buf = new StringWriter();
      IndentingXMLWriter writer = new IndentingXMLWriter(buf, "");

      writer.write("Before Start");
      writer.startElement("tag");
         writer.startElement("child1");
            writer.startElement("child2");
            writer.endElement("child2");
         writer.endElement("child1");
         writer.startElement("childA");

      assertEquals("Before Start<tag>" +
                   SysUtil.LINE_SEP +
                   "<child1>" +
                   SysUtil.LINE_SEP +
                   "<child2>" +
                   SysUtil.LINE_SEP +
                   "</child2>" +
                   SysUtil.LINE_SEP +
                   "</child1>" +
                   SysUtil.LINE_SEP +
                   "<childA>",
                   buf.toString());
   }
}