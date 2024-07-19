// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.StringWriter;
import java.net.URLEncoder;

import junit.framework.TestCase;

public class URLEncodeTest extends TestCase
{
   private XMLWriter m_writer;
   private StringWriter m_sWriter;
   
   protected void setUp() throws Exception
   {
      super.setUp();
      
      m_sWriter = new StringWriter();
      m_writer = new XMLWriter(m_sWriter);
   }

   public void test() throws IOException
   {
      // 1 byte char
      String s = "#abc ";
      
      String s1 = URLEncoder.encode(s, XMLUtil.ENCODING);
      m_writer.writeURL(s);
      //XMLWriter url encoder will hex-encode ' '; hence, "#abc " will be substituted into "%23abc%20".
      assertEquals(s1.replace("+", "%20"), m_sWriter.getBuffer().toString());
      
      reset();
      
      // two byte chars
      s = "футбол"; 

      s1 = URLEncoder.encode(s, XMLUtil.ENCODING);
      m_writer.writeURL(s);
      
      assertEquals(s1, m_sWriter.getBuffer().toString());
      
      reset();
      
      // three byte chars
      s = "\u1002";

      s1 = URLEncoder.encode(s, XMLUtil.ENCODING);
      m_writer.writeURL(s);
      
      assertEquals(s1, m_sWriter.getBuffer().toString());
      
      reset();
      
      // four byte chars, use surrogate chars
      s = "\uD800\uDF30";

      s1 = URLEncoder.encode(s, XMLUtil.ENCODING);
      m_writer.writeURL(s);
      
      assertEquals(s1, m_sWriter.getBuffer().toString());
   }
   
   private void reset()
   {
      m_sWriter = new StringWriter();
      m_writer = new XMLWriter(m_sWriter);
   }
}
