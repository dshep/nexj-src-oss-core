// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import junit.framework.TestCase;

public class MIMEHeaderTest extends TestCase
{
   protected MIMEHeader m_hdr1;
   protected MIMEHeader m_hdr2;
   protected MIMEHeader m_hdr3;
   protected MIMEHeader m_hdr4;
   protected MIMEHeader m_hdr5;
   
   protected void setUp() throws Exception
   {
      super.setUp();
      
      m_hdr1 = new MIMEHeader("content-disposition", "attachment; name=\"file1\"; filename=\"My\u1234\\\"File.txt\"");
      m_hdr2 = new MIMEHeader("content-disposition", "form-data; name=submit");
      m_hdr3 = new MIMEHeader("accept-charset", "windows-1252, utf-8, utf-16, iso-8859-1;q=0.6, *;q=0.1");
      m_hdr4 = new MIMEHeader("Set-Cookie", "PRaf=|269490:1|#; domain=ads.pointroll.com; path=/; expires=Fri, 01-Jan-2010 00:00:00 GMT; secure");
      m_hdr5 = new MIMEHeader("User-Agent", "Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.8.0.8) Gecko/20061025 Firefox/1.5.0.8");
   }

   public void testGetName()
   {
      assertEquals("content-disposition", m_hdr1.getName());
   }

   public void testGetValue()
   {
      assertEquals("form-data; name=submit", m_hdr2.getValue());
   }

   public void testAddValue()
   {
      m_hdr1.addValue(new MIMEHeader.Value("attachment"));
      assertEquals(2, m_hdr1.getValueCount());
      m_hdr1.addValue(new MIMEHeader.Value("q"));
      assertEquals(3, m_hdr1.getValueCount());
   }

   public void testFindValue()
   {
      MIMEHeader.Value value = m_hdr1.findValue("attachment");
      
      assertNotNull(value);
      assertEquals("attachment", value.getName());
      
      assertEquals(2, value.getArgCount());
      assertEquals("file1", value.findArg("name"));
      assertEquals("My\u1234\"File.txt", value.findArg("filename"));
      assertNull(value.findArg("q"));

      assertNull(m_hdr1.findValue("q"));
      
      value = m_hdr3.findValue("iso-8859-1");
      assertEquals("iso-8859-1", value.getName());
      
      assertEquals(1, value.getArgCount());
      assertEquals("0.6", value.findArg("q"));
   }

   public void testGetValueI()
   {
      assertEquals("iso-8859-1", m_hdr3.getValue(3).getName());
      assertEquals("PRaf=|269490:1|#", m_hdr4.getValue(0).getName());
      
      MIMEHeader.Value value = m_hdr4.getValue(0);
      
      assertEquals(4, value.getArgCount());
      assertEquals("expires", value.getArgName(2));
      assertEquals("Fri, 01-Jan-2010 00:00:00 GMT", value.getArgValue(2));
      assertFalse(value.isArgQuoted(2));
      
      assertEquals("Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.8.0.8) Gecko/20061025 Firefox/1.5.0.8", m_hdr5.getValue(0).getName());

      value = m_hdr5.getValue(0);
      assertEquals(0, value.getArgCount());

      try
      {
         m_hdr3.getValue(5);
         fail("Expected ArrayIndexOutOfBoundsException");
      }
      catch (ArrayIndexOutOfBoundsException e)
      {
      }
   }

   public void testGetValueCount()
   {
      assertEquals(5, m_hdr3.getValueCount());
      assertEquals(1, m_hdr4.getValueCount());
      assertEquals(1, m_hdr5.getValueCount());
   }

   public void testParse()
   {
      m_hdr2.parse();
   }

   public void testAppend()
   {
      m_hdr1.parse();
      m_hdr1.append(m_hdr2);
      assertEquals(2, m_hdr1.getValueCount());
      m_hdr1.append(m_hdr3);
      assertEquals(7, m_hdr1.getValueCount());
   }

   public void testToString()
   {
      assertEquals("content-disposition: form-data; name=submit", m_hdr2.toString());
   }
}
