// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

import junit.framework.TestCase;

public class MultipartInputStreamTest extends TestCase
{
   protected MultipartInputStream m_istream;

   protected void setUp() throws Exception
   {
      super.setUp();

      m_istream = new MultipartInputStream(new ByteArrayInputStream((
         "junk--0000\r\n" +
         "Content-Disposition: form-data;  name=\"file1\"; filename=\"My\u1234\\\"File.txt\"\r\n" +
         "Content-Type:  text/plain \r\n" +
         "\r\n" +
         "Test 123\r\n\r\r\n\n00\r\n--000\u1234\r\n" +
         "--0000\r\n" +
         "Content-Disposition : form-data; name=submit \r\n" +
         "\r\n" +
         "Submit\r\n" +
         "--0000--\r\n").getBytes("UTF-8")), "0000".getBytes("UTF-8"), "UTF-8");
   }

   public void testRead() throws IOException
   {
      assertEquals("junk", getString(m_istream));
      assertTrue(m_istream.nextPart());
      assertEquals("Test 123\r\n\r\r\n\n00\r\n--000\u1234", getString(m_istream));
      assertTrue(m_istream.nextPart());
      assertEquals("Submit", getString(m_istream));
      assertFalse(m_istream.nextPart());
   }

   public void testNextPart() throws IOException
   {
      assertTrue(m_istream.nextPart());
      assertTrue(m_istream.nextPart());
      assertFalse(m_istream.nextPart());
   }

   public void testFindHeader() throws IOException
   {
      assertTrue(m_istream.nextPart());
      
      assertNotNull(m_istream.getHeaders().find("Content-Type"));
      assertEquals("text/plain", m_istream.getHeaders().find("Content-Type").getValue());
      
      assertTrue(m_istream.nextPart());
      assertNotNull(m_istream.getHeaders().find("Content-Disposition"));
      assertEquals("form-data; name=submit",  m_istream.getHeaders().find("Content-Disposition").getValue());
   }
   
   public void testGetHeaderCount() throws IOException
   {
      assertTrue(m_istream.nextPart());
      assertEquals(2, m_istream.getHeaders().size());
      assertTrue(m_istream.nextPart());
      assertEquals(1, m_istream.getHeaders().size());
   }

   public void testGetHeader() throws IOException
   {
      assertTrue(m_istream.nextPart());
      assertEquals("Content-Type", m_istream.getHeaders().get(1).getName());
      
      try
      {
         m_istream.getHeaders().get(2);
         fail("Expected ArrayIndexOutOfBoundsException");
      }
      catch (ArrayIndexOutOfBoundsException e)
      {
      }
   }

   public void testMissingSeparator() throws IOException
   {
      MultipartInputStream istream = new MultipartInputStream(
         new ByteArrayInputStream("--0000\r\n\r\n".getBytes("UTF-8")),
         "0000".getBytes("UTF-8"), "UTF-8");

      istream.nextPart();

      try
      {
         istream.nextPart();
         fail("Expected MultipartDataException");
      }
      catch (MultipartDataException e)
      {
      }
   }

   protected static String getString(InputStream istream) throws IOException
   {
      ByteArrayOutputStream ostream = new ByteArrayOutputStream();

      IOUtil.copy(ostream, istream);

      return new String(ostream.toByteArray(), "UTF-8");
   }
}
