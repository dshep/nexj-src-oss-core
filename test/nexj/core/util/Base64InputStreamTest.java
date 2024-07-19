// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

import nexj.core.util.Base64Exception;
import nexj.core.util.Binary;
import nexj.core.util.IOUtil;
import nexj.core.util.XMLUtil;

import junit.framework.TestCase;

/**
 * Tests based on Base64UtilTest.
 */
public class Base64InputStreamTest extends TestCase
{
   private byte[] m_data1 = new byte[]{0, 1, 2, 3};
   private String m_sEnc1 = "AAECAw==";
   private byte[] m_data2 = new byte[]{0, 1, 2, 3, 4};
   private String m_sEnc2 = "AAECAwQ=";
   private byte[] m_data3 = new byte[]{0, 1, 2, 3, 4, 5};
   private String m_sEnc3 = "AAECAwQF";

   public Base64InputStreamTest(String name)
   {
      super(name);
   }

   public void testDecode() throws IOException
   {
      ByteArrayOutputStream ostream = new ByteArrayOutputStream();
      InputStream istream = new ByteArrayInputStream(m_sEnc1.getBytes(XMLUtil.ENCODING));
      IOUtil.copy(ostream, new Base64InputStream(istream));
      assertEquals(0,
                   Binary.compare(m_data1, ostream.toByteArray(), m_data1.length, ostream.size()));

      ostream.reset();
      istream = new ByteArrayInputStream(m_sEnc2.getBytes(XMLUtil.ENCODING));
      IOUtil.copy(ostream, new Base64InputStream(istream));
      assertEquals(0,
                   Binary.compare(m_data2, ostream.toByteArray(), m_data2.length, ostream.size()));

      ostream.reset();
      istream = new ByteArrayInputStream(m_sEnc3.getBytes(XMLUtil.ENCODING));
      IOUtil.copy(ostream, new Base64InputStream(istream));
      assertEquals(0,
                   Binary.compare(m_data3, ostream.toByteArray(), m_data3.length, ostream.size()));

      try
      {
         ostream.reset();
         istream = new ByteArrayInputStream("A".getBytes(XMLUtil.ENCODING));
         IOUtil.copy(ostream, new Base64InputStream(istream));
         fail("Expected Base64Exception");
      }
      catch (Base64Exception e)
      {
      }

      try
      {
         ostream.reset();
         istream = new ByteArrayInputStream("A-==".getBytes(XMLUtil.ENCODING));
         IOUtil.copy(ostream, new Base64InputStream(istream));
         fail("Expected Base64Exception");
      }
      catch (Base64Exception e)
      {
      }
   }

   public void testDecodeString() throws IOException
   {
      ByteArrayOutputStream ostream = new ByteArrayOutputStream();
      InputStream istream = new ByteArrayInputStream("AQID".getBytes(XMLUtil.ENCODING));
      IOUtil.copy(ostream, new Base64InputStream(istream));
      assertEquals(0,
                   Binary.compare(new byte[]{1, 2, 3}, ostream.toByteArray(), 3, ostream.size()));
   }
}