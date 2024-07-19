// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import nexj.core.util.Binary;
import nexj.core.util.IOUtil;
import nexj.core.util.XMLUtil;

import junit.framework.TestCase;

/**
 * Tests based on Base64UtilTest.
 */
public class Base64OutputStreamTest extends TestCase
{
   private byte[] m_data = null;
   private ByteArrayInputStream m_istream = null;

   private byte[] m_data1 = new byte[]{0, 1, 2, 3};
   private String m_sEnc1 = "AAECAw==";
   private byte[] m_data2 = new byte[]{0, 1, 2, 3, 4};
   private String m_sEnc2 = "AAECAwQ=";
   private byte[] m_data3 = new byte[]{0, 1, 2, 3, 4, 5};
   private String m_sEnc3 = "AAECAwQF";

   public Base64OutputStreamTest(String name)
   {
      super(name);
   }

   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();

      m_data = new byte[256 * 3];

      for (int i = 0; i < m_data.length; ++i)
      {
         m_data[i] = (byte)i;
      }

      m_istream = new ByteArrayInputStream(m_data);
   }

   public void testEncode() throws IOException
   {
      ByteArrayOutputStream ostream = new ByteArrayOutputStream();
      OutputStream encoded = new Base64OutputStream(ostream);
      IOUtil.copy(encoded, new ByteArrayInputStream(m_data1));
      encoded.flush();
      assertEquals(m_sEnc1, ostream.toString(XMLUtil.ENCODING));

      ostream.reset();
      IOUtil.copy(encoded, new ByteArrayInputStream(m_data2));
      encoded.flush();
      assertEquals(m_sEnc2, ostream.toString(XMLUtil.ENCODING));

      ostream.reset();
      IOUtil.copy(encoded, new ByteArrayInputStream(m_data3));
      encoded.flush();
      assertEquals(m_sEnc3, ostream.toString(XMLUtil.ENCODING));
   }

   public void testEncode1() throws IOException
   {
      ByteArrayOutputStream ostream = new ByteArrayOutputStream();
      OutputStream encrypted = new Base64OutputStream(ostream);
      IOUtil.copy(encrypted, m_istream);
      encrypted.flush();
      assertEquals(1024, ostream.size());

      InputStream istream = new ByteArrayInputStream(ostream.toByteArray(), 0, ostream.size());
      ostream.reset();
      IOUtil.copy(ostream, new Base64InputStream(istream));
      assertEquals(0,
                   Binary.compare(m_data, ostream.toByteArray(), m_data.length, ostream.size()));
   }

   public void testEncodeString() throws IOException
   {
      ByteArrayOutputStream ostream = new ByteArrayOutputStream();
      OutputStream encrypted = new Base64OutputStream(ostream);
      IOUtil.copy(encrypted, new ByteArrayInputStream(new byte[]{1, 2, 3}));
      encrypted.flush();
      assertEquals("AQID", ostream.toString(XMLUtil.ENCODING));
   }
}