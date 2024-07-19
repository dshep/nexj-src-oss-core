// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

import nexj.core.util.Binary;
import nexj.core.util.IOUtil;
import nexj.core.util.XMLUtil;

import junit.framework.TestCase;

public class HexInputStreamTest extends TestCase
{
   private byte[] m_data1 = new byte[]{1, 2, 3};
   private String m_sEnc1 = "010203";
   private byte[] m_data2 = new byte[]{1, 2, 3, 4};
   private String m_sEnc2 = "01020304";
   private byte[] m_data3 = new byte[]{4, 5, (byte)0xFA, (byte)0xFF};
   private String m_sEnc3 = "0405FAFF";

   public void testDecode() throws IOException
   {
      ByteArrayOutputStream ostream = new ByteArrayOutputStream();
      InputStream istream = new ByteArrayInputStream(m_sEnc1.getBytes(XMLUtil.ENCODING));
      IOUtil.copy(ostream, new HexInputStream(istream));
      assertEquals(0,
                   Binary.compare(m_data1, ostream.toByteArray(), m_data1.length, ostream.size()));

      ostream.reset();
      istream = new ByteArrayInputStream(m_sEnc2.getBytes(XMLUtil.ENCODING));
      IOUtil.copy(ostream, new HexInputStream(istream));
      assertEquals(0,
                   Binary.compare(m_data2, ostream.toByteArray(), m_data2.length, ostream.size()));

      ostream.reset();
      istream = new ByteArrayInputStream(m_sEnc3.getBytes(XMLUtil.ENCODING));
      IOUtil.copy(ostream, new HexInputStream(istream));
      assertEquals(0,
                   Binary.compare(m_data3, ostream.toByteArray(), m_data3.length, ostream.size()));
   }
}