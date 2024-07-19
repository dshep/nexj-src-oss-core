// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import nexj.core.util.IOUtil;
import nexj.core.util.XMLUtil;

import junit.framework.TestCase;

public class HexOutputStreamTest extends TestCase
{
   private byte[] m_data1 = new byte[]{1, 2, 3};
   private String m_sEnc1 = "010203";
   private byte[] m_data2 = new byte[]{1, 2, 3, 4};
   private String m_sEnc2 = "01020304";
   private byte[] m_data3 = new byte[]{4, 5, (byte)0xFA, (byte)0xFF};
   private String m_sEnc3 = "0405FAFF";

   public void testEncode() throws IOException
   {
      ByteArrayOutputStream ostream = new ByteArrayOutputStream();
      OutputStream encrypted = new HexOutputStream(ostream);
      IOUtil.copy(encrypted, new ByteArrayInputStream(m_data1));
      encrypted.flush();
      assertEquals(m_sEnc1, ostream.toString(XMLUtil.ENCODING));

      ostream.reset();
      IOUtil.copy(encrypted, new ByteArrayInputStream(m_data2));
      encrypted.flush();
      assertEquals(m_sEnc2, ostream.toString(XMLUtil.ENCODING));

      ostream.reset();
      IOUtil.copy(encrypted, new ByteArrayInputStream(m_data3));
      encrypted.flush();
      assertEquals(m_sEnc3, ostream.toString(XMLUtil.ENCODING));
   }
}