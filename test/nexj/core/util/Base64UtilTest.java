// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.Arrays;

import junit.framework.TestCase;

public class Base64UtilTest extends TestCase
{
   private byte[] m_data = null;
   private ByteArrayInputStream m_istream = null;
   private StringWriter m_writer = null;
   
   private byte[] m_data1 = new byte[]{0, 1, 2, 3};
   private String m_sEnc1 = "AAECAw==";
   private byte[] m_data2 = new byte[]{0, 1, 2, 3, 4};
   private String m_sEnc2 = "AAECAwQ=";
   private byte[] m_data3 = new byte[]{0, 1, 2, 3, 4, 5};
   private String m_sEnc3 = "AAECAwQF";

   public Base64UtilTest(String name)
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
      m_writer = new StringWriter();
   }

   public void testEncode()
   {
      try
      {
         ByteArrayInputStream istream = new ByteArrayInputStream(m_data1);
         StringWriter writer = new StringWriter();
         assertEquals(4, Base64Util.encode(istream, writer, -1, false));
         assertEquals(m_sEnc1, writer.toString());
         
         istream = new ByteArrayInputStream(m_data2);
         writer = new StringWriter();
         assertEquals(5, Base64Util.encode(istream, writer, -1, false));
         assertEquals(m_sEnc2, writer.toString());
         
         istream = new ByteArrayInputStream(m_data3);
         writer = new StringWriter();
         assertEquals(6, Base64Util.encode(istream, writer, -1, false));
         assertEquals(m_sEnc3, writer.toString());
         
         istream.reset();
         writer = new StringWriter();
         assertEquals(5, Base64Util.encode(istream, writer, 5, false));
         assertEquals(m_sEnc2, writer.toString());

         istream.reset();
         writer = new StringWriter();
         assertEquals(4, Base64Util.encode(istream, writer, 4, false));
         assertEquals(m_sEnc1, writer.toString());
      }
      catch (IOException e)
      {
         fail("IOException");
      }
   }

   public void testDecode()
   {
      try
      {
         StringReader reader = new StringReader(m_sEnc1);
         ByteArrayOutputStream ostream = new ByteArrayOutputStream();
         assertEquals(8, Base64Util.decode(reader, ostream, -1, false));
         assertTrue(Binary.compare(m_data1, ostream.toByteArray()) == 0);
         
         reader = new StringReader(m_sEnc2);
         ostream = new ByteArrayOutputStream();
         assertEquals(8, Base64Util.decode(reader, ostream, -1, false));
         assertTrue(Binary.compare(m_data2, ostream.toByteArray()) == 0);
         
         reader = new StringReader(m_sEnc3);
         ostream = new ByteArrayOutputStream();
         assertEquals(8, Base64Util.decode(reader, ostream, -1, false));
         assertTrue(Binary.compare(m_data3, ostream.toByteArray()) == 0);
         
         reader.reset();
         ostream = new ByteArrayOutputStream();
         assertEquals(7, Base64Util.decode(reader, ostream, 7, false));
         assertTrue(Binary.compare(m_data2, ostream.toByteArray()) == 0);

         reader.reset();
         ostream = new ByteArrayOutputStream();
         assertEquals(6, Base64Util.decode(reader, ostream, 6, false));
         assertTrue(Binary.compare(m_data1, ostream.toByteArray()) == 0);
         reader.reset();
         
         try
         {
            reader = new StringReader("A");
            ostream = new ByteArrayOutputStream();
            Base64Util.decode(reader, ostream, -1, false);
            fail("Expected Base64Exception");
         }
         catch (Base64Exception e)
         {
         }
         
         try
         {
            reader = new StringReader("A-==");
            ostream = new ByteArrayOutputStream();
            Base64Util.decode(reader, ostream, -1, false);
            fail("Expected Base64Exception");
         }
         catch (Base64Exception e)
         {
         }
      }
      catch (IOException e)
      {
         fail("IOException");
      }
   }

   public void testEncode1()
   {
      try
      {
         Base64Util.encode(m_istream, m_writer, -1, false);
         String s = m_writer.toString();
         assertEquals(1024, s.length());

         StringReader reader = new StringReader(s);
         ByteArrayOutputStream ostream = new ByteArrayOutputStream();
         Base64Util.decode(reader, ostream, -1, false);
         assertTrue(Binary.compare(m_data, ostream.toByteArray()) == 0);
      }
      catch (IOException e)
      {
         fail("IOException");
      }
   }

   public void testEncode2()
   {
      try
      {
         Base64Util.encode(m_istream, m_writer, -1, true);
         String s = m_writer.toString();
         assertEquals(1050, s.length());
         
         StringReader reader = new StringReader(s);
         ByteArrayOutputStream ostream = new ByteArrayOutputStream();
         Base64Util.decode(reader, ostream, -1, true);
         assertTrue(Binary.compare(m_data, ostream.toByteArray()) == 0);
      }
      catch (IOException e)
      {
         fail("IOException");
      }
   }
   
   public void testEncodeString()
   {
      assertEquals("AQID", Base64Util.encode(new byte[]{1, 2, 3}));
   }
   
   public void testDecodeString() throws IOException
   {
      assertTrue(Arrays.equals(new byte[]{1, 2, 3}, Base64Util.decode("AQID")));
   }
}
