// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import junit.framework.TestCase;

public class LimitInputStreamTest extends TestCase
{
   protected LimitInputStream m_ist;
   protected LimitInputStream m_is;
   
   protected void setUp() throws Exception
   {
      super.setUp();
      
      m_ist = new LimitInputStream(new ByteArrayInputStream(new byte[]{1, 2, 3, 4, 5}), 3, true);
      m_is = new LimitInputStream(new ByteArrayInputStream(new byte[]{1, 2, 3, 4, 5}), 3, false);
   }

   public void testAvailable() throws IOException
   {
      assertTrue(m_ist.available() > 0);
      assertTrue(m_is.available() > 0);
   }

   public void testRead() throws IOException
   {
      assertEquals(1, m_ist.read());
      assertEquals(2, m_ist.read());
      assertEquals(3, m_ist.read());

      try
      {
         m_ist.read();
         fail("Expected LimitIOException");
      }
      catch (LimitIOException e)
      {
      }

      assertEquals(1, m_is.read());
      assertEquals(2, m_is.read());
      assertEquals(3, m_is.read());
      assertEquals(-1, m_is.read());
   }

   public void testClose() throws IOException
   {
      m_ist.close();
      m_is.close();
   }

   public void testReset() throws IOException
   {
      assertEquals(1, m_ist.read());
      m_ist.mark(2);
      assertEquals(2, m_ist.read());
      assertEquals(3, m_ist.read());
      m_ist.reset();
      assertEquals(2, m_ist.read());
      assertEquals(3, m_ist.read());

      try
      {
         m_ist.read();
         fail("Expected LimitIOException");
      }
      catch (LimitIOException e)
      {
      }

      assertEquals(1, m_is.read());
      m_is.mark(2);
      assertEquals(2, m_is.read());
      assertEquals(3, m_is.read());
      m_is.reset();
      assertEquals(2, m_is.read());
      assertEquals(3, m_is.read());
      assertEquals(-1, m_is.read());
   }

   public void testMarkSupported() throws IOException
   {
      assertTrue(m_ist.markSupported());
      assertTrue(m_is.markSupported());
   }

   public void testSkip() throws IOException
   {
      assertEquals(3, m_ist.skip(3));
      
      try
      {
         m_ist.skip(1);
         fail("Expected LimitIOException");
      }
      catch (LimitIOException e)
      {
      }

      assertEquals(3, m_is.skip(3));
      assertEquals(-1, m_is.skip(1));
   }

   public void testReadByteArray() throws IOException
   {
      byte[] buf = new byte[3];
      
      assertEquals(3, m_ist.read(buf));
      assertTrue(Binary.compare(buf, new byte[]{1, 2, 3}) == 0);
      
      try
      {
         m_ist.read(buf);
         fail("Expected LimitIOException");
      }
      catch (LimitIOException e)
      {
      }

      assertEquals(3, m_is.read(buf));
      assertTrue(Binary.compare(buf, new byte[]{1, 2, 3}) == 0);
      assertEquals(-1, m_is.read(buf));
   }

   public void testReadByteArrayIntInt() throws IOException
   {
      byte[] buf = new byte[3];
      
      assertEquals(3, m_ist.read(buf, 0, buf.length));
      assertTrue(Binary.compare(buf, new byte[]{1, 2, 3}) == 0);
      
      try
      {
         m_ist.read(buf, 0, 1);
         fail("Expected LimitIOException");
      }
      catch (LimitIOException e)
      {
      }

      assertEquals(3, m_is.read(buf, 0, buf.length));
      assertTrue(Binary.compare(buf, new byte[]{1, 2, 3}) == 0);
      assertEquals(-1, m_is.read(buf, 0, 1));
   }
}
