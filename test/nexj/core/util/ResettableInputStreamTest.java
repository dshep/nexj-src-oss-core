// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.ByteArrayInputStream;
import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;

import junit.framework.TestCase;

/**
 * Tests the resettable input stream.
 */
public class ResettableInputStreamTest extends TestCase
{
   protected ByteArrayInputStream m_byteStream;

   public void setUp()
   {
      m_byteStream = new ByteArrayInputStream(new byte[] {0x00, 0x01, 0x02, 0x03});
   }

   public void testReadNoMark() throws Exception
   {
      InputStream istream = new ResettableInputStream(m_byteStream);

      assertEquals(0, istream.read());
      assertEquals(1, istream.read());
      assertEquals(2, istream.read());
      assertEquals(3, istream.read());
      assertEquals(-1, istream.read());
      assertEquals(-1, istream.read());
   }

   public void testReadArrayNoMark() throws Exception
   {
      InputStream istream = new ResettableInputStream(m_byteStream);
      byte[] nData = new byte[4];

      assertEquals(4, istream.read(nData));
      assertEquals(0, nData[0]);
      assertEquals(3, nData[3]);
      assertEquals(-1, istream.read(nData));
      assertEquals(-1, istream.read(nData));
   }

   public void testReadWithMark() throws Exception
   {
      InputStream istream = new ResettableInputStream(m_byteStream);

      assertEquals(0, istream.read());
      assertEquals(1, istream.read());
      istream.mark(2);
      istream.reset();  // test resetting an empty buffer
      assertEquals(2, istream.read());
      istream.reset();
      assertEquals(2, istream.read());
      istream.reset();
      istream.reset();
      assertEquals(2, istream.read());
      assertEquals(3, istream.read());
      istream.reset();
      assertEquals(2, istream.read());
      assertEquals(3, istream.read());
      istream.reset();
      istream.reset();
      assertEquals(2, istream.read());
      istream.reset();
      assertEquals(2, istream.read());
      istream.reset();
      assertEquals(2, istream.read());
      assertEquals(3, istream.read());
      assertEquals(-1, istream.read());
      assertEquals(-1, istream.read());
   }

   public void testReadWithMarkAtStart() throws Exception
   {
      InputStream istream = new ResettableInputStream(m_byteStream);

      istream.mark(2);
      assertEquals(0, istream.read());
      istream.reset();
      assertEquals(0, istream.read());
      assertEquals(1, istream.read());
      istream.reset();
      assertEquals(0, istream.read());
      assertEquals(1, istream.read());
      assertEquals(2, istream.read());

      try
      {
         istream.reset();
         fail("Excpected IOException");
      }
      catch (IOException ex)
      {
         // pass
      }

      assertEquals(3, istream.read());
      assertEquals(-1, istream.read());
   }

   public void testReadWithEOFWhileBuffering() throws Exception
   {
      InputStream istream = new ResettableInputStream(m_byteStream);

      assertEquals(0, istream.read());
      istream.mark(4);
      assertEquals(1, istream.read());
      assertEquals(2, istream.read());
      assertEquals(3, istream.read());
      assertEquals(-1, istream.read());
      assertEquals(-1, istream.read());
      assertEquals(-1, istream.read());
   }

   public void testResetOnEOF() throws Exception
   {
      InputStream istream = new ResettableInputStream(m_byteStream);

      assertEquals(0, istream.read());
      istream.mark(5);
      assertEquals(1, istream.read());
      assertEquals(2, istream.read());
      assertEquals(3, istream.read());
      assertEquals(-1, istream.read());
      istream.reset();
      assertEquals(1, istream.read());
      assertEquals(2, istream.read());
      assertEquals(3, istream.read());
      assertEquals(-1, istream.read());
      assertEquals(-1, istream.read());
      assertEquals(-1, istream.read());
      assertEquals(-1, istream.read());
      istream.reset();
      assertEquals(1, istream.read());
      assertEquals(2, istream.read());
      assertEquals(3, istream.read());
      assertEquals(-1, istream.read());
   }
   
   public void testResetOnEOFArrayReads() throws Exception
   {
      InputStream istream = new ResettableInputStream(m_byteStream);
      byte[] nData = new byte[4];

      assertEquals(0, istream.read());
      istream.mark(5);
      assertEquals(3, istream.read(nData, 0, 4));
      assertEquals(-1, istream.read(nData, 0, 4));
      assertEquals(-1, istream.read());
      istream.reset();
      assertEquals(3, istream.read(nData, 0, 4));
      assertEquals(-1, istream.read(nData, 0, 4));
      assertEquals(-1, istream.read(nData, 0, 4));
      assertEquals(-1, istream.read(nData, 0, 4));
      assertEquals(-1, istream.read(nData, 0, 4));
      assertEquals(-1, istream.read());
      assertEquals(-1, istream.read());
      assertEquals(-1, istream.read());
      assertEquals(-1, istream.read());
      istream.reset();
      assertEquals(1, istream.read());
      assertEquals(2, istream.read());
      assertEquals(3, istream.read());
      assertEquals(-1, istream.read());
   }

   public void testResetEmptyBuffer() throws Exception
   {
      InputStream istream = new ResettableInputStream(m_byteStream);

      assertEquals(0, istream.read());
      assertEquals(1, istream.read());
      istream.mark(2);
      istream.reset();
      assertEquals(2, istream.read());

      m_byteStream.reset();
      istream = new ResettableInputStream(m_byteStream);
      istream.mark(2);
      istream.reset();
      assertEquals(0, istream.read());
      assertEquals(1, istream.read());
      istream.reset();
      assertEquals(0, istream.read());
      assertEquals(1, istream.read());
   }

   public void testReadArrayWithMark() throws Exception
   {
      InputStream istream = new ResettableInputStream(m_byteStream);
      byte[] nData = new byte[3];

      istream.mark(3);
      istream.reset();
      assertEquals(3, istream.read(nData));
      assertEquals(0, nData[0]);
      assertEquals(2, nData[2]);
      istream.reset();
      Arrays.fill(nData, (byte)0);
      assertEquals(3, istream.read(nData));
      assertEquals(0, nData[0]);
      assertEquals(2, nData[2]);
      istream.reset();
      Arrays.fill(nData, (byte)0);
      assertEquals(2, istream.read(nData, 0, 2));
      assertEquals(0, nData[0]);
      assertEquals(1, nData[1]);
      assertEquals(0, nData[2]);
      assertEquals(1, istream.read(nData, 0, 1));
      assertEquals(2, nData[0]);
      istream.reset();
      nData = new byte[6];
      assertEquals(4, istream.read(nData));
      assertEquals(0, nData[0]);
      assertEquals(3, nData[3]);
      assertEquals(0, nData[4]);
      assertEquals(-1, istream.read(nData));
      assertEquals(-1, istream.read(nData));
   }

   public void testReadArrayWithMarkOneByteAtATime() throws Exception
   {
      InputStream istream = new ResettableInputStream(new OneByteInputStream(m_byteStream));
      byte[] nData = new byte[3];

      istream.mark(2);
      assertEquals(1, istream.read(nData));
      assertEquals(1, istream.read(nData, 1, 2));  // read beyond end of buffer, but only get one character from underlying stream (so mark still valid)
      assertEquals(0, nData[0]);
      assertEquals(1, nData[1]);
      istream.reset();
      assertEquals(2, istream.read(nData, 0, 2));
   }

   public void testSkip() throws Exception
   {
      InputStream istream = new ResettableInputStream(m_byteStream);

      assertEquals(0, istream.read());
      assertEquals(2, istream.skip(2));
      assertEquals(3, istream.read());
      assertEquals(-1, istream.read());
   }

   public void testSkipInBuffer() throws Exception
   {
      InputStream istream = new ResettableInputStream(m_byteStream);

      assertEquals(0, istream.read());
      istream.mark(3);
      assertEquals(1, istream.read());
      assertEquals(1, istream.skip(1));  // skip
      assertEquals(3, istream.read());
      istream.reset();
      assertEquals(1, istream.read());
      assertEquals(2, istream.read());  // ensure skipped byte was buffered
      assertEquals(3, istream.read());
   }

   public void testSkipFromBufferedToUnbuffered() throws Exception
   {
      InputStream istream = new ResettableInputStream(m_byteStream);

      assertEquals(0, istream.read());
      istream.mark(2);
      assertEquals(1, istream.read());
      assertEquals(1, istream.skip(1));
      istream.reset();
      assertEquals(1, istream.read());
      assertEquals(2, istream.skip(2));

      try
      {
         istream.reset();
         fail("Expected IOException");
      }
      catch (IOException ex)
      {
         // pass
      }

      assertEquals(-1, istream.read());
      assertEquals(-1, istream.read());
   }

   public void testSkipInBufferOneByteAtATime() throws Exception
   {
      InputStream istream = new ResettableInputStream(new OneByteInputStream(m_byteStream));

      assertEquals(0, istream.read());
      istream.mark(3);
      assertEquals(1, istream.skip(2));
      istream.reset();
      assertEquals(2, istream.skip(3));  // skip beyond end of buffer, but only got one character from underlying stream (so mark still valid)
      istream.reset();
      assertEquals(1, istream.read());
      assertEquals(2, istream.read());
      assertEquals(3, istream.read());
      assertEquals(-1, istream.read());
   }

   public void testResetWithoutMark() throws Exception
   {
      InputStream istream = new ResettableInputStream(m_byteStream);

      assertEquals(0, istream.read());
      assertEquals(1, istream.read());

      try
      {
         istream.reset();
         fail("Expected IOException");
      }
      catch (IOException ex)
      {
         // pass
      }
   }

   // inner classes

   /**
    * An input stream that can only read or skip a single byte at a time.
    */
   public static class OneByteInputStream extends FilterInputStream
   {
      /**
       * @param in
       */
      protected OneByteInputStream(InputStream in)
      {
         super(in);
      }

      /**
       * @see java.io.FilterInputStream#read(byte[], int, int)
       */
      public int read(byte[] b, int nOff, int nLen) throws IOException
      {
         return in.read(b, nOff, 1);
      }

      /**
       * @see java.io.FilterInputStream#skip(long)
       */
      public long skip(long n) throws IOException
      {
         return in.skip(1);
      }
   }
}
