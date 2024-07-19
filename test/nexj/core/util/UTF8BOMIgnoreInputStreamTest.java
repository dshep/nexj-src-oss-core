// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.Arrays;

import junit.framework.TestCase;

/**
 * Tests the UTF8 Byte-Order-Mark-ignoring input stream.
 */
public class UTF8BOMIgnoreInputStreamTest extends TestCase
{
   public void testReadStreamWithoutBOM() throws Exception
   {
      ByteArrayInputStream istream = new ByteArrayInputStream(new byte[] {0x31, 0x32, 0x33, 0x41, 0x42, 0x43});
      InputStream testStream = UTF8BOMIgnoreInputStream.wrap(istream, "utf8");

      assertEquals(0x31, testStream.read());
      assertEquals(0x32, testStream.read());
      assertEquals(0x33, testStream.read());
      assertEquals(0x41, testStream.read());
      assertEquals(0x42, testStream.read());
      assertEquals(0x43, testStream.read());
      assertEquals(-1, testStream.read());
      assertEquals(-1, testStream.read());

      // Test reading byte arrays
      int nRead;
      byte[] nData = new byte[4];

      istream.reset();
      testStream = UTF8BOMIgnoreInputStream.wrap(istream, "utf8");
      nRead = testStream.read(nData, 0, 4);
      assertEquals(4, nRead);
      assertTrue(Arrays.equals(new byte[] {0x31, 0x32, 0x33, 0x41}, nData));
      nRead = testStream.read(nData, 0, 4);
      assertEquals(2, nRead);
      assertTrue(Arrays.equals(new byte[] {0x42, 0x43, 0x33, 0x41}, nData));
      assertEquals(-1, testStream.read(nData, 0, 4));
   }

   public void testReadStreamWithBOM() throws Exception
   {
      ByteArrayInputStream istream = new ByteArrayInputStream(new byte[] {(byte)0xef, (byte)0xbb, (byte)0xbf, 0x31, 0x32, 0x33, 0x41, 0x42, 0x43});
      InputStream testStream = UTF8BOMIgnoreInputStream.wrap(istream, "utf8");

      assertEquals(0x31, testStream.read());
      assertEquals(0x32, testStream.read());
      assertEquals(0x33, testStream.read());
      assertEquals(0x41, testStream.read());
      assertEquals(0x42, testStream.read());
      assertEquals(0x43, testStream.read());
      assertEquals(-1, testStream.read());
      assertEquals(-1, testStream.read());

      // Test reading byte arrays
      int nRead;
      byte[] nData = new byte[4];

      istream.reset();
      testStream = UTF8BOMIgnoreInputStream.wrap(istream, "utf8");
      nRead = testStream.read(nData, 0, 4);
      assertEquals(4, nRead);
      assertTrue(Arrays.equals(new byte[] {0x31, 0x32, 0x33, 0x41}, nData));
      nRead = testStream.read(nData, 0, 4);
      assertEquals(2, nRead);
      assertTrue(Arrays.equals(new byte[] {0x42, 0x43, 0x33, 0x41}, nData));
      assertEquals(-1, testStream.read(nData, 0, 4));
   }

   public void testReadEOFAfterBom() throws Exception
   {
      ByteArrayInputStream istream = new ByteArrayInputStream(new byte[] {(byte)0xef, (byte)0xbb, (byte)0xbf});
      InputStream testStream = UTF8BOMIgnoreInputStream.wrap(istream, "utf8");

      assertEquals(-1, testStream.read());
      assertEquals(-1, testStream.read());

      // Test reading byte arrays
      int nRead;
      byte[] nData = new byte[4];

      istream.reset();
      testStream = UTF8BOMIgnoreInputStream.wrap(istream, "utf8");
      nRead = testStream.read(nData, 0, 4);
      assertEquals(-1, nRead);
   }

   public void testReadImmediateEOF() throws Exception
   {
      ByteArrayInputStream istream = new ByteArrayInputStream(new byte[0]);
      InputStream testStream = UTF8BOMIgnoreInputStream.wrap(istream, "utf8");

      assertEquals(-1, testStream.read());
      assertEquals(-1, testStream.read());

      // Test reading byte arrays
      int nRead;
      byte[] nData = new byte[4];

      istream.reset();
      testStream = UTF8BOMIgnoreInputStream.wrap(istream, "utf8");
      nRead = testStream.read(nData, 0, 4);
      assertEquals(-1, nRead);
   }

   public void testReadEOFInBOM() throws Exception
   {
      ByteArrayInputStream istream = new ByteArrayInputStream(new byte[] {(byte)0xef});
      InputStream testStream = UTF8BOMIgnoreInputStream.wrap(istream, "utf8");

      assertEquals(0xef, testStream.read());
      assertEquals(-1, testStream.read());
      assertEquals(-1, testStream.read());

      // Test reading byte arrays
      int nRead;
      byte[] nData = new byte[4];

      istream.reset();
      testStream = UTF8BOMIgnoreInputStream.wrap(istream, "utf8");
      nRead = testStream.read(nData, 0, 4);
      assertEquals(1, nRead);
      assertTrue(Arrays.equals(new byte[] {(byte)0xef, 0, 0, 0}, nData));
      assertEquals(-1, testStream.read(nData, 0, 4));
   }

   public void testAvailableWithoutBOM() throws Exception
   {
      InputStream testStream = UTF8BOMIgnoreInputStream.wrap(new ByteArrayInputStream(new byte[] {0x31, 0x32, 0x33, 0x41, 0x42, 0x43}), "utf8");

      assertEquals(6, testStream.available());
      assertEquals(0x31, testStream.read());
      assertEquals(5, testStream.available());
      assertEquals(0x32, testStream.read());
      assertEquals(0x33, testStream.read());
      assertEquals(3, testStream.available());
      assertEquals(0x41, testStream.read());
      assertEquals(2, testStream.available());
      assertEquals(2, testStream.skip(2));
      assertEquals(0, testStream.available());
      assertEquals(-1, testStream.read());
   }

   public void testAvailableWithBOM() throws Exception
   {
      InputStream testStream = UTF8BOMIgnoreInputStream.wrap(new ByteArrayInputStream(new byte[] {(byte)0xef, (byte)0xbb, (byte)0xbf, 0x31, 0x32, 0x33, 0x41, 0x42, 0x43}), "utf8");

      assertEquals(6, testStream.available());
      assertEquals(0x31, testStream.read());
      assertEquals(5, testStream.available());
      assertEquals(0x32, testStream.read());
      assertEquals(0x33, testStream.read());
      assertEquals(3, testStream.available());
      assertEquals(0x41, testStream.read());
      assertEquals(2, testStream.available());
      assertEquals(2, testStream.skip(2));
      assertEquals(0, testStream.available());
      assertEquals(-1, testStream.read());
   }

   public void testSkipWithoutBOM() throws Exception
   {
      InputStream testStream;

      testStream = UTF8BOMIgnoreInputStream.wrap(new ByteArrayInputStream(new byte[] {0x31, 0x32, 0x33, 0x41, 0x42, 0x43}), "utf8");
      assertEquals(6, testStream.skip(6));
      assertEquals(-1, testStream.read());

      testStream = UTF8BOMIgnoreInputStream.wrap(new ByteArrayInputStream(new byte[] {0x31, 0x32, 0x33, 0x41, 0x42, 0x43}), "utf8");
      assertEquals(1, testStream.skip(1));
      assertEquals(0x32, testStream.read());

      testStream = UTF8BOMIgnoreInputStream.wrap(new ByteArrayInputStream(new byte[] {0x31, 0x32, 0x33, 0x41, 0x42, 0x43}), "utf8");
      assertEquals(3, testStream.skip(3));
      assertEquals(0x41, testStream.read());
   }

   public void testSkipWithBOM() throws Exception
   {
      InputStream testStream;

      testStream = UTF8BOMIgnoreInputStream.wrap(new ByteArrayInputStream(new byte[] {(byte)0xef, (byte)0xbb, (byte)0xbf, 0x31, 0x32, 0x33, 0x41, 0x42, 0x43}), "utf8");
      assertEquals(6, testStream.skip(6));
      assertEquals(-1, testStream.read());

      testStream = UTF8BOMIgnoreInputStream.wrap(new ByteArrayInputStream(new byte[] {(byte)0xef, (byte)0xbb, (byte)0xbf, 0x31, 0x32, 0x33, 0x41, 0x42, 0x43}), "utf8");
      assertEquals(1, testStream.skip(1));
      assertEquals(0x32, testStream.read());

      testStream = UTF8BOMIgnoreInputStream.wrap(new ByteArrayInputStream(new byte[] {(byte)0xef, (byte)0xbb, (byte)0xbf, 0x31, 0x32, 0x33, 0x41, 0x42, 0x43}), "utf8");
      assertEquals(3, testStream.skip(3));
      assertEquals(0x41, testStream.read());
   }

   public void testMarkSupported()
   {
      UTF8BOMIgnoreInputStream testStream = new UTF8BOMIgnoreInputStream(new ByteArrayInputStream(new byte[] {0x31, 0x32, 0x33, 0x41, 0x42, 0x43}));

      assertTrue(testStream.markSupported());
   }
}
