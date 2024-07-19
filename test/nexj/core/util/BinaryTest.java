// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.StringWriter;
import java.util.Arrays;
import java.util.Locale;

import junit.framework.TestCase;

public class BinaryTest extends TestCase
{
   private Binary m_bin = null;
   private Binary m_bin2 = null;
   private Binary m_bin3 = null;
   private Binary m_bin4 = null;
   private Binary m_largeBin1;
   private byte[] m_nLargeArray1;
   private Binary m_largeBin2;
   private static int TEST_SWAP_THRESHOLD = 10000;

   /**
    * Constructor for BinaryTest.
    * @param name
    */
   public BinaryTest(String name)
   {
      super(name);
   }

   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      m_bin = new Binary(new byte[]{1, 2, 3});
      m_bin2 = new Binary(new byte[]{1, 2, 3, 4});
      m_bin3 = new Binary(new byte[]{4, 5, (byte)0xFA});
      m_bin4 = new Binary(m_bin);

      // Creates a large binary that will be swapped to disk
      m_nLargeArray1 = new byte[TEST_SWAP_THRESHOLD + 10];

      for (int i = 0; i < m_nLargeArray1.length; i++)
      {
         m_nLargeArray1[i] = (byte)(i + 1);
      }

      m_largeBin1 = new TestPagedBinary(new ByteArrayInputStream(m_nLargeArray1), TEST_SWAP_THRESHOLD);
      m_largeBin2 = new TestPagedBinary(m_largeBin1);
   }

   /**
    * Cleans up the large Binary objects allocated in setUp().
    * 
    * @see junit.framework.TestCase#tearDown()
    */
   protected void tearDown() throws Exception
   {
      ((PagedBinary)m_largeBin1).dispose();
      ((PagedBinary)m_largeBin2).dispose();
   }

   /**
    * Tests the ability to instantiate a Binary object from a stream.
    */
   public void testInstantiateFromStream() throws Exception
   {
      Binary smallFromStream = new TestPagedBinary(new ByteArrayInputStream(new byte[] {4, 5, (byte)0xFA}), TEST_SWAP_THRESHOLD);
      Binary largeFromStream = new TestPagedBinary(new ByteArrayInputStream(m_nLargeArray1), TEST_SWAP_THRESHOLD);

      assertFalse(((TestPagedBinary)smallFromStream).isPaged());
      assertTrue(((TestPagedBinary)largeFromStream).isPaged());

      assertEquals(m_bin3, smallFromStream);
      assertEquals(m_largeBin1, largeFromStream);
      ((PagedBinary)smallFromStream).dispose();
      ((PagedBinary)largeFromStream).dispose();
   }

   /*
    * Test for int hashCode()
    */
   public void testHashCode() throws Exception
   {
      assertTrue(m_bin.hashCode() != 0);
      assertTrue(m_largeBin1.hashCode() != 0);

      assertEquals(m_largeBin1.hashCode(), m_largeBin2.hashCode());

      Binary smallFromStream = new TestPagedBinary(new ByteArrayInputStream(new byte[] {4, 5, (byte)0xFA}), TEST_SWAP_THRESHOLD);
      Binary largeFromStream = new TestPagedBinary(new ByteArrayInputStream(m_nLargeArray1), TEST_SWAP_THRESHOLD);

      assertEquals(m_bin3.hashCode(), smallFromStream.hashCode());
      assertEquals(m_largeBin1.hashCode(), largeFromStream.hashCode());
      ((PagedBinary)largeFromStream).dispose();
      ((PagedBinary)smallFromStream).dispose();
   }

   public void testGetData()
   {
      assertEquals(2, m_bin.getData()[1]);

      assertTrue(Arrays.equals(m_nLargeArray1, m_largeBin1.getData()));

      assertTrue(((TestPagedBinary)m_largeBin1).isPaged());
      assertTrue(((TestPagedBinary)m_largeBin2).isPaged());
   }

   /**
    * Tests the ability to retrieve data from a Binary via a stream.
    */
   public void testGetInputStream() throws Exception
   {
      ByteArrayOutputStream bos;
      InputStream istream;

      // Small data size
      bos = new ByteArrayOutputStream();
      istream = m_bin.getInputStream();
      IOUtil.copy(bos, istream);
      istream.close();
      assertTrue(Arrays.equals(new byte[]{1, 2, 3}, bos.toByteArray()));

      bos = new ByteArrayOutputStream();
      istream = m_bin3.getInputStream();
      IOUtil.copy(bos, istream);
      istream.close();
      assertTrue(Arrays.equals(new byte[]{4, 5, (byte)0xFA}, bos.toByteArray()));

      // Large data size
      bos = new ByteArrayOutputStream();
      istream = m_largeBin1.getInputStream();
      IOUtil.copy(bos, istream);
      istream.close();
      assertTrue(Arrays.equals(m_nLargeArray1, bos.toByteArray()));

      bos = new ByteArrayOutputStream();
      istream = m_largeBin2.getInputStream();
      IOUtil.copy(bos, istream);
      istream.close();
      assertTrue(Arrays.equals(m_nLargeArray1, bos.toByteArray()));
   }

   /**
    * Tests the getSize() method.
    */
   public void testGetSize() throws Exception
   {
      // Small data size
      assertEquals(3, m_bin.getSize());
      assertEquals(4, m_bin2.getSize());
      assertEquals(3, m_bin3.getSize());
      assertEquals(3, m_bin4.getSize());

      // Large data size
      assertEquals(TEST_SWAP_THRESHOLD + 10, m_largeBin1.getSize());
      assertEquals(TEST_SWAP_THRESHOLD + 10, m_largeBin2.getSize());
   }

   /*
    * Test for boolean equals(Object)
    */
   public void testEqualsObject() throws Exception
   {
      assertTrue(m_bin.equals(m_bin));
      assertTrue(m_bin.equals(m_bin4));
      assertTrue(m_bin4.equals(m_bin));
      assertFalse(m_bin.equals(m_bin2));
      assertFalse(m_bin2.equals(m_bin));
      assertFalse(m_bin.equals(m_bin3));
      assertFalse(m_bin3.equals(m_bin));

      Binary largeFromArray = new TestPagedBinary(m_nLargeArray1);
      Binary largeFromStream = new TestPagedBinary(new ByteArrayInputStream(m_nLargeArray1), TEST_SWAP_THRESHOLD);
      Binary largeBinary = new Binary(m_nLargeArray1);

      assertFalse(((TestPagedBinary)largeFromArray).isPaged());
      assertTrue(((TestPagedBinary)largeFromStream).isPaged());

      assertTrue(m_largeBin1.equals(largeFromArray));
      assertTrue(m_largeBin1.equals(largeFromStream));
      assertTrue(m_largeBin1.equals(largeBinary));

      assertTrue(largeFromStream.equals(largeFromArray));
      assertTrue(largeFromStream.equals(largeFromStream));
      assertTrue(largeFromStream.equals(largeBinary));

      assertTrue(largeBinary.equals(largeFromArray));
      assertTrue(largeBinary.equals(largeFromStream));
      assertTrue(largeBinary.equals(largeBinary));

      // Try with unequal objects
      assertFalse(m_bin.equals(largeFromArray));
      assertFalse(m_bin.equals(largeFromStream));
      assertFalse(m_bin.equals(largeBinary));

      assertFalse(largeFromArray.equals(m_bin));
      assertFalse(largeFromStream.equals(m_bin));
      assertFalse(largeBinary.equals(m_bin));

      ((PagedBinary)largeFromArray).dispose();
      ((PagedBinary)largeFromStream).dispose();
   }

   /*
    * Test for String toString()
    */
   public void testToString()
   {
      // Test for small data size
      assertEquals("010203", m_bin.toString());
      assertEquals("0405FA", m_bin3.toString());

      // Test for large data size
      StringBuilder buf = new StringBuilder();

      for (int i = 0; i < 256; i++)
      {
         if ((m_nLargeArray1[i] & 0xff) < 16)
         {
            buf.append('0');
         }

         buf.append(Integer.toHexString(m_nLargeArray1[i] & 0xff).toUpperCase(Locale.ENGLISH));
      }

      buf.append("...");
      assertEquals(buf.toString(), m_largeBin1.toString());
   }

   /*
    * Test for int hashCode(byte[])
    */
   public void testHashCodeByteArray()
   {
      assertTrue(Binary.hashCode(new byte[]{1, 2, 3}) != 0);
   }

   public void testAppend()
   {
      StringBuffer buf = new StringBuffer();
      
      Binary.append(buf, new byte[]{1, 2, 3}, -1);
      
      assertEquals("010203", buf.toString());
   }

   public void testParse() throws Exception
   {
      assertEquals(new Binary(new byte[]{(byte)0xAB, (byte)0xCD, (byte)0xEF}), Binary.parse("ABCDEF"));
      assertEquals(new Binary(new byte[]{(byte)0xB, (byte)0xCD, (byte)0xEF}), Binary.parse("BCDEF"));
      
      try
      {
         Binary.parse("ABCDEF...");
         fail("NumberFormatException expected");
      }
      catch (NumberFormatException e)
      {
      }

      Binary largeParsed;
      StringWriter writer = new StringWriter();

      Binary.write(writer, m_nLargeArray1, 0, m_nLargeArray1.length);
      writer.close();
      largeParsed = Binary.parse(writer.toString());
      assertEquals(m_largeBin1, largeParsed);
   }

   public void testCompare()
   {
      assertTrue(Binary.compare(new byte[]{1, 2, 3}, new byte[]{4, 5, 6}) < 0);
   }

   public void testCompareTo() throws Exception
   {
      assertTrue(m_bin.compareTo(m_bin4) == 0);
      assertTrue(m_bin.compareTo(m_bin2) < 0);
      assertTrue(m_bin.compareTo(m_bin3) < 0);
      assertTrue(m_bin2.compareTo(m_bin) > 0);
      assertTrue(m_bin3.compareTo(m_bin) > 0);

      assertTrue(m_largeBin1.compareTo(m_largeBin1) == 0);
      assertTrue(m_largeBin1.compareTo(m_largeBin2) == 0);
      assertTrue(m_largeBin1.compareTo(m_bin) > 0);
      assertTrue(m_largeBin1.compareTo(m_bin2) > 0);
      assertTrue(m_largeBin1.compareTo(m_bin3) < 0);

      Binary largeFromArray = new TestPagedBinary(m_nLargeArray1);
      Binary largeFromStream = new TestPagedBinary(new ByteArrayInputStream(m_nLargeArray1), TEST_SWAP_THRESHOLD);
      Binary largeBinary = new Binary(m_nLargeArray1);

      assertTrue(m_largeBin1.compareTo(largeFromArray) == 0);
      assertTrue(m_largeBin1.compareTo(largeFromStream) == 0);
      assertTrue(m_largeBin1.compareTo(largeBinary) == 0);

      assertTrue(largeFromStream.compareTo(largeFromArray) == 0);
      assertTrue(largeFromStream.compareTo(largeFromStream) == 0);
      assertTrue(largeFromStream.compareTo(largeBinary) == 0);

      assertTrue(largeBinary.compareTo(largeFromArray) == 0);
      assertTrue(largeBinary.compareTo(largeFromStream) == 0);
      assertTrue(largeBinary.compareTo(largeBinary) == 0);

      assertTrue(m_bin.compareTo(largeFromArray) < 0);
      assertTrue(m_bin.compareTo(largeFromStream) < 0);
      assertTrue(m_bin.compareTo(largeBinary) < 0);
      
      assertTrue(largeFromArray.compareTo(m_bin) > 0);
      assertTrue(largeFromStream.compareTo(m_bin) > 0);
      assertTrue(largeBinary.compareTo(m_bin) > 0);

      ((PagedBinary)largeFromArray).dispose();
      ((PagedBinary)largeFromStream).dispose();
   }

   /**
    * Tests the conversion between Binary objects and Base64 encoding.
    */
   public void testBase64() throws Exception
   {
      // Small data size
      assertEquals("AQID", m_bin.toBase64());
      assertEquals(m_bin, Binary.fromBase64("AQID"));

      // Large data size
      Binary decoded;

      decoded = Binary.fromBase64(Base64Util.encode(m_nLargeArray1));
      assertEquals(Base64Util.encode(m_nLargeArray1), m_largeBin1.toBase64());
      assertEquals(m_largeBin1, decoded);
   }

   /**
    * Tests that Binary itself may be serialized and deserialized.
    */
   public void testSerializationOfBinary() throws Exception
   {
      byte[] nSerializedArray;
      ByteArrayInputStream istream;
      ObjectInputStream ois;
      Binary deserialized;
      ByteArrayOutputStream ostream = new ByteArrayOutputStream();
      ObjectOutputStream oos = new ObjectOutputStream(ostream);

      // Test serialization and deserialization of a small binary
      oos.writeObject(m_bin);
      oos.close();
      nSerializedArray = ostream.toByteArray();

      assertTrue(nSerializedArray.length < m_nLargeArray1.length);

      istream = new ByteArrayInputStream(nSerializedArray);
      ois = new ObjectInputStream(istream);
      deserialized = (Binary)ois.readObject();

      assertEquals(-1, ois.read());
      ois.close();

      assertEquals(m_bin, deserialized);
      assertTrue(Arrays.equals(new byte[] {1, 2, 3}, deserialized.getData()));

      // Test serialization and deserialization of a large binary
      ostream = new ByteArrayOutputStream();
      oos = new ObjectOutputStream(ostream);
      oos.writeObject(m_largeBin1);
      oos.close();
      nSerializedArray = ostream.toByteArray();

      assertTrue(nSerializedArray.length > m_nLargeArray1.length);

      istream = new ByteArrayInputStream(nSerializedArray);
      ois = new ObjectInputStream(istream);
      deserialized = (Binary)ois.readObject();

      assertEquals(-1, ois.read());
      ois.close();

      assertEquals(m_largeBin1, deserialized);
      assertTrue(Arrays.equals(m_nLargeArray1, deserialized.getData()));

      ((PagedBinary)deserialized).dispose();
   }

   /**
    * Tests that objects can be serialized to and deserialized from Binaries.
    */
   public void testSerialization() throws Exception
   {
      String sSmallSerialization = "This string will be serialized.";
      Binary result;
      ObjectOutputStream oos;
      ByteArrayOutputStream ostream;
      byte[] nSerializedArray;
      Binary serializedBin;

      // Compute reference serialization
      ostream = new ByteArrayOutputStream();
      oos = new ObjectOutputStream(ostream);
      oos.writeObject(sSmallSerialization);
      oos.close();
      nSerializedArray = ostream.toByteArray();

      // Test serialization
      result = Binary.fromObject(sSmallSerialization);
      assertTrue(Arrays.equals(nSerializedArray, result.getData()));

      // Test deserialization
      serializedBin = new Binary(nSerializedArray);
      assertEquals(sSmallSerialization, serializedBin.toObject());
   }

   /**
    * Tests the setData() method.
    */
   public void testSetData() throws Exception
   {
      // Fail--initialized Binaries are immutable
      try
      {
         m_bin.setData(new byte[] {1, 2, 3, 5, 8});
         fail();
      }
      catch (IllegalStateException ex)
      {
      }

      Binary nullBin = new Binary((byte[])null);

      // Test setData() for small data size
      nullBin.setData(new byte[] {1, 2, 3});
      assertEquals(3, nullBin.getSize());
      assertEquals(m_bin, nullBin);

      nullBin = new TestPagedBinary(new ByteArrayInputStream(new byte[0]));
      assertFalse(((TestPagedBinary)nullBin).isPaged());
      nullBin.setData(new byte[] {1, 2, 3, 4});
      assertFalse(((TestPagedBinary)nullBin).isPaged());
      assertEquals(4, nullBin.getSize());
      assertEquals(m_bin2, nullBin);


      // Test setData() from large data size
      nullBin = new TestPagedBinary((byte[])null);
      assertFalse(((TestPagedBinary)nullBin).isPaged());
      nullBin.setData(m_nLargeArray1);
      assertFalse(((TestPagedBinary)nullBin).isPaged());
      assertEquals(TEST_SWAP_THRESHOLD + 10, nullBin.getSize());
      assertEquals(m_largeBin1, nullBin);
   }

   // inner classes

   protected static class TestPagedBinary extends PagedBinary
   {
      // constants

      /**
       * The serialization version.
       */
      private static final long serialVersionUID = -6809182335956837617L;


      // constructors

      public TestPagedBinary(InputStream istream, int nSwapThreshold) throws IOException
      {
         super(istream, nSwapThreshold);
      }

      public TestPagedBinary(InputStream istream) throws IOException
      {
         super(istream);
      }

      public TestPagedBinary(byte[] nDataArray)
      {
         super(nDataArray);
      }

      public TestPagedBinary(Binary src)
      {
         super(src);
      }


      // operations

      public boolean isPaged()
      {
         return m_storage != null;
      }
   }
}
