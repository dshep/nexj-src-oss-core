// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.Reader;
import java.io.StringReader;

import junit.framework.TestCase;

/**
 * Test cases for the UlimitedMarkReader.
 */
public class UnlimitedMarkReaderTest extends TestCase
{
   /**
    * Tests the UnlimitedMarkReader used by the XMLMessageParser.
    */
   public void testUnlimitedMarkReader() throws Exception
   {
      final String sData = "0123456789abcdef";
      Reader srcReader = new StringReader(sData);
      Reader reader = new UnlimitedMarkReader(srcReader);

      // Read bytewise
      int i = 0;

      while (i < 16)
      {
         int nCh = reader.read();

         assertEquals(Character.forDigit(i, 16), (char)nCh);
         i++;
      }

      assertEquals(-1, reader.read());

      srcReader.reset();
      reader = new UnlimitedMarkReader(srcReader);


      // Read bytewise via character arrays
      i = 0;
      char[] chArray = new char[1];

      while (i < 16)
      {
         assertEquals(1, reader.read(chArray, 0, 1));
         assertEquals(Character.forDigit(i, 16), chArray[0]);
         i++;
      }

      assertEquals(-1, reader.read(chArray, 0, 1));

      srcReader.reset();
      reader = new UnlimitedMarkReader(srcReader);


      // Read it all via character arrays
      chArray = new char[16];

      assertEquals(16, reader.read(chArray, 0, 16));
      assertEquals(-1, reader.read(chArray, 0, 1));
      assertEquals(sData, new String(chArray));

      srcReader.reset();
      reader = new UnlimitedMarkReader(srcReader);


      // Read more than all via character arrays
      chArray = new char[32];

      assertEquals(16, reader.read(chArray, 0, 16));
      assertEquals(-1, reader.read(chArray, 0, 1));
      assertEquals(sData, new String(chArray, 0, 16));

      srcReader.reset();
      reader = new UnlimitedMarkReader(srcReader);


      // Mark it all, read, reset, read
      chArray = new char[32];
      reader.mark(1);
      assertEquals(16, reader.read(chArray));
      assertEquals(-1, reader.read());
      assertEquals(sData, new String(chArray, 0, 16));
      reader.reset();
      assertEquals(16, reader.read(chArray));
      assertEquals(-1, reader.read());
      assertEquals(sData, new String(chArray, 0, 16));

      srcReader.reset();
      reader = new UnlimitedMarkReader(srcReader);


      // Mark, read half, reset, read all
      chArray = new char[32];
      reader.mark(1);
      assertEquals(8, reader.read(chArray, 0, 8));
      assertEquals("01234567", new String(chArray, 0, 8));
      reader.reset();
      assertEquals(16, reader.read(chArray));
      assertEquals(sData, new String(chArray, 0, 16));
      assertEquals(-1, reader.read());

      srcReader.reset();
      reader = new UnlimitedMarkReader(srcReader);


      // Mark at EOF
      chArray = new char[32];
      assertEquals(16, reader.read(chArray));
      assertEquals(-1, reader.read());
      assertEquals(sData, new String(chArray, 0, 16));
      reader.mark(10);
      assertEquals(-1, reader.read());
      reader.reset();
      assertEquals(-1, reader.read());

      srcReader.reset();
      reader = new UnlimitedMarkReader(srcReader);


      // Tests the skip method
      assertEquals('0', reader.read());
      assertEquals(1, reader.skip(1));
      assertEquals(0, reader.skip(0));
      assertEquals('2', reader.read());
      reader.mark(1);
      assertEquals('3', reader.read());
      assertEquals(2, reader.skip(2));
      assertEquals('6', reader.read());
      reader.reset();
      assertEquals('3', reader.read());
      assertEquals('4', reader.read());
      assertEquals('5', reader.read());
      assertEquals(2, reader.skip(2));  // skips across buffer boundary
      assertEquals('8', reader.read());
      reader.mark(1);
      assertEquals(7, reader.skip(10));
      assertEquals(-1, reader.read());
      reader.reset();
      assertEquals('9', reader.read());
   }
}
