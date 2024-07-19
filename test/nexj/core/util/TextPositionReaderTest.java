// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.StringReader;

import junit.framework.TestCase;

/**
 * Test case for nexj.core.util.TextPositionReader
 */
public class TextPositionReaderTest extends TestCase
{
   private TextPositionReader m_reader = null;

   /**
    * Constructor for TextPositionReaderTest.
    * @param arg0
    */
   public TextPositionReaderTest(String arg0)
   {
      super(arg0);
   }

   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();
      m_reader = new TextPositionReader(new StringReader("abc\r\ndefg\r\nhijklmnop"));
   }

   /*
    * Test for int read()
    */
   public void testRead()
   {
      try
      {
         assertEquals('a', m_reader.read());
         assertEquals('b', m_reader.read());
      }
      catch (IOException e)
      {
         fail("IOException");
      }
   }

   public void testClose()
   {
      try
      {
         m_reader.close();
      }
      catch (IOException e)
      {
         fail("IOException");
      }
      
      try
      {
         m_reader.read();
         fail("Exception expected");
      }
      catch (Exception e)
      {
      }
   }

   public void testMarkReset()
   {
      try
      {
         m_reader.read();
         m_reader.mark(2);
         m_reader.read();
         assertEquals('c', m_reader.read());
         m_reader.reset();
         assertEquals('b', m_reader.read());
      }
      catch (IOException e)
      {
         fail("IOException");
      }
   }

   public void testMarkSupported()
   {
      assertTrue(m_reader.markSupported());
   }

   public void testReady()
   {
      try
      {
         assertTrue(m_reader.ready());
      }
      catch (IOException e)
      {
         fail("IOException");
      }
   }

   public void testSkip()
   {
      try
      {
         m_reader.skip(5);
         assertEquals('d', m_reader.read());
      }
      catch (IOException e)
      {
         fail("IOException");
      }
   }

   /*
    * Test for int read(char[], int, int)
    */
   public void testReadcharArrayintint()
   {
      try
      {
         char[] buf = new char[7];
         
         m_reader.read();
         assertEquals(7, m_reader.read(buf, 0, 7));
         assertEquals("bc\r\ndef", new String(buf));
      }
      catch (IOException e)
      {
         fail("IOException");
      }
   }

   public void testGetTextPosition()
   {
      try
      {
         assertEquals(0, m_reader.getTextPosition().getLine());
         assertEquals(-1, m_reader.getTextPosition().getColumn());
         m_reader.read();
         assertEquals(0, m_reader.getTextPosition().getLine());
         assertEquals(0, m_reader.getTextPosition().getColumn());
         m_reader.read(new char[12]);
         assertEquals(2, m_reader.getTextPosition().getLine());
         assertEquals(1, m_reader.getTextPosition().getColumn());
      }
      catch(IOException e)
      {
         fail("IOException");
      }
   }
}
