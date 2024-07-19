// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.Locale;

import junit.framework.TestCase;

public class SubstReaderTest extends TestCase
{
   protected Reader m_reader1 = new TestSubstReader(new StringReader("${A} + ${B} != ${CD}"));
   protected Reader m_reader2 = new TestSubstReader(new StringReader("{A} + $B != ${C"));
   
   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();
   }

   /*
    * Class under test for int read(char[])
    */
   public void testReadcharArray() throws IOException
   {
      int n;
      char[] cbuf = new char[128];

      n = m_reader1.read(cbuf);
      assertEquals("65 + 66 != 6768", new String(cbuf, 0, n));
      assertEquals(-1, m_reader1.read());
      
      n = m_reader2.read(cbuf);
      assertEquals("{A} + $B != ", new String(cbuf, 0, n));
      assertEquals(-1, m_reader2.read());
   }

   public void testRecursiveEvaluation() throws IOException
   {
      Reader reader = new SubstReader(new StringReader("${A} ${B:${C}}"))
      {
         protected String getValue(String sName) throws IOException
         {
            if (sName.startsWith("B:"))
            {
               return substitute(sName.substring(2)); // evaluate everything after B:
            }

            return sName.toLowerCase(Locale.ENGLISH);
         }
      };
      char[] cbuf = new char[128];
      int n = reader.read(cbuf);

      assertEquals("a c", new String(cbuf, 0, n));
      assertEquals(-1, reader.read());
   }

   public static class TestSubstReader extends SubstReader
   {
      public TestSubstReader(Reader reader)
      {
         super(reader);
      }

      /**
       * @see nexj.core.util.SubstReader#getValue(java.lang.String)
       */
      protected String getValue(String sName)
      {
         StringBuffer buf = new StringBuffer();

         for (int i = 0; i < sName.length(); ++i)
         {
            buf.append((int)sName.charAt(i));
         }

         return buf.toString();
      }
   }
}
