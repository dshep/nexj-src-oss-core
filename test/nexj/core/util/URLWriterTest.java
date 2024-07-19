// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.StringWriter;

import junit.framework.TestCase;

public class URLWriterTest extends TestCase
{
   protected URLWriter m_writer;
   protected StringWriter m_swriter;

   protected void setUp() throws Exception
   {
      m_swriter = new StringWriter();
      m_writer = new URLWriter(m_swriter);
   }

   public void testWriteInt() throws IOException
   {
      m_writer.write('A');
      m_writer.write('%');
      m_writer.write('\u0345');
      m_writer.write('\u9123');
      m_writer.write('\n');

      assertEquals("A%25%CD%85%E9%84%A3%0A", m_swriter.toString());
   }

   public void testWriteCharArrayIntInt() throws IOException
   {
      m_writer.write("  A%\u0345\u9123\n ".toCharArray(), 2, 5);

      assertEquals("A%25%CD%85%E9%84%A3%0A", m_swriter.toString());
   }

   public void testWriteString() throws IOException
   {
      m_writer.write("A%\u0345\u9123\n");

      assertEquals("A%25%CD%85%E9%84%A3%0A", m_swriter.toString());
   }

   public void testFlush() throws IOException
   {
      m_writer.flush();
   }

   public void testClose() throws IOException
   {
      m_writer.close();
   }
}
