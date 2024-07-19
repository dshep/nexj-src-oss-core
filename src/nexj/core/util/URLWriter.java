// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.Writer;

/**
 * URL-encoded writer.
 */
public class URLWriter extends Writer implements CharSequence
{
   /**
    * Sequence offset.
    */
   protected int m_nOfs;  

   /**
    * Sequence length.
    */
   protected int m_nLen;

   /**
    * Temporary buffer.
    */
   protected char[] m_buf;

   /**
    * The wrapped writer.
    */
   protected XMLWriter m_writer;

   /**
    * Constructs the writer.
    * @param writer The wrapped writer.
    */
   public URLWriter(Writer writer)
   {
      if (writer instanceof XMLWriter)
      {
         m_writer = (XMLWriter)writer;
      }
      else
      {
         m_writer = new XMLWriter(writer);
      }
   }

   /**
    * @see java.io.Writer#write(int)
    */
   public void write(int ch) throws IOException
   {
      m_writer.writeURL(ch);
   }

   /**
    * @see java.io.Writer#write(char[], int, int)
    */
   public void write(char[] buf, int nOfs, int nLen) throws IOException
   {
      m_buf = buf;
      m_nOfs = nOfs;
      m_nLen = nLen;
      m_writer.writeURL(this);
   }

   /**
    * @see java.io.Writer#write(java.lang.String)
    */
   public void write(String s) throws IOException
   {
      m_writer.writeURL(s);
   }

   /**
    * @see java.io.Writer#flush()
    */
   public void flush() throws IOException
   {
      m_writer.flush();
   }

   /**
    * @see java.io.Writer#close()
    */
   public void close() throws IOException
   {
      m_writer.close();
   }

   /**
    * @see java.lang.CharSequence#charAt(int)
    */
   public char charAt(int i)
   {
      return m_buf[i + m_nOfs];
   }

   /**
    * @see java.lang.CharSequence#length()
    */
   public int length()
   {
      return m_nLen;
   }

   /**
    * @see java.lang.CharSequence#subSequence(int, int)
    */
   public CharSequence subSequence(int start, int end)
   {
      throw new UnsupportedOperationException();
   }
}
