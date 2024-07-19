// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;

import nexj.core.util.Binary;

/**
 * Hex decode input.
 */
public class HexInputStream extends InputStream
{
   /**
    * Flag indicating if end of stream was seen.
    */
   protected boolean m_bEOF = false;

   /**
    * The reader for the stream to read data from.
    */
   protected Reader m_reader;

   /**
    * Buffer used for reading bytes from stream.
    */
   protected byte[] m_buf = new byte[1];

   /**
    * Constructor.
    * @param stream The stream to read data from (not null).
    */
   public HexInputStream(final InputStream stream)
   {
      assert stream != null;

      m_reader = new Reader() // a byte reader (no char interpretation)
      {
         public void close() throws IOException
         {
            stream.close();
         }

         public int read() throws IOException
         {
            return stream.read();
         }

         public int read(char[] chArray, int nOffset, int nLength) throws IOException
         {
            for (int i = nOffset, nCount = nOffset + nLength; i < nCount; ++i)
            {
               int n = read();

               if (n < 0)
               {
                  return i - nOffset;
               }

               chArray[i] = (char)n;
            }

            return nLength;
         }
      };
   }

   /**
    * @see java.io.InputStream#read()
    */
   public int read() throws IOException
   {
      if (m_bEOF)
      {
         return -1;
      }

      int n = Binary.read(m_reader, m_buf, 0, 2); // read 2 hex chars to make one byte

      m_bEOF = n < 2;

      return (n > 0) ? m_buf[0] & 0xff : -1;
   }
}