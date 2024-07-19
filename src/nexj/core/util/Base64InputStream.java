// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.InputStream;

import nexj.core.util.Base64Exception;
import nexj.core.util.Base64Util;

/**
 * Base64 decode input.
 * Code adapted from Base64Util to work incrementally on a stream.
 */
public class Base64InputStream extends InputStream
{
   /**
    * Flag indicating if end of stream was seen.
    */
   protected boolean m_bEOF = false;

   /**
    * Buffer used for storing decoded bytes from stream.
    * Bytes stored in stack fashion e.g. {n+2, n+1, n}.
    */
   protected byte[] m_buf = new byte[3]; // 3 == base64 block size

   /**
    * Number of bytes waiting to be output.
    */
   protected int m_nByteCount = 0;

   /**
    * The stream to read data from.
    */
   private InputStream m_stream;

   /**
    * Constructor.
    * @param stream The stream to read data from (not null).
    */
   public Base64InputStream(final InputStream stream)
   {
      assert stream != null;
      m_stream = stream;
   }

   /**
    * @see java.io.InputStream#read()
    */
   public int read() throws IOException
   {
      if (m_nByteCount <= 0)
      {
         if (m_bEOF)
         {
            return -1;
         }

         m_nByteCount = refill();
         m_bEOF = m_nByteCount < m_buf.length;
      }

      return (m_nByteCount > 0) ? m_buf[--m_nByteCount] & 0xff : -1;
   }

   /**
    * Refill the buffer from the stream.
    * Buffer refilled stack fashion e.g. {n+2, n+1, n} or {n+1, n, ?} for partially full buffer.
    * @return The number of bytes of the buffer filled (partially full buffer == EOF)
    * @throws IOException On read or decode error.
    */
   protected int refill() throws IOException
   {
      int nBytes = 0;
      int nByteCount = 0;

      for (;;)
      {
         int ch = m_stream.read();

         if (ch < 0)
         {
            if (nByteCount == 1)
            {
               throw new Base64Exception("Unexpected EOF");
            }

            break;
         }

         if (ch == '=')
         {
            if (nByteCount < 2)
            {
               throw new Base64Exception("Unexpected termination character =");
            }

            if (nByteCount == 2)
            {
               ch = m_stream.read();

               if (ch >= 0 && ch != '=')
               {
                  throw new Base64Exception("Missing second termination character =");
               }
            }

            break;
         }

         if (ch > 127)
         {
            throw new Base64Exception("Invalid character (ascii=" + ch + ")");
         }

         int nCode = Base64Util.DECODING_ARRAY[ch];

         if (nCode >= 0)
         {
            nBytes <<= 6;
            nBytes |= nCode;

            if (++nByteCount == 4)
            {
               m_buf[2] = (byte)(nBytes >> 16);
               m_buf[1] = (byte)(nBytes >> 8);
               m_buf[0] = (byte)nBytes;

               return 3;
            }
         }
         else if (nCode == -1)
         {
            throw new Base64Exception("Invalid character (ascii=" + ch + ")");
         }
      }

      if (nByteCount == 2)
      {
         m_buf[0] = (byte)(nBytes >> 4);

         return 1;
      }
      else if (nByteCount == 3)
      {
         m_buf[1] = (byte)(nBytes >> 10);
         m_buf[0] = (byte)(nBytes >> 2);

         return 2;
      }

      return 0;
   }
}