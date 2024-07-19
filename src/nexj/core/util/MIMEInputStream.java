// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.InputStream;

/**
 * Input stream wrapper for MIME data parsing.
 */
public class MIMEInputStream extends InputStream
{
   // constants

   /**
    * The default header encoding.
    */
   public final static String ENCODING = "ISO-8859-1";

   /**
    * EOF character.
    */
   protected final static int CHAR_EOF = -1;

   /**
    * CR character.
    */
   protected final static int CHAR_CR = '\r';

   /**
    * LF character.
    */
   protected final static int CHAR_LF = '\n';

   /**
    * - character
    */
   protected final static int CHAR_DASH = '-';

   // attributes

   /**
    * The header encoding.
    */
   protected String m_sEncoding;

   // associations
   
   /**
    * The wrapped input stream.
    */
   protected InputStream m_istream;
   
   // constructors
   
   /**
    * Constructs the input stream.
    * @param istream The input stream.
    * @param sEncoding The default header encoding. Can be null to use Latin1.
    */
   protected MIMEInputStream(InputStream istream, String sEncoding)
   {
      m_istream = istream;

      if (sEncoding == null)
      {
         sEncoding = ENCODING;
      }

      m_sEncoding = sEncoding;
   }

   // operations

   /**
    * Parses the MIME headers out of the input stream.
    * @param headerMap The header map. 
    */
   protected void parseHeaders(MIMEHeaderMap headerMap) throws IOException
   {
      byte[] buf = null;
      int nCount = 0;

      for (;;)
      {
         int ch = read();

         if (ch == CHAR_LF)
         {
            if (nCount > 0 && buf[nCount - 1] == CHAR_CR)
            {
               --nCount;
            }

            if (nCount == 0)
            {
               break;
            }

            ch = read();

            if (ch != ' ' && ch != '\t')
            {
               addHeader(headerMap, buf, nCount);
               nCount = 0;
            }
         }
         else if (ch == CHAR_EOF)
         {
            if (nCount != 0)
            {
               addHeader(headerMap, buf, nCount);
            }

            break;
         }

         if (buf == null)
         {
            buf = new byte[256];
         }
         else if (nCount == buf.length)
         {
            byte[] data = new byte[nCount << 1];
            
            System.arraycopy(buf, 0, data, 0, nCount);
            buf = data;
         }

         buf[nCount++] = (byte)ch;
      }
   }

   /**
    * Adds a header from byte data.
    * @param headerMap The destination header map.
    * @param buf The buffer containing the bytes.
    * @param nCount The count of bytes in the buffer.
    */
   protected void addHeader(MIMEHeaderMap headerMap, byte[] buf, int nCount) throws IOException
   {
      int nStart = 0;

      while (nStart < nCount && Character.isWhitespace(buf[nStart]))
      {
         ++nStart;
      }

      while (nCount > nStart && Character.isWhitespace(buf[nCount - 1]))
      {
         --nCount;
      }

      for (int i = nStart; i < nCount; ++i)
      {
         if (buf[i] == ':')
         {
            int nEnd = i++;

            while (nEnd > 0 && Character.isWhitespace(buf[nEnd - 1]))
            {
               --nEnd;
            }

            if (nEnd == 0)
            {
               break;
            }

            String sName = new String(buf, nStart, nEnd - nStart, ENCODING);

            while (i < nCount && Character.isWhitespace(buf[i]))
            {
               ++i;
            }

            headerMap.add(new MIMEHeader(sName, new String(buf, i, nCount - i, m_sEncoding)));

            return;
         }
      }

      throw new MIMEDataException("Invalid header");
   }

   /**
    * @see java.io.InputStream#read()
    */
   public int read() throws IOException
   {
      return m_istream.read();
   }

   /**
    * @see java.io.InputStream#available()
    */
   public int available() throws IOException
   {
      return m_istream.available();
   }

   /**
    * @see java.io.InputStream#close()
    */
   public void close() throws IOException
   {
      m_istream.close();
   }
}
