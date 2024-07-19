// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.InputStream;

/**
 * Input stream wrapper for multipart data parsing.
 */
public class MultipartInputStream extends MIMEInputStream
{
   // attributes

   /**
    * The circular buffer end index (next past the last byte).
    */
   protected int m_nEnd;

   /**
    * The circular buffer start index.
    */
   protected int m_nStart;

   /**
    * True if at the start of a part.
    */
   protected boolean m_bSOP = true;

   /**
    * True if end-of-part has been reached.
    */
   protected boolean m_bEOP;

   /**
    * True if end-of-file has been reached.
    */
   protected boolean m_bEOF;

   // associations

   /**
    * The separator bytes.
    */
   protected byte[] m_separator;

   /**
    * The read-ahead circular buffer.
    */
   protected byte[] m_buf;

   /**
    * Header map.
    */
   protected MIMEHeaderMap m_headerMap;

   // constructors
   
   /**
    * Constructs the input stream.
    * @param istream The input stream.
    * @param separator The separator bytes.
    * @param sEncoding The default header encoding.
    */
   public MultipartInputStream(InputStream istream, byte[] separator, String sEncoding)
   {
      super(istream, sEncoding);

      assert separator != null;
      assert separator.length != 0;

      m_separator = separator;

      // Two extra bytes for "\r\n", two for "--", two for "\r\n" or "--"
      // and one extra for the circular buffer implementation
      m_buf = new byte[separator.length + 7];
   }

   // operations
   
   /**
    * Removes a byte from the buffer start.
    * @return The byte from the buffer start, of CHAR_EOF if none available.
    */
   protected int next()
   {
      if (m_nStart == m_nEnd)
      {
         return CHAR_EOF;
      }
      
      int ch = m_buf[m_nStart++] & 0xFF;
      
      if (m_nStart == m_buf.length)
      {
         m_nStart = 0;
      }
      
      return ch;
   }
   
   /**
    * Gets a byte at a given offset from the start of the buffer.
    * @param i The index of the byte.
    * @return The byte, of CHAR_EOF if not within the buffer range.
    */
   protected int get(int i) throws IOException
   {
      i += m_nStart;
      
      if (m_nStart <= m_nEnd)
      {
         if (i < m_nEnd)
         {
            return m_buf[i] & 0xFF;
         }
      }
      else
      {
         if (i >= m_buf.length)
         {
            i -= m_buf.length;
         }
         
         if (i >= m_nStart || i < m_nEnd)
         {
            return m_buf[i] & 0xFF;
         }
      }

      if (i == m_nEnd)
      {
         int ch = m_istream.read();
         
         if (ch != CHAR_EOF)
         {
            m_buf[m_nEnd++] = (byte)ch;
            
            if (m_nEnd == m_buf.length)
            {
               m_nEnd = 0;
            }
         }
         
         return ch;
      }
      
      return CHAR_EOF;
   }
   
   /**
    * Skips the specified number of bytes from the beginning of the buffer.
    * @param n The number of bytes to skip.
    */
   public void forget(int n)
   {
      m_nStart += n;
      
      if (m_nStart >= m_buf.length)
      {
         m_nStart -= m_buf.length;
      }
   }
   
   /**
    * @see java.io.InputStream#read()
    */
   public int read() throws IOException
   {
      if (m_bEOP)
      {
         return CHAR_EOF;
      }
      
      int i = 0;
      int ch = get(i++);
      
      if (m_bSOP)
      {
         if (ch == CHAR_CR || ch == CHAR_LF)
         {
            m_bSOP = false;
         }
      }
      
      if (!m_bSOP)
      {
         if (ch != CHAR_CR || get(i++) != CHAR_LF)
         {
            return next();
         }
         
         ch = get(i++);
      }
      
      if (ch != CHAR_DASH || get(i++) != CHAR_DASH)
      {
         return next();
      }
      
      for (int k = 0; k != m_separator.length; ++k)
      {
         if (get(i++) != (m_separator[k] & 0xFF))
         {
            return next();
         }
      }
      
      ch = get(i++);
      
      if (ch == CHAR_CR)
      {
         if (get(i++) != CHAR_LF)
         {
            throw new MultipartDataException("Invalid separator");
         }
      }
      else if (ch == CHAR_DASH)
      {
         if (get(i++) != CHAR_DASH)
         {
            throw new MultipartDataException("Invalid termination separator");
         }
         
         m_bEOF = true;
      }
      else if (ch != CHAR_LF)
      {
         throw new MultipartDataException("Invalid separator");
      }
      
      forget(i);
      m_bEOP = true;
      m_bSOP = true;
      
      return CHAR_EOF;
   }
   
   /**
    * Starts reading of the next part, including the headers.
    * @return False if no next part is available.
    */
   public boolean nextPart() throws IOException
   {
      while (read() != CHAR_EOF) ;

      if (!m_bEOP)
      {
         throw new MultipartDataException("Missing separator");
      }

      if (m_bEOF)
      {
         return false;
      }

      m_bEOP = false;
      m_headerMap = new MIMEHeaderMap();
      parseHeaders(m_headerMap);
      return true;
   }

   /**
    * @return The header map.
    */
   public MIMEHeaderMap getHeaders()
   {
      return m_headerMap;
   }
   
   /**
    * @see java.io.InputStream#close()
    */
   public void close() throws IOException
   {
      m_istream.close();
      m_nStart = m_nEnd = 0;
      m_bEOP = m_bEOF = false;
      m_headerMap = null;
   }
}
