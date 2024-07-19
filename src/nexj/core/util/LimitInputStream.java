// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.InputStream;

/**
 * Input stream wrapper that limits the number of read bytes
 * by truncating the stream or by throwing an IOException
 * when the limit has been exceeded.
 */
public class LimitInputStream extends InputStream
{
   // attributes
   
   /**
    * The maximum count of bytes to read.
    */
   protected long m_lMaxCount;
   
   /**
    * The current count of read bytes.
    */
   protected long m_lCount;
   
   /**
    * The marked count of read bytes.
    */
   protected long m_lMarkedCount;

   /**
    * True if an exception should be thrown when
    * the limit is exceeded.
    */
   protected boolean m_bThrow;

   // associations

   /**
    * The wrapped input stream.
    */
   protected InputStream m_istream;
   
   // constructors

   /**
    * Constructs the input stream.
    * @param istream The wrapped input stream.
    * @param lMaxCount The maximum number of bytes to read.
    * @param bThrow True to throw a LimitIOException when the limit is exceeded.
    */
   public LimitInputStream(InputStream istream, long lMaxCount, boolean bThrow)
   {
      m_istream = istream;
      m_lMaxCount = lMaxCount;
      m_bThrow = bThrow;
   }

   // operations

   /**
    * @return The current count of read bytes.
    */
   public long getCount()
   {
      return m_lCount;
   }

   /**
    * @see java.io.InputStream#available()
    */
   public int available() throws IOException
   {
      return m_istream.available();
   }

   /**
    * @see java.io.InputStream#markSupported()
    */
   public boolean markSupported()
   {
      return m_istream.markSupported();
   }

   /**
    * @see java.io.InputStream#mark(int)
    */
   public synchronized void mark(int nCount)
   {
      m_istream.mark(nCount);
      m_lMarkedCount = m_lCount;
   }

   /**
    * @see java.io.InputStream#reset()
    */
   public synchronized void reset() throws IOException
   {
      m_istream.reset();
      m_lCount = m_lMarkedCount;
   }

   /**
    * @see java.io.InputStream#read()
    */
   public int read() throws IOException
   {
      if (m_lCount < m_lMaxCount + ((m_bThrow) ? 1 : 0))
      {
         int ch = m_istream.read();
         
         if (ch < 0 || ++m_lCount <= m_lMaxCount)
         {
            return ch;
         }
      }
      else if (!m_bThrow)
      {
         return -1;
      }
      
      return throwLimitException();
   }

   /**
    * @see java.io.InputStream#read(byte[], int, int)
    */
   public int read(byte[] buf, int nOff, int nLen) throws IOException
   {
      if (m_lCount < m_lMaxCount + ((m_bThrow) ? 1 : 0))
      {
         int nCount = m_istream.read(buf, nOff, (int)Math.min(m_lMaxCount - m_lCount + ((m_bThrow) ? 1 : 0), nLen));

         if (nCount < 0)
         {
            return nCount;
         }
         
         m_lCount += nCount;
         
         if (m_lCount <= m_lMaxCount)
         {
            return nCount;
         }
      }
      else if (!m_bThrow)
      {
         return -1;
      }

      return throwLimitException();
   }

   /**
    * @see java.io.InputStream#skip(long)
    */
   public long skip(long lCount) throws IOException
   {
      if (m_lCount < m_lMaxCount + ((m_bThrow) ? 1 : 0))
      {
         lCount = m_istream.skip(Math.min(m_lMaxCount - m_lCount + ((m_bThrow) ? 1 : 0), lCount));

         if (lCount < 0)
         {
            return lCount;
         }
         
         m_lCount += lCount;
         
         if (m_lCount <= m_lMaxCount)
         {
            return lCount;
         }
      }
      else if (!m_bThrow)
      {
         return -1;
      }

      return throwLimitException();
   }

   /**
    * @see java.io.InputStream#close()
    */
   public void close() throws IOException
   {
      m_istream.close();
      m_lCount = 0;
   }

   /**
    * @throws LimitIOException always.
    */
   protected int throwLimitException() throws LimitIOException
   {
      throw new LimitIOException("Exceeded the maximum input byte count of " + m_lMaxCount);
   }
}
