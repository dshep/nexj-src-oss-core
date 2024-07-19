// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * A non-buffered input stream that implements mark/reset via an undo buffer.
 */
public class ResettableInputStream extends FilterInputStream
{
   // attributes

   /**
    * The buffer position for putting newly-read bytes.
    */
   protected int m_nBufTail;

   /**
    * The index in the buffer to read; -1 to read from underlying stream (reset has not been called).
    */
   protected int m_nReadOffset = -1;

   // associations

   /**
    * The undo buffer.
    */
   protected byte[] m_buf;

   // constructors

   /**
    * Constructs a new BOM-ignoring stream.
    * @param in The stream with a potential BOM.
    */
   protected ResettableInputStream(InputStream in)
   {
      super(in);
   }

   // operations

   /**
    * @see java.io.FilterInputStream#read()
    */
   public int read() throws IOException
   {
      if (m_buf == null)
      {
         return in.read();
      }

      int nByte;

      if (m_nReadOffset >= 0)
      {
         /*
          * Get byte from buffer.
          */
         if (m_nReadOffset < m_nBufTail)
         {
            nByte = (int)m_buf[m_nReadOffset++] & 0xff;
         }
         else
         {
            // End of undo buffer, read new data
            m_nReadOffset = -1;
            nByte = read();  // might need to buffer the new data
         }

         return nByte;
      }

      /*
       * Read 1 byte from underlying stream and buffer it.
       */ 
      nByte = in.read();

      if (nByte >= 0)
      {
         if (m_nBufTail < m_buf.length)
         {
            m_buf[m_nBufTail++] = (byte)nByte;
         }
         else
         {
            invalidateMark();
         }
      }

      return nByte;
   }

   /**
    * @see java.io.FilterInputStream#read(byte[], int, int)
    */
   public int read(byte[] nByteArray, int nOff, int nLen) throws IOException
   {
      if (m_buf == null)
      {
         return in.read(nByteArray, nOff, nLen);
      }

      int nReadCount;

      // Get bytes from buffer
      if (m_nReadOffset >= 0)
      {
         int nAvailable = m_nBufTail - m_nReadOffset;

         if (nAvailable <= 0)
         {
            m_nReadOffset = -1;

            return read(nByteArray, nOff, nLen);
         }
         else if (nAvailable >= nLen)
         {
            System.arraycopy(m_buf, m_nReadOffset, nByteArray, nOff, nLen);
            m_nReadOffset += nLen;

            return nLen;
         }
         else
         {
            // End of undo buffer, read new data
            System.arraycopy(m_buf, m_nReadOffset, nByteArray, nOff, nAvailable);
            m_nReadOffset = -1;
            nReadCount = read(nByteArray, nOff + nAvailable, nLen - nAvailable);

            return nAvailable + ((nReadCount > 0) ? nReadCount : 0);
         }
      }

      // Get bytes from underlying stream and buffer them.
      int nFreeSpace = m_buf.length - m_nBufTail;

      // Read past end of buffer
      if (nFreeSpace < nLen)
      {
         nReadCount = in.read(nByteArray, nOff, nLen);

         if (nReadCount > nFreeSpace)
         {
            invalidateMark();
         }
         else if (nReadCount > 0)
         {
            System.arraycopy(nByteArray, nOff, m_buf, m_nBufTail, nReadCount);
            m_nBufTail += nReadCount;
         }

         return nReadCount;
      }

      nReadCount = in.read(m_buf, m_nBufTail, nLen);

      if (nReadCount < 0)
      {
         return -1;
      }

      System.arraycopy(m_buf, m_nBufTail, nByteArray, nOff, nReadCount);
      m_nBufTail += nReadCount;

      return nReadCount;
   }

   /**
    * @see java.io.FilterInputStream#available()
    */
   public int available() throws IOException
   {
      int nAvailable = 0;

      if (m_buf != null && m_nReadOffset >= 0)
      {
         nAvailable = m_nBufTail - m_nReadOffset;
      }

      return nAvailable + in.available();
   }

   /**
    * @see java.io.FilterInputStream#skip(long)
    */
   public long skip(long lSkipCount) throws IOException
   {
      if (m_buf == null)
      {
         return in.skip(lSkipCount);
      }

      // Skip bytes in buffer
      if (m_nReadOffset >= 0)
      {
         int nAvailable = m_nBufTail - m_nReadOffset;

         if (nAvailable >= lSkipCount)
         {
            m_nReadOffset += lSkipCount;

            return lSkipCount;
         }
         else
         {
            m_nReadOffset = -1;

            return nAvailable + skip(lSkipCount - nAvailable);
         }
      }

      // Need to read bytes from underlying stream.
      int nFreeSpace = m_buf.length - m_nBufTail;
      int nReadCount;

      // Potential to skip past end of buffer
      if (nFreeSpace < lSkipCount)
      {
         nReadCount = in.read(m_buf, m_nBufTail, nFreeSpace);

         if (nReadCount < nFreeSpace)
         {
            m_nBufTail += nReadCount;

            return nReadCount;
         }
         else
         {
            long lActualSkip = in.skip(lSkipCount - nReadCount);

            if (lActualSkip > 0)
            {
               invalidateMark();
            }

            return lActualSkip + nReadCount;
         }
      }

      nReadCount = in.read(m_buf, m_nBufTail, (int)lSkipCount);

      if (nReadCount < 0)
      {
         return 0;
      }

      m_nBufTail += nReadCount;

      return nReadCount;
   }

   /**
    * @see java.io.FilterInputStream#markSupported()
    */
   public boolean markSupported()
   {
      return true;
   }

   /**
    * @see java.io.FilterInputStream#mark(int)
    */
   public synchronized void mark(int nReadLimit)
   {
      invalidateMark();
      m_buf = new byte[nReadLimit];
   }

   /**
    * @see java.io.FilterInputStream#reset()
    */
   public synchronized void reset() throws IOException
   {
      if (m_buf == null)
      {
         throw new IOException("Not marked or mark invalid");
      }

      m_nReadOffset = 0;
   }

   /**
    * Invalidates the current mark, if any.
    */
   protected void invalidateMark()
   {
      m_nReadOffset = -1;
      m_buf = null;
      m_nBufTail = 0;
   }
}
