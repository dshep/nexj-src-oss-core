// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.FilterReader;
import java.io.IOException;
import java.io.Reader;

/**
 * Reader that stores marked data in a growable buffer so that calls to reset()
 * can always succeed.
 */
public class UnlimitedMarkReader extends FilterReader
{
   // attributes

   /**
    * The position in the buffer, when reading marked data (after a reset() call)
    */
   protected int m_nBufferPos;

   /**
    * The size of the data in the buffer.
    */
   protected int m_nBufferLength;


   // associations

   /**
    * The growable buffer for storing marked data.
    */
   protected char[] m_chBuffer;


   // constructors

   /**
    * @param in The reader to wrap.
    */
   public UnlimitedMarkReader(Reader in)
   {
      super(in);
      m_nBufferPos = -1;
   }


   // operations

   /**
    * @see java.io.FilterReader#close()
    */
   public void close() throws IOException
   {
      in.close();
      in = null;
      m_chBuffer = null;
   }

   /**
    * @see java.io.FilterReader#mark(int)
    */
   public void mark(int nReadAheadLimit) throws IOException
   {
      m_chBuffer = new char[nReadAheadLimit];
      m_nBufferLength = 0;
      m_nBufferPos = -1;
   }

   /**
    * @see java.io.FilterReader#markSupported()
    */
   public boolean markSupported()
   {
      return true;
   }

   /**
    * @see java.io.FilterReader#read()
    */
   public int read() throws IOException
   {
      int nCh;

      if (m_nBufferPos < 0)
      {
         nCh = in.read();

         if (m_chBuffer != null && nCh != -1)
         {
            appendToMark((char)nCh);
         }
      }
      else
      {
         if (m_nBufferPos == m_nBufferLength)
         {
            m_nBufferPos = -1;
            m_chBuffer = null;
            m_nBufferLength = 0;

            return read();
         }

         nCh = m_chBuffer[m_nBufferPos++] & 0xffff;
      }

      return nCh;
   }

   /**
    * @see java.io.FilterReader#read(char[], int, int)
    */
   public int read(char[] chArray, int nOff, int nLen) throws IOException
   {
      int nCount;

      if (m_nBufferPos < 0)
      {
         nCount = in.read(chArray, nOff, nLen);

         if (m_chBuffer != null && nCount != -1)
         {
            appendToMark(chArray, nOff, nCount);
         }
      }
      else
      {
         int nDataInBufferCount = m_nBufferLength - m_nBufferPos;

         if (nDataInBufferCount <= nLen)
         {
            System.arraycopy(m_chBuffer, m_nBufferPos, chArray, nOff, nDataInBufferCount);
            m_nBufferPos = -1;
            m_chBuffer = null;
            m_nBufferLength = 0;
            nCount = nDataInBufferCount;

            if (nLen - nDataInBufferCount > 0)
            {
               int nReadCount = in.read(chArray, nOff + nDataInBufferCount, nLen - nDataInBufferCount);

               if (nReadCount >= 0)
               {
                  nCount += nReadCount;
               }
            }
         }
         else  // nDataInBufferCount > nLen
         {
            System.arraycopy(m_chBuffer, m_nBufferPos, chArray, nOff, nLen);
            m_nBufferPos += nLen;
            nCount = nLen;
         }
      }

      return nCount;
   }

   /**
    * @see java.io.FilterReader#ready()
    */
   public boolean ready() throws IOException
   {
      if (m_chBuffer != null)
      {
         if (m_nBufferPos > 0 && m_nBufferPos < m_nBufferLength)
         {
            return true;
         }
      }

      return in.ready();
   }

   /**
    * @see java.io.FilterReader#reset()
    */
   public void reset() throws IOException
   {
      if (m_chBuffer != null)
      {
         m_nBufferPos = 0;
      }
      else
      {
         throw new IllegalStateException("reset() without mark()");
      }
   }

   /**
    * @see java.io.FilterReader#skip(long)
    */
   public long skip(long lCount) throws IOException
   {
      long lSkippedCount = 0;

      while (lCount > 0)
      {
         int nCh = read();

         if (nCh <= 0)
         {
            break;
         }

         lSkippedCount++;
         lCount--;
      }

      return lSkippedCount;
   }

   /**
    * Appends the given character to the mark buffer, expanding the
    * buffer as necessary.
    * 
    * @param chToAppend The character to append.
    */
   protected void appendToMark(char chToAppend)
   {
      ensureIndexInBounds(m_nBufferLength);

      m_chBuffer[m_nBufferLength++] = chToAppend;
   }

   /**
    * Appends a given character array to the mark buffer, expanding the
    * buffer as necessary.
    * 
    * @param chArray The character array to append.
    * @param nOff The offset in the character array to start appending.
    * @param nLen The number of characters in the array to append.
    */
   protected void appendToMark(char[] chArray, int nOff, int nLen)
   {
      ensureIndexInBounds(m_nBufferLength + nLen);

      System.arraycopy(chArray, nOff, m_chBuffer, m_nBufferLength, nLen);
      m_nBufferLength += nLen;
   }

   /**
    * Ensures that the buffer is large enough so that the given
    * index is within bounds.
    * 
    * @param nIndex The index to ensure is within bounds.
    */
   protected void ensureIndexInBounds(int nIndex)
   {
      if (nIndex >= m_chBuffer.length)
      {
         int nNewSize = m_chBuffer.length;

         while (nIndex >= nNewSize)
         {
            nNewSize <<= 1;
         }

         char[] chBiggerBuffer = new char[nNewSize];

         System.arraycopy(m_chBuffer, 0, chBiggerBuffer, 0, m_chBuffer.length);
         m_chBuffer = chBiggerBuffer;
      }
   }
}