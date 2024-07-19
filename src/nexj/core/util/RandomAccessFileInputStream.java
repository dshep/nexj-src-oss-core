// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.InputStream;
import java.io.RandomAccessFile;

/**
 * Wraps a RandomAccessFile to provide an InputStream interface. Closing
 * the input stream does not actually close the underlying file, so it
 * is safe to pass this to clients that expect a normal InputStream.
 */
public class RandomAccessFileInputStream extends InputStream
{
   // attributes

   /**
    * The file position set in mark().
    */
   protected long m_lMarkedPos;
   
   // associations

   /**
    * The random access file object.
    */
   protected RandomAccessFile m_raf;

   // constructors

   /**
    * Constructs the input stream.
    * @param raf The random access file to wrap. 
    */
   public RandomAccessFileInputStream(RandomAccessFile raf)
   {
      m_raf = raf;
   }

   // operations

   /**
    * @return The wrapped random access file.
    */
   public RandomAccessFile getRandomAccessFile()
   {
      return m_raf;
   }

   /**
    * @see java.io.InputStream#available()
    */
   public int available() throws IOException
   {
      return (int)Math.min(m_raf.length() - m_raf.getFilePointer(), Integer.MAX_VALUE);
   }

   /**
    * Does not close the wrapped random access file.
    * @see java.io.InputStream#close()
    */
   public void close() throws IOException
   {
   }

   /**
    * @see java.io.InputStream#read()
    */
   public int read() throws IOException
   {
      return m_raf.read();
   }

   /**
    * @see java.io.InputStream#read(byte[], int, int)
    */
   public int read(byte[] buf, int nOfs, int nLen) throws IOException
   {
      return m_raf.read(buf, nOfs, nLen);
   }

   /**
    * @see java.io.InputStream#read(byte[])
    */
   public int read(byte[] buf) throws IOException
   {
      return m_raf.read(buf);
   }

   /**
    * @see java.io.InputStream#skip(long)
    */
   public long skip(long lCount) throws IOException
   {
      long lSkippedCount = 0;

      while (lCount > 0)
      {
         int nCount = m_raf.skipBytes((int)Math.min(Integer.MAX_VALUE, lCount));

         if (nCount <= 0)
         {
            break;
         }

         lSkippedCount += nCount;
         lCount -= nCount;
      }

      return lSkippedCount;
   }

   /**
    * @see java.io.InputStream#markSupported()
    */
   public boolean markSupported()
   {
      return true;
   }

   /**
    * @see java.io.InputStream#mark(int)
    */
   public void mark(int nLimit)
   {
      try
      {
         m_lMarkedPos = m_raf.getFilePointer();
      }
      catch (IOException e)
      {
         ObjUtil.rethrow(e);
      }
   }

   /**
    * @see java.io.InputStream#reset()
    */
   public void reset() throws IOException
   {
      m_raf.seek(m_lMarkedPos);
   }
}
