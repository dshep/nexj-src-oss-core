// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.OutputStream;
import java.io.RandomAccessFile;

/**
 * Wraps a RandomAccessFile to provide an OutputStream interface. Closing
 * the output stream does not actually close the underlying file, so it
 * is safe to pass this to clients that expect a normal OutputStream.
 */
public class RandomAccessFileOutputStream extends OutputStream
{
   // associations

   /**
    * The random access file object. 
    */
   protected RandomAccessFile m_raf;

   // constructors

   /**
    * Constructs the output stream.
    * @param raf The random access file to wrap. 
    */
   public RandomAccessFileOutputStream(RandomAccessFile raf)
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
    * Does not close the wrapped random access file.
    * @see java.io.OutputStream#close()
    */
   public void close() throws IOException
   {
   }

   /**
    * @see java.io.OutputStream#flush()
    */
   public void flush() throws IOException
   {
   }

   /**
    * @see java.io.OutputStream#write(byte[], int, int)
    */
   public void write(byte[] buf, int nOfs, int nLen) throws IOException
   {
      m_raf.write(buf, nOfs, nLen);
   }

   /**
    * @see java.io.OutputStream#write(byte[])
    */
   public void write(byte[] buf) throws IOException
   {
      m_raf.write(buf);
   }

   /**
    * @see java.io.OutputStream#write(int)
    */
   public void write(int nByte) throws IOException
   {
      m_raf.write(nByte);
   }
}
