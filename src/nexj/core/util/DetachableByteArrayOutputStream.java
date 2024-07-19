package nexj.core.util;

import java.io.ByteArrayOutputStream;

/**
 * Binary output stream based on a byte array.
 */
public class DetachableByteArrayOutputStream extends ByteArrayOutputStream
{
   // constructors

   /**
    * Creates the output stream with a preallocated array.
    * @param nCount The array size to preallocate.
    */
   public DetachableByteArrayOutputStream(int nCount)
   {
      super(nCount);
   }

   // operations

   /**
    * @return The internal buffer length.
    */
   public int length()
   {
      if (buf == null)
      {
         return 0;
      }

      return buf.length;
   }

   /**
    * Resets the stream.
    * @param nCount The array size to preallocate.
    */
   public void reset(int nCount)
   {
      if (buf == null || buf.length < nCount)
      {
         buf = new byte[nCount];
      }

      count = 0;
   }

   /**
    * Detaches the stream output array and returns it.
    * @return The stream output array.
    */
   public byte[] detach()
   {
      byte[] data = buf;

      buf = null;

      return data;
   }
}
