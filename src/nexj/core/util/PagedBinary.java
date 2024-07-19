// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.SequenceInputStream;
import java.io.Serializable;
import java.io.StringWriter;

/**
 * Byte array value.
 */
public class PagedBinary extends Binary
{
   // constants

   /**
    * Serialization version.
    */
   private static final long serialVersionUID = 1628681993331992415L;

   /**
    * The size (in bytes) at which the binary object's data will be stored on disk.
    */
   public final static int DEFAULT_SWAP_THRESHOLD = 5 << 20;

   /**
    * The first RETAINED_SIZE bytes of the paged binary will be held in memory
    * at all times to speed operations such as toString().
    */
   public final static int RETAINED_SIZE = TOSTRING_LENGTH + 1;

   /**
    * The buffer size for file operations.
    */
   public final static int BUFFER_SIZE = 8192;


   // associations

   /**
    * The on-disk contents of the binary. Null if not paged to disk.
    */
   protected SwappedBinaryStorageStrategy m_storage;


   // constructors

   /**
    * Creates a binary object from the data in a given input stream.
    * 
    * @param istream The input stream containing the data for the binary.
    * @param nSwapThreshold Overrides the default swap threshold. Data streams
    *                       whose size is equal to or greater than the swap
    *                       threshold will be paged to disk.
    * @throws IOException If an I/O error occurs.
    */
   public PagedBinary(InputStream istream, int nSwapThreshold) throws IOException
   {
      super((byte[])null);

      byte[] nDataArray;
      LimitInputStream limitStream = new LimitInputStream(istream, nSwapThreshold, false);
      ByteArrayOutputStream bos = new ByteArrayOutputStream();

      IOUtil.copy(bos, limitStream);
      bos.close();
      limitStream = null;
      nDataArray = bos.toByteArray();

      if (nDataArray.length == 0)
      {
         m_storage = null;
      }
      else if (nDataArray.length < nSwapThreshold)
      {
         m_data = nDataArray;
         m_storage = null;
      }
      else
      {
         // Requires NoCloseInputStream because SequenceInputStream calls close() 
         m_storage = new SwappedBinaryStorageStrategy(new SequenceInputStream(
            new ByteArrayInputStream(nDataArray), new NoCloseInputStream(istream)));
         m_data = new byte[Math.min(RETAINED_SIZE, nSwapThreshold)];
         System.arraycopy(nDataArray, 0, m_data, 0, m_data.length);
      }
   }

   /**
    * Creates a binary object from the data in a given input stream, using
    * the default swap threshold.
    * 
    * @param istream The input stream containing the data for the binary.
    * @throws IOException If an I/O error occurs.
    */
   public PagedBinary(InputStream istream) throws IOException
   {
      this(istream, DEFAULT_SWAP_THRESHOLD);
   }

   /**
    * Creates a binary object from a given byte array.
    * @param data The byte array.
    */
   public PagedBinary(byte[] nDataArray)
   {
      super(nDataArray);

      m_storage = null;
   }

   /**
    * Creates a binary object as a copy of another binary object.
    * @param src The source binary object.
    */
   public PagedBinary(Binary src)
   {
      super(src);

      if (src instanceof PagedBinary)
      {
         PagedBinary pagedSrc = (PagedBinary)src;

         if (pagedSrc.m_storage == null)
         {
            m_storage = null;
         }
         else
         {
            InputStream istream = null;

            try
            {
               istream = pagedSrc.m_storage.getInputStream();

               m_storage = new SwappedBinaryStorageStrategy(istream);
            }
            catch (IOException ex)
            {
               throw ObjUtil.rethrow(ex);
            }
            finally
            {
               IOUtil.close(istream);
            }
         }
      }
   }


   // operations

   /**
    * Gets the size of the data contained in this object.
    * 
    * @return The size of this Binary in bytes.
    */
   public long getSize()
   {
      if (m_storage == null)
      {
         return super.getSize();
      }

      return ((SwappedBinaryStorageStrategy)m_storage).getSize();
   }

   /**
    * @return The contained byte array. Do NOT modify the array afterwards!
    */
   public byte[] getData()
   {
      if (m_storage == null)
      {
         return super.getData();
      }
      else
      {
         return ((SwappedBinaryStorageStrategy)m_storage).getData();
      }
   }

   /**
    * Gets a byte input stream which gets its data from the binary data of this instance.
    * @return An input stream sourced by the underlying binary data.
    */
   public InputStream getInputStream()
   {
      if (m_storage == null)
      {
         return super.getInputStream();
      }
      else
      {
         return ((SwappedBinaryStorageStrategy)m_storage).getInputStream();
      }
   }

   /**
    * @see java.lang.Object#equals(java.lang.Object)
    */
   public boolean equals(Object obj)
   {
      if (!(obj instanceof Binary))
      {
         return false;
      }

      if (hashCode() != obj.hashCode())
      {
         return false;
      }

      return compareTo(obj) == 0;
   }

   /**
    * @see java.lang.Object#hashCode()
    */
   public int hashCode()
   {
      if (m_storage == null)
      {
         return super.hashCode();
      }
      else
      {
         return m_storage.hashCode();
      }
   }

   /**
    * Destroys this binary object, releasing any resources held
    * by its data storage.
    */
   public void dispose()
   {
      if (m_storage != null)
      {
         ((SwappedBinaryStorageStrategy)m_storage).dispose();
      }
   }

   /**
    * @see java.lang.Object#finalize()
    */
   protected void finalize() throws Throwable
   {
      dispose();
   }

   /**
    * Encodes the binary value as a Base64 string.
    * @return The encoded string.
    */
   public String toBase64()
   {
      if (m_storage == null)
      {
         return super.toBase64();
      }
      else
      {
         return ((SwappedBinaryStorageStrategy)m_storage).toBase64();
      }
   }

   /**
    * Deserializes an object from the binary value.
    * @return The deserialized object.
    * @throws IOException if an error occurs.
    * @throws ClassNotFoundException if the stream contains an unknown class.
    */
   public Serializable toObject() throws IOException, ClassNotFoundException
   {
      if (m_storage == null)
      {
         return super.toObject();
      }
      else
      {
         return ((SwappedBinaryStorageStrategy)m_storage).toObject();
      }
   }

   /**
    * Compares two byte streams bytewise, unsigned comparison.
    * @param left The first byte input stream.
    * @param right The second byte input stream.
    * @return negative, 0, or positive number, depending on whether
    * left is less than, equal to, or greater than right, respectively.
    * @throws IOException If an I/O error occurs.
    */
   public static int compare(InputStream left, InputStream right) throws IOException
   {
      byte[] nLeftArray = new byte[BUFFER_SIZE];
      byte[] nRightArray = new byte[BUFFER_SIZE];

      for (;;)
      {
         int nLeftCount = left.read(nLeftArray);
         int nRightCount = right.read(nRightArray);

         if ((nLeftCount | nRightCount) >= 0)
         {
            int nDifference = Binary.compare(nLeftArray, nRightArray, nLeftCount, nRightCount);
   
            if (nDifference != 0)
            {
               return nDifference;
            }
         }
         else if (nLeftCount == -1)
         {
            if (nRightCount == -1)
            {
               return 0;
            }
            else
            {
               return -1;
            }
         }
         else
         {
            return 1;
         }
      }
   }


   /**
    * @see java.lang.Comparable#compareTo(java.lang.Object)
    */
   public int compareTo(Object obj)
   {
      Binary other = (Binary)obj;
      int i = 0;
      boolean bDoneSelf = false;
      boolean bDoneOther = false;

      for (;;)
      {
         bDoneSelf = (i >= m_data.length);
         bDoneOther = (i >= other.m_data.length);

         if (!(bDoneSelf || bDoneOther))
         {
            int k = (m_data[i] & 0xFF) - (other.m_data[i] & 0xFF);

            if (k != 0)
            {
               return k;
            }
         }
         else
         {
            break;
         }

         i++;
      }

      // Compare on-disk data.
      if (m_storage != null)
      {
         if (obj instanceof PagedBinary)
         {
            PagedBinary paged = (PagedBinary)obj;

            if (paged.m_storage != null)
            {
               return m_storage.compareTo(paged.m_storage);
            }
         }

         return m_storage.compareTo(other.m_data);
      }
      else
      {
         if (obj instanceof PagedBinary)
         {
            PagedBinary paged = (PagedBinary)obj;

            if (paged.m_storage != null)
            {
               return -paged.m_storage.compareTo(m_data);
            }
         }

         if (bDoneSelf && bDoneOther)
         {
            return 0;
         }
         else if (bDoneSelf)
         {
            return -1;
         }
         else
         {
            return 1;
         }
      }
   }


   // inner classes

   /**
    * Stores the byte data in a temporary file.
    */
   protected static class SwappedBinaryStorageStrategy implements Serializable
   {
      // constants

      /**
       * The serialization version.
       */
      private static final long serialVersionUID = -4164920374421311081L;


      // attributes

      /**
       * The cached hash code.
       */
      private int m_nHashCode = 0;

      /**
       * The size of the binary data.
       */
      private long m_lSize;


      // associations

      /**
       * The file that serves as the backing store for the binary data.
       */
      protected transient File m_file;


      // constructors

      /**
       * Creates a new instance from an input stream.
       * 
       * @param istream The input stream from which the data shall be read.
       * @throws IOException If an I/O error occurs.
       */
      public SwappedBinaryStorageStrategy(InputStream istream) throws IOException
      {
         m_file = makeFile();

         OutputStream fos = null;

         try
         {
            byte[] buf = new byte[BUFFER_SIZE];
            long lSize = 0;
            int nHash = 0;

            fos = new BufferedOutputStream(new FileOutputStream(m_file));

            // Copy the data
            for (;;)
            {
               int nCount = istream.read(buf);

               if (nCount <= 0)
               {
                  break;
               }

               fos.write(buf, 0, nCount);
               lSize += nCount;

               // Update hash code
               for (int i = 0; i < nCount; i++)
               {
                  nHash = (nHash << 1) ^ buf[i] ^ (nHash >>> (Integer.SIZE - 1));
               }
            }

            m_nHashCode = (nHash == 0) ? 1 : nHash;
            m_lSize = lSize;
         }
         finally
         {
            if (fos != null)
            {
               fos.close();
            }

            assert m_lSize == m_file.length();
         }
      }


      // operations

      /**
       * Makes a temporary file for storing the Binary's data.
       * @return The abstract path to a temporary file.
       * @throws IOException If an I/O error occurs.
       */
      protected File makeFile() throws IOException
      {
         File tempDir = J2EEUtil.getTempDir();

         if (!tempDir.exists())
         {
            tempDir.mkdir();
         }

         return File.createTempFile("bin", ".swp", tempDir);
      }

      /**
       * Gets the size of the data stored in the storage.
       * 
       * @return The size of the data, in bytes.
       */
      public long getSize()
      {
         return m_lSize;
      }

      /**
       * @see java.lang.Comparable#compareTo(java.lang.Object)
       */
      public int compareTo(Object other)
      {
         InputStream istream = null;
         InputStream otherInputStream = null;

         try
         {
            istream = new BufferedInputStream(new FileInputStream(m_file));

            if (other instanceof SwappedBinaryStorageStrategy)
            {
               otherInputStream = ((SwappedBinaryStorageStrategy)other).getInputStream();
            }
            else
            {
               otherInputStream = new ByteArrayInputStream((byte[])other);
            }

            return PagedBinary.compare(istream, otherInputStream);
         }
         catch (IOException ex)
         {
            ObjUtil.rethrow(ex);
            return 0;
         }
         finally
         {
            IOUtil.close(istream);
            IOUtil.close(otherInputStream);
         }
      }

      /**
       * Compares the contents of two binary stores, bytewise.
       * 
       * @param other The data storage to compare with.
       * @return True if equal; false otherwise.
       */
      public boolean equals(Object other)
      {
         InputStream istream = null;
         InputStream otherInputStream = null;

         try
         {
            istream = new BufferedInputStream(new FileInputStream(m_file));

            if (other instanceof SwappedBinaryStorageStrategy)
            {
               otherInputStream = ((SwappedBinaryStorageStrategy)other).getInputStream();
            }
            else
            {
               otherInputStream = new ByteArrayInputStream((byte[])other);
            }

            return PagedBinary.compare(istream, otherInputStream) == 0;
         }
         catch (IOException ex)
         {
            ObjUtil.rethrow(ex);
            return false;
         }
         finally
         {
            IOUtil.close(istream);
            IOUtil.close(otherInputStream);
         }
      }

      /**
       * Gets the data as an array.
       * 
       * @return The data as a byte array.
       */
      public byte[] getData()
      {
         InputStream istream = null;
   
         try
         {
            ByteArrayOutputStream bos = new ByteArrayOutputStream();

            istream = new BufferedInputStream(new FileInputStream(m_file));
            IOUtil.copy(bos, istream);
            bos.close();
            return bos.toByteArray();
         }
         catch (IOException ex)
         {
            ObjUtil.rethrow(ex);
            return null;
         }
         finally
         {
            IOUtil.close(istream);
         }
      }

      /**
       * @see nexj.core.util.Binary#getInputStream()
       */
      public InputStream getInputStream()
      {
         try
         {
            return new BufferedInputStream(new FileInputStream(m_file));
         }
         catch (IOException ex)
         {
            ObjUtil.rethrow(ex);
            return null;
         }
      }

      /**
       * @see nexj.core.util.Binary#toBase64()
       */
      public String toBase64()
      {
         InputStream fis = null;

         try
         {
            StringWriter writer = new StringWriter();

            fis = new BufferedInputStream(new FileInputStream(m_file));
            Base64Util.encode(fis, writer, -1, false);
            writer.close();
            return writer.toString();
         }
         catch (IOException ex)
         {
            ObjUtil.rethrow(ex);
            return null;
         }
         finally
         {
            IOUtil.close(fis);
         }
      }

      /**
       * @see nexj.core.util.Binary#toObject()
       */
      public Serializable toObject() throws IOException, ClassNotFoundException
      {
         InputStream fis = null;

         try
         {
            fis = new BufferedInputStream(new FileInputStream(m_file));

            ObjectInputStream ois = new ObjectInputStream(fis);

            return (Serializable)ois.readObject();
         }
         finally
         {
            IOUtil.close(fis);
         }
      }

      /**
       * Gets the hash code of the data.
       * 
       * @return The hash code.
       */
      public int hashCode()
      {
         return m_nHashCode;
      }

      /**
       * Releases any resources held in the storage of these data.
       */
      public void dispose()
      {
         if (m_file == null)
         {
            return;
         }

         if (m_file.exists())
         {
            if (!m_file.delete())
            {
               m_file.deleteOnExit();
            }
         }
      }

      /**
       * @see java.lang.Object#finalize()
       */
      protected void finalize() throws Throwable
      {
         dispose();
      }

      /**
       * @see java.io.Serializable
       */
      private void writeObject(ObjectOutputStream out) throws IOException
      {
         out.defaultWriteObject();

         InputStream istream = null;

         try
         {
            istream = new BufferedInputStream(new FileInputStream(m_file));
            IOUtil.copy(out, istream);
         }
         finally
         {
            IOUtil.close(istream);
         }
      }

      /**
       * @see java.io.Serializable
       */
      private void readObject(ObjectInputStream in) throws IOException, ClassNotFoundException
      {
         in.defaultReadObject();

         m_file = makeFile();

         InputStream istream = new LimitInputStream(in, m_lSize, false);
         OutputStream ostream = null;

         try
         {
            ostream = new BufferedOutputStream(new FileOutputStream(m_file));
            IOUtil.copy(ostream, istream);
         }
         finally
         {
            if (ostream != null)
            {
               ostream.close();
            }
         }
      }
   }
}
