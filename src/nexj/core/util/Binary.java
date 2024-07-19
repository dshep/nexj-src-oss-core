// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Reader;
import java.io.Serializable;
import java.io.Writer;
import java.util.Arrays;

/**
 * Byte array value. 
 */
public class Binary implements Printable, Comparable, java.io.Serializable
{
   // constants
   
   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = -3144976377826416649L;

   /**
    * Hex digit table.
    */
   protected final static char[] HEX_DIGITS = "0123456789ABCDEF".toCharArray(); 

   /**
    * The number of bytes to convert when running toString().
    */
   protected final static int TOSTRING_LENGTH = 256;

   /**
    * Empty byte array
    */
   private final static byte[] EMPTY_BYTE_ARRAY = new byte[0];

   // attributes

   /**
    * The contained byte array.
    */
   protected byte[] m_data;
   
   /**
    * The cached hash code.
    */
   protected transient int m_nHashCode = 0;

   // constructors

   /**
    * Creates a binary object from a given byte array.
    * @param data The byte array.
    */
   public Binary(byte[] data)
   {
      m_data = (data == null) ? EMPTY_BYTE_ARRAY : data;
   }

   /**
    * Creates a binary object as a copy of another binary object.
    * @param src The source binary object.
    */
   public Binary(Binary src)
   {
      if (src == null || src.m_data == null)
      {
         m_data = EMPTY_BYTE_ARRAY;
      }
      else
      {
         m_data = new byte[src.m_data.length];
         System.arraycopy(src.m_data, 0, m_data, 0, src.m_data.length);
      }
   }

   // operations

   /**
    * Sets the contained byte array.
    * This method is for INTERNAL USE ONLY.
    * @param data The byte array.
    */
   public void setData(byte[] data)
   {
      if (m_data != EMPTY_BYTE_ARRAY)
      {
         throw new IllegalStateException("Attempt to reset the data of Binary");
      }

      m_data = data;
   }

   /**
    * @return The contained byte array. Do NOT modify the array afterwards!
    */
   public byte[] getData()
   {
      return m_data;
   }

   /**
    * Gets the size of the data contained in this object.
    * 
    * @return The size of this Binary in bytes.
    */
   public long getSize()
   {
      return m_data.length;
   }

   /**
    * Gets a byte input stream which gets its data from the binary data of this instance.
    * @return An input stream sourced by the underlying binary data.
    */
   public InputStream getInputStream()
   {
      return new ByteArrayInputStream(m_data);
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

      if (getClass() == obj.getClass())
      {
         return Arrays.equals(m_data, ((Binary)obj).m_data);
      }

      return obj.equals(this);
   }

   /**
    * @see java.lang.Object#hashCode()
    */
   public int hashCode()
   {
      if (m_nHashCode == 0)
      {
         m_nHashCode = hashCode(m_data);
      }

      return m_nHashCode;
   }

   /**
    * @see nexj.core.util.Printable#printOn(nexj.core.util.PrintWriter)
    */
   public void printOn(PrintWriter writer) throws IOException
   {
      writer.write("#z");
      write(writer, m_data, 0, Math.min(TOSTRING_LENGTH, m_data.length));

      if (m_data.length > TOSTRING_LENGTH)
      {
         writer.write("...");
      }
   }

   /**
    * Converts the first TOSTRING_LENGTH bytes (currently 256) of the object
    * to string and appends it to a given buffer.
    * @param buf The destination buffer.
    */
   public void appendTo(StringBuffer buf)
   {
      append(buf, m_data, TOSTRING_LENGTH);
   }

   /**
    * Converts the first TOSTRING_LENGTH bytes (currently 256) of the object
    * to string.
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuffer buf = new StringBuffer(m_data.length << 1);

      appendTo(buf);

      return buf.toString();
   }

   /**
    * Computes the hash code of a byte array.
    * @param data The byte array for which to compute the hash code.
    * @return The hash code.
    */
   public static int hashCode(byte[] data)
   {
      if (data == null)
      {
         return 0;
      }

      int n = 0;

      for (int i = 0; i < data.length; ++i)
      {
         n = (n << 1) ^ data[i] ^ (n >>> (Integer.SIZE - 1));
      }

      return (n == 0) ? 1 : n;
   }

   /**
    * Converts a byte array to a hex string and appends it to a string buffer.
    * @param buf The destination string buffer.
    * @param data The byte array to append.
    * @param nMaxCount The maximum number of bytes to append. -1 means unlimited.
    * If data is longer, ... will be appended at the end.
    */
   public static void append(StringBuffer buf, byte[] data, int nMaxCount)
   {
      if (data == null)
      {
         return;
      }

      if (nMaxCount < 0)
      {
         nMaxCount = data.length;
      }
      else
      {
         nMaxCount = Math.min(nMaxCount, data.length);
      }

      for (int i = 0; i < nMaxCount; ++i)
      {
         int n = data[i];

         buf.append(HEX_DIGITS[(n >> 4) & 0x0F]);         
         buf.append(HEX_DIGITS[n & 0x0F]);
      }

      if (nMaxCount < data.length)
      {
         buf.append("...");
      }
   }

   /**
    * Reads a byte array from a character stream containing a hex representation.
    * @param reader The character stream reader.
    * @param data The byte array.
    * @param nOfs The offset of the first byte in the array.
    * @param nCount The count of characters to read, or -1 to read until the end of the array.
    * @return The count of read characters.
    * @throws IOException if an input error occurs.
    * @throws NumberFormatException if a non-hex character has been encountered.
    */
   public static int read(Reader reader, byte[] data, int nOfs, int nCount) throws IOException, NumberFormatException
   {
      if (nCount < 0)
      {
         nCount = data.length - nOfs;
      }
      
      int n = 0;
      boolean bLow = false;
      
      while (nCount-- != 0)
      {
         int ch = reader.read();
         
         if (ch < 0)
         {
            break;
         }

         ++n;
         
         int nDigit = Character.digit((char)ch, 16);
         
         if (nDigit < 0)
         {
            throw new NumberFormatException("Invalid hex character at offset " + n);
         }
         
         if (bLow)
         {
            data[nOfs] <<= 4;
            data[nOfs++] |= nDigit;
            bLow = false;
         }
         else
         {
            data[nOfs] = (byte)nDigit;
            bLow = true;
         }
      }
      
      return n;
   }

   /**
    * Writes a byte array as a hex string to a character stream.
    * @param writer The character stream writer.
    * @param data The byte array.
    * @param nOfs The offset of the first byte.
    * @param nCount The count of bytes to convert.
    * @throws IOException if an output error occurs.
    */
   public static void write(Writer writer, byte[] data, int nOfs, int nCount) throws IOException
   {
      while (nCount-- > 0)
      {
         int n = data[nOfs++];

         writer.write(HEX_DIGITS[(n >> 4) & 0x0F]);         
         writer.write(HEX_DIGITS[n & 0x0F]);
      }
   }
   
   /**
    * Parses a binary value out of a hex string.
    * @param s The character sequence to parse.
    * @return The parsed binary value.
    * @throws NumberFormatException if the string does not contain only hex digits.
    */
   public static Binary parse(CharSequence s) throws NumberFormatException
   {
      byte[] data = new byte[(s.length() + 1) >> 1];
      boolean bLow = ((s.length() & 1) != 0);
      int k = 0;
      
      for (int i = 0; i < s.length(); ++i)
      {
         int nDigit = Character.digit(s.charAt(i), 16);
         
         if (nDigit < 0)
         {
            throw new NumberFormatException("Invalid character in the binary value at position " + i);
         }
         
         if (bLow)
         {
            data[k] <<= 4;
            data[k++] |= nDigit;
            bLow = false;
         }
         else
         {
            data[k] = (byte)nDigit;
            bLow = true;
         }
      }
      
      return new Binary(data);
   }

   /**
    * Creates a binary value from a Base64 string.
    * @param sText The Base64 string.
    * @return The binary value.
    * @throws IOException if an error occurs.
    */
   public static Binary fromBase64(String sText) throws IOException
   {
      return new Binary(Base64Util.decode(sText));
   }

   /**
    * Encodes the binary value as a Base64 string.
    * @return The encoded string.
    */
   public String toBase64()
   {
      return Base64Util.encode(m_data);
   }

   /**
    * Creates a binary value from a serializable object.
    * @param obj The object to serialize.
    * @return The serialized binary value.
    * @throws IOException if an error occurs.
    */
   public static Binary fromObject(Serializable obj) throws IOException
   {
      ByteArrayOutputStream aos = new ByteArrayOutputStream(256);
      ObjectOutputStream oos = new ObjectOutputStream(aos);

      oos.writeObject(obj);
      oos.close();
      
      return new Binary(aos.toByteArray());
   }

   /**
    * Deserializes an object from the binary value.
    * @return The deserialized object.
    * @throws IOException if an error occurs.
    * @throws ClassNotFoundException if the stream contains an unknown class.
    */
   public Serializable toObject() throws IOException, ClassNotFoundException
   {
      ByteArrayInputStream ais = new ByteArrayInputStream(m_data);
      ObjectInputStream ois = new ObjectInputStream(ais);
      
      return (Serializable)ois.readObject();
   }

   /**
    * Compares two byte arrays bytewise, unsigned comparison.
    * @param left The first byte array.
    * @param right The second byte array.
    * @return negative, 0 or positive number, depending on whether
    * left is less than, equal to or greater than right.
    */
   public static int compare(byte[] left, byte[] right)
   {
      return compare(left, right, left.length, right.length);
   }

   /**
    * Compares two byte arrays bytewise, unsigned comparison.
    * @param left The first byte array.
    * @param right The second byte array.
    * @param nLeftSize The number of bytes to compare in the first array.
    * @param nRightSize The number of bytes to compare in the second array.
    * @return negative, 0 or positive number, depending on whether
    * left is less than, equal to or greater than right.
    */
   public static int compare(byte[] left, byte[] right, int nLeftSize, int nRightSize)
   {
      int n = Math.min(nLeftSize, nRightSize);

      for (int i = 0; i < n; ++i)
      {
         int k = (left[i] & 0xFF) - (right[i] & 0xFF);

         if (k != 0)
         {
            return k;
         }
      }

      return nLeftSize - nRightSize;
   }

   /**
    * @see java.lang.Comparable#compareTo(java.lang.Object)
    */
   public int compareTo(Object obj)
   {
      if (obj.getClass() == getClass())
      {
         return compare(m_data, ((Binary)obj).m_data);
      }
      else
      {
         return -((Comparable)obj).compareTo(this);
      }
   }
}
