// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInput;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.util.Date;

/**
 * Utility for parsing and manipulating binary structures.
 */
public class BinaryUtil
{
   // constants

   /**
    * Replacement character to use when an invalid codepoint is encountered.
    */
   protected final static int INVALID_CODE_POINT = 0xFFFD;
   
   // constructors

   /**
    * Protected constructor to prevent instantiation of the utility class
    */
   protected BinaryUtil()
   {
   }
   
   // operations
   
   /**
    * @return The integer value of the 4 contiguous bytes of data starting at offset.  The
    *    integer is assumed to be in little-endian encoding.
    * @param data The byte array from which to read.  
    * @param nOffset The offset at which to read.
    */
   public static int getInt(byte[] data, int nOffset)
   {
      return (int)getUnsignedInt(data, nOffset);
   }
   
   /**
    * @return The positive integer value of the 4 contiguous bytes of data starting at offset.  The
    *    integer is assumed to be in little-endian encoding.
    * @param data The byte array from which to read.  
    * @param nOffset The offset at which to read.
    */
   public static long getUnsignedInt(byte[] data, int nOffset)
   {
      return (getByte(data, nOffset)
         + (getByte(data, nOffset + 1) << 8) 
         + (getByte(data, nOffset + 2) << 16)
         + (((long)getByte(data, nOffset + 3)) << 24));
   }
   
   /**
    * @return The integer value of the 2 contiguous bytes of data starting at offset.  The
    *    integer is assumed to be in little-endian encoding.
    * @param data The byte array from which to read.  
    * @param data The offset at which to read.
    */
   public static int getShort(byte[] data, int nOffset)
   {
      return getByte(data, nOffset) + (getByte(data, nOffset + 1) << 8);
   }
   
   /**
    * @return The unsigned value of the byte at offset in data
    * @param data The byte array from which to read.  
    * @param nOffset The offset at which to read.
    */
   public static int getByte(byte[] data, int nOffset)
   {
      return 0x00FF & data[nOffset];
   }

   /**
    * Returns the int value of a sequence of nCount bytes in nArray, starting from nIndex.
    * The index range is assumed to be valid; no error checking is performed.
    * @param nArray The byte array to reference.
    * @param nIndex The starting byte index.
    * @param nCount The number of bytes that form the return value.
    * @param bBigEndian Indicates whether to use big-endian (if true) or little-endian byte order (if false).
    * @param bSigned Indicates whether the bytes represent a signed (if true) or unsigned number (if false).
    * @return The int value of the sequence of bytes.
    */
   public static int getInt(byte[] nArray, int nIndex, int nCount, boolean bBigEndian, boolean bSigned)
   {
      int nInc = 1;
      int nEnd = nIndex + nCount;

      if (!bBigEndian)
      {
         nEnd = nIndex - 1;
         nIndex += nCount - 1;
         nInc = -1;
      }

      int nValue = nArray[nIndex];

      if (!bSigned)
      {
         nValue &= 0xFFL;
      }

      for (nIndex += nInc; nIndex != nEnd; nIndex += nInc)
      {
         nValue = (nValue << Byte.SIZE) | (nArray[nIndex] & 0xFF);
      }

      return nValue;
   }

   /**
    * Returns the long value of a sequence of nCount bytes in nArray, starting from nIndex.
    * The index range is assumed to be valid; no error checking is performed.
    * @param nArray The byte array to reference.
    * @param nIndex The starting byte index.
    * @param nCount The number of bytes that form the return value.
    * @param bBigEndian Indicates whether to use big-endian (if true) or little-endian byte order (if false).
    * @param bSigned Indicates whether the bytes represent a signed (if true) or unsigned number (if false).
    * @return The long value of the sequence of bytes.
    */
   public static long getLong(byte[] nArray, int nIndex, int nCount, boolean bBigEndian, boolean bSigned)
   {
      int nInc = 1;
      int nEnd = nIndex + nCount;

      if (!bBigEndian)
      {
         nEnd = nIndex - 1;
         nIndex += nCount - 1;
         nInc = -1;
      }

      long lValue = nArray[nIndex];

      if (!bSigned)
      {
         lValue &= 0xFFL;
      }

      for (nIndex += nInc; nIndex != nEnd; nIndex += nInc)
      {
         lValue = (lValue << Byte.SIZE) | (nArray[nIndex] & 0xFF);
      }

      return lValue;
   }

   /**
    * Returns the value of a sequence of nCount bytes in nArray, starting from nIndex, as a BigInteger.
    * The index range is assumed to be valid; no error checking is performed.
    * @param nArray The byte array to reference.
    * @param nIndex The starting byte index.
    * @param nCount The number of bytes that form the return value.
    * @param bBigEndian Indicates whether to use big-endian (if true) or little-endian byte order (if false).
    * @param bSigned Indicates whether the bytes represent a signed (if true) or unsigned number (if false).
    * @return The value of the sequence of bytes as a BigInteger.
    */
   public static BigInteger getBigInteger(byte[] nArray, int nIndex, int nCount, boolean bBigEndian, boolean bSigned)
   {
      if (bSigned)
      {
         if (bBigEndian)
         {
            if (nCount != nArray.length)
            {
               byte[] nSubArray = new byte[nCount];
   
               System.arraycopy(nArray, nIndex, nSubArray, 0, nCount);
               nArray = nSubArray;
            }
         }
         else
         {
            byte[] nReversedArray = new byte[nCount];

            reverse(nArray, nIndex, nReversedArray, 0, nCount);
            nArray = nReversedArray;
         }
      }
      else
      {
         if (bBigEndian)
         {
            if (nArray[0] < 0x80)
            {
               byte[] nSubArray = new byte[nCount+1];

               nSubArray[0] = 0;
               System.arraycopy(nArray, nIndex, nSubArray, 1, nCount);
               nArray = nSubArray;
            }
         }
         else
         {
            byte[] nReversedArray = new byte[nCount + 1];

            nReversedArray[0] = 0;
            reverse(nArray, nIndex, nReversedArray, 1, nCount);
            nArray = nReversedArray;
         }
      }

      return new BigInteger(nArray);
   }

   /**
    * Returns the numerical value of a sequence of nCount bytes in nArray, starting from nIndex.
    * The index range is assumed to be valid; no error checking is performed.
    * @param nArray The byte array to reference.
    * @param nIndex The starting byte index.
    * @param nCount The number of bytes that form the return value.
    * @param bBigEndian Indicates whether to use big-endian (if true) or little-endian byte order (if false).
    * @param bSigned Indicates whether the bytes represent a signed (if true) or unsigned number (if false).
    * @return The numerical value of the sequence of bytes, either as a Long or a BigDecimal.
    */
   public static Number getNumber(byte[] nArray, int nIndex, int nCount, boolean bBigEndian, boolean bSigned)
   {
      if (nCount < (Long.SIZE >> 3) + ((bSigned) ? 1 : 0))
      {
         return Long.valueOf(getLong(nArray, nIndex, nCount, bBigEndian, bSigned));
      }

      return new BigDecimal(getBigInteger(nArray, nIndex, nCount, bBigEndian, bSigned));
   }
   
   /**
    * Return a string value of the given length starting at the given index in the given 
    * array in the default system encoding.
    * @return The string
    * @param data The byte array to read from.
    * @param nOffset The offset at which to start reading.
    * @param nLength The number of bytes to read.
    */
   public static String getString(byte[] data, int nOffset, int nLength)
   {
      try
      {
         return new String(data, nOffset, nLength, "ISO-8859-1");
      }
      catch (java.io.UnsupportedEncodingException e)
      {
         throw new Error(e);
      }
   }

   /**
    * Returns the String value of the given byte array, starting from the given index up to the given number of bytes.
    * @param nArray The byte array to reference.
    * @param nIndex The starting byte index.
    * @param nLength The number of bytes to read.
    * @param bBigEndian Indicates whether to use big-endian (if true) or little-endian byte order (if false).
    * @return The resulting String.
    */
   public static String getStringUTF32(byte[] nArray, int nIndex, int nLength, boolean bBigEndian)
   {
      int[] nCodePtArray = new int[(nLength - nIndex) / 4 + 1];
      int nCount = 0;

      int nEnd = nLength - 4; // index of the most significant byte of the last 4-byte word

      if (bBigEndian)
      {
         while (nIndex <= nEnd)
         {
            nCodePtArray[nCount++] = createCodePoint(nArray[nIndex], nArray[nIndex + 1], nArray[nIndex + 2], nArray[nIndex + 3]);
            nIndex += 4;
         }
      }
      else
      {
         while (nIndex <= nEnd)
         {
            nCodePtArray[nCount++] = createCodePoint(nArray[nIndex + 3], nArray[nIndex + 2], nArray[nIndex + 1], nArray[nIndex]);
            nIndex += 4;
         }
      }

      if (nIndex < nLength)
      {
         nCodePtArray[nCount++] = INVALID_CODE_POINT;
      }

      return new String(nCodePtArray, 0, nCount);
   }

   /**
    * Convert the given string to a byte array.
    * @param sValue The string to convert.
    * @param bBigEndian The byte array is to be in big-endian byte order, if true,
    * and in little-endian byte order, if false.
    * @return The byte array corresponding to the given string in UTF-32BE encoding.
    */
   public static byte[] getBytesUTF32(String sValue, boolean bBigEndian)
   {
      int nLength = sValue.length();
      byte[] nArray = new byte[nLength * 4];
      int k = 0;

      if (bBigEndian)
      {
         for (int i = 0; i < nLength; i += 1)
         {
            int nValue = sValue.codePointAt(i);

            nArray[k++] = (byte)(nValue >> 24);
            nArray[k++] = (byte)(nValue >> 16);
            nArray[k++] = (byte)(nValue >> 8);
            nArray[k++] = (byte)nValue;
         }
      }
      else
      {
         for (int i = 0; i < nLength; i += 1)
         {
            int nValue = sValue.codePointAt(i);

            nArray[k++] = (byte)nValue;
            nArray[k++] = (byte)(nValue >> 8);
            nArray[k++] = (byte)(nValue >> 16);
            nArray[k++] = (byte)(nValue >> 24);
         }
      }

      return nArray;
   }

   /**
    * Sets the byte at offset nOffset.
    * @param data The byte array to which to write.  
    * @param nOffset The offset at which to write.
    * @param nValue The value to write.
    */
   public static void setByte(byte[] data, int nOffset, int nValue)
   {
      data[nOffset] = (byte)nValue;
   }

   /**
    * Writes a 4 byte number to an array in little-endian format, starting
    * at offset nOffset.
    * @param data The byte array to which to write.  
    * @param nOffset The offset at which to write.
    * @param nValue The integer value to write.
    */
   public static void setInt(byte[] data, int nOffset, int nValue)
   {
      setUnsignedInt(data, nOffset, nValue);
   }
   
   /**
    * Writes a 4 byte number to an array in little-endian format, starting
    * at offset nOffset.
    * @param data The byte array to which to write.  
    * @param nOffset The offset at which to write.
    * @param lValue The integer value to write.
    */
   public static void setUnsignedInt(byte[] data, int nOffset, long lValue)
   {
      setByte(data, nOffset, (int)(lValue));
      setByte(data, nOffset + 1, (int)(lValue >> 8));
      setByte(data, nOffset + 2, (int)(lValue >> 16));
      setByte(data, nOffset + 3, (int)(lValue >> 24));
   }
   
   /**
    * Writes a 2 byte number to an array in little-endian format, starting
    * at offset nOffset.
    * @param data The byte array to which to write.  
    * @param nOffset The offset at which to write.
    * @param nValue The value to write.
    */
   public static void setShort(byte[] data, int nOffset, int nValue)
   {
      setByte(data, nOffset, nValue);
      setByte(data, nOffset + 1, nValue >> 8);
   }

   /**
    * Replaces the sequence of bytes in nArray, starting from nIndex, by the sequence of nCount bytes
    * that form newValue. The index range is assumed to be valid; no error checking is performed.
    * @param nArray The byte array to modify.
    * @param nIndex The starting index in nArray.
    * @param nCount The number of bytes in nArray to be replaced.
    * @param newValue The numerical value used to replace the byte sequence. Assumed to fit into nCount bytes.
    * @param bBigEndian Indicates whether to use big-endian (if true) or little-endian byte order (if false).
    * @param bSigned Indicates whether to use the signed (if true) or unsigned representation (if false).
    */
   public static void setNumber(byte[] nArray, int nIndex, int nCount, Number newValue, boolean bBigEndian, boolean bSigned)
   {
      if (nCount <= Long.SIZE >> 3)
      {
         setLong(nArray, nIndex, nCount, newValue.longValue(), bBigEndian);
      }
      else
      {
         setBigInteger(nArray, nIndex, nCount, ((BigDecimal)newValue).toBigInteger(), bBigEndian, bSigned);
      }
   }

   /**
    * Replaces the sequence of bytes in nArray, starting from nIndex, by the sequence of nCount bytes
    * that form intValue. The index range is assumed to be valid; no error checking is performed.
    * @param nArray The byte array to modify.
    * @param nIndex The starting index in nArray.
    * @param nCount The number of bytes in nArray to be replaced.
    * @param intValue The numerical value used to replace the byte sequence. Assumed to fit into nCount bytes.
    * @param bBigEndian Indicates whether to use big-endian (if true) or little-endian byte order (if false).
    * @param bSigned Indicates whether to use the signed (if true) or unsigned representation (if false).
    */
   public static void setBigInteger(byte[] nArray, int nIndex, int nCount, BigInteger intValue, boolean bBigEndian, boolean bSigned)
   {
      byte nSignByte = 0;

      if (bSigned && intValue.compareTo(BigInteger.ZERO) < 0)
      {
         nSignByte = -1;
      }

      byte[] nSrcArray = intValue.toByteArray();
      int nLength = nSrcArray.length;
      int nStart = 0;

      if (bBigEndian)
      {
         if (nLength > nCount) // nSrcArray contains extra bytes at the beginning
         {
            nStart = nLength - nCount;
            nLength = nCount;
         }
         else // nSrcArray contains less than nCount bytes, need to left-pad with the sign byte
         {
            int nEnd = nIndex + nCount - nLength;

            while (nIndex < nEnd)
            {
               nArray[nIndex++] = nSignByte;
            }
         }

         System.arraycopy(nSrcArray, nStart, nArray, nIndex, nLength);
      }
      else
      {
         if (nLength > nCount) // nSrcArray contains extra bytes at the beginning
         {
            nStart = nLength - nCount;
            nLength = nCount;
         }
         else // nSrcArray contains less than nCount bytes, need to right-pad with the sign byte
         {
            int i = nIndex + nLength;
            int nEnd = nIndex + nCount;

            while (i < nEnd)
            {
               nArray[i++] = nSignByte;
            }
         }

         reverse(nSrcArray, nStart, nArray, nIndex, nLength);
      }
   }

   /**
    * Replaces the sequence of bytes in nArray, starting from nIndex, by the sequence of nCount bytes
    * that form lValue. The index range is assumed to be valid; no error checking is performed.
    * @param nArray The byte array to modify.
    * @param nIndex The starting index in nArray.
    * @param nCount The number of bytes in nArray to be replaced.
    * @param lValue The long value used to replace the byte sequence. Assumed to fit into nCount bytes.
    * @param bBigEndian Indicates whether to use big-endian (if true) or little-endian byte order (if false).
    */
   public static void setLong(byte[] nArray, int nIndex, int nCount, long lValue, boolean bBigEndian)
   {
      int i = nIndex + nCount - 1;

      if (bBigEndian)
      {
         while (i >= nIndex)
         {
            nArray[i--] = (byte)(lValue & 0xFF);
            lValue >>= 8;
         }
      }
      else
      {
         while (nIndex <= i)
         {
            nArray[nIndex++] = (byte)(lValue & 0xFF);
            lValue >>= 8;
         }
      }
   }

   /**
    * Replaces the sequence of bytes in nArray, starting from nIndex, by the sequence of nCount bytes
    * that form nValue. The index range is assumed to be valid; no error checking is performed.
    * @param nArray The byte array to modify.
    * @param nIndex The starting index in nArray.
    * @param nCount The number of bytes in nArray to be replaced.
    * @param nValue The int value used to replace the byte sequence. Assumed to fit into nCount bytes.
    * @param bBigEndian Indicates whether to use big-endian (if true) or little-endian byte order (if false).
    */
   public static void setInt(byte[] nArray, int nIndex, int nCount, int nValue, boolean bBigEndian)
   {
      int i = nIndex + nCount - 1;

      if (bBigEndian)
      {
         while (i >= nIndex)
         {
            nArray[i--] = (byte)(nValue & 0xFF);
            nValue >>= 8;
         }
      }
      else
      {
         while (nIndex <= i)
         {
            nArray[nIndex++] = (byte)(nValue & 0xFF);
            nValue >>= 8;
         }
      }
   }

   /**
    * Reads a specific bit from an integer.
    * @return The value of nBitmask at nOffset.
    * @param nBitmask The bitmask to read from.
    * @param nOffset The index of the bit to read (0 is lowest significance).
    */
   public static int getBit(int nBitmask, int nOffset)
   {
      return ((nBitmask & (1 << nOffset)) >> nOffset);
   }
   
   /**
    * Reads a specific bit from an integer.
    * @return The value of nBitmask at nOffset.
    * @param nBitmask The bitmask to read from.
    * @param nOffset The index of the bit to test (0 is lowest significance).
    */
   public static boolean hasBit(int nBitmask, int nOffset)
   {
      return (nBitmask & (1 << nOffset)) != 0;
   }
   
   /**
    * Writes a specific bit to an integer.
    * @return The value of nBitmask, with the bit at nOffset set to nValue.
    * @param nBitmask The bitmask to write to.
    * @param nOffset The index of the bit to write (0 is lowest significance).
    * @param nValue The value of the bit to write (0 or 1).
    */
   public static int setBit(int nBitmask, int nOffset, int nValue)
   {
      return (nBitmask & (nBitmask ^ (1 << nOffset))) | (nValue << nOffset);
   }

   /**
    * Reads a single serialized object from a data input stream.
    * This method is for INTERNAL USE ONLY.
    * @param is The input stream.
    * @return the deserialized object.
    * @throws EOFException On end of stream.
    * @throws IOException On stream input failure.
    */
   public static Object read(DataInput is) throws IOException
   {
      int nType;

      if (is instanceof ObjectInput)
      {
         nType = ((ObjectInput)is).read();
      }
      else
      {
         nType = ((InputStream)is).read();
      }

      if (nType == -1)
      {
         throw new EOFException();
      }

      if ((nType & 0xC0) == 0xC0)
      {
         switch (nType & 0x3F)
         {
            case 0:
               return null;
            case 1:
               return Integer.valueOf(is.readInt());
            case 2:
               return Long.valueOf(is.readLong());
            case 3:
               return Float.valueOf(is.readFloat());
            case 4:
               return Double.valueOf(is.readDouble());
            case 5:
               return new Timestamp(is.readLong());
            case 6:
               return Boolean.FALSE;
            case 7:
               return Boolean.TRUE;
            default:
               throw new IOException("Invalid Object type: " + nType);
         }
      }

      int nLength = ((nType & 0x3F) == 0x3F) ? is.readInt() : nType & 0x3F;

      switch ((nType >> 6) & 0x03)
      {
         case 0:
         {
            byte[] data = new byte[nLength];

            is.readFully(data);

            return new Binary(data);
         }
         case 1:
         {
            byte[] data = new byte[nLength];

            is.readFully(data);

            return new String(data, "UTF-8");
         }
         case 2:
         {
            int nScale = is.readInt();
            byte[] data = new byte[nLength];

            is.readFully(data);

            return new BigDecimal(new BigInteger(data), nScale);
         }
         default:
            return null;
      }
   }

   /**
    * Writes a single serialized object to a data output stream.
    * This method is for INTERNAL USE ONLY.
    * @param os The output stream, to which to write the data.
    * @param value The object to serialize to the output stream.
    * @throws IOException if an error occurs.
    */
   public static void write(DataOutput os, Object value) throws IOException
   {
      if (value instanceof Binary)
      {
         byte[] data = ((Binary)value).getData();

         if (data.length < 0x3F)
         {
            os.writeByte(data.length);
         }
         else
         {
            os.writeByte(0x3F);
            os.writeInt(data.length);
         }

         os.write(data);
      }
      else if (value instanceof String)
      {
         byte[] data = ((String)value).getBytes("UTF-8");

         if (data.length < 0x3F)
         {
            os.writeByte(0x40 | data.length);
         }
         else
         {
            os.writeByte(0x7F);
            os.writeInt(data.length);
         }

         os.write(data);
      }
      else if (value instanceof Number)
      {
         if (value instanceof Integer)
         {
            os.writeByte(0xC1);
            os.writeInt(((Integer)value).intValue());
         }
         else if (value instanceof Long)
         {
            os.writeByte(0xC2);
            os.writeLong(((Long)value).longValue());
         }
         else if (value instanceof Float)
         {
            os.writeByte(0xC3);
            os.writeFloat(((Float)value).floatValue());
         }
         else if (value instanceof Double)
         {
            os.writeByte(0xC4);
            os.writeDouble(((Double)value).doubleValue());
         }
         else if (value instanceof BigDecimal)
         {
            BigDecimal dec = (BigDecimal)value;
            byte[] data = dec.unscaledValue().toByteArray();

            if (data.length < 0x3F)
            {
               os.writeByte(0x80 | data.length);
            }
            else
            {
               os.writeByte(0xBF);
               os.writeInt(data.length);
            }

            os.writeInt(dec.scale());
            os.write(data);
         }
         else
         {
            throw new IOException("Invalid Object type: " + value.getClass().getName());
         }
      }
      else if (value instanceof Date)
      {
         os.writeByte(0xC5);
         os.writeLong(((Date)value).getTime());
      }
      else if (value instanceof Boolean)
      {
         os.writeByte((((Boolean)value).booleanValue()) ? 0xC7 : 0xC6);
      }
      else if (value == null)
      {
         os.writeByte(0xC0);
      }
      else
      {
         throw new IOException("Invalid Object type: " + value.getClass().getName());
      }
   }

   /**
    * Copies n elements from nSrcArray, starting from nSrcIndex, to nDstArray,
    * starting from nDstIndex, in reversed order. The index ranges are
    * assumed to be valid; no error checking is performed. The memory regions
    * for the source and the destination are assumed to be non-overlapping. 
    * @param nSrcArray The source array to copy from.
    * @param nSrcIndex The starting index in the source array.
    * @param nDstArray The destination array to copy to in reversed order.
    * @param nDstIndex The starting index in the destination array.
    * @param n The number of elements to copy.
    * @return The modified destination array.
    */
   private static byte[] reverse(byte[] nSrcArray, int nSrcIndex, byte[] nDstArray, int nDstIndex, int n)
   {
      int i = nDstIndex + n - 1;

      while (i >= nDstIndex)
      {
         nDstArray[i--] = nSrcArray[nSrcIndex++];
      }

      return nDstArray;
   }

   /**
    * Converts the given bytes, interpreted in UTF-32BE encoding, to a Unicode code point.
    * @param nFirst The first byte.
    * @param nSecond The second byte.
    * @param nThird The third byte.
    * @param nFourth The fourth byte.
    * @return The corresponding Unicode code point, or U+FFFD if the given bytes do not
    * form a valid UTF-32BE code point.
    */
   private static int createCodePoint(byte nFirst, byte nSecond, byte nThird, byte nFourth)
   {
      if (nFirst != 0 || nSecond == 0x11 || (nSecond == 0 && nThird >= (byte)0xD8 && nThird <= (byte)0xDF))
      {
         return INVALID_CODE_POINT;
      }

      return nFirst << 24 | (nSecond & 0xFF) << 16 | (nThird & 0xFF) << 8 | (nFourth & 0xFF);
   }
}