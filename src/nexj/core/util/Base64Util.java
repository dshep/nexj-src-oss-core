// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.util.Arrays;

/**
 * RFC 2045 Base64 encoding/decoding utilities.
 */
public class Base64Util
{
   // constants

   /**
    * The Base64 character encoding array (byte value to char code).
    */
   protected final static char[] ENCODING_ARRAY =
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/".toCharArray();

   /**
    * The Base64 character decoding array (char code to byte value).
    */
   protected final static int[] DECODING_ARRAY = new int[128];

   static
   {
      Arrays.fill(DECODING_ARRAY, -1);

      for (int i = 0; i < ENCODING_ARRAY.length; ++i)
      {
         if (Character.isWhitespace((char)i))
         {
            DECODING_ARRAY[i] = -2;
         }
      }

      for (int i = 0; i < ENCODING_ARRAY.length; ++i)
      {
         DECODING_ARRAY[ENCODING_ARRAY[i]] = i;
      }
   }

   // operations

   /**
    * Prevents instantiation.
    */
   protected Base64Util()
   {
   }

   /**
    * Encodes a binary input stream into a Base64 character output stream.
    * @param istream The binary input stream.
    * @param writer The character output stream writer.
    * @param lMaxCount The maximum number bytes to read. -1 means until the istream EOF.
    * @param bLineBreak True to break lines according to the RFC. False for SOAP encoding.
    * @return The total number of bytes read.
    */
   public static long encode(InputStream istream, Writer writer,
      long lMaxCount, boolean bLineBreak) throws IOException
   {
      long lTotalCount = 0;
      int nColCount = 0;
      int i = 0;
      int n = 0;

      while (lTotalCount < lMaxCount || lMaxCount < 0)
      {
         int ch = istream.read();

         if (ch < 0)
         {
            break;
         }

         ++lTotalCount;

         n <<= 8;
         n |= ch;

         if (++i == 3)
         {
            writer.write(ENCODING_ARRAY[(n >> 18) & 0x3F]);
            writer.write(ENCODING_ARRAY[(n >> 12) & 0x3F]);
            writer.write(ENCODING_ARRAY[(n >> 6) & 0x3F]);
            writer.write(ENCODING_ARRAY[n & 0x3F]);

            i = 0;

            if (bLineBreak && ++nColCount == 19)
            {
               writer.write('\r');
               writer.write('\n');

               nColCount = 0;
            }
         }
      }

      if (i == 1)
      {
         writer.write(ENCODING_ARRAY[(n >> 2) & 0x3F]);
         writer.write(ENCODING_ARRAY[(n << 4) & 0x3F]);
         writer.write('=');
         writer.write('=');
      }
      else if (i == 2)
      {
         writer.write(ENCODING_ARRAY[(n >> 10) & 0x3F]);
         writer.write(ENCODING_ARRAY[(n >> 4) & 0x3F]);
         writer.write(ENCODING_ARRAY[(n << 2) & 0x3F]);
         writer.write('=');
      }

      return lTotalCount;
   }

   /**
    * Encodes a byte array into a Base64 string.
    * @param data The byte array to encode.
    * @return The encoded string.
    */
   public static String encode(byte[] data)
   {
      StringWriter sw = new StringWriter(((data.length + 2) / 3) << 2);

      try
      {
         encode(new ByteArrayInputStream(data), sw, -1, false);
      }
      catch (IOException e)
      {
      }

      return sw.toString();
   }

   /**
    * Decodes a Base64 input character stream into a binary stream.
    * @param reader The character input stream reader.
    * @param ostream The binary output stream.
    * @param lMaxCount The maximum number of characters to read,
    * including skipped characters. -1 means until reader EOF.
    * @param bSpacesAllowed True if space characters are allowed.
    * @return The total number of characters read (including skipped chars).
    */
   public static long decode(Reader reader, OutputStream ostream,
      long lMaxCount, boolean bSpacesAllowed) throws IOException
   {
      long lTotalCount = 0;
      int i = 0;
      int n = 0;

      while (lTotalCount < lMaxCount || lMaxCount < 0)
      {
         int ch = reader.read();

         if (ch < 0)
         {
            if (i == 1)
            {
               throw new Base64Exception("Unexpected EOF");
            }

            break;
         }

         ++lTotalCount;

         if (ch == '=')
         {
            if (i < 2)
            {
               throw new Base64Exception("Unexpected termination character =");
            }

            if (i == 2)
            {
               if (lTotalCount < lMaxCount || lMaxCount < 0)
               {
                  ch = reader.read();

                  ++lTotalCount;

                  if (ch >= 0 && ch != '=')
                  {
                     throw new Base64Exception("Missing second termination character =");
                  }
               }
            }

            break;
         }

         if (ch > 127)
         {
            throw new Base64Exception("Invalid character (ascii=" + ch + ")");
         }

         int nCode = DECODING_ARRAY[ch];

         if (nCode >= 0)
         {
            n <<= 6;
            n |= nCode;

            if (++i == 4)
            {
               ostream.write(n >> 16);
               ostream.write(n >> 8);
               ostream.write(n);

               i = 0;
            }
         }
         else if (nCode == -1 || !bSpacesAllowed)
         {
            throw new Base64Exception("Invalid character (ascii=" + ch + ")");
         }
      }

      if (i == 2)
      {
         ostream.write(n >> 4);
      }
      else if (i == 3)
      {
         ostream.write(n >> 10);
         ostream.write(n >> 2);
      }

      return lTotalCount;
   }

   /**
    * Decodes a Base64 string into a byte array.
    * @param sText The Base64 string.
    * @return The decoded byte array.
    * @throws IOException if an error occurs.
    */
   public static byte[] decode(String sText) throws IOException
   {
      int nCount = sText.length();
      DetachableByteArrayOutputStream ostream64 = new DetachableByteArrayOutputStream(((nCount + 3) >> 2) * 3);

      decode(new StringReader(sText), ostream64, nCount, false);

      if (ostream64.size() == ostream64.length())
      {
         return ostream64.detach();
      }

      return ostream64.toByteArray();
   }
}
