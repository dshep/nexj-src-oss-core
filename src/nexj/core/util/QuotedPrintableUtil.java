// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.InputStream;
import java.io.Writer;

/**
 * RFC 1521 MIME Quoted-Printable encoder.
 */
public class QuotedPrintableUtil
{
   // constants

   /**
    * Hex digit table.
    */
   protected final static char[] HEX_DIGITS = "0123456789ABCDEF".toCharArray();

   /**
    * RFC 1521 maximum line length for Quoted-Printable encoding, not including
    * CRLF pair.
    */
   protected final static int MAX_LINE_LENGTH = 76;

   // operations

   /**
    * Encodes byte data from the input stream to character data on the output stream,
    * using RFC 1521 Quoted-Printable encoding.
    * 
    * @param in The input stream.
    * @param out The output stream.
    * @param nCurrentLength The length of the current line.
    * @return The length of the last line of output. The last line is not CRLF-terminated.
    * @throws IOException If an I/O error occurs.
    */
   public static int encode(InputStream in, Writer out, int nCurrentLength) throws IOException
   {
      int nCh;

      while ((nCh = in.read()) >= 0)
      {
         if (nCh >= 32 && nCh <= 126 && nCh != '=')
         {
            if (nCurrentLength >= (MAX_LINE_LENGTH - 1))
            {
               out.write("=\r\n");
               nCurrentLength = 0;
            }

            out.write(nCh);
            nCurrentLength++;
         }
         else
         {
            if (nCurrentLength >= (MAX_LINE_LENGTH - 3))
            {
               out.write("=\r\n");
               nCurrentLength = 0;
            }

            out.write('=');
            out.write(HEX_DIGITS[(nCh >> 4) & 0x0f]);
            out.write(HEX_DIGITS[nCh & 0x0f]);
            nCurrentLength += 3;
         }
      }

      return nCurrentLength;
   }
}
