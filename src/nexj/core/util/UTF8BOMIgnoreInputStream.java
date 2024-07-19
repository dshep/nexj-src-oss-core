// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.InputStream;

/**
 * Ignores the Byte-Order-Mark at the beginning of a UTF8-encoded stream.
 */
public class UTF8BOMIgnoreInputStream extends ResettableInputStream
{
   // constants

   /**
    * The Byte-Order-Mark in UTF-8 encoding.
    */
   protected static int[] BOM = new int[] {0xef, 0xbb, 0xbf};

   // constructors

   /**
    * Constructs a new BOM-ignoring stream.
    * @param in The stream with a potential BOM.
    */
   protected UTF8BOMIgnoreInputStream(InputStream in)
   {
      super(in);
   }

   // operations

   /**
    * Strips the leading BOM from a stream and returns the de-BOMed stream.
    * @param istream The stream from which the leading BOM shall be stripped.
    * @param sEncoding The name of the encoding; strips the BOM preamble only if encoding
    * is "utf8" or "utf-8".
    * @return A stream from which the BOM-stripped data may be read.
    * @throws IOException If an I/O error occurs.
    */
   public static InputStream wrap(InputStream istream, String sEncoding) throws IOException
   {
      if (sEncoding.equalsIgnoreCase("utf8") || sEncoding.equalsIgnoreCase("utf-8"))
      {
         if (!istream.markSupported())
         {
            istream = new UTF8BOMIgnoreInputStream(istream);
         }

         istream.mark(BOM.length + 1);

         for (int nIndex = 0; nIndex < BOM.length; nIndex++)
         {
            if (istream.read() != BOM[nIndex])
            {
               istream.reset();

               break;
            }
         }
      }

      return istream;
   }
}
