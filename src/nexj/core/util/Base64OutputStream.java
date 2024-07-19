// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.OutputStream;

import nexj.core.util.Base64Util;

/**
 * Base64 encode output before sending to destination stream.
 * Code adapted from Base64Util to work incrementally on a stream.
 */
public class Base64OutputStream extends OutputStream
{
   /**
    * The bytes waiting to be output.
    */
   protected int m_nBytes;

   /**
    * Number of bytes waiting to be output.
    */
   protected int m_nByteCount = 0;

   /**
    * The stream to write data to.
    */
   protected OutputStream m_stream;

   /**
    * Constructor.
    * @param stream The stream to write data to (not null).
    */
   public Base64OutputStream(OutputStream stream)
   {
      assert stream != null;
      m_stream = stream;
   }

   /**
    * @see java.io.OutputStream#close()
    */
   public void close() throws IOException
   {
      flush();
      m_stream.close();
   }

   /**
    * @see java.io.OutputStream#flush()
    */
   public void flush() throws IOException
   {
      if (m_nByteCount == 1)
      {
         m_stream.write(Base64Util.ENCODING_ARRAY[(m_nBytes >> 2) & 0x3F]);
         m_stream.write(Base64Util.ENCODING_ARRAY[(m_nBytes << 4) & 0x3F]);
         m_stream.write('=');
         m_stream.write('=');
      }
      else if (m_nByteCount == 2)
      {
         m_stream.write(Base64Util.ENCODING_ARRAY[(m_nBytes >> 10) & 0x3F]);
         m_stream.write(Base64Util.ENCODING_ARRAY[(m_nBytes >> 4) & 0x3F]);
         m_stream.write(Base64Util.ENCODING_ARRAY[(m_nBytes << 2) & 0x3F]);
         m_stream.write('=');
      }

      m_nByteCount = 0;
      m_stream.flush();
   }

   /**
    * @see java.io.OutputStream#write(int)
    */
   public void write(int n) throws IOException
   {
      m_nBytes <<= 8;
      m_nBytes |= n & 0xff;

      if (++m_nByteCount == 3)
      {
         m_stream.write(Base64Util.ENCODING_ARRAY[(m_nBytes >> 18) & 0x3F]);
         m_stream.write(Base64Util.ENCODING_ARRAY[(m_nBytes >> 12) & 0x3F]);
         m_stream.write(Base64Util.ENCODING_ARRAY[(m_nBytes >> 6) & 0x3F]);
         m_stream.write(Base64Util.ENCODING_ARRAY[m_nBytes & 0x3F]);
         m_nByteCount = 0;
      }
   }
}