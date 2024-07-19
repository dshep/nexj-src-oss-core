// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;

import nexj.core.util.Binary;
import nexj.core.util.ObjUtil;
import nexj.core.util.XMLUtil;

/**
 * Hex encode output before sending to destination stream.
 */
public class HexOutputStream extends OutputStream
{
   /**
    * The writer for the stream to write data to.
    */
   protected Writer m_writer;

   /**
    * Buffer used for writing bytes from stream.
    */
   protected byte[] m_buf = new byte[1];

   /**
    * Constructor.
    * @param stream The stream to write data to (not null).
    */
   public HexOutputStream(OutputStream stream)
   {
      assert stream != null;

      try
      {
         m_writer = new OutputStreamWriter(stream, XMLUtil.ENCODING);
      }
      catch (Exception e)
      {
         ObjUtil.rethrow(e);
      }
   }

   /**
    * @see java.io.OutputStream#close()
    */
   public void close() throws IOException
   {
      m_writer.close();
   }

   /**
    * @see java.io.OutputStream#flush()
    */
   public void flush() throws IOException
   {
      m_writer.flush();
   }

   /**
    * @see java.io.OutputStream#write(int)
    */
   public void write(int n) throws IOException
   {
      m_buf[0] = (byte)n;
      Binary.write(m_writer, m_buf, 0, 1);
   }
}