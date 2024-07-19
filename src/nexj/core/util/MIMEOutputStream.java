// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;
import java.io.OutputStream;

/**
 * Output stream wrapper for MIME data writing.
 */
public class MIMEOutputStream extends OutputStream
{
   // associations

   /**
    * The output stream.
    */
   protected OutputStream m_ostream;

   // constructors

   /**
    * Constructs the MIME output stream.
    * @param ostream The output stream to wrap.
    */
   public MIMEOutputStream(OutputStream ostream)
   {
      m_ostream = ostream;
   }

   // operations

   /**
    * Writes a string to the stream using Latin1 encoding.
    * @param s The string to write. Can be null.
    */
   public void write(String s) throws IOException
   {
      if (s != null)
      {
         for (int i = 0, n = s.length(); i != n; ++i)
         {
            write(s.charAt(i));
         }
      }
   }
   
   /**
    * @see java.io.OutputStream#write(int)
    */
   public void write(int ch) throws IOException
   {
      m_ostream.write(ch);
   }

   /**
    * @see java.io.OutputStream#flush()
    */
   public void flush() throws IOException
   {
      m_ostream.flush();
   }
}
