// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.OutputStream;

/**
 * OutputStream decorator that disables the close method.
 */
public class NoCloseOutputStream extends FilterOutputStream
{
   // constructors

   /**
    * Constructs the stream.
    * @param in The decorated stream.
    */
   public NoCloseOutputStream(OutputStream out)
   {
      super(out);
   }

   // operations

   /**
    * Does nothing.
    * @see java.io.OutputStream#close()
    */
   public void close() throws IOException
   {
   }
}
