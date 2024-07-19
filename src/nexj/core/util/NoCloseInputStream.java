// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * InputStream decorator that disables the close method.
 * This is needed to work around bugs in third party packages
 * that close streams that they never opened. 
 */
public class NoCloseInputStream extends FilterInputStream
{
   /**
    * Constructs the stream.
    * @param in The decorated stream.
    */
   public NoCloseInputStream(InputStream in)
   {
      super(in);
   }

   /**
    * Does nothing.
    * @see java.io.InputStream#close()
    */
   public void close() throws IOException
   {
   }
}
