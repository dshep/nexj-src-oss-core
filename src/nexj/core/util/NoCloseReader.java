// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.FilterReader;
import java.io.IOException;
import java.io.Reader;

/**
 * Reader decorator that disables the close method.
 * This is needed to work around bugs in third party packages
 * that close streams that they never opened. 
 */
public class NoCloseReader extends FilterReader
{
   /**
    * Constructs the reader.
    * @param in The decorated reader.
    */
   public NoCloseReader(Reader in)
   {
      super(in);
   }

   /**
    * Does nothing.
    * @see java.io.Reader#close()
    */
   public void close() throws IOException
   {
   }
}
