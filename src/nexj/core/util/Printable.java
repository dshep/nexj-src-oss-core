// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;

/**
 * Implementors provide a more efficient replacement for toString().
 */
public interface Printable
{
   /**
    * Prints a description of an object.
    * @param writer The destination writer.
    * @throws IOException thrown by the writer.
    */
   void printOn(PrintWriter writer) throws IOException;
}
