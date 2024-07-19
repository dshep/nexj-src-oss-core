// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration;

import java.io.InputStream;
import java.io.OutputStream;

/**
 * Creates message streams to support custom message boundaries in a stream.
 */
public interface MessageStreamFactory
{
   /**
    * Create a message input stream based on an existing input stream.
    * @param istream An existing input stream.
    * @return A MessageInputStream.
    */
   MessageInputStream createMessageInputStream(InputStream istream);

   /**
    * Create a message output stream based on an existing output stream.
    * @param ostream An existing output stream.
    * @return A MessageOutputStream.
    */
   MessageOutputStream createMessageOutputStream(OutputStream ostream);
}
