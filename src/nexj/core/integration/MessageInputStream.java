// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration;

import java.io.IOException;
import java.io.InputStream;

import nexj.core.rpc.TransferObject;

/**
 * Input stream to support custom message boundaries in a stream.
 *
 * This stream returns -1 when it reads the end of the message.
 */
public abstract class MessageInputStream extends InputStream
{
   // associations

   /**
    * The base input stream.
    */
   protected InputStream m_istream;

   // constructors

   /**
    * Creates a new message input stream.
    * @param istream The base input stream.
    */
   public MessageInputStream(InputStream istream)
   {
      m_istream = istream;
   }

   // operations

   /**
    * Starts parsing the message and populates raw message properties.
    * This has to be invoked before the stream can return any data.
    * If next() is called before the previous message has been read in
    * its entirety, this method should first read the previous message
    * completely and discard the bytes.
    * @param raw The raw message. Can be null.
    * @return True if there is a message, false for EOF.
    * @throws IOException If an I/O error occurs.
    */
   public abstract boolean next(TransferObject raw) throws IOException;

   /**
    * @see java.io.InputStream#close()
    */
   public void close() throws IOException
   {
      m_istream.close();
   }
}
