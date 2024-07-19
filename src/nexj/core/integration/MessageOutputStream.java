// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration;

import java.io.IOException;
import java.io.OutputStream;

import nexj.core.rpc.TransferObject;

/**
 * Output stream to support custom message boundaries in a stream.
 */
public abstract class MessageOutputStream extends OutputStream
{
   // associations

   /**
    * The base output stream.
    */
   protected OutputStream m_ostream;

   // constructors

   /**
    * Creates a new message output stream.
    * @param ostream The base output stream.
    */
   public MessageOutputStream(OutputStream ostream)
   {
      m_ostream = ostream;
   }

   // operations

   /**
    * Starts a new message. It is terminated whenever the stream is closed.
    * Then, a new message can be started, if the return value is true.
    * @param raw The raw transfer object.
    * @return False if the connection must be closed (and not pooled anymore).
    * @throws IOException If an I/O error occurs.
    */
   public abstract boolean start(TransferObject raw) throws IOException;

   /**
    * Called after the message body has been written to the output stream.
    * @param raw The raw transfer object.
    * @throws IOException If an I/O error occurs.
    */
   public void end(TransferObject raw) throws IOException
   {
   }

   /**
    * @see java.io.OutputStream#close()
    */
   public void close() throws IOException
   {
      m_ostream.close();
   }
}
