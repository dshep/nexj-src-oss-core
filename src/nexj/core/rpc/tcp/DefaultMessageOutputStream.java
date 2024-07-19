// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.tcp;

import java.io.IOException;
import java.io.OutputStream;

import nexj.core.integration.MessageOutputStream;
import nexj.core.rpc.TransferObject;

/**
 * A default message output stream that simply writes all bytes
 * to the underlying output stream.
 */
public class DefaultMessageOutputStream extends MessageOutputStream
{
   // constructors

   /**
    * @param ostream The base output stream.
    */
   public DefaultMessageOutputStream(OutputStream ostream)
   {
      super(ostream);
   }

   // operations

   /**
    * @see nexj.core.integration.MessageOutputStream#start(nexj.core.rpc.TransferObject)
    */
   public boolean start(TransferObject raw) throws IOException
   {
      return true;
   }

   /**
    * @see java.io.OutputStream#write(int)
    */
   public void write(int b) throws IOException
   {
      m_ostream.write(b);
   }
}
