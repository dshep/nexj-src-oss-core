// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.tcp;

import java.io.InputStream;
import java.io.OutputStream;

import nexj.core.integration.MessageInputStream;
import nexj.core.integration.MessageOutputStream;
import nexj.core.integration.MessageStreamFactory;

/**
 * A message stream factory used to create the default message input and output streams.
 */
public class DefaultMessageStreamFactory implements MessageStreamFactory
{
   // operations

   /**
    * @see nexj.core.integration.MessageStreamFactory#createMessageInputStream(java.io.InputStream)
    */
   public MessageInputStream createMessageInputStream(InputStream istream)
   {
      return new DefaultMessageInputStream(istream);
   }

   /**
    * @see nexj.core.integration.MessageStreamFactory#createMessageOutputStream(java.io.OutputStream)
    */
   public MessageOutputStream createMessageOutputStream(OutputStream ostream)
   {
      return new DefaultMessageOutputStream(ostream);
   }

}
