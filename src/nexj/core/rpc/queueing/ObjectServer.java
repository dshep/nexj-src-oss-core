// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.queueing;

import nexj.core.runtime.Instance;
import nexj.core.runtime.InvocationContext;

/**
 * Interface for receiving messages.
 */
public interface ObjectServer
{
   /**
    * Sets the receiver.
    * @param receiver The receiver.
    */
   void setReceiver(ObjectReceiver receiver);
   
   /**
    * Receives a message.
    * @param message The message to receive.
    * @param instance The persisted message, may be null.
    * @param context The invocation context.
    * @return True if the message has been processed.
    */
   public boolean receive(Object message, Instance instance, InvocationContext context);
}
