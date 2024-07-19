// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.jms;

import javax.jms.JMSException;
import javax.jms.Message;

import nexj.core.runtime.InvocationContext;

/**
 * Interface for receiving JMS messages.
 */
public interface JMSServer
{
   /**
    * Sets the JMS receiver.
    * @param receiver The JMS receiver.
    */
   void setReceiver(JMSReceiver receiver);
   
   /**
    * Receives a JMS message.
    * @param message The message to receive.
    * @param context The invocation context.
    * @return True if the message has been processed.
    * @throws JMSException if an error occurs.
    */
   boolean receive(Message message, InvocationContext context) throws JMSException;
}
