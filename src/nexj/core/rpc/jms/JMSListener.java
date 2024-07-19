// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.jms;

import javax.jms.Message;

/**
 * Interface implemented by custom JMS message listeners. 
 */
public interface JMSListener
{
   /**
    * Receives a JMS message.
    * @param message The message to receive.
    */
   void onMessage(Message message);
}
