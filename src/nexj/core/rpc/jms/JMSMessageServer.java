// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.jms;

import javax.jms.JMSException;
import javax.jms.Message;

import nexj.core.runtime.InvocationContext;
import nexj.core.scripting.Intrinsic;
import nexj.core.util.Logger;

/**
 * Generic JMS message server, which reformats the message into a Request.
 */
public class JMSMessageServer extends JMSMessageDispatcher implements JMSServer
{
   // associations
   
   /**
    * The JMS receiver.
    */
   protected JMSReceiver m_receiver;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(JMSMessageServer.class);
   
   // operations

   /**
    * Sets the JMS receiver.
    * @param receiver The JMS receiver to set.
    */
   public void setReceiver(JMSReceiver receiver)
   {
      m_receiver = receiver;
   }

   /**
    * @return The JMS receiver.
    */
   public JMSReceiver getReceiver()
   {
      return m_receiver;
   }
   
   /**
    * @see nexj.core.rpc.jms.JMSServer#receive(javax.jms.Message, nexj.core.runtime.InvocationContext)
    */
   public boolean receive(Message message, InvocationContext context) throws JMSException
   {
      s_logger.debug("Processing a JMS custom message");

      return Intrinsic.isTrue(invoke(m_metaclass.getName(), m_sEvent, m_receiver.createMessage(message), 
         (m_sProperties == null) ? null : JMSReceiver.getProperties(message), context));
   }
}
