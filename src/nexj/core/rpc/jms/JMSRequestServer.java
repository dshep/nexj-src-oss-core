// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.jms;

import javax.jms.JMSException;
import javax.jms.Message;

import nexj.core.meta.Component;
import nexj.core.rpc.Request;
import nexj.core.rpc.Server;
import nexj.core.runtime.InvocationContext;
import nexj.core.util.Logger;

/**
 * JMS server for receiving Request objects.
 */
public class JMSRequestServer implements JMSServer
{
   // associations
   
   /**
    * The server component.
    */
   protected Component m_server;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(JMSRequestServer.class);
   
   // operations
   
   /**
    * Sets the server component.
    * @param server The server component to set.
    */
   public void setServer(Component server)
   {
      m_server = server;
   }

   /**
    * @return The server component.
    */
   public Component getServer()
   {
      return m_server;
   }
   
   /**
    * @see nexj.core.rpc.jms.JMSServer#setReceiver(nexj.core.rpc.jms.JMSReceiver)
    */
   public void setReceiver(JMSReceiver receiver)
   {
   }
   
   /**
    * @see nexj.core.rpc.jms.JMSServer#receive(javax.jms.Message, nexj.core.runtime.InvocationContext)
    */
   public boolean receive(Message message, InvocationContext context) throws JMSException
   {
      Object obj = JMSReceiver.getObject(message);

      if (!(obj instanceof Request))
      {
         return false;
      }

      s_logger.debug("Processing a generic asynchronous server request");

      Request request = (Request)obj;

      request.setAsync(false);
      ((Server)m_server.getInstance(context)).invoke(request);

      return true;
   }
}
