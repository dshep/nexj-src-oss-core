// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.queueing;

import nexj.core.meta.Component;
import nexj.core.rpc.Request;
import nexj.core.rpc.Server;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InvocationContext;
import nexj.core.util.Logger;

/**
 * JMS server for receiving Request objects.
 */
public class ObjectRequestServer implements ObjectServer
{
   // associations

   /**
    * The server component.
    */
   protected Component m_server;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(ObjectRequestServer.class);

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
    * Sets the receiver.
    * @param receiver The receiver.
    */
   public void setReceiver(ObjectReceiver receiver)
   {
   }

   /**
    * Receives a message.
    * @param message The message to receive.
    * @param instance The persisted message, may be null.
    * @param context The invocation context.
    * @return True if the message has been processed.
    */
   public boolean receive(Object message, Instance instance, InvocationContext context)
   {
      if (!(message instanceof Request))
      {
         return false;
      }

      s_logger.debug("Processing a generic asynchronous server request");

      Request request = (Request)message;

      request.setAsync(false);
      ((Server)m_server.getInstance(context)).invoke(request);

      return true;
   }
}
