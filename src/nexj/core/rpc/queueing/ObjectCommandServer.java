// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.queueing;

import nexj.core.integration.ChannelAware;
import nexj.core.integration.RetryAware;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.util.Executable;
import nexj.core.util.HashHolder;
import nexj.core.util.Holder;
import nexj.core.util.Logger;

/**
 * JMS server for command objects.
 */
public class ObjectCommandServer implements ObjectServer
{
   // associations

   /**
    * The Receiver.
    */
   protected ObjectReceiver m_receiver;

   /**
    * Set of allowed command class names.
    */
   protected Holder m_commandSet = new HashHolder();

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(ObjectCommandServer.class);

   // operations

   /**
    * Adds a command class name to the set of allowed names.
    */
   public void addCommandClass(String sClassName)
   {
      m_commandSet.add(sClassName);
   }

   /**
    * Sets the receiver.
    * @param receiver The receiver.
    */
   public void setReceiver(ObjectReceiver receiver)
   {
      m_receiver = receiver;
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
      if (!(message instanceof Executable))
      {
         return false;
      }

      if (!m_commandSet.contains(message.getClass().getName()))
      {
         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Command \"" + message.getClass().getName() + "\" rejected");
         }

         return false;
      }

      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Executing command \"" + message.getClass().getName() + '"');
      }

      if (message instanceof InvocationContextAware)
      {
         ((InvocationContextAware)message).setInvocationContext(context);
      }

      if (message instanceof ChannelAware)
      {
         ((ChannelAware)message).setChannel(m_receiver.m_channel);
      }

      if (message instanceof RetryAware)
      {
         int nMaxRetryCount = 1;
         int nRetryCount = 0;

         if (instance != null)
         {
            nRetryCount = ((Integer)instance.getValue("errorCount")).intValue();
            nMaxRetryCount = ((Integer)((Instance)instance.getValue("queue")).getValue("errorCount")).intValue();
         }

         ((RetryAware)message).setMaxRetryCount(nMaxRetryCount);
         ((RetryAware)message).setRetryCount(nRetryCount);
      }

      ((Executable)message).execute();

      return true;
   }
}
