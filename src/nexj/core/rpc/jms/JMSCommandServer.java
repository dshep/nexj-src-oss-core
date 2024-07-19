// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.jms;

import javax.jms.JMSException;
import javax.jms.Message;

import nexj.core.integration.ChannelAware;
import nexj.core.integration.RetryAware;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.util.Executable;
import nexj.core.util.HashHolder;
import nexj.core.util.Holder;
import nexj.core.util.Logger;

/**
 * JMS server for command objects.
 */
public class JMSCommandServer implements JMSServer
{
   // associations

   /**
    * The JMSReceiver.
    */
   protected JMSReceiver m_receiver;

   /**
    * Set of allowed command class names.
    */
   protected Holder m_commandSet = new HashHolder();

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(JMSCommandServer.class);

   // operations

   /**
    * Adds a command class name to the set of allowed names.
    */
   public void addCommandClass(String sClassName)
   {
      m_commandSet.add(sClassName);
   }

   /**
    * @see nexj.core.rpc.jms.JMSServer#setReceiver(nexj.core.rpc.jms.JMSReceiver)
    */
   public void setReceiver(JMSReceiver receiver)
   {
      m_receiver = receiver;
   }

   /**
    * @see nexj.core.rpc.jms.JMSServer#receive(javax.jms.Message, nexj.core.runtime.InvocationContext)
    */
   public boolean receive(Message message, InvocationContext context) throws JMSException
   {
      Object obj = JMSReceiver.getObject(message);

      if (!(obj instanceof Executable))
      {
         return false;
      }

      if (!m_commandSet.contains(obj.getClass().getName()))
      {
         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Command \"" + obj.getClass().getName() + "\" rejected");
         }

         return false;
      }

      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Executing command \"" + obj.getClass().getName() + "\"");
      }

      if (obj instanceof InvocationContextAware)
      {
         ((InvocationContextAware)obj).setInvocationContext(context);
      }

      if (obj instanceof ChannelAware)
      {
         ((ChannelAware)obj).setChannel(m_receiver.m_channel);
      }

      if (obj instanceof RetryAware)
      {
         int nMaxRetryCount = JMSUtil.getMaxErrorCount(m_receiver.m_channel, message);
         int nRetryCount = JMSUtil.getErrorCount(m_receiver.m_channel, message, 0, false);

         ((RetryAware)obj).setMaxRetryCount(nMaxRetryCount);
         ((RetryAware)obj).setRetryCount(nRetryCount);
      }

      ((Executable)obj).execute();

      return true;
   }
}
