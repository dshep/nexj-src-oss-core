// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.jms;

import javax.jms.Connection;
import javax.jms.ConnectionFactory;
import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.MessageProducer;
import javax.jms.Queue;
import javax.jms.Session;
import javax.jms.Topic;
import javax.transaction.Status;
import javax.transaction.TransactionManager;

import nexj.core.meta.Component;
import nexj.core.meta.Repository;
import nexj.core.meta.integration.channel.jms.MessageQueue;
import nexj.core.rpc.IntegrationMDB;
import nexj.core.rpc.ServerException;
import nexj.core.rpc.jms.JMSUtil.MessagePropertyUpdater;
import nexj.core.util.Logger;
import nexj.core.util.ObjUtil;

/**
 * The JMS server Message Driven Bean
 */
public class JMSServerMDB extends IntegrationMDB implements JMSListener, MessageListener
{
   // constants

   /**
    * Serialization version.
    */
   private final static long serialVersionUID = 6229679996585443650L;

   // associations

   /**
    * The channel. A temporary variable that is set during message delivery and cleared at the end
    * of message delivery.
    */
   protected transient MessageQueue m_channel;

   /**
    * The MDB logger.
    */
   protected transient Logger m_logger;

   /**
    * Message rejection message updater.
    */
   protected final static MessagePropertyUpdater s_redirectionMessageUpdater = new MessagePropertyUpdater()
   {
      public void getProperties(Message message) throws JMSException
      {
      }

      public void setProperties(Message message) throws JMSException
      {
         message.setBooleanProperty(JMS.REDIRECT, false);
      }
   };

   // operations

   /**
    * @see nexj.core.rpc.ServerMDB#getLogger()
    */
   protected Logger getLogger()
   {
      return m_logger;
   }

   /**
    * @see nexj.core.rpc.IntegrationMDB#init()
    */
   protected void init() throws Exception
   {
      super.init();
      m_logger = Logger.getLogger(getClass().getName() + '.' + m_sChannelName);
   }

   /**
    * @see javax.jms.MessageListener#onMessage(javax.jms.Message)
    */
   public void onMessage(Message message)
   {
      boolean bReject = false;
      boolean bRedirect = false;

      try
      {
         m_channel = (MessageQueue)Repository.getMetadata().getChannel(m_sChannelName);

         try
         {
            bReject = message.propertyExists(JMS.MESSAGE_REJECTED) && message.getBooleanProperty(JMS.MESSAGE_REJECTED);
         }
         catch (JMSException e)
         {
            m_logger.error("Unable to retrieve property " + JMS.MESSAGE_REJECTED + " from message " + JMSUtil.getId(message), e);
         }

         try
         {
            bRedirect = message.propertyExists(JMS.REDIRECT) && message.getBooleanProperty(JMS.REDIRECT);
         }
         catch (JMSException e)
         {
            m_logger.error("Unable to retrieve property " + JMS.REDIRECT + " from message " + JMSUtil.getId(message), e);
         }

         if (bReject)
         {
            try
            {
               redeliver(message);
            }
            catch (Throwable t)
            {
               m_logger.error("Unable to reject message " + JMSUtil.getId(message), t);
            }
         }
         else if (bRedirect)
         {
            try
            {
               redirect(message, m_channel, s_redirectionMessageUpdater, false);
            }
            catch (Throwable t)
            {
               m_logger.error("Unable to redirect message " + JMSUtil.getId(message), t);
               bRedirect = false;
            }
         }

         if (!bReject && !bRedirect)
         {
            try
            {
               MessageListener receiver = (MessageListener)m_channel.getReceiver().getInstance(null);

               if (!message.getJMSRedelivered() || redeliver(message))
               {
                  receiver.onMessage(message);
               }
            }
            catch (Throwable t)
            {
               if (!(t instanceof ServerException))
               {
                  m_logger.debug("Message processing error", t);
               }

               boolean bDone = false;

               try
               {
                  TransactionManager manager = getTransactionManager();

                  if (manager.getStatus() != Status.STATUS_NO_TRANSACTION)
                  {
                     m_logger.debug("Rolling back the transaction");
                     manager.setRollbackOnly();
                     bDone = true;
                  }
               }
               catch (Throwable x)
               {
                  m_logger.debug("Unable to mark the transaction for rollback", x);
               }

               if (!bDone)
               {
                  ObjUtil.rethrow(t);
               }
            }
         }
      }
      finally
      {
         m_channel = null;
      }
   }

   /**
    * Redelivers the message.
    * @param message The message to process.
    * @return True if the message should still be delivered to the MessageListener.
    */
   public boolean redeliver(Message message) throws Throwable
   {
      boolean bDone = false;

      try
      {
         int nErrorCount = 1;
         int nMaxErrorCount = JMSUtil.getMaxErrorCount(m_channel, message);
         
         // 0 means unlimited delivery so we don't care what the actual error count is.
         if (nMaxErrorCount > 0)
         {
            nErrorCount = JMSUtil.getErrorCount(m_channel, message, Integer.MAX_VALUE, true);
            
            if (nErrorCount == Integer.MAX_VALUE && m_logger.isWarnEnabled())
            {
               m_logger.warn("Unknown error/delivery count in message "
                  + JMSUtil.getId(message) + " from " + m_channel.getName());
               m_logger.dump(message);
            }
         }

         // For unlimited delivery we never redirect to error Q.
         if (nMaxErrorCount > 0 && nErrorCount >= nMaxErrorCount)
         {
            if (m_channel.getErrorQueue() != null)
            {
               if (m_logger.isDebugEnabled())
               {
                  m_logger.debug("Redirecting message " + JMSUtil.getId(message) + " to the error queue");
                  log(Logger.DUMP, message);
               }

               try
               {
                  redirect(message, m_channel.getErrorQueue(),
                     new MessagePropertyUpdater()
                     {
                        protected String m_sOldMessageId;
                        protected String m_sOldDestination;

                        public void getProperties(Message message) throws JMSException
                        {
                           m_sOldMessageId = message.getJMSMessageID();

                           Destination destination = message.getJMSDestination();

                           if (destination != null)
                           {
                              m_sOldDestination = (destination instanceof Queue) ?
                                 "queue:" + ((Queue)destination).getQueueName() :
                                 "topic:" + ((Topic)destination).getTopicName();
                           }
                        }

                        public void setProperties(Message message) throws JMSException
                        {
                           if (m_sOldMessageId != null)
                           {
                              message.setStringProperty(JMS.OLD_MESSAGE_ID, m_sOldMessageId);
                           }

                           if (m_sOldDestination != null)
                           {
                              message.setStringProperty(JMS.OLD_DESTINATION, m_sOldDestination);
                           }
                        }
                     }, true);
               }
               catch (Throwable t)
               {
                  if (m_channel.isTransactional())
                  {
                     throw t;
                  }

                  m_logger.error("Unable to redirect message " + message.getJMSMessageID() +
                     " to the error queue", t);
                  log(Logger.ERROR, message);
               }
            }

            bDone = true;
            
            return false;
         }
         else if (message.propertyExists(JMS.BACKOFF_DELAY))
         {
            if (m_channel.isBroadcast())
            {
               m_logger.error("Message back-off not supported for topics (message " +
                  JMSUtil.getId(message) + ")");
               log(Logger.DUMP, message);
            }
            else
            {
               if (m_logger.isDebugEnabled())
               {
                  m_logger.debug("Delaying message " + JMSUtil.getId(message));
                  log(Logger.DUMP, message);
               }

               redirect(message, m_channel,
                  new MessagePropertyUpdater()
                  {
                     protected long m_lBackoffDelay;
                     protected long m_lMaxBackoffDelay;
                     protected int m_nErrorCount;

                     public void getProperties(Message message) throws JMSException
                     {
                        m_lBackoffDelay = message.getLongProperty(JMS.BACKOFF_DELAY);

                        if (message.propertyExists(JMS.MAX_BACKOFF_DELAY))
                        {
                           m_lMaxBackoffDelay = message.getLongProperty(JMS.MAX_BACKOFF_DELAY);
                        }

                        if (message.propertyExists(JMS.ERROR_COUNT))
                        {
                           m_nErrorCount = message.getIntProperty(JMS.ERROR_COUNT);
                        }
                     }

                     public void setProperties(Message message) throws JMSException
                     {
                        message.setLongProperty(JMS.BACKOFF_DELAY, limitBackoffDelay(m_lBackoffDelay << 1));
                        message.setIntProperty(JMS.ERROR_COUNT, m_nErrorCount + 1);

                        if (message.propertyExists(JMS.JBOSS_REDELIVERY_COUNT) || message.getClass().getName().startsWith("org.jboss."))
                        {
                           message.setLongProperty(JMS.JBOSS_SCHEDULED_DELIVERY, System.currentTimeMillis() + limitBackoffDelay(m_lBackoffDelay));
                        }
                        else if (message.propertyExists(JMS.ACTIVEMQ_BROKER_IN_TIME))
                        {
                           message.setLongProperty(JMS.ACTIVEMQ_SCHEDULED_DELAY, limitBackoffDelay(m_lBackoffDelay));
                        }
                        else
                        {
                           message.setLongProperty(JMS.DELIVERY_TIME, System.currentTimeMillis() + limitBackoffDelay(m_lBackoffDelay));
                        }
                     }

                     private long limitBackoffDelay(long lBackoffDelay)
                     {
                        if (m_lMaxBackoffDelay <= 0 || lBackoffDelay <= m_lMaxBackoffDelay)
                        {
                           return lBackoffDelay;
                        }

                        return m_lMaxBackoffDelay;
                     }
                  }, false);

               bDone = true;

               return false;
            }
         }

         bDone = true;
      }
      finally
      {
         if (!bDone)
         {
            TransactionManager tm = getTransactionManager();

            switch (tm.getStatus())
            {
               case Status.STATUS_ACTIVE:
               case Status.STATUS_MARKED_ROLLBACK:
                  tm.rollback();
                  break;
            }
         }
      }

      return true;
   }

   /**
    * Redirects a message to a given destination.
    * @param message The message to redirect.
    * @param mq The destination queue.
    * @param updater The message property updater strategy.
    * @param bBridge True if redirecting to a different engine type.
    */
   protected void redirect(Message message, MessageQueue mq,
      MessagePropertyUpdater updater, boolean bBridge) throws Exception
   {
      Connection connection = null;
      Session session = null;
      MessageProducer producer = null;

      try
      {
         long lExpiration = message.getJMSExpiration();
         long lTTL = (lExpiration == 0) ? Message.DEFAULT_TIME_TO_LIVE : lExpiration - System.currentTimeMillis();

         if (lExpiration != 0 && lTTL <= 0)
         {
            if (m_logger.isDebugEnabled())
            {
               m_logger.debug("Message " + JMSUtil.getId(message) + " expired");
               log(Logger.DUMP, message);
            }
         }
         else
         {
            Component component = mq.getSender();

            if (component == null)
            {
               throw new IllegalStateException("Queue \"" + mq.getName() + "\" is not a sender");
            }

            JMSSender sender = (JMSSender)component.getInstance(null);
            ConnectionFactory cf = sender.getConnectionFactoryObject();

            connection = (mq.getUser() != null) ?
               cf.createConnection(mq.getUser(), mq.getPassword()) :
               cf.createConnection();

            if (mq.getClientId() != null)
            {
               connection.setClientID(mq.getClientId());
            }

            connection.start();
            session = connection.createSession(mq.isTransactional(), mq.getAckMode());
            producer = session.createProducer(sender.getDestinationObject());

            if (bBridge)
            {
               message = JMSUtil.copy(message, session, updater);
            }
            else
            {
               JMSUtil.update(message, updater);
            }

            producer.send(message, message.getJMSDeliveryMode(), message.getJMSPriority(), lTTL);
         }
      }
      finally
      {
         if (producer != null)
         {
            try
            {
               producer.close();
            }
            catch (Throwable t)
            {
               m_logger.warn("Unable to close the message producer", t);
            }
         }
         
         if (session != null)
         {
            try
            {
               session.close();
            }
            catch (Throwable t)
            {
               m_logger.warn("Unable to close the queue session", t);
            }
         }

         if (connection != null)
         {
            try
            {
               connection.close();
            }
            catch (Throwable t)
            {
               m_logger.warn("Unable to close the queue connection", t);
            }
         }
      }
   }

   /**
    * Logs the message at the specified log level.
    * @param nLogLevel One of the Logger.* level constants.
    * @param message The message to dump.
    */
   protected void log(int nLogLevel, Message message)
   {
      if (m_logger.isLevelEnabled(nLogLevel))
      {
         try
         {
            m_logger.log(nLogLevel, JMSReceiver.transfer(message, m_channel));
         }
         catch (Throwable t)
         {
            try
            {
               m_logger.log(nLogLevel, message, t);
            }
            catch (Throwable x)
            {
               m_logger.error("Unable to log the delivered message", x);
            }
         }
      }
   }

   /**
    * @return The transaction manager
    */
   protected TransactionManager getTransactionManager()
   {
      return (TransactionManager)m_channel.getType().getMetadata()
         .getComponent("System.TransactionManager").getInstance(null);
   }
}
