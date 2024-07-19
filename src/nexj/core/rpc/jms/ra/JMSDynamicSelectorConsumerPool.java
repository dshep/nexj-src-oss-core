// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.jms.ra;

import java.util.Timer;
import java.util.TimerTask;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.resource.spi.endpoint.MessageEndpointFactory;

import nexj.core.rpc.jms.JMS;
import nexj.core.rpc.jms.JMSUtil;
import nexj.core.rpc.jms.JMSUtil.MessagePropertyUpdater;
import nexj.core.rpc.ra.GenericConsumerConfig;
import nexj.core.rpc.ra.GenericConsumerPool;
import nexj.core.rpc.ra.GenericResourceAdapter;
import nexj.core.util.Logger;

/**
 * Dynamic Selector JMS Consumer Pool enables dynamic updating of the message selector criteris
 */
public abstract class JMSDynamicSelectorConsumerPool extends GenericConsumerPool
{
   // attributes
   
   /**
    * True if running in dynamic selector mode.
    */
   protected boolean m_bDynamicSelectorMode;

   /**
    * True if the dynamic selector initialization has started.
    */
   protected boolean m_bDynamicSelectorInit;

   // associations

   /**
    * Synchronizes access to m_bDynamicSelectorInit.
    */
   protected final Object m_dynamicSelectorInitMutex = new Object();

   /**
    * Timer for periodically (specified in the consumer configuration) resetting the message selector.
    */
   protected Timer m_dynamicSelectorTimer;

   /**
    * Message redirection message updater.
    */
   protected final static MessagePropertyUpdater s_redirectionMessageUpdater = new MessagePropertyUpdater()
   {
      public void getProperties(Message message) throws JMSException
      {
      }

      public void setProperties(Message message) throws JMSException
      {
         message.setBooleanProperty(JMS.REDIRECT, true);
      }
   };

   // constructors
   
   /**
    * Constructs the consumer pool.
    * @param adapter The resource adapter.
    * @param factory The endpoint factory.
    * @param config The pool configuration.
    * @param config The logger.
    */
   protected JMSDynamicSelectorConsumerPool(GenericResourceAdapter adapter, MessageEndpointFactory factory,
      GenericConsumerConfig config, Logger logger)
   {
      super(adapter, factory, config, logger);
   }
   
   // operations
   
   /**
    * @see nexj.core.rpc.ra.GenericConsumerPool#shutdown()
    */
   public void shutdown()
   {
      super.shutdown();

      synchronized (this)
      {
         if (m_dynamicSelectorTimer != null)
         {
            m_dynamicSelectorTimer.cancel();
         }
      }
   }

   /**
    * @return True if running in dynamic selector mode.
    */
   public boolean isDynamicSelectorMode()
   {
      return m_bDynamicSelectorMode;
   }
   
   /**
    * Checks if the message has the delayed delivery flag and turns on the dynamic selector mode if so.
    * Marks the message for redirection so that its delivery is also delayed.  The actual redirection is performed in the MDB.
    */
   public void checkForDelayedDelivery(Message message)
   {
      try
      {
         if (!isDynamicSelectorMode() && message.propertyExists(JMS.DELIVERY_TIME))
         {
            if (m_logger.isDebugEnabled())
            {
               m_logger.debug("Message " + message + " triggered dynamic selector mode in " + this);
            }
            
            startDynamicSelectorMode();
            
            boolean bBroacast = ((JMSConsumerConfig)m_config).isBroadcast();
            long lDeliveryDelay = message.getLongProperty(JMS.DELIVERY_TIME) - System.currentTimeMillis();

            // Redelivery not supported for topics.
            // First message with delayed delivery flag to a topic will NOT be delayed.
            if (!bBroacast && lDeliveryDelay > 0)
            {
               JMSUtil.update(message, s_redirectionMessageUpdater);
               
               if (m_logger.isDebugEnabled())
               {
                  m_logger.debug("Marking first delayed message for redirection " + message + " in " + this);
               }
            }
         }
      }
      catch (Throwable t)
      {
         m_logger.error("Unable to determine delayed delivery in " + message + " in class " + this, t);
      }
   }
   
   /**
    * Starts a timer thread that periodically resets the message selector criteria thus making it dynamic.
    * @param nStartDelay The initial timer delay in millisconds.
    */
   public void startDynamicSelectorMode()
   {
      synchronized (m_dynamicSelectorInitMutex)
      {
         if (m_bDynamicSelectorInit)
         {
            return;
         }

         m_bDynamicSelectorInit = true;
      }

      synchronized (this)
      {
         if (m_bShutdown)
         {
            return;
         }
      }

      if (m_logger.isDebugEnabled())
      {
         m_logger.debug("Starting dynamic selector mode in " + this);
      }

      try
      {
         m_dynamicSelectorTimer = m_adapter.getContext().createTimer();
         m_dynamicSelectorTimer.schedule(new TimerTask()
         {
            public void run()
            {
               synchronized (JMSDynamicSelectorConsumerPool.this)
               {
                  if (!m_bShutdown)
                  {
                     resetMessageSelector();

                     if (!m_bDynamicSelectorMode)
                     {
                        m_bDynamicSelectorMode = true;
                     }
                  }
               }
            }
         }, 0, ((JMSConsumerConfig)m_config).getSelectorUpdateInterval());
      }
      catch (Throwable t)
      {
         m_logger.error("Unable to create dynamic selector timer in " + this, t);
      }
   }

   /**
    * Performs the actual reset of the message selector criteria.
    */
   protected abstract void resetMessageSelector();

   /**
    * @return The message selector with new criteria.
    */
   protected String getMessageSelector()
   {
      String sSelector = ((JMSConsumerConfig)m_config).getMessageSelector();
      String sDeliveryTimeClause = "jmsDeliveryTime is null or jmsDeliveryTime <= " + System.currentTimeMillis();

      return (sSelector == null) ? sDeliveryTimeClause : "(" + sSelector + ") and (" + sDeliveryTimeClause + ")";
   }
}