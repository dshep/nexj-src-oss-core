// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.jms.ra.platform;

import java.lang.reflect.Method;

import javax.jms.Message;
import javax.jms.MessageListener;
import javax.resource.ResourceException;
import javax.resource.spi.endpoint.MessageEndpoint;

import nexj.core.rpc.jms.JMSListener;
import nexj.core.rpc.jms.ra.JMSConsumer;

/**
 * Message endpoint interface adapter implementing java.jms.MessageListener.  
 */
public class JMSPlatformMessageEndpoint implements MessageEndpoint, MessageListener
{
   // associations
   
   /**
    * The consumer pool that uses this endpoint.
    */
   protected JMSPlatformConsumerPool m_pool;

   /**
    * The adapted endpoint.
    */
   protected MessageEndpoint m_endpoint;

   // constructors

   /**
    * Constructs the adapter.
    * @param endpoint The adapted endpoint.
    */
   public JMSPlatformMessageEndpoint(JMSPlatformConsumerPool pool, MessageEndpoint endpoint)
   {
      m_pool = pool;
      m_endpoint = endpoint;
   }

   // operations

   /**
    * @see javax.resource.spi.endpoint.MessageEndpoint#beforeDelivery(java.lang.reflect.Method)
    */
   public void beforeDelivery(Method method) throws NoSuchMethodException, ResourceException
   {
      m_pool.m_platformAdapter.beforeDelivery(this);

      boolean bDone = false;

      try
      {
         m_endpoint.beforeDelivery(JMSConsumer.JMSLISTENER_ONMESSAGE_METHOD);
         bDone = true;
      }
      finally
      {
         if (!bDone)
         {
            m_pool.getPlatformAdapter().afterDelivery(this);
         }
      }
   }

   /**
    * @see javax.resource.spi.endpoint.MessageEndpoint#afterDelivery()
    */
   public void afterDelivery() throws ResourceException
   {
      try
      {
         m_endpoint.afterDelivery();
      }
      finally
      {
         m_pool.getPlatformAdapter().afterDelivery(this);
      }
   }

   /**
    * @see javax.resource.spi.endpoint.MessageEndpoint#release()
    */
   public void release()
   {
      m_endpoint.release();
   }

   /**
    * @see javax.jms.MessageListener#onMessage(javax.jms.Message)
    */
   public void onMessage(Message message)
   {
      m_pool.checkForDelayedDelivery(message);
      ((JMSListener)m_endpoint).onMessage(m_pool.m_platformAdapter.prepareMessage(message)); 
   }
}