// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.jms.ra.platform;

import java.util.ArrayList;

import javax.resource.ResourceException;
import javax.resource.spi.ActivationSpec;
import javax.resource.spi.ResourceAdapter;
import javax.resource.spi.endpoint.MessageEndpointFactory;

import nexj.core.rpc.jms.JMSUtil;
import nexj.core.rpc.jms.ra.JMSConsumer;
import nexj.core.rpc.jms.ra.JMSConsumerConfig;
import nexj.core.rpc.jms.ra.JMSDynamicSelectorConsumerPool;
import nexj.core.rpc.jms.ra.JMSResourceAdapter;
import nexj.core.rpc.ra.GenericConsumer;
import nexj.core.util.J2EEUtil;
import nexj.core.util.Logger;

/**
 * Consumer pool adapting 
 */
public class JMSPlatformConsumerPool extends JMSDynamicSelectorConsumerPool
{
   // attributes

   /**
    * True if the message delivery is transactional.
    */
   protected boolean m_bTransactional;

   /**
    * True if the platform RA has been started.
    */
   protected boolean m_bStarted;

   // associations
   
   /**
    * The platform adapter.
    */
   protected JMSPlatformAdapter m_platformAdapter;
   
   /**
    * The resource adapter instance.
    */
   protected ResourceAdapter m_resourceAdapter;

   /**
    * The platform activation spec.
    */
   protected ActivationSpec m_activationSpec;

   /**
    * The platform ednpoint factory.
    */
   protected MessageEndpointFactory m_endpointFactory;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(JMSPlatformConsumerPool.class);

   // constructors

   /**
    * Constructs the consumer pool.
    * @param adapter The resource adapter.
    * @param factory The endpoint factory.
    * @param config The pool configuration.
    * @param platformAdapter The platform adapter.
    * @throws ResourceException if an initialization error occurs.
    */
   public JMSPlatformConsumerPool(JMSResourceAdapter adapter, MessageEndpointFactory factory,
      JMSConsumerConfig config, JMSPlatformAdapter platformAdapter) throws ResourceException
   {
      super(adapter, factory, config, s_logger);

      try
      {
         m_bTransactional = factory.isDeliveryTransacted(JMSConsumer.JMSLISTENER_ONMESSAGE_METHOD);
      }
      catch (NoSuchMethodException e)
      {
         throw new ResourceException("Unable to determine if the delivery is transactional", e);
      }

      try
      {
         m_platformAdapter = platformAdapter;
         m_resourceAdapter = platformAdapter.createResourceAdapter(config);
         m_endpointFactory = new JMSPlatformMessageEndpointFactory(this);
      }
      catch (Throwable t)
      {
         throw new ResourceException("Unable to create the platform objects", t);
      }

      ((ArrayList)m_idleConsumerList).trimToSize();
   }

   // operations

   /**
    * @return The platform adapter.
    */
   public JMSPlatformAdapter getPlatformAdapter()
   {
      return m_platformAdapter;
   }
   
   /**
    * @see nexj.core.rpc.ra.GenericConsumerPool#connect()
    */
   protected void connect() throws Throwable
   {
      if (!m_bStarted && m_config.getMaxPoolSize() > 0)
      {
         m_activationSpec = m_platformAdapter.createActivationSpec((JMSConsumerConfig)m_config);
         m_activationSpec.setResourceAdapter(m_resourceAdapter);
         
         ClassLoader classLoaderSaved = JMSUtil.setContextClassLoader(getClassLoader());

         try
         {
            m_resourceAdapter.start(m_adapter.getContext());
            m_resourceAdapter.endpointActivation(m_endpointFactory, m_activationSpec);
            m_bStarted = true;
         }
         finally
         {
            JMSUtil.setContextClassLoader(classLoaderSaved);
         }

         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Activated endpoint " + m_activationSpec); 
         }
      }
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumerPool#stop()
    */
   protected void stop()
   {
      if (m_bStarted)
      {
         m_platformAdapter.beforeStop(this);
            
         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Deactivating endpoint " + m_activationSpec); 
         }

         m_resourceAdapter.endpointDeactivation(m_endpointFactory, m_activationSpec);
         m_bStarted = false;

         m_platformAdapter.afterStop(this);
      }
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumerPool#close()
    */
   protected void close()
   {
      if (m_bStarted)
      {
         m_resourceAdapter.stop();
      }
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumerPool#createConsumer()
    */
   protected GenericConsumer createConsumer() throws Throwable
   {
      throw new UnsupportedOperationException();
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumerPool#isTransactional()
    */
   public boolean isTransactional()
   {
      return m_bTransactional;
   }

   /**
    * @see nexj.core.rpc.jms.ra.JMSDynamicSelectorConsumerPool#resetMessageSelector()
    */
   protected void resetMessageSelector()
   {
      stop();

      try
      {
         String sMessageSelector = getMessageSelector();

         m_platformAdapter.setMessageSelector(m_activationSpec, sMessageSelector);

         if (m_logger.isDebugEnabled())
         {
            m_logger.debug("Resetting the message selector to " + sMessageSelector + " in " + this);
         }
      }
      catch (Throwable t)
      {
         m_logger.error("Unable to reset the message selector in " + this, t);
      }
      finally
      {
         try
         {
            m_resourceAdapter.endpointActivation(m_endpointFactory, m_activationSpec);
            m_bStarted = true;
         }
         catch (Throwable t)
         {
            m_logger.error("Unable to reactivate endpoint in " + this, t);
         }
      }
   }

   /**
    * @return The class loader needed for starting the resource adapter.
    */
   protected ClassLoader getClassLoader()
   {
      if (J2EEUtil.CONTAINER == J2EEUtil.WEBSPHERE)
      {
         return Thread.currentThread().getContextClassLoader().getParent();
      }

      return null;
   }
}