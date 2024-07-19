// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.ra;

import java.util.Iterator;

import javax.resource.ResourceException;
import javax.resource.spi.ActivationSpec;
import javax.resource.spi.BootstrapContext;
import javax.resource.spi.ResourceAdapter;
import javax.resource.spi.ResourceAdapterInternalException;
import javax.resource.spi.endpoint.MessageEndpointFactory;
import javax.transaction.xa.XAResource;

import nexj.core.util.HashTab;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;

/**
 * Generic resource adapter implementation.
 */
public abstract class GenericResourceAdapter implements ResourceAdapter
{
   // associations

   /**
    * The bootstrap context.
    */
   protected BootstrapContext m_context;

   /**
    * Map of consumer configurations to consumer pools: GenericConsumerPool[GenericConsumerConfig]. 
    */
   protected final Lookup m_poolMap = new HashTab(4);

   /**
    * Map of instances: GenericResourceAdapter[Class]
    */
   protected final static Lookup s_instanceMap = new HashTab(4);

   /**
    * Map of new consumer configurations to channel names: GenericConsumerConfig[ChannelName]. 
    */
   protected final Lookup m_configMap = new HashTab(4);

   /**
    * The class logger.
    */
   protected final Logger m_logger; 

   // constructors

   /**
    * Constructs the resource adapter.
    * @param logger The class logger.
    */
   protected GenericResourceAdapter(Logger logger)
   {
      m_logger = logger;
   }

   // operations

   /**
    * Creates a new consumer pool.
    * @param factory The endpoint factory.
    * @param cfg The activation specification.
    */
   protected abstract GenericConsumerPool createConsumerPool(MessageEndpointFactory factory, ActivationSpec cfg) throws ResourceException;

   /**
    * @return The bootstrap context.
    */
   public BootstrapContext getContext()
   {
      return m_context;
   }

   /**
    * @see javax.resource.spi.ResourceAdapter#start(javax.resource.spi.BootstrapContext)
    */
   public void start(BootstrapContext context) throws ResourceAdapterInternalException
   {
      m_context = context;
      m_logger.info("Started");

      synchronized (s_instanceMap)
      {
         s_instanceMap.put(this.getClass(), this);
      }
   }

   /**
    * @see javax.resource.spi.ResourceAdapter#stop()
    */
   public void stop()
   {
      synchronized (m_poolMap)
      {
         for (Iterator itr = m_poolMap.valueIterator(); itr.hasNext();)
         {
            GenericConsumerPool pool = (GenericConsumerPool)itr.next();

            try
            {
               pool.shutdown();
            }
            catch (Throwable e)
            {
               m_logger.error("Unable to shutdown " + pool, e);
            }
         }
      }

      synchronized (s_instanceMap)
      {
         s_instanceMap.remove(this.getClass());
      }

      m_logger.info("Stopped");
   }

   /**
    * @see javax.resource.spi.ResourceAdapter#endpointActivation(javax.resource.spi.endpoint.MessageEndpointFactory, javax.resource.spi.ActivationSpec)
    */
   public void endpointActivation(MessageEndpointFactory factory, ActivationSpec cfg) throws ResourceException
   {
      if (cfg instanceof GenericConsumerConfig)
      {
         synchronized (m_poolMap)
         {
            GenericConsumerConfig currentConfig = ((GenericConsumerConfig)cfg);
            String sChannelName = currentConfig.getChannel();

            if (sChannelName != null)
            {
               GenericConsumerConfig originalConfig = (GenericConsumerConfig)m_configMap.get(sChannelName);

               if (originalConfig != null)
               {
                  currentConfig.update(originalConfig);
               }

               m_configMap.put(sChannelName, currentConfig);
            }
         }
      }

      GenericConsumerPool pool = createConsumerPool(factory, cfg);

      synchronized (m_poolMap)
      {
         m_poolMap.put(cfg, pool);
      }

      boolean bDone = false;

      try
      {
         pool.startup();
         bDone = true; 
      }
      finally
      {
         if (!bDone)
         {
            synchronized (m_poolMap)
            {
               m_poolMap.remove(cfg);
            }
         }
      }
   }

   /**
    * @see javax.resource.spi.ResourceAdapter#endpointDeactivation(javax.resource.spi.endpoint.MessageEndpointFactory, javax.resource.spi.ActivationSpec)
    */
   public void endpointDeactivation(MessageEndpointFactory factory, ActivationSpec cfg)
   {
      GenericConsumerPool pool;

      synchronized (m_poolMap)
      {
         pool = (GenericConsumerPool)m_poolMap.remove(cfg);
      }

      if (pool != null)
      {
         try
         {
            pool.shutdown();
         }
         catch (Throwable e)
         {
            m_logger.error("Unable to shutdown " + pool, e);
         }
      }
   }

   /**
    * @see javax.resource.spi.ResourceAdapter#getXAResources(javax.resource.spi.ActivationSpec[])
    */
   public XAResource[] getXAResources(ActivationSpec[] specs) throws ResourceException
   {
      return null;
   }

   /**
    * Update the current consumer config object with the properties from
    * the specified consumer config object.
    * @param newConfig A consumer config object with updated properties.
    * @throws ResourceException if the consumer pool fails to startup again
    *    or the operation is not supported.
    */
   public void update(GenericConsumerConfig newConfig) throws ResourceException
   {
      GenericConsumerConfig oldConfig = null;
      GenericConsumerPool pool = null;

      synchronized (m_poolMap)
      {
         String sChannelName = newConfig.getChannel();

         oldConfig = (GenericConsumerConfig)m_configMap.get(sChannelName);

         if (oldConfig == null)
         {
            m_configMap.put(sChannelName, newConfig);
         }
         else
         {
            pool = (GenericConsumerPool)m_poolMap.get(oldConfig);
         }
      }

      if (pool != null && newConfig.isRestartRequired(oldConfig))
      {
         pool.restart(newConfig);
      }
   }

   /**
    * @param clazz The class object of the desired instance.
    * @return The instance for the specified class.
    */
   public static GenericResourceAdapter getInstance(Class clazz)
   {
      synchronized (s_instanceMap)
      {
         return (GenericResourceAdapter)s_instanceMap.get(clazz);
      }
   }
}
