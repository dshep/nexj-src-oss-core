package nexj.core.rpc.pool;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.transaction.TransactionManager;

import nexj.core.meta.Component;
import nexj.core.meta.Metadata;
import nexj.core.meta.MetadataFinder;
import nexj.core.meta.MetadataObject;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.persistence.DataSource;
import nexj.core.monitoring.jmx.StatManager;
import nexj.core.monitoring.jmx.StatManagerAware;
import nexj.core.rpc.timer.TimeoutProcessor;
import nexj.core.runtime.Initializable;
import nexj.core.util.EmptyIterator;
import nexj.core.util.J2EEUtil;
import nexj.core.util.Lifecycle;
import nexj.core.util.Logger;
import nexj.core.util.Named;
import nexj.core.util.ObjUtil;
import nexj.core.util.pool.consumer.Consumer;
import nexj.core.util.pool.consumer.ConsumerAdapter;
import nexj.core.util.pool.consumer.ConsumerConfig;
import nexj.core.util.pool.consumer.ConsumerPool;
import nexj.core.util.pool.consumer.ConsumerPoolProvider;
import nexj.core.util.pool.resource.ResourcePool;
import nexj.core.util.pool.resource.ResourcePoolProvider;

/**
 * Pool manager.
 */
public class PoolManager implements Lifecycle, Initializable, TimeoutProcessor, StatManagerAware
{
   // attributes

   /**
    * Non-consumer request reference count.
    * After metadata assumption the old pools are released
    * only after the reference count reaches 0.
    */
   protected int m_nRefCount;

   /**
    * True to shut down the pool manager after the reference count reaches 0. 
    */
   protected boolean m_bShutdown;

   /**
    * True if the pools have been started.
    */ 
   protected boolean m_bStarted;

   /**
    * True to enable receivers.
    */
   protected boolean m_bReceiveEnabled = (J2EEUtil.CONTAINER != J2EEUtil.NONE);

   // associations

   /**
    * Mutex for the reference count.
    */
   protected Object m_refMutex = new Object();

   /**
    * The root metadata object.
    */
   protected Metadata m_metadata;

   /**
    * The statistics manager. Can be null.
    */
   protected StatManager m_statManager;

   /**
    * The pool manager that has assumed the pools from this pool manager.
    */
   protected PoolManager m_assumingManager;

   /**
    * Custom pool providers: Object[].
    * Lazy-initialized.
    */
   protected List m_providerList;

   /**
    * The consumer adapter.
    */
   protected ConsumerAdapter m_consumerAdapter;

   /**
    * Externally set consumer adapter instance.  
    */
   protected static ConsumerAdapter s_consumerAdapter;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(PoolManager.class);

   // operations

   /**
    * Sets the receiver enablement flag.
    * @param bReceiveEnabled The receiver enablement flag to set.
    */
   public void setReceiveEnabled(boolean bReceiveEnabled)
   {
      m_bReceiveEnabled = bReceiveEnabled;
   }

   /**
    * @return The receiver enablement flag.
    */
   public boolean isReceiveEnabled()
   {
      return m_bReceiveEnabled;
   }

   /**
    * Adds a pool provider to the pool manager.
    * @param provider The provider to add.
    */
   public void addPoolProvider(Object provider)
   {
      if (m_providerList == null)
      {
         m_providerList = new ArrayList();
      }

      m_providerList.add(provider);
   }

   /**
    * @return The resource pool provider iterator.
    */
   public Iterator getPoolProviderIterator()
   {
      return (m_providerList == null) ? EmptyIterator.getInstance() : m_providerList.iterator();
   }

   /**
    * Sets the root metadata object.
    * @param metadata The root metadata object.
    */
   public void setMetadata(Metadata metadata)
   {
      m_metadata = metadata;
   }

   /**
    * @return The root metadata object.
    */
   public Metadata getMetadata()
   {
      return m_metadata;
   }

   /**
    * Sets the statistics manager.
    * @param manager The statistics manager.
    */
   public void setStatManager(StatManager manager)
   {
      m_statManager = manager;
   }

   /**
    * Increases the reference count.
    */
   public void reference()
   {
      synchronized (m_refMutex)
      {
         ++m_nRefCount;
      }
   }

   /**
    * Decreases the reference count.
    */
   public void release()
   {
      synchronized (m_refMutex)
      {
         if (--m_nRefCount == 0 && m_bShutdown)
         {
            m_refMutex.notifyAll();
         }
      }
   }

   /**
    * Sets the consumer adapter.
    * @param adapter The consumer adapter.
    */
   public static synchronized void setConsumerAdapter(ConsumerAdapter adapter)
   {
      s_consumerAdapter = (adapter == null) ? InvalidConsumerAdapter.INSTANCE : adapter;
      PoolManager.class.notifyAll();
   }

   /**
    * @return The consumer adapter.
    */
   public synchronized ConsumerAdapter getConsumerAdapter()
   {
      return m_consumerAdapter;
   }

   /**
    * @see nexj.core.runtime.Initializable#initialize()
    */
   public void initialize() throws Exception
   {
      if (m_metadata == null)
      {
         m_metadata = m_statManager.getMetadata();
      }
      else if (m_statManager == null)
      {
         Component component = m_metadata.findComponent("System.StatManager");

         if (component != null)
         {
            m_statManager = (StatManager)component.getInstance(null);
         }
      }

      if (J2EEUtil.CONTAINER == J2EEUtil.NONE)
      {
         startup();
      }
   }

   /**
    * Initializes a given object (with StatManager etc).
    * @param obj The object to initialize.
    */
   protected void init(Object obj)
   {
      if (m_statManager != null && obj instanceof StatManagerAware)
      {
         ((StatManagerAware)obj).setStatManager(m_statManager);
      }
   }

   /**
    * Initializes all the objects in the iterator.
    * @param itr The object iterator.
    */
   protected void init(Iterator itr)
   {
      while (itr.hasNext())
      {
         init(itr.next());
      }
   }

   /**
    * Determines if a given resource pool provider is enabled.
    * @param provider The resource pool provider.
    */
   protected boolean isEnabled(ResourcePoolProvider provider)
   {
      if (provider instanceof DataSource)
      {
         return ((DataSource)provider).isEnabled();
      }

      if (provider instanceof Channel)
      {         
         return ((Channel)provider).isSendable();
      }

      return true;
   }

   /**
    * Determines if a given consumer pool provider is enabled.
    * @param provider The consumer pool provider.
    */
   protected boolean isEnabled(ConsumerPoolProvider provider)
   {
      if (provider instanceof DataSource)
      {
         return ((DataSource)provider).isEnabled();
      }

      if (provider instanceof Channel)
      {
         return ((Channel)provider).isReceivable();
      }

      return true;
   }

   /**
    * Creates all resource pools in providers referenced by the iterator.
    * @param itr The iterator.
    */
   protected void createResources(Iterator itr)
   {
      while (itr.hasNext())
      {
         Object obj = itr.next();

         if (obj instanceof ResourcePoolProvider)
         {
            ResourcePoolProvider provider = (ResourcePoolProvider)obj;

            if (isEnabled(provider) && provider.getResourcePool() == null)
            {
               try
               {
                  ResourcePool pool = provider.createResourcePool();

                  provider.setResourcePool(pool);
                  init(pool);
               }
               catch (Throwable t)
               {
                  s_logger.error("Unable to start the resource pool of " + obj, t);
               }
            }
         }
      }
   }

   /**
    * Starts all resource pools in providers referenced by the iterator.
    * @param itr The iterator.
    */
   protected void startConsumers(Iterator itr)
   {
      while (itr.hasNext())
      {
         Object obj = itr.next();

         if (obj instanceof ConsumerPoolProvider)
         {
            ConsumerPoolProvider provider = (ConsumerPoolProvider)obj;

            if (isEnabled(provider) && provider.getConsumerPool() == null)
            {
               try
               {
                  ConsumerPool pool = provider.createConsumerPool(m_consumerAdapter);

                  provider.setConsumerPool(pool);
                  init(pool);
                  pool.start();
               }
               catch (Throwable t)
               {
                  s_logger.error("Unable to start the consumer pool of " + obj, t);
               }
            }
         }
      }
   }

   /**
    * @see nexj.core.util.Lifecycle#startup()
    */
   public synchronized void startup() throws Exception
   {
      if (!m_bStarted)
      {
         if (m_consumerAdapter == null)
         {
            switch (J2EEUtil.CONTAINER)
            {
            case J2EEUtil.NONE:
               if (m_bReceiveEnabled)
               {
                  m_consumerAdapter = new GenericConsumerAdapter();
               }

               break;

            case J2EEUtil.TEEE:
               m_consumerAdapter = new GenericConsumerAdapter();
               break;

            default:
               synchronized (PoolManager.class)
               {
                  if (s_consumerAdapter == null)
                  {
                     s_logger.debug("Waiting for consumer adapter initialization");
                  }

                  while (s_consumerAdapter == null)
                  {
                     PoolManager.class.wait();
                  }

                  if (s_consumerAdapter instanceof InvalidConsumerAdapter)
                  {
                     return;
                  }

                  m_consumerAdapter = s_consumerAdapter;
               }

               break;
            }
         }

         try
         {
            init(getPoolProviderIterator());
            createResources(getPoolProviderIterator());

            if (m_metadata != null)
            {
               init(m_metadata.getDataSourceFragmentIterator());
               init(m_metadata.getChannelIterator());
               createResources(m_metadata.getDataSourceFragmentIterator());
               createResources(m_metadata.getChannelIterator());
            }

            if (m_consumerAdapter != null)
            {
               init(m_consumerAdapter);
               startConsumers(getPoolProviderIterator());

               if (m_metadata != null)
               {
                  startConsumers(m_metadata.getDataSourceFragmentIterator());
                  startConsumers(m_metadata.getChannelIterator());
               }
            }
         }
         catch (Throwable t)
         {
            stop();
            ObjUtil.rethrow(t);
         }

         m_bStarted = true;
      }
   }

   /**
    * Stops all consumer pools in providers referenced by the iterator.
    * @param itr The iterator.
    * @param finder The finder for provider matching.
    * @param bWait True to wait for the processing to complete.
    */
   protected void stopConsumers(Iterator itr, ConsumerPoolProviderFinder finder, boolean bWait)
   {
      while (itr.hasNext())
      {
         Object obj = itr.next();

         if (obj instanceof ConsumerPoolProvider)
         {
            ConsumerPoolProvider provider = (ConsumerPoolProvider)obj;
            ConsumerPool pool = provider.getConsumerPool();

            if (pool != null)
            {
               try
               {
                  // This is not supposed to throw any exceptions
                  if (!isAssumed(provider, finder))
                  {
                     pool.stop(bWait);
                  }
               }
               catch (Throwable t)
               {
                  s_logger.error("Unable to stop the consumer pool of " + obj, t);
               }

               provider.setConsumerPool(null);
            }
         }
      }
   }

   /**
    * Determines if the consumer pool has been assumed.
    * @param provider The consumer pool provider.
    * @param finder The finder for matching the provider.
    * @return True if the consumer pool has been assumed.
    */
   protected boolean isAssumed(ConsumerPoolProvider provider, ConsumerPoolProviderFinder finder)
   {
      ConsumerPool pool = provider.getConsumerPool();

      if (pool != null && finder != null)
      {
         ConsumerPoolProvider newProvider = finder.findConsumerPoolProvider(provider);

         return newProvider != null && newProvider.getConsumerPool() == pool;
      }

      return false;
   }

   /**
    * Disposes of all resource pools in providers referenced by the iterator.
    * @param itr The iterator.
    * @param finder Finder for provider matching.
    */
   protected void disposeResources(Iterator itr, ResourcePoolProviderFinder finder)
   {
      while (itr.hasNext())
      {
         Object obj = itr.next();

         if (obj instanceof ResourcePoolProvider)
         {
            ResourcePoolProvider provider = (ResourcePoolProvider)obj;
            ResourcePool pool = provider.getResourcePool();

            if (pool != null)
            {
               try
               {
                  // This is not supposed to throw any exceptions
                  if (!isAssumed(provider, finder))
                  {
                     pool.dispose();
                  }
               }
               catch (Throwable t)
               {
                  s_logger.error("Unable to dispose the resource pool of " + obj, t);
               }

               provider.setResourcePool(null);
            }
         }
      }
   }

   /**
    * Determines if the resource pool has been assumed.
    * @param provider The resource pool provider.
    * @param finder Finder for provider matching.
    * @return True if the resource pool has been assumed.
    */
   protected boolean isAssumed(ResourcePoolProvider provider, ResourcePoolProviderFinder finder)
   {
      ResourcePool pool = provider.getResourcePool();

      if (pool != null && finder != null)
      {
         ResourcePoolProvider newProvider = finder.findResourcePoolProvider(provider);

         return newProvider != null && newProvider.getResourcePool() == pool;
      }

      return false;
   }

   /**
    * Stops all pools in all providers. 
    */
   protected void stop()
   {
      boolean bAssumed = (m_assumingManager != null);

      if (m_consumerAdapter != null)
      {
         ConsumerPoolProviderFinder metadataFinder = (bAssumed) ? m_assumingManager.getMetadataConsumerPoolProviderFinder() : null;
         ConsumerPoolProviderFinder customFinder = (bAssumed) ? m_assumingManager.getConsumerPoolProviderFinder() : null;

         if (m_metadata != null)
         {
            stopConsumers(m_metadata.getChannelIterator(), metadataFinder, false);
            stopConsumers(m_metadata.getDataSourceFragmentIterator(), metadataFinder, false);
         }

         stopConsumers(getPoolProviderIterator(), customFinder, false);

         if (m_metadata != null)
         {
            stopConsumers(m_metadata.getChannelIterator(), metadataFinder, true);
            stopConsumers(m_metadata.getDataSourceFragmentIterator(), metadataFinder, true);
         }

         stopConsumers(getPoolProviderIterator(), customFinder, true);

         if (!bAssumed)
         {
            m_consumerAdapter.dispose();
         }
      }

      ResourcePoolProviderFinder finder = (bAssumed) ? m_assumingManager.getMetadataResourcePoolProviderFinder() : null;

      if (m_metadata != null)
      {
         disposeResources(m_metadata.getChannelIterator(), finder);
         disposeResources(m_metadata.getDataSourceFragmentIterator(), finder);
      }

      disposeResources(getPoolProviderIterator(), (bAssumed) ? m_assumingManager.getResourcePoolProviderFinder() : null);
   }

   /**
    * @see nexj.core.util.Lifecycle#shutdown()
    */
   public synchronized void shutdown()
   {
      if (m_bStarted)
      {
         stop();
         m_bStarted = false;
      }
   }

   /**
    * @see nexj.core.util.Suspendable#suspend()
    */
   public void suspend() throws Exception
   {
      shutdown();
   }

   /**
    * @see nexj.core.util.Suspendable#resume()
    */
   public void resume() throws Exception
   {
      startup();
   }

   /**
    * Assumes all resource pools in providers referenced by the iterator.
    * @param itr The iterator.
    * @param finder The finder for matching resource pool providers.
    */
   protected void assumeResources(Iterator itr, ResourcePoolProviderFinder finder)
   {
      while (itr.hasNext())
      {
         Object obj = itr.next();

         if (obj instanceof ResourcePoolProvider)
         {
            ResourcePoolProvider oldProvider = (ResourcePoolProvider)obj;
            ResourcePool pool = oldProvider.getResourcePool();

            if (pool != null)
            {
               try
               {
                  ResourcePoolProvider provider = finder.findResourcePoolProvider(oldProvider); 

                  if (provider != null && isEnabled(provider))
                  {
                     provider.setResourcePool(pool);
                     init(pool);
                  }
               }
               catch (Throwable t)
               {
                  s_logger.error("Unable to assume the resource pool of " + obj, t);
               }
            }
         }
      }
   }

   /**
    * Assumes all consumer pools in providers referenced by the iterator.
    * @param itr The iterator.
    * @param finder The finder for matching consumer pool providers.
    */
   protected void assumeConsumers(Iterator itr, ConsumerPoolProviderFinder finder)
   {
      while (itr.hasNext())
      {
         Object obj = itr.next();

         if (obj instanceof ConsumerPoolProvider)
         {
            ConsumerPoolProvider oldProvider = (ConsumerPoolProvider)obj;
            ConsumerPool pool = oldProvider.getConsumerPool();

            if (pool != null)
            {
               try
               {
                  ConsumerPoolProvider provider = finder.findConsumerPoolProvider(oldProvider);

                  if (provider != null && isEnabled(provider) &&
                     ((ConsumerConfig)provider).isCompatible((ConsumerConfig)oldProvider))
                  {
                     provider.setConsumerPool(pool);
                     pool.setConfig((ConsumerConfig)((MetadataObject)provider).clone());
                     init(pool);
                  }
                  else
                  {
                     // This is not supposed to throw any exceptions
                     pool.stop(false);
                  }
               }
               catch (Throwable t)
               {
                  s_logger.error("Unable to assume the consumer pool of " + obj, t);
               }
            }
         }
      }
   }

   /**
    * Finds a custom provider by name.
    * @param pool The pool to match.
    * @return The pool provider, or null if not found.
    */
   protected Object findPoolProvider(Object pool)
   {
      if (pool instanceof Named && m_providerList != null)
      {
         String sName = ((Named)pool).getName();

         if (sName != null)
         {
            for (int i = 0; i < m_providerList.size(); ++i)
            {
               Object provider = m_providerList.get(i);

               if (provider instanceof Named &&
                  sName.equals(((Named)provider).getName()))
               {
                  return provider;
               }
            }
         }
      }

      return null;
   }

   /**
    * Assumes the pools from another pool manager.
    * @param manager The source pool manager.
    */
   public void assume(PoolManager manager)
   {
      synchronized (manager)
      {
         m_consumerAdapter = manager.getConsumerAdapter();

         init(getPoolProviderIterator());
         assumeResources(manager.getPoolProviderIterator(), getResourcePoolProviderFinder());

         Metadata metadata = manager.getMetadata();

         if (metadata != null)
         {
            init(metadata.getDataSourceFragmentIterator());
            init(metadata.getChannelIterator());

            ResourcePoolProviderFinder resourcePoolProviderFinder = getMetadataResourcePoolProviderFinder();

            assumeResources(metadata.getDataSourceFragmentIterator(), resourcePoolProviderFinder);
            assumeResources(metadata.getChannelIterator(), resourcePoolProviderFinder);
         }

         createResources(getPoolProviderIterator());

         if (m_metadata != null)
         {
            createResources(m_metadata.getDataSourceFragmentIterator());
            createResources(m_metadata.getChannelIterator());
         }

         if (m_consumerAdapter != null)
         {
            init(m_consumerAdapter);
            assumeConsumers(manager.getPoolProviderIterator(), getConsumerPoolProviderFinder());

            if (metadata != null)
            {
               ConsumerPoolProviderFinder consumerPoolProviderFinder = getMetadataConsumerPoolProviderFinder();

               assumeConsumers(metadata.getDataSourceFragmentIterator(), consumerPoolProviderFinder);
               assumeConsumers(metadata.getChannelIterator(), consumerPoolProviderFinder);
            }

            startConsumers(getPoolProviderIterator());

            if (m_metadata != null)
            {
               startConsumers(m_metadata.getDataSourceFragmentIterator());
               startConsumers(m_metadata.getChannelIterator());
            }
         }

         manager.m_assumingManager = this;
      }
   }

   /**
    * Completes assuming the pools.
    */
   public void complete()
   {
      assert m_assumingManager != null;

      ConsumerAdapter adapter = m_consumerAdapter;

      if (adapter == null)
      {
         adapter = new GenericConsumerAdapter();
      }

      try
      {
         adapter.run(new Runnable()
         {
            public void run()
            {
               synchronized (m_refMutex)
               {
                  m_bShutdown = true;

                  while (m_nRefCount != 0)
                  {
                     try
                     {
                        m_refMutex.wait();
                     }
                     catch (InterruptedException e)
                     {
                     }
                  }
               }

               shutdown();

               if (s_logger.isDebugEnabled())
               {
                  s_logger.debug("Released the pool manager of " + m_metadata);
               }
            }

            /**
             * @see java.lang.Object#toString()
             */
            public String toString()
            {
               return "Pool Manager Finalizer";
            }
         });
      }
      catch (Throwable t)
      {
         s_logger.error("Unexpected error encountered when releasing " +
            "the pool manager for " + m_metadata, t);

         shutdown();

         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Released the pool manager for " + m_metadata);
         }
      }

      if (adapter != m_consumerAdapter)
      {
         adapter.dispose();
      }
   }

   /**
    * Maintains all pools in providers referenced by the iterator.
    * @param itr The iterator.
    */
   protected void maintain(Iterator itr)
   {
      while (itr.hasNext())
      {
         Object obj = itr.next();

         if (obj instanceof ResourcePoolProvider)
         {
            ResourcePool pool = ((ResourcePoolProvider)obj).getResourcePool();

            if (pool != null)
            {
               pool.maintain();
            }
         }

         if (obj instanceof ConsumerPoolProvider && m_consumerAdapter != null)
         {
            ConsumerPool pool = ((ConsumerPoolProvider)obj).getConsumerPool();

            if (pool != null)
            {
               pool.maintain();
            }
         }
      }
   }

   /**
    * Maintains the resource pools.
    */
   public synchronized void maintain()
   {
      if (m_bStarted)
      {
         maintain(getPoolProviderIterator());

         if (m_metadata != null)
         {
            maintain(m_metadata.getDataSourceFragmentIterator());
            maintain(m_metadata.getChannelIterator());
         }

         if (m_consumerAdapter != null)
         {
            m_consumerAdapter.maintain();
         }
      }
   }

   /**
    * @see nexj.core.rpc.timer.TimeoutProcessor#timeout()
    */
   public long timeout()
   {
      maintain();

      return 0;
   }

   /**
    * @return A finder for the custom resource pool providers.
    */
   protected ResourcePoolProviderFinder getResourcePoolProviderFinder()
   {
      return new ResourcePoolProviderFinder()
      {
         public ResourcePoolProvider findResourcePoolProvider(ResourcePoolProvider old)
         {
            Object provider = findPoolProvider(old);

            if (provider instanceof ResourcePoolProvider)
            {
               return (ResourcePoolProvider)provider;
            }

            return null;
         }
      };   
   }

   /**
    * @return A finder for the metadata resource pool providers.
    */
   protected ResourcePoolProviderFinder getMetadataResourcePoolProviderFinder()
   {
      return new ResourcePoolProviderFinder()
      {
         public ResourcePoolProvider findResourcePoolProvider(ResourcePoolProvider old)
         {
            return (old instanceof MetadataFinder && m_metadata != null) ?
               (ResourcePoolProvider)((MetadataFinder)old).find(m_metadata) : null;
         }
      };
   }

   /**
    * @return A finder for the custom consumer pool providers.
    */
   protected ConsumerPoolProviderFinder getConsumerPoolProviderFinder()
   {
      return new ConsumerPoolProviderFinder()
      {
         public ConsumerPoolProvider findConsumerPoolProvider(ConsumerPoolProvider old)
         {
            Object provider = findPoolProvider(old);

            if (provider instanceof ConsumerPoolProvider)
            {
               return (ConsumerPoolProvider)provider;
            }

            return null;
         }
      };
   }

   /**
    * @return A finder for the metadata consumer pool providers.
    */
   protected ConsumerPoolProviderFinder getMetadataConsumerPoolProviderFinder()
   {
      return new ConsumerPoolProviderFinder()
      {
         public ConsumerPoolProvider findConsumerPoolProvider(ConsumerPoolProvider old)
         {
            return (old instanceof MetadataFinder && m_metadata != null) ?
               (ConsumerPoolProvider)((MetadataFinder)old).find(m_metadata) : null;
         }
      };
   }

   /**
    * Gets the pool manager instance from the root metadata object.
    * @param metadata The root metadata object.
    * @return The pool manager instance.
    */
   public static PoolManager getInstance(Metadata metadata)
   {
      return (PoolManager)metadata.getComponent("System.PoolManager").getInstance(null);
   }

   // inner classes

   /**
    * Interface for finding a matching resource pool provider.
    */
   protected interface ResourcePoolProviderFinder
   {
      /**
       * Finds a resource pool provider.
       * @param provider The matching pool provider.
       * @return The found resource pool provider, or null if not found.
       */
      ResourcePoolProvider findResourcePoolProvider(ResourcePoolProvider provider);
   }

   /**
    * Interface for finding a matching consumer pool provider.
    */
   protected interface ConsumerPoolProviderFinder
   {
      /**
       * Finds a consumer pool provider.
       * @param pool The matching pool provider.
       * @return The found consumer pool provider, or null if not found.
       */
      ConsumerPoolProvider findConsumerPoolProvider(ConsumerPoolProvider provider);
   }

   /**
    * Invalid consumer adapter implementation.
    */
   public static class InvalidConsumerAdapter implements ConsumerAdapter
   {
      // constants

      /**
       * Singleton instance of the invalid consumer adapter. 
       */
      public final static ConsumerAdapter INSTANCE = new InvalidConsumerAdapter();

      // operations

      /**
       * @see nexj.core.util.pool.consumer.ConsumerAdapter#getTransactionManager()
       */
      public TransactionManager getTransactionManager()
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see nexj.core.util.pool.consumer.ConsumerAdapter#run(java.lang.Runnable)
       */
      public void run(Runnable runnable) throws Throwable
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see nexj.core.util.pool.consumer.ConsumerAdapter#run(nexj.core.util.pool.consumer.Consumer)
       */
      public void run(Consumer consumer) throws Throwable
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see nexj.core.util.pool.consumer.ConsumerAdapter#getInterceptor(nexj.core.util.pool.consumer.Consumer)
       */
      public Object getInterceptor(Consumer consumer) throws Throwable
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see nexj.core.util.pool.consumer.ConsumerAdapter#begin(nexj.core.util.pool.consumer.Consumer, java.lang.Object)
       */
      public void begin(Consumer consumer, Object interceptor) throws Throwable
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see nexj.core.util.pool.consumer.ConsumerAdapter#process(nexj.core.util.pool.consumer.Consumer, java.lang.Object, java.lang.Object)
       */
      public void process(Consumer consumer, Object interceptor, Object request) throws Throwable
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see nexj.core.util.pool.consumer.ConsumerAdapter#end(java.lang.Object)
       */
      public void end(Object interceptor) throws Throwable
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see nexj.core.util.pool.consumer.ConsumerAdapter#releaseInterceptor(java.lang.Object)
       */
      public void releaseInterceptor(Object interceptor) throws Throwable
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see nexj.core.util.pool.consumer.ConsumerAdapter#dispose()
       */
      public void dispose()
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see nexj.core.util.pool.consumer.ConsumerAdapter#maintain()
       */
      public void maintain()
      {
         throw new UnsupportedOperationException();
      }
   }
}
