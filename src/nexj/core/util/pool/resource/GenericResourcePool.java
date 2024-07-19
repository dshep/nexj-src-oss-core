package nexj.core.util.pool.resource;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import nexj.core.util.HashDeque;
import nexj.core.util.HashTab;
import nexj.core.util.HolderDeque;
import nexj.core.util.IdentityHashTab;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.Null;
import nexj.core.util.ObjUtil;

/**
 * Generic resource pool implementation.
 * NOTE: All synchronization should be done through the lock()/unlock() methods.
 */
public abstract class GenericResourcePool implements ResourcePool
{
   // attributes

   /**
    * Current resource count, including both active and idle resources.
    */
   protected int m_nResourceCount;

   /**
    * Current idle resource count.
    */
   protected int m_nIdleCount;

   // associations

   /**
    * Pool mutex.
    */
   protected final Lock m_lock;

   /**
    * Idle resource change condition.
    */
   protected final Condition m_idle;

   /**
    * Idle pooled resource config to resource set map: HolderDeque[Object].
    */
   protected Lookup m_idleMap = new HashTab();

   /**
    * Active resource key map: Key[Resource].
    */
   protected Lookup m_activeMap = new IdentityHashTab();

   /**
    * Resource map: HolderDeque[Key].
    */
   protected Lookup m_resourceMap = new HashTab();

   /**
    * The pool logger.
    */
   protected Logger m_logger;

   // constructors

   /**
    * Constructs the pool.
    */
   protected GenericResourcePool()
   {
      m_lock = new ReentrantLock(isFIFO());
      m_idle = m_lock.newCondition();
      m_logger = getLogger();
   }

   // operations

   /**
    * @return The resource pool logger.
    */
   protected Logger getLogger()
   {
      return Logger.getLogger(getClass());
   }

   /**
    * @return True to allocate the resources between threads using FIFO priority,
    * false to schedule the threads for optimal CPU utilization.
    */
   protected boolean isFIFO()
   {
      return true;
   }

   /**
    * Locks the pool.
    */
   protected void lock()
   {
      m_lock.lock();
   }

   /**
    * Unlocks the pool.
    */
   protected void unlock()
   {
      m_lock.unlock();
   }

   /**
    * Wakes the threads waiting on idle resources.
    */
   protected void wake()
   {
      m_idle.signalAll();
   }

   /**
    * @see nexj.core.util.pool.resource.ResourcePool#get(java.lang.Object)
    */
   public Resource get(Object config) throws PoolBusyException, ResourceFactoryException
   {
      Object assoc = getAssoc();
      Key key = createKey(config, assoc);
      Resource resource = null;
      HolderDeque pool = null;
      boolean bMonitored = isMonitored();
      boolean bLocked = false;
      boolean bNew = false;
      boolean bDone = false;
      boolean bAssociated = false;
      boolean bStarted = false;
      boolean bElapsed = true;
      int nResourceCount = 0;
      int nIdleCount = 0;
      long lStartTime = 0;
      long lElapsedTime = 0;

      try
      {
         lock();
         bLocked = true;

         try
         {
            do
            {
               Resource idle = null;

               if (key.isAssociated())
               {
                  pool = (HolderDeque)m_resourceMap.get(key);

                  if (pool != null)
                  {
                     try
                     {
                        idle = find(pool, config);
                     }
                     catch (UnsupportedOperationException e)
                     {
                        key.setPooled(false);
                     }

                     if (idle != null)
                     {
                        bAssociated = true;
                        key = (Key)m_activeMap.get(idle);
                        key.setActive(true);
                     }
                  }
               }

               if (idle == null && key.isPooled())
               {
                  pool = (HolderDeque)m_idleMap.get(key.getConfig());

                  if (pool != null)
                  {
                     try
                     {
                        idle = reserve(pool, config);
                     }
                     catch (UnsupportedOperationException e)
                     {
                        key.setPooled(false);
                     }

                     if (idle != null && pool.remove(idle))
                     {
                        --m_nIdleCount;
                     }
                  }
               }

               resource = idle;

               if (resource != null)
               {
                  resource.reference();
               }
               else
               {
                  int nMaxSize = getMaxSize();

                  if (m_nResourceCount >= nMaxSize && nMaxSize >= 0)
                  {
                     long lBusyTimeout = getBusyTimeout();

                     try
                     {
                        if (!bStarted && (bMonitored || lBusyTimeout >= 0))
                        {
                           lStartTime = System.nanoTime();
                           bStarted = true;
                        }

                        // Try to dispose of a LRU idle resource
                        if (disposeLRU(true))
                        {
                           continue;
                        }

                        // Wait for a resource to become available
                        if (lBusyTimeout < 0)
                        {
                           m_idle.await();
                        }
                        else
                        {
                           if (!bElapsed)
                           {
                              lElapsedTime = (System.nanoTime() - lStartTime) / 1000000;
                              bElapsed = true;
                           }

                           if (lElapsedTime >= lBusyTimeout)
                           {
                              throw new PoolBusyException(lBusyTimeout, nMaxSize);
                           }

                           m_idle.await(lBusyTimeout - lElapsedTime, TimeUnit.MILLISECONDS);
                        }
                     }
                     catch (InterruptedException e)
                     {
                     }

                     bElapsed = false;
                  }
                  else
                  {
                     ++m_nResourceCount;
                     bNew = true;
                  }
               }
            }
            while (resource == null && !bNew);

            nResourceCount = m_nResourceCount;
            nIdleCount = m_nIdleCount;
         }
         finally
         {
            unlock();
            bLocked = false;
         }

         if (resource == null)
         {
            try
            {
               resource = create(config);
            }
            catch (Throwable t)
            {
               throw new ResourceFactoryException("err.pool.resource.create", t);
            }

            resource.setPool(this);
            resource.reference();
         }

         if (!bAssociated)
         {
            if (assoc != null)
            {
               associate(resource, assoc);
            }

            lock();
            bLocked = true;

            try
            {
               m_activeMap.put(resource, key);
               pool = (HolderDeque)m_resourceMap.get(key);

               if (pool == null)
               {
                  pool = new HashDeque();
                  m_resourceMap.put(key, pool);
               }

               pool.addLast(resource);
            }
            finally
            {
               unlock();
               bLocked = false;
            }
         }

         resource.setTime(System.currentTimeMillis());

         if (m_logger.isDebugEnabled())
         {
            m_logger.debug(((bNew) ? "Created " : "Activated ") + resource);
         }

         bDone = true;
      }
      finally
      {
         if (bLocked)
         {
            unlock();
            bLocked = false;
         }

         if (!bDone && !bAssociated && resource != null)
         {
            if (resource != null)
            {
               resource.setPool(null);
               resource.dispose();
            }

            lock();

            try
            {
               if (resource != null)
               {
                  remove(resource);
               }

               pool = (HolderDeque)m_idleMap.get(key.getConfig());

               if (pool != null && pool.isEmpty())
               {
                  m_idleMap.remove(key.getConfig());
               }

               nResourceCount = --m_nResourceCount;
               nIdleCount = m_nIdleCount;
               wake();
            }
            finally
            {
               unlock();
            }
         }

         if (bMonitored)
         {
            if (!bElapsed)
            {
               lElapsedTime = (System.nanoTime() - lStartTime) / 1000000;
            }

            monitorActivation(nIdleCount, nResourceCount - nIdleCount, lElapsedTime, bDone);
         }
      }

      return resource;
   }

   /**
    * Creates a resource lookup key.
    * @param config The resource configuration. Can be null.
    * @param assoc The resource association key. Can be null.
    */
   protected Key createKey(Object config, Object assoc)
   {
      return new Key(config, assoc);
   }

   /**
    * Finds a resource in a pool and validates it.
    * Invalid resources are removed from the pool.
    * @param pool The pool where to search for the resource.
    * @param config The resource configuration. Can be null.
    * @return The found resource, or null if not found.
    * @throws UnsupportedOperationException if pooling is not supported.
    */
   protected Resource reserve(HolderDeque pool, Object config) throws UnsupportedOperationException
   {
      for (;;)
      {
         Resource resource = find(pool, config);

         if (resource == null)
         {
            return null;
         }

         try
         {
            if (isValid(resource, config))
            {
               return resource;
            }
         }
         catch (Throwable t)
         {
         }

         pool.remove(resource);
         resource.setPool(null);
         resource.dispose();
         --m_nIdleCount;
         --m_nResourceCount;
      }
   }

   /**
    * Finds a resource in a resource pool.
    * @param pool The pool where to search for the resource.
    * @param config The resource configuration. Can be null.
    * @return The found resource, or null if not found.
    * @throws UnsupportedOperationException if pooling is not supported.
    */
   protected Resource find(HolderDeque pool, Object config) throws UnsupportedOperationException
   {
      return (Resource)pool.last();
   }

   /**
    * Validates a resource (e.g. tests the connection by pinging the server).
    * @param resource The resource to validate.
    * @param config The resource configuration. Can be null.
    * @return True if the resource is valid.
    * @throws Exception if the validation fails. Interpreted as returning false.
    */
   protected boolean isValid(Resource resource, Object config) throws Exception
   {
      assert resource != null;

      return true;
   }

   /**
    * Creates a new resource and validates it.
    * @param config The resource configuration. Can be null.
    */
   protected abstract Resource create(Object config) throws Exception;

   /**
    * @return The association object. Can be null.
    */
   protected Object getAssoc()
   {
      return null;
   }

   /**
    * Template method to associate a resource with an object.
    * @param resource The resource to associate.
    * @param assoc The object to associate with.
    */
   protected void associate(Resource resource, Object assoc)
   {
   }

   /**
    * Determines if a resource is still associated.
    * The caller must synchronize the access properly.
    * @param resource The resource to check.
    * @return True if it is still associated.
    */
   protected boolean isAssociated(Resource resource)
   {
      return false;
   }

   /**
    * Template method to dissociate a resource from a previously associated object.
    * @param resource The resource to dissociate.
    * @return True if the resource has been dissociated successfully. 
    */
   protected boolean dissociate(Resource resource)
   {
      return true;
   }

   /**
    * @return True if the pool maintains statistics.
    */
   protected boolean isMonitored()
   {
      return false;
   }

   /**
    * Template method to update the resource activation statistics.
    * @param nIdleCount The idle resource count.
    * @param nActiveCount The active resource count.
    * @param lBusyTime The time spent waiting for the resource to become available (ms).
    * @param bSuccess True if get() has succeeded.
    */
   protected void monitorActivation(int nIdleCount, int nActiveCount, long lBusyTime, boolean bSuccess)
   {
   }

   /**
    * Template method to update the resource deactivation statistics.
    * @param nIdleCount The idle resource count.
    * @param nActiveCount The active resource count.
    * @param lActiveTime The time during which the resource has been active (ms).
    */
   protected void monitorDeactivation(int nIdleCount, int nActiveCount, long lActiveTime)
   {
   }

   /**
    * @see nexj.core.util.pool.resource.ResourcePool#release(nexj.core.util.pool.resource.Resource)
    */
   public void release(Resource resource)
   {
      assert resource != null;

      lock();

      try
      {
         Key key = (Key)m_activeMap.get(resource);

         if (key == null)
         {
            return;
         }

         key.setActive(false);

         if (key.isAssociated() && isAssociated(resource))
         {
            return;
         }
      }
      finally
      {
         unlock();
      }

      deactivate(resource);
   }

   /**
    * Cleans up a resource and returns it to the pool.
    * If the resource is not pooled, it will be disposed of.
    * @param resource The resource to deactivate.
    */
   protected void deactivate(Resource resource)
   {
      assert resource != null;

      Object config = null;
      boolean bAssociated = false;

      lock();

      try
      {
         Key key = remove(resource);

         if (key != null)
         {
            bAssociated = key.isAssociated();

            if (key.isPooled())
            {
               config = key.getConfig();
            }
         }
      }
      finally
      {
         unlock();
      }

      if (bAssociated && !dissociate(resource))
      {
         config = null;
      }

      if (config != null)
      {
         if (m_logger.isDebugEnabled())
         {
            m_logger.debug("Deactivating " + resource);
         }

         try
         {
            resource.reset();
         }
         catch (Throwable t)
         {
            m_logger.error("Unable to reset " + resource, t);
            config = null;
         }
      }

      if (config != null)
      {
         boolean bMonitored = isMonitored();
         long lTime = System.currentTimeMillis();
         long lActiveTime = (bMonitored && resource.getTime() != 0) ? lTime - resource.getTime() : 0; 
         int nResourceCount;
         int nIdleCount;

         lock();

         try
         {
            HolderDeque pool = (HolderDeque)m_idleMap.get(config);

            if (pool == null)
            {
               pool = new HashDeque();
               m_idleMap.put(config, pool);
            }

            resource.setTime(lTime);

            if (pool.addLast(resource))
            {
               ++m_nIdleCount;
               wake();
            }

            nResourceCount = m_nResourceCount;
            nIdleCount = m_nIdleCount;
         }
         finally
         {
            unlock();
         }

         if (bMonitored)
         {
            monitorDeactivation(nIdleCount, nResourceCount - nIdleCount, lActiveTime);
         }
      }
      else
      {
         dispose(resource);
      }
   }

   /**
    * Helper removing a resource from the active resource map.
    * The caller must synchronize the access properly.
    * @param resource The resource to remove.
    * @return The resource key. Can be null.
    */
   protected Key remove(Resource resource)
   {
      Key key = (Key)m_activeMap.remove(resource);

      if (key != null)
      {
         HolderDeque pool = (HolderDeque)m_resourceMap.get(key);

         if (pool != null)
         {
            pool.remove(resource);

            if (pool.isEmpty())
            {
               m_resourceMap.remove(key);
            }
         }
      }

      return key;
   }

   /**
    * Removes the resource from the pools and disposes of it.
    * @param resource The resource to dispose of.
    * @see ResourcePool#dispose(Resource)
    */
   public void dispose(Resource resource)
   {
      assert resource != null;

      boolean bMonitored = isMonitored();
      long lActiveTime = (bMonitored && resource.getTime() != 0) ? System.currentTimeMillis() - resource.getTime() : 0; 

      if (m_logger.isDebugEnabled())
      {
         m_logger.debug("Disposing of " + resource);
      }

      Key key = null;

      lock();

      try
      {
         key = remove(resource);

         if (key != null)
         {
            if (key.isPooled())
            {
               HolderDeque pool = (HolderDeque)m_idleMap.get(key.getConfig());

               if (pool != null)
               {
                  if (pool.remove(resource))
                  {
                     --m_nIdleCount;
                  }

                  if (pool.isEmpty())
                  {
                     m_idleMap.remove(key.getConfig());
                  }
               }
            }
         }
         else
         {
            // Remove an idle resource
            for (Iterator itr = m_idleMap.valueIterator(); itr.hasNext();)
            {
               HolderDeque pool = (HolderDeque)itr.next();

               if (pool.remove(resource))
               {
                  --m_nIdleCount;
               }

               if (pool.isEmpty())
               {
                  itr.remove();
               }
            }
         }
      }
      finally
      {
         unlock();
      }

      if (key != null && key.isAssociated())
      {
         dissociate(resource);
      }

      resource.setPool(null);
      resource.dispose();

      int nResourceCount;
      int nIdleCount;

      lock();

      try
      {
         nResourceCount = --m_nResourceCount;
         nIdleCount = m_nIdleCount;
         wake();
      }
      finally
      {
         unlock();
      }

      if (bMonitored)
      {
         monitorDeactivation(nIdleCount, nResourceCount - nIdleCount, lActiveTime);
      }
   }

   /**
    * Disposes of one LRU resource.
    * @param bLocked True if the current thread owns the pool mutex.
    * @return True if a resource has been disposed of. 
    */
   protected boolean disposeLRU(boolean bLocked)
   {
      Resource lruResource = null;
      HolderDeque lruPool = null;
      Object lruConfig = null;

      if (!bLocked)
      {
         lock();
      }

      try
      {
         if (m_nIdleCount == 0)
         {
            return false;
         }

         for (Lookup.Iterator dequeItr = m_idleMap.valueIterator(); dequeItr.hasNext();)
         {
            HolderDeque pool = (HolderDeque)dequeItr.next();
            Resource resource = (Resource)pool.first();

            if (resource != null &&
               (lruResource == null || lruResource.getTime() > resource.getTime()))
            {
               lruConfig = dequeItr.getKey();
               lruResource = resource;
               lruPool = pool;
            }
         }

         if (lruResource == null)
         {
            return false;
         }

         lruPool.removeFirst();
         --m_nIdleCount;

         if (lruPool.isEmpty())
         {
            m_idleMap.remove(lruConfig);
         }
      }
      finally
      {
         if (!bLocked)
         {
            unlock();
         }
      }

      if (bLocked)
      {
         unlock();
      }

      try
      {
         if (m_logger.isDebugEnabled())
         {
            m_logger.debug("Disposing of LRU idle " + lruResource);
         }

         lruResource.setPool(null);
         lruResource.dispose();
      }
      finally
      {
         lock();
         --m_nResourceCount;

         if (!bLocked)
         {
            try
            {
               wake();
            }
            finally
            {
               unlock();
            }
         }
      }

      return true;
   }

   /**
    * @see nexj.core.util.pool.resource.ResourcePool#dispose()
    */
   public void dispose()
   {
      lock();

      try
      {
         for (Iterator itr = m_activeMap.iterator(); itr.hasNext();)
         {
            Resource resource = (Resource)itr.next();

            resource.setTime(0);
            dispose(resource);
         }

         for (Iterator dequeItr = m_idleMap.valueIterator(); dequeItr.hasNext();)
         {
            for (Iterator itr = ((HolderDeque)dequeItr.next()).iterator(); itr.hasNext();)
            {
               Resource resource = (Resource)itr.next();

               resource.setTime(0);
               dispose(resource);
            }
         }

         assert m_nIdleCount == 0;
         assert m_idleMap.size() == 0;
         assert m_resourceMap.size() == 0;
         assert m_activeMap.size() == 0;
      }
      finally
      {
         unlock();
      }
   }

   /**
    * @see nexj.core.util.pool.resource.ResourcePool#maintain()
    */
   public void maintain()
   {
      long lTimeout = getIdleTimeout();

      if (lTimeout >= 0)
      {
         List resourceList = null;

         lock();

         try
         {
            long lTime = 0;

            for (Iterator dequeItr = m_idleMap.valueIterator(); dequeItr.hasNext();)
            {
               HolderDeque pool = (HolderDeque)dequeItr.next();

               for (;;)
               {
                  Resource resource = (Resource)pool.first();

                  if (resource == null)
                  {
                     break;
                  }

                  if (lTime == 0)
                  {
                     lTime = System.currentTimeMillis();
                  }

                  long lElapsed = lTime - resource.getTime();

                  if (lElapsed >= lTimeout)
                  {
                     if (resourceList == null)
                     {
                        resourceList = new ArrayList();
                     }

                     resourceList.add(resource);
                     pool.removeFirst();
                  }
                  else if (lElapsed < 0)
                  {
                     resource.setTime(lTime);
                  }
                  else
                  {
                     break;
                  }
               }

               if (pool.isEmpty())
               {
                  dequeItr.remove();
               }
            }
         }
         finally
         {
            unlock();

            if (resourceList != null)
            {
               int nCount = resourceList.size();

               for (int i = 0; i < nCount; ++i)
               {
                  Resource resource = (Resource)resourceList.get(i);

                  if (m_logger.isDebugEnabled())
                  {
                     m_logger.debug("Disposing of idle " + resource);
                  }

                  resource.setPool(null);
                  resource.dispose();
               }

               lock();

               try
               {
                  m_nResourceCount -= nCount;
                  m_nIdleCount -= nCount;
                  wake();
               }
               finally
               {
                  unlock();
               }
            }
         }
      }
   }

   /**
    * @see nexj.core.util.pool.resource.ResourcePool#getBusyTimeout()
    */
   public long getBusyTimeout()
   {
      return 10000;
   }

   /**
    * @see nexj.core.util.pool.resource.ResourcePool#getIdleTimeout()
    */
   public long getIdleTimeout()
   {
      return 60000;
   }

   /**
    * @see nexj.core.util.pool.resource.ResourcePool#isTransactional()
    */
   public boolean isTransactional()
   {
      return false;
   }

   // inner classes

   /**
    * Resource key.
    */
   protected static class Key
   {
      // attributes

      /**
       * True if the resource is active.
       */
      protected boolean m_bActive = true;

      /**
       * True if the resource is pooled.
       */
      protected boolean m_bPooled = true;

      // associations

      /**
       * The resource configuration.
       */
      protected Object m_config;

      /**
       * The association object. Can be null.
       */
      protected Object m_assoc;

      // constructors

      /**
       * Constructs the resource key.
       * @param config The resource configuration.
       * @param assoc The association object. Can be null.
       */
      public Key(Object config, Object assoc)
      {
         m_config = (config == null) ? Null.VALUE : config;
         m_assoc = assoc;
      }

      // operations

      /**
       * @return The resource configuration.
       */
      public Object getConfig()
      {
         return m_config;
      }

      /**
       * @return True if there is an association object.
       */
      public boolean isAssociated()
      {
         return m_assoc != null;
      }

      /**
       * Sets the activity flag.
       * @param bActive The activity flag to set.
       */
      public void setActive(boolean bActive)
      {
         m_bActive = bActive;
      }

      /**
       * @return The activity flag.
       */
      public boolean isActive()
      {
         return m_bActive;
      }

      /**
       * Sets the resource pooling flag.
       * This must be properly synchronized.
       * @param bPooled True if the resource is pooled.
       */
      public void setPooled(boolean bPooled)
      {
         m_bPooled = bPooled;
      }

      /**
       * @return True if the resource is pooled.
       */
      public boolean isPooled()
      {
         return m_bPooled;
      }

      /**
       * @see java.lang.Object#equals(java.lang.Object)
       */
      public boolean equals(Object obj)
      {
         if (!(obj instanceof Key))
         {
            return false;
         }

         Key key = (Key)obj;

         return m_config.equals(key.m_config) && ObjUtil.equal(m_assoc, key.m_assoc);
      }

      /**
       * @see java.lang.Object#hashCode()
       */
      public int hashCode()
      {
         return m_config.hashCode() ^ ((m_assoc == null) ? 0 : m_assoc.hashCode());
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         StringBuilder buf = new StringBuilder(64);

         buf.append("Key(config=");
         buf.append(m_config);

         if (m_assoc != null)
         {
            buf.append(", assoc=");
            buf.append(m_assoc);
         }

         buf.append(')');

         return buf.toString();
      }
   }
}
