// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime.platform.generic;

import java.lang.ref.ReferenceQueue;
import java.lang.ref.SoftReference;
import java.util.Iterator;

import nexj.core.runtime.DataCache;
import nexj.core.util.HashTab;
import nexj.core.util.Holder;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;

/**
 * Generic implementation of an in-process data cache using soft references.
 */
public class GenericDataCache implements DataCache
{
   // constants
   
   /**
    * Special object indicating a removed entry.
    */
   protected final static Object REMOVED = new Object();
   
   // attributes
   
   /**
    * Timeout for removed objects, ms.
    * 5 min by default.
    */
   protected int m_nTimeout = 5 * 60 * 1000;
   
   /**
    * Current timestamp.
    */
   protected long m_lTimestamp;

   /**
    * The cache distribution flag.
    */
   protected boolean m_bDistributed;

   // associations
   
   /**
    * The cache map.
    */
   protected Lookup m_cacheMap = new HashTab(1024);
   
   /**
    * The stale reference queue, populated by the garbage collector.
    */
   protected ReferenceQueue m_refq = new ReferenceQueue();
   
   /**
    * The first removed reference.
    */
   protected RemovedRef m_firstRemoved;
   
   /**
    * The last removed reference.
    */
   protected RemovedRef m_lastRemoved;
   
   /**
    * The class logger.
    */
   private final static Logger s_logger = Logger.getLogger(GenericDataCache.class);

   // operations

   /**
    * Sets the removal timeout, ms.
    * @param nTimeout The removal timeout, ms to set.
    */
   public synchronized void setTimeout(int nTimeout)
   {
      m_nTimeout = nTimeout;
   }

   /**
    * @return The removal timeout, ms.
    */
   public synchronized int getTimeout()
   {
      return m_nTimeout;
   }

   /**
    * Sets the cache distribution flag.
    * @param bDistributed The cache distribution flag to set.
    */
   public void setDistributed(boolean bDistributed)
   {
      m_bDistributed = bDistributed;
   }

   /**
    * @return The cache distribution flag.
    */
   public boolean isDistributed()
   {
      return m_bDistributed;
   }

   /**
    * Adds a removed reference.
    * @param key The object key.
    * @param lTimestamp The removal timestamp.
    */
   protected void addRemoved(Object key, long lTimestamp)
   {
      RemovedRef ref = new RemovedRef(key, lTimestamp, m_refq);
      
      if (m_lastRemoved == null)
      {
         m_firstRemoved = m_lastRemoved = ref;
      }
      else
      {
         m_lastRemoved.next = ref;
         m_lastRemoved = ref;
      }
      
      m_cacheMap.put(key, ref);
   }
   
   /**
    * Eliminates the old references to removed elements.
    */
   protected void eliminateRemovedReferences()
   {
      long lTimestamp = m_lTimestamp - m_nTimeout;
      int nCount = 0;
      
      while (m_firstRemoved != null && m_firstRemoved.timestamp < lTimestamp)
      {
         if (m_cacheMap.get(m_firstRemoved.key) == m_firstRemoved)
         {
            m_cacheMap.remove(m_firstRemoved.key);
         }

         m_firstRemoved = m_firstRemoved.next;
         ++nCount;
      }

      if (m_firstRemoved == null)
      {
         m_lastRemoved = null;
      }

      if (nCount != 0 && s_logger.isDebugEnabled())
      {
         s_logger.debug("Eliminated " + nCount + " references to removed items");
      }
   }
   
   /**
    * Removes the references freed by the garbage collector.
    */
   protected void removeStaleReferences()
   {
      Object obj;
      
      while ((obj = m_refq.poll()) != null)
      {
         Ref ref = (Ref)obj;
         Ref oldRef = (Ref)m_cacheMap.get(ref.key);

         if (oldRef == null || oldRef.get() == null)
         {
            addRemoved(ref.key, ref.timestamp);
         }
      }
   }

   /**
    * Adds the specified entries into the local cache.
    * @param uncacheSet The set of keys to remove.
    * @param cacheMap The cache map containing the entries.
    * @param lTimestamp The transaction start timestamp.
    */
   protected void cacheLocal(Holder uncacheSet, Lookup cacheMap, long lTimestamp)
   {
      removeStaleReferences();

      if (uncacheSet != null)
      {
         for (Iterator itr = uncacheSet.iterator(); itr.hasNext();)
         {
            Object key = itr.next();
            
            if (cacheMap != null && cacheMap.contains(key))
            {
               m_cacheMap.remove(key);
            }
            else
            {
               addRemoved(key, m_lTimestamp);
            }
         }
      }

      if (cacheMap != null)
      {
         for (Lookup.Iterator itr = cacheMap.iterator(); itr.hasNext();)
         {
            itr.next();

            Object obj = m_cacheMap.put(itr.getKey(), new Ref(itr.getKey(), itr.getValue(), m_lTimestamp, m_refq));

            if (obj != null)
            {
               Ref ref = (Ref)obj;

               if (lTimestamp < ref.timestamp)
               {
                  m_cacheMap.put(itr.getKey(), obj);
               }
            }
         }
      }
   }

   /**
    * @see nexj.core.runtime.DataCache#get(java.lang.Object)
    */
   public Object get(Object key)
   {
      Object obj;

      synchronized (this)
      {
         obj = m_cacheMap.get(key);
      }

      if (obj != null)
      {
         obj = ((Ref)obj).get();
         
         if (obj == REMOVED)
         {
            obj = null;
         }
      }

      return obj;
   }

   /**
    * @see nexj.core.runtime.DataCache#update(nexj.core.util.Holder, nexj.core.util.Lookup, long)
    */
   public void update(Holder uncacheSet, Lookup cacheMap, long lTimestamp)
   {
      if (s_logger.isDumpEnabled())
      {
         if (uncacheSet != null)
         {
            s_logger.dump("Invalidating " + uncacheSet.size() +
               " item(s) (Tx timestamp=" + lTimestamp + "): " + uncacheSet);
         }

         if (cacheMap != null)
         {
            s_logger.dump("Caching " + cacheMap.size() +
               " item(s) (Tx timestamp=" + lTimestamp + "): " + cacheMap);
         }
      }

      synchronized (this)
      {
         m_lTimestamp = System.currentTimeMillis();
         eliminateRemovedReferences();
         cacheLocal(uncacheSet, cacheMap, lTimestamp);
      }
      
      // The UOW currently removes the uncacheSet keys from the cluster
   }

   /**
    * @see nexj.core.runtime.DataCache#clear()
    */
   public void clear()
   {
      synchronized (this)
      {
         m_lTimestamp = System.currentTimeMillis();
         eliminateRemovedReferences();
         removeStaleReferences();
         m_cacheMap.clear();
      }
   }

   // inner classes

   /**
    * Soft reference.
    */
   protected static class Ref extends SoftReference
   {
      public Object key;
      public long timestamp;

      public Ref(Object key, Object obj, long lTimestamp, ReferenceQueue refq)
      {
         super(obj, refq);
         this.key = key;
         this.timestamp = lTimestamp;
      }
   }

   /**
    * Removed reference.
    */
   protected static class RemovedRef extends Ref
   {
      public RemovedRef next;

      public RemovedRef(Object key, long lTimestamp, ReferenceQueue refq)
      {
         super(key, REMOVED, lTimestamp, refq);
      }
   }
}
