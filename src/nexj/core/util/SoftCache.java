// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.SoftReference;


/**
 * Cache using soft references to store the values and invalidating cached
 * data after a time period has passed. Based on nexj.core.util.SoftHashTab.
 */
public class SoftCache
{
   // attributes

   /**
    * The maximum duration (in ms) to keep a value in the cache.
    */
   protected long m_lMaxDuration = 15 * 60 * 1000;

   // associations

   /**
    * The cache.
    */
   protected HashTab m_cache;

   /**
    * The reference queue with stale references populated by the garbage collector. 
    */
   protected transient ReferenceQueue m_refq = new ReferenceQueue();

   // constructors

   /**
    * Creates a cache with the default max duration.
    */
   public SoftCache()
   {
      m_cache = new HashTab();
   }

   /**
    * Creates a cache.
    * @param lMaxDuration The maximum time (in ms) to keep a pair in cache.
    */
   public SoftCache(long lMaxDuration)
   {
      m_cache = new HashTab();
      m_lMaxDuration = lMaxDuration;
   }

   // operations

   /**
    * Gets the current time.
    * @return The current time.
    */
   protected long getTime()
   {
      return System.currentTimeMillis();
   }

   /**
    * Removes the stale references from the queue and the hash table.
    */
   protected void removeStaleReferences()
   {
      Reference ref;
      long lTime = getTime();

      while ((ref = m_refq.poll()) != null)
      {
         SoftCache.Ref oldRef = (SoftCache.Ref)m_cache.remove(((SoftCache.Ref)ref).key);

         if (oldRef != null && oldRef.get() != null && (lTime - oldRef.m_lTimeStamp) <= m_lMaxDuration)
         {
            m_cache.put(oldRef.key, oldRef);
         }
      }
   }

   /**
    * Gets a value by key.
    * @param key The key. Cannot be null.
    * @return The value corresponding to this key (or null if none).
    */
   public synchronized Object get(Object key)
   {
      removeStaleReferences();

      Object obj = m_cache.get(key);

      if (obj != null)
      {
         if ((getTime() - ((SoftCache.Ref)obj).m_lTimeStamp) > m_lMaxDuration)
         {
            m_cache.remove(key);
            obj = null;
         }
         else
         {
            obj = ((SoftCache.Ref)obj).get();
         }
      }

      return obj;
   }

   /**
    * Inserts a key-value pair into the cache.
    * @param key The key. Cannot be null.
    * @param value The value.
    */
   public synchronized void put(Object key, Object value)
   {
      removeStaleReferences();

      m_cache.put(key, new Ref(key, value, m_refq, getTime()));
   }

   /**
    * Removes an entry from the cache.
    * @param key The key of the value to remove.
    * @return The value of the entry that was removed.
    */
   public synchronized Object remove(Object key)
   {
      removeStaleReferences();

      return m_cache.remove(key);
   }

   /**
    * Removes all data from the cache.
    */
   public synchronized void clear()
   {
      m_cache.clear();

      while (m_refq.poll() != null);
   }

   // inner classes

   /**
    * The soft reference.
    */
   protected static class Ref extends SoftReference
   {
      // attributes

      /**
       * The time stamp.
       */
      public long m_lTimeStamp;

      // associations

      /**
       * The object key.
       */
      public Object key;

      // constructor

      /**
       * Constructs the soft reference.
       * @param key The object key.
       * @param referent The referred object.
       * @param q The reference queue.
       * @param lTimeStamp The time stamp.
       */
      public Ref(Object key, Object referent, ReferenceQueue q, long lTimeStamp)
      {
         super(referent, q);
         this.key = key;
         m_lTimeStamp = lTimeStamp;
      }
   }
}