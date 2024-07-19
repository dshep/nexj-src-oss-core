// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

import nexj.core.util.Holder;
import nexj.core.util.Lookup;

/**
 * Global data cache. 
 */
public interface DataCache
{
   /**
    * Gets an object from the cache.
    * @param key The item key.
    * @return The object, or null if not found.
    */
   Object get(Object key);

   /**
    * Updates the cache.
    * NOTE: This cannot be done with just one map, as caching
    * does not imply invalidation in a distributed environment.
    * @param uncacheSet The set of keys to remove from the cache. Can be null.
    * @param cacheMap The map of objects to put into the cache. Can be null.
    * @param lTimestamp The transaction start timestamp.
    */
   void update(Holder uncacheSet, Lookup cacheMap, long lTimestamp);

   /**
    * Removes all the items from the cache.
    */
   void clear();

   /**
    * @return True if the cache is distributed.
    */
   boolean isDistributed();
}
