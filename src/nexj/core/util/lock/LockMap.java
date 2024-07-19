package nexj.core.util.lock;

import nexj.core.util.Lookup;

/**
 * Lock map.
 */
public abstract class LockMap
{
   // associations

   /**
    * Maps an object to a lock object: Lock[Object].
    */
   protected Lookup m_lockMap;

   // operations

   /**
    * Gets a lock object for the specified object.
    * @param obj The object for which to get a lock. Can be null.
    * @param lTimeout The lock timeout.
    */
   public Lock get(Object obj, long lTimeout)
   {
      if (obj == null)
      {
         return new Lock(lTimeout);
      }

      synchronized (m_lockMap)
      {
         Lock lock = (Lock)m_lockMap.get(obj);

         if (lock == null)
         {
            lock = new Lock(lTimeout);
            m_lockMap.put(obj, lock);
         }

         return lock;
      }
   }

}
