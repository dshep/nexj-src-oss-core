package nexj.core.util.lock;

import nexj.core.util.WeakHashTab;

/**
 * Weak lock map.
 */
public class WeakLockMap extends LockMap
{
   // constructors

   /**
    * Constructs the lock map.
    */
   public WeakLockMap()
   {
      m_lockMap = new WeakHashTab();
   }
}
