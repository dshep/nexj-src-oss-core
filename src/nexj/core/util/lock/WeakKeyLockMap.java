package nexj.core.util.lock;

import nexj.core.util.WeakKeyHashTab;

/**
 * Weak key lock map.
 */
public class WeakKeyLockMap extends LockMap
{
   // constructors

   /**
    * Constructs the lock map.
    */
   public WeakKeyLockMap()
   {
      m_lockMap = new WeakKeyHashTab();
   }
}
