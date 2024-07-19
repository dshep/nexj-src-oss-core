// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;

import nexj.core.meta.Metaclass;
import nexj.core.persistence.OID;

/**
 * Weak instance reference maintaining
 * instance/UOW participation lists. 
 */
public class InstanceRef extends WeakReference
{
   // associations

   /**
    * The instance OID.
    */
   protected OID m_oid;
   
   /**
    * The instance persistence root metaclass.
    */
   protected Metaclass m_metaclass;
   
   /**
    * The first lock in the reference chain.
    */
   protected Lock m_lock;

   // constructors
   
   /**
    * @param instance The referenced instance.
    * @param queue The reference finalization queue.
    */
   protected InstanceRef(Instance instance, ReferenceQueue queue)
   {
      super(instance, queue);

      m_oid = instance.getOID();
      m_metaclass = instance.getLazyMetaclass().getPersistenceRoot();
   }

   // operations

   /**
    * @return The referenced instance, or null
    * if it has been released already.
    */
   public Instance getInstance()
   {
      return (Instance)get();
   }

   /**
    * @return True if the instance is share-locked in any UOW.
    */
   public boolean isLocked()
   {
      return m_lock != null;
   }

   /**
    * Determines if the instance is share-locked in a UOW.
    * @param uow The UOW.
    * @return True if the instance is locked.
    */
   public boolean isLocked(UnitOfWork uow)
   {
      assert uow != null;

      for (Lock lock = m_lock; lock != null; lock = lock.m_next)
      {
         if (lock.m_uow == uow)
         {
            return true;
         }
      }
      
      return false;
   }

   /**
    * Locks the instance reference in a given UOW.
    * Does not check for duplicates, this responsibility is on the caller.
    * @param uow The UOW.
    * @return The created lock.
    */
   public Lock lock(UnitOfWork uow)
   {
      assert uow != null;

      for (Lock lock = m_lock; lock != null; lock = lock.m_next)
      {
         if (lock.m_uow == uow)
         {
            return lock;
         }
      }

      Lock lock = new Lock(this, uow, m_lock);

      m_lock = lock;
      uow.addLock(lock);

      return lock;
   }

   /**
    * Removes a share-lock corresponding to a UOW from the reference chain.
    * Does not remove it from the UOW chain.
    * @param uow The UOW.
    * @return The unlinked lock, or null if not found.
    */
   protected Lock unlink(UnitOfWork uow)
   {
      Lock prev = null;

      for (Lock lock = m_lock; lock != null; lock = lock.m_succ)
      {
         if (lock.m_uow == uow)
         {
            if (prev == null)
            {
               m_lock = lock.m_succ;
            }
            else
            {
               prev.m_succ = lock.m_succ;
            }

            return lock;
         }

         prev = lock;
      }

      return null;
   }

   /**
    * Removes all the UOW share-locks from the reference.
    */
   public void unlock()
   {
      while (m_lock != null)
      {
         m_lock.m_uow.removeLock(m_lock);
         m_lock = m_lock.m_succ;
      }
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "Ref: " + get();
   }

   // inner classes

   /**
    * UOW read lock: an association between an instance reference and a UOW,
    * cross-linked from the instance reference and the UOW.
    */
   public static class Lock
   {
      // associations
      
      /**
       * The instance reference.
       */
      protected InstanceRef m_ref;

      /**
       * The UOW.
       */
      protected UnitOfWork m_uow;

      /**
       * The next lock in the reference chain.
       */
      protected Lock m_succ;

      /**
       * The next lock in the UOW chain.
       */
      protected Lock m_next;
      
      /**
       * The previous lock in the UOW chain. 
       */
      protected Lock m_prev;

      // constructors

      /**
       * Constructs the UOW lock.
       * @param ref The instance reference.
       * @param uow The UOW.
       * @param succ The next lock in the reference chain.
       */
      public Lock(InstanceRef ref, UnitOfWork uow, Lock succ)
      {
         m_ref = ref;
         m_uow = uow;
         m_succ = succ;
      }
   }
}
