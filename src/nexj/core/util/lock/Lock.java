// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util.lock;


/**
 * Thread lock implementation.
 */
public final class Lock
{
   // constants

   /**
    * Infinite timeout.
    */
   public final static long INFINITE = 0; 

   // attributes

   /**
    * The reentrant lock counter.
    */
   private int m_nCount;
   
   /**
    * The lock timeout in milliseconds (INFINITE for infinite).
    */
   private final long m_lTimeout;

   // associations

   /**
    * The owner of the lock.
    */
   private Thread m_owner;

   // constructors

   /**
    * Constructs the session lock.
    * @param nTimeout The timeout in milliseconds.
    */
   public Lock(long lTimeout)
   {
      m_lTimeout = lTimeout;
   }

   // operations

   /**
    * Acquires a lock.
    * @param lTimeout The timeout to use. INFINITE for infinite timeout.
    * @return True if the lock has been newly acquired, false if the lock has been reentered.
    * @throws TimeoutException if a lock timeout has occurred.
    */
   public synchronized boolean lock(long lTimeout) throws TimeoutException
   {
      Thread thread = Thread.currentThread();
      long lEnd = 0; 

      while (m_owner != null && m_owner != thread)
      {
         try
         {
            if (lTimeout > 0)
            {
               long lNow = System.currentTimeMillis();

               if (lEnd == 0)
               {
                  lEnd = lNow + lTimeout;
               }

               if (lEnd > lNow)
               {
                  wait(lEnd - lNow);
               }
               else
               {
                  throw new TimeoutException();
               }
            }
            else
            {
               wait();
            }
         }
         catch (InterruptedException e)
         {
         }
      }

      m_owner = thread;

      return m_nCount++ == 0;
   }

   /**
    * Acquires a lock with the default timeout.
    * @return True if the lock has been newly acquired, false if the lock has been reentered.
    * @throws TimeoutException if a lock timeout has occurred.
    */
   public boolean lock() throws TimeoutException
   {
      return lock(m_lTimeout);
   }

   /**
    * Releases a lock.
    * @return True if the lock has been completely released, false if a reentrant lock has been released.
    */
   public synchronized boolean unlock()
   {
      assert m_nCount > 0;

      Thread thread = Thread.currentThread();

      if (m_owner == thread && --m_nCount == 0)
      {
         m_owner = null;
         notifyAll();

         return true;
      }

      return false;
   }
}
