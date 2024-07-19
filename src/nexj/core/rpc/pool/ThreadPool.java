package nexj.core.rpc.pool;

import java.security.AccessController;
import java.security.PrivilegedActionException;
import java.security.PrivilegedExceptionAction;

import nexj.core.util.Logger;
import nexj.core.util.Named;
import nexj.core.util.SysUtil;
import nexj.core.util.pool.resource.GenericResource;
import nexj.core.util.pool.resource.Resource;

/**
 * Thread pool.
 */
public class ThreadPool extends StatResourcePool implements Named
{
   // attributes

   /**
    * The pool name.
    */
   protected String m_sName;

   // constructors

   /**
    * Constructs the thread pool.
    * @param sName The thread pool name.
    */
   public ThreadPool(String sName)
   {
      m_sName = sName;
   }

   // operations

   /**
    * @see nexj.core.util.Named#getName()
    */
   public String getName()
   {
      return m_sName;
   }

   /**
    * @see nexj.core.rpc.pool.StatResourcePool#getStatPath()
    */
   protected String getStatPath()
   {
      return m_sName;
   }

   /**
    * @see nexj.core.rpc.pool.StatResourcePool#getStatClassName()
    */
   protected String getStatClassName()
   {
      return null;
   }

   /**
    * @see nexj.core.rpc.pool.StatResourcePool#getResourceName()
    */
   protected String getResourceName()
   {
      return "Thread";
   }

   /**
    * @see nexj.core.util.pool.resource.TransactionalResourcePool#isTransactional()
    */
   public boolean isTransactional()
   {
      return false;
   }

   /**
    * @see nexj.core.util.pool.resource.TransactionalResourcePool#isAssociated()
    */
   public boolean isAssociated()
   {
      return false;
   }

   /**
    * @see nexj.core.util.pool.resource.ResourcePool#getMaxSize()
    */
   public int getMaxSize()
   {
      return -1;
   }

   /**
    * @see nexj.core.util.pool.resource.GenericResourcePool#create(java.lang.Object)
    */
   protected Resource create(Object config) throws Exception
   {
      return new Worker(getLogger());
   }

   // inner classes

   /**
    * Worker thread resource.
    */
   public static class Worker extends GenericResource
   {
      // attributes

      /**
       * True to enable logging in the thread.
       */
      protected boolean m_bLogEnabled;

      // associations

      /**
       * The work item to run.
       */
      protected Runnable m_work;

      /**
       * The thread.
       */
      protected Thread m_thread;

      /**
       * The thread logger.
       */
      protected Logger m_logger;

      // constructors

      /**
       * Constructs the worker thread.
       * @param logger The thread logger.
       */
      protected Worker(Logger logger) throws Exception
      {
         try
         {
            m_logger = logger;
            m_thread = (Thread)AccessController.doPrivileged(
               new PrivilegedExceptionAction()
               {
                  public Object run() throws Exception
                  {
                     return new Thread(getDefaultName())
                     {
                        public void run()
                        {
                           for (;;)
                           {
                              synchronized (Worker.this)
                              {
                                 if (m_work == null)
                                 {
                                    if (m_thread == null)
                                    {
                                       return;
                                    }

                                    try
                                    {
                                       Worker.this.wait();
                                    }
                                    catch (InterruptedException e)
                                    {
                                    }

                                    continue;
                                 }

                                 Logger.setEnabled(m_bLogEnabled);
                              }

                              setName(SysUtil.CAPTION + ' ' + m_work + "#" + Worker.this.hashCode());

                              int nCookie = Logger.pushContext(getName());

                              try
                              {
                                 m_work.run();
                              }
                              catch (Throwable t)
                              {
                                 m_logger.error("Unexpected error in " + m_work, t);
                              }

                              synchronized (Worker.this)
                              {
                                 m_work = null;
                              }

                              release();

                              Logger.resetContext(nCookie);
                           }
                        }
                     };
                  }
               });
         }
         catch (PrivilegedActionException e)
         {
            throw e.getException();
         }

         m_thread.start();
      }

      // operations

      /**
       * @return The default thread name.
       */
      protected String getDefaultName()
      {
         return SysUtil.CAPTION + " Idle Thread #" + hashCode();
      }

      /**
       * Runs a work item on a dedicated thread from the pool.
       * After it completes, the thread is returned to the pool automatically.
       * @param work The work item to run.
       */
      public synchronized void run(Runnable work)
      {
         if (m_work != null)
         {
            throw new IllegalStateException();
         }

         m_work = work;
         m_bLogEnabled = Logger.isEnabled();
         notifyAll();
      }

      /**
       * @see nexj.core.util.pool.resource.Resource#reset()
       */
      public synchronized void reset()
      {
         if (m_thread != null)
         {
            m_thread.setName(getDefaultName());
         }
      }

      /**
       * @see nexj.core.util.pool.resource.GenericResource#drop()
       */
      protected synchronized void drop() throws Throwable
      {
         m_thread = null;
         notifyAll();
      }

      /**
       * @see nexj.core.util.pool.resource.GenericResource#getLogger()
       */
      protected Logger getLogger()
      {
         return m_logger;
      }

      /**
       * @see java.lang.Object#toString()
       */
      public synchronized String toString()
      {
         return "Worker(thread=" + m_thread + ", work=" + m_work + ')';
      }
   }
}
