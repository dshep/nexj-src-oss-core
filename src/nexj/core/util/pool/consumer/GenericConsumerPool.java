package nexj.core.util.pool.consumer;

import java.util.ArrayList;
import java.util.List;

import nexj.core.util.Logger;
import nexj.core.util.Named;
import nexj.core.util.ObjUtil;

/**
 * Generic request consumer pool implementation.
 */
public abstract class GenericConsumerPool implements ConsumerPool, Runnable
{
   // attributes

   /**
    * True to connect.
    */
   protected boolean m_bConnect;

   /**
    * True to stop the pool.
    */
   protected boolean m_bStop;

   /**
    * True if the pool is running.
    */
   protected boolean m_bRunning;

   /**
    * True if the maximum pool size has been reached.
    */
   protected boolean m_bMaxReached;

   /**
    * The total consumer count.
    */
   protected int m_nConsumerCount;

   // associations

   /**
    * The idle consumer list (used as a stack).
    */
   protected List m_idleConsumerList = new ArrayList();

   /**
    * The consumer configuration. Immutable.
    */
   protected ConsumerConfig m_config;

   /**
    * The run-time environment adapter.
    */
   protected ConsumerAdapter m_adapter;

   /**
    * The class logger.
    */
   protected Logger m_logger;

   /**
    * The system log enabler.
    */
   protected Logger m_enabler;

   // constructors

   /**
    * Constructs the pool.
    * @param config The consumer configuration.
    * @param adapter The run-time environment adapter.
    * @param logger The class logger. Can be null to use a default logger.
    * @param logger The logger enabler. Non-null for stealth logging.
    */
   protected GenericConsumerPool(ConsumerConfig config,
      ConsumerAdapter adapter, Logger logger, Logger enabler)
   {
      m_config = config;
      m_adapter = adapter;

      if (logger == null)
      {
         logger = Logger.getLogger(getClass().getName() +
            ((config instanceof Named) ? '.' + ((Named)config).getName() : ""));
      }

      m_logger = logger;
      m_enabler = enabler;
   }

   /**
    * Constructs the pool with a default logger.
    * @param config The consumer configuration.
    * @param adapter The run-time environment adapter.
    */
   protected GenericConsumerPool(ConsumerConfig config, ConsumerAdapter adapter)
   {
      this(config, adapter, null, null);
   }

   // operations

   /**
    * @see nexj.core.util.pool.consumer.ConsumerPool#getAdapter()
    */
   public ConsumerAdapter getAdapter()
   {
      return m_adapter;
   }

   /**
    * @return The pool logger.
    */
   public Logger getLogger()
   {
      return m_logger;
   }

   /**
    * @return The system logger enabler, or null if not in stealth mode.
    */
   public Logger getEnabler()
   {
      return m_enabler;
   }

   /**
    * @return True if the message delivery is transactional.
    */
   public abstract boolean isTransactional();

   /**
    * @return The maximum pool size.
    */
   protected int getMaxSize()
   {
      return getConfig().getMaxPoolSize();
   }

   /**
    * @return The timeout in milliseconds, after which idle resources are disposed of.
    * -1 means unlimited.
    */
   protected long getIdleTimeout()
   {
      return 60000;
   }

   /**
    * @return True to run a synchronized listener loop.
    */
   protected boolean isSynchronized()
   {
      return true;
   }

   /**
    * @see nexj.core.util.pool.consumer.ConsumerPool#setConfig(nexj.core.util.pool.consumer.ConsumerConfig)
    */
   public void setConfig(ConsumerConfig config) throws ConsumerException
   {
      synchronized (this)
      {
         if (!m_bStop & (m_bRunning | m_bConnect | m_bMaxReached) && m_config != null &&
            ((m_config.getMaxPoolSize() != 0) != (config.getMaxPoolSize() != 0) ||
               !config.isCompatible(m_config)))
         {
            stop(true);
            m_config = config;
            start();

            return;
         }

         m_config = config;
         notifyAll();
      }

      synchronized (m_idleConsumerList)
      {
         m_idleConsumerList.notifyAll();
      }
   }

   /**
    * @see nexj.core.util.pool.consumer.ConsumerPool#getConfig()
    */
   public synchronized ConsumerConfig getConfig()
   {
      return m_config;
   }

   /**
    * @see nexj.core.util.pool.consumer.ConsumerPool#start()
    */
   public void start() throws ConsumerException
   {
      synchronized (this)
      {
         if (m_bRunning || m_bConnect)
         {
            return;
         }

         m_bRunning = false;
         m_bStop = false;
         m_bConnect = (m_config.getMaxPoolSize() != 0);
         m_bMaxReached = !m_bConnect;

         if (m_bConnect)
         {
            try
            {
               if (isMonitored())
               {
                  monitorStatus(false);
               }

               m_adapter.run(this);
            }
            catch (Throwable t)
            {
               m_bConnect = false;

               throw new RunException(t);
            }
         }
      }
   }

   /**
    * @see nexj.core.util.pool.consumer.ConsumerPool#stop(boolean)
    */
   public void stop(boolean bWait)
   {
      synchronized (this)
      {
         if (!m_bRunning && !m_bConnect)
         {
            return;
         }

         m_bStop = true;
         interrupt();
         notifyAll();

         if (bWait)
         {
            while (m_bRunning || m_bConnect)
            {
               try
               {
                  wait();
               }
               catch (InterruptedException e)
               {
               }
            }
         }
      }
   }

   /**
    * @see java.lang.Runnable#run()
    */
   public void run()
   {
      long lRetryDelay = getConfig().getRetryDelay();
      boolean bLogEnabledSaved = Logger.isEnabled();

      if (m_enabler != null)
      {
         m_enabler.enable();
      }

      if (m_logger.isDebugEnabled())
      {
         m_logger.debug("Started " + this);
      }

      for (;;)
      {
         boolean bStop;

         if (getMaxSize() != 0)
         {
            if (isSynchronized())
            {
               synchronized (this)
               {
                  m_bRunning = true;

                  while (!(m_bStop | m_bConnect))
                  {
                     if (m_enabler != null)
                     {
                        m_enabler.enable();
                     }

                     listen();
                  }

                  m_bConnect = false;
                  bStop = m_bStop;
               }
            }
            else
            {
               for (;;)
               {
                  synchronized (this)
                  {
                     m_bRunning = true;

                     if (m_bStop | m_bConnect)
                     {
                        m_bConnect = false;
                        bStop = m_bStop;

                        break;
                     }
                  }

                  if (m_enabler != null)
                  {
                     m_enabler.enable();
                  }

                  listen();
               }
            }
         }
         else
         {
            synchronized (this)
            {
               m_bRunning = true;

               while (!m_bStop && getMaxSize() == 0)
               {
                  try
                  {
                     wait();
                  }
                  catch (InterruptedException e)
                  {
                  }
               }

               m_bConnect = false;
               bStop = true;
            }
         }

         disconnect();

         if (bStop)
         {
            synchronized (this)
            {
               m_bRunning = false;
               notifyAll();
            }

            if (isMonitored())
            {
               monitorStatus(false);
            }

            if (m_logger.isDebugEnabled())
            {
               m_logger.debug("Stopped " + this);
            }

            Logger.setEnabled(bLogEnabledSaved);

            return;
         }

         try
         {
            connect();
            lRetryDelay = getConfig().getRetryDelay();

            if (isMonitored())
            {
               monitorStatus(true);
            }

            continue;
         }
         catch (Throwable e)
         {
            m_logger.error("Error in " + this, e);

            if (isMonitored())
            {
               monitorStatus(false);
            }

            synchronized (this)
            {
               m_bConnect = true;
            }
         }

         disconnect();

         if (lRetryDelay > 0)
         {
            synchronized (this)
            {
               if (!m_bStop)
               {
                  try
                  {
                     wait(lRetryDelay);
                  }
                  catch (InterruptedException e)
                  {
                  }
               }

               lRetryDelay = Math.min(lRetryDelay << 1, getConfig().getMaxRetryDelay());
            }
         }
      }
   }

   /**
    * Establishes the consumer connection.
    */
   protected abstract void connect() throws Throwable;

   /**
    * Asynchronously reestablishes the consumer connection.
    */
   protected void reconnect()
   {
      synchronized (this)
      {
         m_bConnect = true;
         interrupt();
      }

      synchronized (m_idleConsumerList)
      {
         m_idleConsumerList.notifyAll();
      }
   }

   /**
    * Disconnects from the destination.
    */
   protected void disconnect()
   {
      try
      {
         synchronized (this)
         {
            suspend();
            notifyAll();
         }

         synchronized (m_idleConsumerList)
         {
            m_idleConsumerList.notifyAll();
         }

         for (;;)
         {
            GenericConsumer consumer;

            synchronized (m_idleConsumerList)
            {
               while (m_nConsumerCount != 0 && m_idleConsumerList.isEmpty())
               {
                  try
                  {
                     m_idleConsumerList.wait();
                  }
                  catch (InterruptedException e)
                  {
                  }
               }

               if (m_idleConsumerList.isEmpty())
               {
                  break;
               }

               consumer = (GenericConsumer)m_idleConsumerList.remove(m_idleConsumerList.size() - 1);
               m_idleConsumerList.notifyAll();
            }

            consumer.dispose();
         }

         synchronized (this)
         {
            close();
            notifyAll();
         }
      }
      catch (Throwable e)
      {
         m_logger.error("Unable to disconnect " + this, e);
      }
   }

   /**
    * Suspends the consumer connection.
    */
   protected abstract void suspend();

   /**
    * Releases the resources associated with the consumer connection.
    */
   protected abstract void close();

   /**
    * Blocks waiting for incoming requests.
    */
   protected void listen()
   {
      try
      {
         wait();
      }
      catch (InterruptedException e)
      {
      }
   }

   /**
    * Interrupts the listener loop.
    */
   protected void interrupt()
   {
      notifyAll();
   }

   /**
    * @return A consumer from the pool, creating new ones if needed
    * and blocking when the maximum pool size has been reached.
    */
   protected GenericConsumer getConsumer() throws Throwable
   {
      boolean bMonitored = isMonitored();
      long lStartTime = (bMonitored) ? System.currentTimeMillis() : 0;
      int nMaxPoolSize = getMaxSize();
      GenericConsumer consumer = null;
      int nActiveCount = 0;
      int nIdleCount = 0;

      for (;;)
      {
         synchronized (m_idleConsumerList)
         {
            for (;;)
            {
               if (!m_idleConsumerList.isEmpty())
               {
                  m_idleConsumerList.notifyAll();
                  consumer = (GenericConsumer)m_idleConsumerList.remove(nIdleCount);

                  break;
               }

               if (nMaxPoolSize >= 0 && m_nConsumerCount >= nMaxPoolSize)
               {
                  if (!m_bMaxReached)
                  {
                     m_bMaxReached = true;

                     if (m_logger.isDebugEnabled())
                     {
                        m_logger.debug("Maximum size of " + nMaxPoolSize + " reached by " + this);
                     }
                  }

                  try
                  {
                     m_idleConsumerList.wait();
                  }
                  catch (InterruptedException e)
                  {
                  }
               }
               else
               {
                  ++m_nConsumerCount;
                  m_idleConsumerList.notifyAll();

                  break;
               }
            }

            nIdleCount = m_idleConsumerList.size();
            nActiveCount = m_nConsumerCount - nIdleCount;
         }

         if (consumer == null)
         {
            break;
         }

         synchronized (this)
         {
            if (consumer.getConfig() == m_config)
            {
               break;
            }

            consumer.dispose();
            consumer = null;

            synchronized (m_idleConsumerList)
            {
               --m_nConsumerCount;
            }
         }
      }

      try
      {
         if (consumer == null)
         {
            consumer = createConsumer();
         }
      }
      finally
      {
         if (consumer == null)
         {
            synchronized (m_idleConsumerList)
            {
               nIdleCount = m_idleConsumerList.size();
               nActiveCount = --m_nConsumerCount - nIdleCount;
               m_idleConsumerList.notifyAll();
            }
         }

         if (bMonitored)
         {
            long lTime = System.currentTimeMillis();

            monitorActivation(nIdleCount, nActiveCount, Math.max(0, lTime - lStartTime));
            consumer.setTime(lTime);
         }
         else
         {
            consumer.setTime(0);
         }
      }

      return consumer;
   }

   /**
    * @return A new consumer instance.
    */
   protected abstract GenericConsumer createConsumer() throws Throwable;

   /**
    * Deactivates a consumer.
    * @param consumer The consumer to deactivate.
    */
   public void deactivate(GenericConsumer consumer)
   {
      boolean bMonitored = isMonitored();
      long lTime = System.currentTimeMillis();
      long lActiveTime = -1;

      if (bMonitored)
      {
         long lStartTime = consumer.getTime();

         if (lStartTime != 0)
         {
            lActiveTime = Math.max(0, lTime - lStartTime);
         }
      }

      consumer.setTime(lTime);

      int nMaxSize = getMaxSize();
      int nActiveCount;
      int nIdleCount;
      List disposalList;

      synchronized (m_idleConsumerList)
      {
         m_idleConsumerList.add(consumer);
         disposalList = findIdle(null, nMaxSize, lTime);
         nIdleCount = m_idleConsumerList.size();
         nActiveCount = m_nConsumerCount - nIdleCount;
         m_idleConsumerList.notifyAll();
      }

      if (disposalList != null)
      {
         dispose(disposalList);

         synchronized (m_idleConsumerList)
         {
            nIdleCount = m_idleConsumerList.size();
            nActiveCount = m_nConsumerCount - nIdleCount;
         }
      }

      if (bMonitored)
      {
         monitorDeactivation(nIdleCount, nActiveCount, lActiveTime);
      }
   }

   /**
    * Removes an active consumer.
    * @param consumer The consumer to remove.
    */
   public void remove(GenericConsumer consumer)
   {
      synchronized (m_idleConsumerList)
      {
         --m_nConsumerCount;
         m_idleConsumerList.notifyAll();
      }
   }

   /**
    * Finds the excess of idle consumers.
    * The caller must synchronize on m_idleConsumerList.
    * @param idleList The list where to add them. Can be null.
    * @param nMaxSize The maximum pool size. -1 mean unlimited.
    * @param lTime The current time in milliseconds since 1-Jan-1970 UTC.
    * @return The idleList, or a newly allocated one if it was null.
    */
   protected List findIdle(List idleList, int nMaxSize, long lTime)
   {
      long lIdleTimeout = getIdleTimeout();

      if (lIdleTimeout >= 0)
      {
         lTime -= lIdleTimeout;
      }

      int i = 0;

      for (int nCount = m_idleConsumerList.size(); i < nCount; ++i)
      {
         GenericConsumer consumer = (GenericConsumer)m_idleConsumerList.get(i);

         if (!m_bStop &&
            (nMaxSize < 0 || m_nConsumerCount - i <= nMaxSize) &&
            (lIdleTimeout < 0 || consumer.getTime() > lTime))
         {
            break;
         }
      }

      if (i != 0)
      {
         if (idleList == null)
         {
            idleList = new ArrayList(i);
         }

         List removalList = m_idleConsumerList.subList(0, i);

         idleList.addAll(removalList);
         removalList.clear();
      }

      return idleList;
   }

   /**
    * Disposes of multiple consumers.
    * @param consumerList The consumer list. Can be null.
    */
   protected static void dispose(List consumerList)
   {
      if (consumerList != null)
      {
         for (int i = 0, nCount = consumerList.size(); i < nCount; ++i)
         {
            ((GenericConsumer)consumerList.get(i)).dispose();
         }
      }
   }

   /**
    * @see ConsumerPool#maintain()
    */
   public void maintain()
   {
      int nMaxSize = getMaxSize();
      long lTime = System.currentTimeMillis();
      List disposalList;

      synchronized (m_idleConsumerList)
      {
         disposalList = findIdle(null, nMaxSize, lTime);
      }

      if (disposalList != null)
      {
         dispose(disposalList);

         if (isMonitored())
         {
            int nIdleCount, nActiveCount;

            synchronized (m_idleConsumerList)
            {
               nIdleCount = m_idleConsumerList.size();
               nActiveCount = m_nConsumerCount - nIdleCount;
            }

            monitorDeactivation(nIdleCount, nActiveCount, -1);
         }
      }
   }

   /**
    * @return True if the pool maintains statistics.
    */
   protected boolean isMonitored()
   {
      return false;
   }

   /**
    * Template method to update the consumer activation statistics.
    * @param nIdleCount The idle consumer count.
    * @param nActiveCount The active consumer count.
    * @param lBusyTime The time spent waiting for the consumer to become available (ms).
    */
   protected void monitorActivation(int nIdleCount, int nActiveCount, long lBusyTime)
   {
   }

   /**
    * Template method to update the consumer deactivation statistics.
    * @param nIdleCount The idle consumer count.
    * @param nActiveCount The active consumer count.
    * @param lActiveTime The time during which the consumer has been active (ms).
    * -1 means that it is not available.
    */
   protected void monitorDeactivation(int nIdleCount, int nActiveCount, long lActiveTime)
   {
   }

   /**
    * Template method to update the pool status.
    * @param bOk False to indicate a connectivity error.
    */
   protected void monitorStatus(boolean bOk)
   {
   }

   /**
    * Handles a listener exception.
    * @param t The exception that was thrown.
    */
   protected void handleException(Throwable t)
   {
      m_logger.error("Error in " + this, t);
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuffer buf = new StringBuffer(128);

      buf.append(ObjUtil.getShortClassName(this));
      buf.append('@');
      buf.append(System.identityHashCode(this));
      buf.append('(');
      buf.append(getConfig());

      if (isTransactional())
      {
         buf.append(", tx");
      }

      buf.append(')');

      return buf.toString();
   }
}
