package nexj.core.rpc.pool;

import nexj.core.monitoring.Average;
import nexj.core.monitoring.Flag;
import nexj.core.monitoring.jmx.StatManager;
import nexj.core.monitoring.jmx.StatManagerAware;
import nexj.core.util.Logger;
import nexj.core.util.pool.consumer.ConsumerAdapter;
import nexj.core.util.pool.consumer.ConsumerConfig;
import nexj.core.util.pool.consumer.GenericConsumerPool;

/**
 * Consumer pool with statistics.
 */
public abstract class StatConsumerPool extends GenericConsumerPool implements StatManagerAware
{
   // associations

   /**
    * Average idle consumer count.
    */
   protected Average m_idleCount; 

   /**
    * Average active consumer count.
    */
   protected Average m_activeCount;

   /**
    * Average pool busy wait time (ms). 
    */
   protected Average m_waitTime;

   /**
    * Average consumer active time (ms).
    */
   protected Average m_activeTime;

   /**
    * True if the pool is available.
    */
   protected Flag m_available;

   // constructors

   /**
    * Constructs the consumer pool.
    * @see GenericConsumerPool
    */
   public StatConsumerPool(ConsumerConfig config, ConsumerAdapter adapter, Logger logger, Logger enabler)
   {
      super(config, adapter, logger, enabler);
   }

   /**
    * Constructs the consumer pool.
    * @see GenericConsumerPool
    */
   public StatConsumerPool(ConsumerConfig config, ConsumerAdapter adapter)
   {
      super(config, adapter);
   }

   // operations

   /**
    * @return The statistics path. Can be null to disable the statistics.
    */
   protected abstract String getStatPath();

   /**
    * @return The statistics persistent class name.
    */
   protected abstract String getStatClassName();

   /**
    * @return The consumer name.
    */
   protected abstract String getConsumerName();

   /**
    * @see nexj.core.monitoring.jmx.StatManagerAware#setStatManager(nexj.core.monitoring.jmx.StatManager)
    */
   public synchronized void setStatManager(StatManager statManager)
   {
      String sPath = getStatPath();

      if (sPath != null)
      {
         String sClass = getStatClassName();
         String sName = getConsumerName();

         m_idleCount = (Average)statManager.defineStatistic(sPath, "Average Idle " + sName + " Count",
            "mean", true, sClass).getStatistic();

         m_activeCount = (Average)statManager.defineStatistic(sPath, "Average Active " + sName + " Count",
            "mean", true, sClass).getStatistic();

         m_waitTime = (Average)statManager.defineStatistic(sPath, "Average " + sName + " Pool Busy Wait Time (ms)",
            "mean", true, sClass).getStatistic();

         m_activeTime = (Average)statManager.defineStatistic(sPath, "Average " + sName + " Active Time (ms)",
            "mean", true, sClass).getStatistic();

         m_available = (Flag)statManager.defineStatistic(sPath, sName + " Pool Connection Available",
            "flag", true, sClass).getStatistic();
      }
   }

   /**
    * @see nexj.core.util.pool.consumer.GenericConsumerPool#isMonitored()
    */
   protected boolean isMonitored()
   {
      return m_idleCount != null;
   }

   /**
    * @see nexj.core.util.pool.consumer.GenericConsumerPool#monitorActivation(int, int, long)
    */
   protected synchronized void monitorActivation(int nIdleCount, int nActiveCount, long lBusyTime)
   {
      if (isMonitored())
      {
         synchronized (this)
         {
            m_idleCount.add(nIdleCount);
            m_activeCount.add(nActiveCount);
            m_waitTime.add(lBusyTime);
         }
      }
   }

   /**
    * @see nexj.core.util.pool.consumer.GenericConsumerPool#monitorDeactivation(int, int, long)
    */
   protected synchronized void monitorDeactivation(int nIdleCount, int nActiveCount, long lActiveTime)
   {
      if (isMonitored())
      {
         m_idleCount.add(nIdleCount);
         m_activeCount.add(nActiveCount);

         if (lActiveTime >= 0)
         {
            m_activeTime.add(lActiveTime);
         }
      }
   }

   /**
    * @see nexj.core.util.pool.consumer.GenericConsumerPool#monitorStatus(boolean)
    */
   protected synchronized void monitorStatus(boolean bOk)
   {
      if (isMonitored())
      {
         m_available.set(bOk);
      }
   }

   /**
    * @return The total pool use count.
    */
   public synchronized long getUseCount()
   {
      return (m_waitTime == null) ? 0 : m_waitTime.getCount();
   }

   /**
    * @return The average idle consumer count.
    */
   public synchronized double getIdleCount()
   {
      return (m_idleCount == null) ? 0 : m_idleCount.getMean();
   }

   /**
    * @return The average active consumer count.
    */
   public synchronized double getActiveCount()
   {
      return (m_activeCount == null) ? 0 : m_activeCount.getMean();
   }

   /**
    * @return True if the pool is available, i.e. accepting incoming requests.
    */
   public synchronized boolean isAvailable()
   {
      return (m_available == null) ? false : m_available.get();
   }

   /**
    * @return The average resource pool busy time (ms)
    */
   public synchronized double getWaitTime()
   {
      return (m_waitTime == null) ? 0 : m_waitTime.getMean();
   }

   /**
    * @return The average resource active time (ms)
    */
   public synchronized double getActiveTime()
   {
      return (m_activeTime == null) ? 0 : m_activeTime.getMean();
   }
}
