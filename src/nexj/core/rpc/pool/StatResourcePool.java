package nexj.core.rpc.pool;

import nexj.core.monitoring.Average;
import nexj.core.monitoring.jmx.StatManager;
import nexj.core.monitoring.jmx.StatManagerAware;
import nexj.core.util.pool.resource.TransactionalResourcePool;

/**
 * Resource pool with statistics.
 */
public abstract class StatResourcePool extends TransactionalResourcePool implements StatManagerAware
{
   // associations

   /**
    * Average idle resource count.
    */
   protected Average m_idleCount; 

   /**
    * Average active resource count.
    */
   protected Average m_activeCount;

   /**
    * Average resource activation success count (%).
    */
   protected Average m_availability;

   /**
    * Average pool busy wait time (ms). 
    */
   protected Average m_waitTime;

   /**
    * Average resource active time (ms).
    */
   protected Average m_activeTime;

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
    * @return The resource name.
    */
   protected abstract String getResourceName();

   /**
    * @see nexj.core.monitoring.jmx.StatManagerAware#setStatManager(nexj.core.monitoring.jmx.StatManager)
    */
   public synchronized void setStatManager(StatManager statManager)
   {
      String sPath = getStatPath();

      if (sPath != null)
      {
         String sClass = getStatClassName();
         String sName = getResourceName();

         m_idleCount = (Average)statManager.defineStatistic(sPath, "Average Idle " + sName + " Count",
            "mean", true, sClass).getStatistic();

         m_activeCount = (Average)statManager.defineStatistic(sPath, "Average Active " + sName + " Count",
            "mean", true, sClass).getStatistic();

         m_availability = (Average)statManager.defineStatistic(sPath, sName + " Availability (%)",
            "mean", true, sClass).getStatistic();

         m_waitTime = (Average)statManager.defineStatistic(sPath, "Average " + sName + " Pool Busy Wait Time (ms)",
            "mean", true, sClass).getStatistic();

         m_activeTime = (Average)statManager.defineStatistic(sPath, "Average " + sName + " Active Time (ms)",
            "mean", true, sClass).getStatistic();
      }
   }

   /**
    * @see nexj.core.util.pool.resource.GenericResourcePool#isMonitored()
    */
   protected boolean isMonitored()
   {
      return m_idleCount != null;
   }

   /**
    * @see nexj.core.util.pool.resource.GenericResourcePool#monitorActivation(int, int, long, boolean)
    */
   protected synchronized void monitorActivation(int nIdleCount, int nActiveCount, long lBusyTime, boolean bSuccess)
   {
      if (isMonitored())
      {
         m_idleCount.add(nIdleCount);
         m_activeCount.add(nActiveCount);
         m_waitTime.add(lBusyTime);
         m_availability.add((bSuccess) ? 100 : 0);
      }
   }

   /**
    * @see nexj.core.util.pool.resource.GenericResourcePool#monitorDeactivation(int, int, long)
    */
   protected synchronized void monitorDeactivation(int nIdleCount, int nActiveCount, long lActiveTime)
   {
      if (isMonitored())
      {
         m_idleCount.add(nIdleCount);
         m_activeCount.add(nActiveCount);
         m_activeTime.add(lActiveTime);
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
    * @return The average idle resource count.
    */
   public synchronized double getIdleCount()
   {
      return (m_idleCount == null) ? 0 : m_idleCount.getMean();
   }

   /**
    * @return The average active resource count.
    */
   public synchronized double getActiveCount()
   {
      return (m_activeCount == null) ? 0 : m_activeCount.getMean();
   }

   /**
    * @return The average resource activation success count (%)
    */
   public synchronized double getAvailability()
   {
      return (m_availability == null) ? 0 : m_availability.getMean();
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
