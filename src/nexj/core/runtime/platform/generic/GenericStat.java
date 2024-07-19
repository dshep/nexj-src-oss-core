// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime.platform.generic;

import java.lang.management.GarbageCollectorMXBean;
import java.lang.management.ManagementFactory;
import java.lang.management.MemoryMXBean;
import java.lang.management.MemoryUsage;
import java.lang.management.OperatingSystemMXBean;
import java.lang.management.RuntimeMXBean;
import java.lang.management.ThreadMXBean;
import java.lang.reflect.Method;
import java.util.Iterator;

import nexj.core.runtime.platform.PlatformStat;
import nexj.core.util.Logger;

/**
 * Generic runtime statistic provider.
 */
public class GenericStat extends PlatformStat
{
   // attributes

   /**
    * The previous process CPU time.
    */
   protected long m_lLastProcessCPUTime; 

   /**
    * The timestamp of the last invocation of getCPUUtilization().
    */
   protected long m_lLastCPUUtilizationTime;

   /**
    * The process CPU utilization.
    */
   protected int m_nProcessCPUUtilization = 50;

   // associations

   /**
    * The memory JMX bean.
    */
   protected MemoryMXBean m_memoryMXBean;

   /**
    * The OS JMX bean.
    */
   protected OperatingSystemMXBean m_osMXBean;

   /**
    * The runtime JMX bean.
    */
   protected RuntimeMXBean m_runtimeMXBean;

   /**
    * The thread JMX bean.
    */
   protected ThreadMXBean m_threadMXBean;

   /**
    * The OperatingSystemMXBean.getSystemLoadAverage() method (JDK 6+ only).
    */
   protected Method m_getSystemLoadAverage;

   /**
    * The OperatingSystemMXBean.getProcessCpuTime() method (Sun JDK only).
    */
   protected Method m_getProcessCPUTime;

   /**
    * The OperatingSystemMXBean.getTotalPhysicalMemorySize() method (Sun JDK only),
    * or the OperatingSystemMXBean.getTotalPhysicalMemory() method (IBM JDK only).
    */
   protected Method m_getTotalPhysicalMemory;

   /**
    * The OperatingSystemMXBean.getFreePhysicalMemorySize() method (Sun JDK only). 
    */
   protected Method m_getFreePhysicalMemory;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(GenericStat.class);

   // constructors

   /**
    * Constructs the statistics provider.
    */
   public GenericStat()
   {
      m_memoryMXBean = ManagementFactory.getMemoryMXBean();
      m_osMXBean = ManagementFactory.getOperatingSystemMXBean();
      m_runtimeMXBean = ManagementFactory.getRuntimeMXBean();
      m_threadMXBean = ManagementFactory.getThreadMXBean();

      Class osMXBeanClass = m_osMXBean.getClass();

      try
      {
         m_getSystemLoadAverage = osMXBeanClass.getDeclaredMethod("getSystemLoadAverage", null);
      }
      catch (Throwable t)
      {
      }

      try
      {
         m_getProcessCPUTime = osMXBeanClass.getDeclaredMethod("getProcessCpuTime", null);
         m_getProcessCPUTime.setAccessible(true);
      }
      catch (Throwable t)
      {
         m_getProcessCPUTime = null;
      }

      try
      {
         m_getTotalPhysicalMemory = osMXBeanClass.getDeclaredMethod("getTotalPhysicalMemorySize", null);
      }
      catch (Throwable t)
      {
      }

      if (m_getTotalPhysicalMemory == null)
      {
         try
         {
            m_getTotalPhysicalMemory = osMXBeanClass.getDeclaredMethod("getTotalPhysicalMemory", null);
         }
         catch (Throwable t)
         {
         }
      }

      if (m_getTotalPhysicalMemory != null)
      {
         try
         {
            m_getTotalPhysicalMemory.setAccessible(true);
         }
         catch (Throwable t)
         {
            m_getTotalPhysicalMemory = null;
         }
      }
      
      try
      {
         m_getFreePhysicalMemory = osMXBeanClass.getDeclaredMethod("getFreePhysicalMemorySize", null);
         m_getFreePhysicalMemory.setAccessible(true);
      }
      catch (Throwable t)
      {
         m_getFreePhysicalMemory = null;
      }
   }

   // operations

   /**
    * @see nexj.core.runtime.platform.PlatformStat#getProcessCPUUtilization()
    */
   protected int getProcessCPUUtilization()
   {
      long lTime = System.currentTimeMillis();
      int nCPU = m_nProcessCPUUtilization; 

      if (lTime - m_lLastCPUUtilizationTime >= 500)
      {
         if (m_getProcessCPUTime != null)
         {
            try
            {
               long lProcessCPUTime = ((Number)m_getProcessCPUTime.invoke(m_osMXBean, null)).longValue();

               if (m_lLastCPUUtilizationTime != 0)
               {
                  nCPU = m_nProcessCPUUtilization = Math.min(100, (int)(((lProcessCPUTime - m_lLastProcessCPUTime) /
                     (lTime - m_lLastCPUUtilizationTime) + 5000) / 10000 / m_osMXBean.getAvailableProcessors()));
               }

               m_lLastProcessCPUTime = lProcessCPUTime;
            }
            catch (Throwable t)
            {
               s_logger.error("Unexpected exception", t);
            }
         }

         m_lLastCPUUtilizationTime = lTime;
      }

      return nCPU;
   }

   /**
    * @see nexj.core.runtime.platform.PlatformStat#getSystemCPUUtilization()
    */
   protected int getSystemCPUUtilization()
   {
      if (m_getSystemLoadAverage != null)
      {
         try
         {
            return Math.max(0, (int)(100 * ((Number)m_getSystemLoadAverage.invoke(m_osMXBean, null)).doubleValue() + 0.5));
         }
         catch (Throwable t)
         {
            s_logger.error("Unexpected exception", t);
         }
      }

      return 0;
   }

   /**
    * @see nexj.core.runtime.platform.PlatformStat#getProcessMemoryUtilization()
    */
   protected int getProcessMemoryUtilization()
   {
      try
      {
         MemoryUsage memu = m_memoryMXBean.getHeapMemoryUsage();
         long lMax = memu.getMax();

         if (lMax > 0)
         {
            return (int)((memu.getUsed() * 100 + (lMax >> 1)) / lMax);
         }
      }
      catch (Throwable t)
      {
         s_logger.error("Unexpected exception", t);
      }

      return -1;
   }

   /**
    * @return The free physical memory in bytes, or -1 if not available.
    */
   protected long getFreePhysicalMemory() throws Exception
   {
      if (m_getFreePhysicalMemory != null)
      {
         return ((Number)m_getFreePhysicalMemory.invoke(m_osMXBean, null)).longValue();
      }

      return -1;
   }

   /**
    * @see nexj.core.runtime.platform.PlatformStat#getSystemMemoryUtilization()
    */
   protected int getSystemMemoryUtilization()
   {
      try
      {
         if (m_getTotalPhysicalMemory != null)
         {
            long lFree = getFreePhysicalMemory();
            long lMax = ((Number)m_getTotalPhysicalMemory.invoke(m_osMXBean, null)).longValue();

            if (lMax >= 0 && lFree >= 0)
            {
               return Math.max(0, Math.min(100, (int)(((lMax - lFree) * 100 + (lMax >> 1)) / lMax)));
            }
         }
      }
      catch (Throwable t)
      {
         s_logger.error("Unexpected exception", t);
      }

      return 50;
   }

   /**
    * @see nexj.core.runtime.platform.PlatformStat#getGarbageCollectionCount()
    */
   public long getGarbageCollectionCount()
   {
      long lGarbageCollectionCount = 0;

      for (Iterator itr = ManagementFactory.getGarbageCollectorMXBeans().iterator(); itr.hasNext();)
      {
         lGarbageCollectionCount += ((GarbageCollectorMXBean)itr.next()).getCollectionCount();
      }

      return lGarbageCollectionCount;
   }

   /**
    * @see nexj.core.runtime.platform.PlatformStat#getGarbageCollectionTime()
    */
   public long getGarbageCollectionTime()
   {
      long lGarbageCollectionTime = 0;

      for (Iterator itr = ManagementFactory.getGarbageCollectorMXBeans().iterator(); itr.hasNext();)
      {
         lGarbageCollectionTime += ((GarbageCollectorMXBean)itr.next()).getCollectionTime();
      }

      return lGarbageCollectionTime;
   }

   /**
    * @see nexj.core.runtime.platform.PlatformStat#getUptime()
    */
   public long getUptime()
   {
      return m_runtimeMXBean.getUptime();
   }

   /**
    * @see nexj.core.runtime.platform.PlatformStat#getLiveThreadCount()
    */
   public int getLiveThreadCount()
   {
      return m_threadMXBean.getThreadCount();
   }

   /**
    * @see nexj.core.runtime.platform.PlatformStat#getTotalStartedThreadCount()
    */
   public long getTotalStartedThreadCount()
   {
      return m_threadMXBean.getTotalStartedThreadCount();
   }
}
