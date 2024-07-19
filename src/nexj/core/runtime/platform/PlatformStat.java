// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime.platform;

/**
 * Runtime statistics provider.
 */
public abstract class PlatformStat
{
   // operations

   /**
    * @return The system CPU utilization (0..100).
    */
   protected abstract int getSystemCPUUtilization();

   /**
    * @return The process CPU utilization (0..100).
    */
   protected abstract int getProcessCPUUtilization();

   /**
    * @return The maximum of process and system CPU utilization (0..100).
    */
   public synchronized int getCPUUtilization()
   {
      return Math.max(getProcessCPUUtilization(), getSystemCPUUtilization());
   }

   /**
    * @return The system memory utilization (0..100).
    */
   protected abstract int getSystemMemoryUtilization();

   /**
    * @return The process memory utilization (0..100), or -1 if not available.
    */
   protected abstract int getProcessMemoryUtilization();

   /**
    * @return The process (or the system, depending on info availability) memory utilization (0..100).
    */
   public synchronized int getMemoryUtilization()
   {
      int nMem = getProcessMemoryUtilization();

      if (nMem >= 0)
      {
         return nMem;
      }

      return getSystemMemoryUtilization();
   }

   /**
    * Get garbage collection count.
    * @return Garbage collection count.
    */
   public abstract long getGarbageCollectionCount();

   /**
    * Get garbage collection time in milliseconds.
    * @return Garbage collection time.
    */
   public abstract long getGarbageCollectionTime();

   /**
    * Get system uptime in milliseconds.
    * @return System uptime.
    */
   public abstract long getUptime();

   /**
    * Get live thread count.
    * @return Live thread count.
    */
   public abstract int getLiveThreadCount();

   /**
    * Get total started thread count.
    * @return Total started thread count.
    */
   public abstract long getTotalStartedThreadCount();
}
