// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.monitoring;

/**
 * Item counter.
 */
public class ThreadLocalCounter implements Statistic
{
   // attributes

   /**
    * The delegate counter
    */
   protected ThreadLocal m_counter =  new ThreadLocal()
   {
      protected synchronized Object initialValue()
      {
         return new Counter();
      }
   };

   // operations

   /**
    * Add to the counter.
    * @param lDelta The amount to add.
    * @return The new value of the counter.
    */
   public long add(long lDelta)
   {
      return ((Counter)m_counter.get()).add(lDelta);
   }

   /**
    * @return The current count.
    */
   public long get()
   {
      return ((Counter)m_counter.get()).get();
   }

   /**
    * @see nexj.core.monitoring.Statistic#accumulate(nexj.core.monitoring.Statistic)
    */
   public void accumulate(Statistic src)
   {
      ((Counter)m_counter.get()).accumulate(src);
   }

   /**
    * @see nexj.core.monitoring.Statistic#reset()
    */
   public void reset()
   {
      ((Counter)m_counter.get()).reset();
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "ThreadLocalCounter(count=" + get() + ")";
   }
}
