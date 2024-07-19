// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.monitoring;

import java.io.Serializable;

/**
 * Item counter.
 */
public class Counter implements Statistic, Serializable, Cloneable
{
   // constants

   /**
    * Serial version id.
    */
   private final static long serialVersionUID = -4557212392877251656L;

   // attributes

   /**
    * The count.
    */
   protected long m_lCount;

   // operations

   /**
    * Add to the counter.
    * @param lDelta The amount to add.
    * @return The new value of the counter.
    */
   public synchronized long add(long lDelta)
   {
      return m_lCount += lDelta;
   }

   /**
    * @return The current count.
    */
   public synchronized long get()
   {
      return m_lCount;
   }

   /**
    * @return The counter average change rate.
    * @param lDuration The time interval in ms.
    * @return The rate [#/s].
    */
   public synchronized double getRate(long lDuration)
   {
      if (lDuration == 0)
      {
         return 0;
      }

      return (1000.0 * m_lCount) / lDuration;
   }

   /**
    * @see nexj.core.monitoring.Statistic#accumulate(nexj.core.monitoring.Statistic)
    */
   public void accumulate(Statistic src)
   {
      long lCount = ((Counter)src).get();

      synchronized (this)
      {
         m_lCount += lCount;
      }
   }

   /**
    * @see nexj.core.monitoring.Statistic#reset()
    */
   public synchronized void reset()
   {
      m_lCount = 0;
   }

   /**
    * @see java.lang.Object#toString()
    */
   public synchronized String toString()
   {
      return "Counter(count=" + m_lCount + ")";
   }

   /**
    * @see java.lang.Object#clone()
    */
   public Object clone()
   {
      try
      {
         return super.clone();
      }
      catch (CloneNotSupportedException e)
      {
         return null;
      }
   }
}
