package nexj.core.rpc.timer;

import nexj.core.meta.Component;

/**
 * Timer timeout.
 */
public class Timeout implements Comparable
{
   // attributes

   /**
    * The time in msec since 1-Jan-1970 00:00:00 UTC.
    */
   protected long m_lTime;

   /**
    * The period in msec.
    */
   protected long m_lPeriod;

   /**
    * The cancelation request flag.
    */
   protected boolean m_bCancelationRequested;

   /**
    * True if the timeout has been canceled.
    */
   protected boolean m_bCanceled;

   // associations

   /**
    * The timeout receiver component.
    * Null to use the channel receiver.
    */
   protected Component m_receiver;

   // constructors

   /**
    * Constructs the timeout.
    * @param timeout The timeout metadata.
    */
   public Timeout(nexj.core.meta.integration.channel.timer.Timeout timeout)
   {
      m_receiver = timeout.getReceiver();
      m_lTime = System.currentTimeMillis();
      m_lPeriod = timeout.getPeriod();
      add(timeout.getDelay());
   }

   /**
    * Constructs an uninitialized timeout.
    */
   public Timeout()
   {
   }

   // operations

   /**
    * @return The timeout receiver component.
    */
   public Component getReceiver()
   {
      return m_receiver;
   }

   /**
    * Sets the time in msec since 1-Jan-1970 00:00:00 UTC.
    * @param lTime The time to set.
    */
   public void setTime(long lTime)
   {
      m_lTime = lTime;
   }

   /**
    * @return The time in msec since 1-Jan-1970 00:00:00 UTC.
    */
   public long getTime()
   {
      return m_lTime;
   }

   /**
    * Sets the period in msec.
    * @param lPeriod The period in msec to set.
    */
   public void setPeriod(long lPeriod)
   {
      m_lPeriod = lPeriod;
   }

   /**
    * @return The period in msec.
    */
   public long getPeriod()
   {
      return m_lPeriod;
   }

   /**
    * Adds the specified delay to the timeout, taking care of overflows.
    * @param lDelay The delay in msec.
    */
   public void add(long lDelay)
   {
      if (lDelay > 0)
      {
         if (m_lTime > Long.MAX_VALUE - lDelay)
         {
            m_lTime = Long.MAX_VALUE;
         }
         else
         {
            m_lTime += lDelay;
         }
      }
      else
      {
         if (m_lTime < Long.MIN_VALUE - lDelay)
         {
            m_lTime = Long.MIN_VALUE;
         }
         else
         {
            m_lTime -= lDelay;
         }
      }
   }

   /**
    * Sets the cancelation request flag.
    * @param bCancelationRequested The cancelation request flag to set.
    */
   public void setCancelationRequested(boolean bCancelationRequested)
   {
      m_bCancelationRequested = bCancelationRequested;
   }

   /**
    * @return The cancelation request flag.
    */
   public boolean isCancelationRequested()
   {
      return m_bCancelationRequested;
   }
   
   /**
    * Sets the cancelation flag.
    * @param bCanceled True if the timeout has been canceled.
    */
   public void setCanceled(boolean bCanceled)
   {
      m_bCanceled = bCanceled;
   }

   /**
    * @return True if the timeout has been canceled.
    */
   public boolean isCanceled()
   {
      return m_bCanceled;
   }

   /**
    * @see java.lang.Comparable#compareTo(java.lang.Object)
    */
   public int compareTo(Object obj)
   {
      Timeout tm = (Timeout)obj;

      if (m_lTime < tm.m_lTime)
      {
         return -1;
      }

      if (m_lTime == tm.m_lTime)
      {
         return 0;
      }

      return 1;
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuffer buf = new StringBuffer(32);
      
      buf.append("Timeout(time=");
      buf.append(m_lTime);

      if (m_lPeriod != Long.MAX_VALUE)
      {
         buf.append(", period=");
         buf.append(m_lPeriod);
      }

      buf.append(')');

      return buf.toString();
   }
}
