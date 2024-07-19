package nexj.core.meta.integration.channel.timer;

import nexj.core.meta.Component;
import nexj.core.meta.MetadataObject;

/**
 * Timer timeout metadata.
 */
public class Timeout extends MetadataObject
{
   // attributes

   /**
    * The start delay in msec.
    */
   protected long m_lDelay;

   /**
    * The period in msec.
    * Long.MAX_VALUE means aperiodic.
    */
   protected long m_lPeriod = Long.MAX_VALUE;

   // associations

   /**
    * The timeout receiver component.
    * Null to use the channel receiver.
    */
   protected Component m_receiver;

   // constructors

   /**
    * Constructs an aperiodic timeout expiring immediately.
    */
   public Timeout()
   {
   }

   /**
    * Constructs an aperiodic timeout expiring after a certain delay.
    * @param lDelay The delay in milliseconds. 
    */
   public Timeout(long lDelay)
   {
      m_lDelay = lDelay;
   }

   /**
    * Constructs a periodic timeout expiring after a certain delay.
    * @param lDelay The delay in milliseconds. 
    * @param lPeriod The period in milliseconds. Long.MAX_VALUE means aperiodic.  
    */
   public Timeout(long lDelay, long lPeriod)
   {
      m_lDelay = lDelay;
      m_lPeriod = lPeriod;
   }

   // operations

   /**
    * Sets the start delay in msec.
    * @param lDelay The start delay in msec to set.
    */
   public void setDelay(long lDelay)
   {
      verifyNotReadOnly();
      m_lDelay = lDelay;
   }

   /**
    * @return The start delay in msec.
    */
   public long getDelay()
   {
      return m_lDelay;
   }

   /**
    * Sets the period in msec.
    * @param lPeriod The period in msec to set.
    */
   public void setPeriod(long lPeriod)
   {
      verifyNotReadOnly();
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
    * Sets the timeout receiver component.
    * @param receiver The timeout receiver component to set.
    */
   public void setReceiver(Component receiver)
   {
      verifyNotReadOnly();
      m_receiver = receiver;
   }

   /**
    * @return The timeout receiver component.
    */
   public Component getReceiver()
   {
      return m_receiver;
   }
}
