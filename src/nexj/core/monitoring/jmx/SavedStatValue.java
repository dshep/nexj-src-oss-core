package nexj.core.monitoring.jmx;

import java.io.Serializable;

import nexj.core.monitoring.Statistic;

/**
 * Saved statistic value. It's a statistic value wrapper that allows saving statistic's value.
 */
public class SavedStatValue implements StatValue, Serializable
{
   // constants

   /**
    * Serial version id.
    */
   private final static long serialVersionUID = -1796847753337088718L;

   // associations

   /**
    * Saved value.
    */
   private Object m_savedValue;

   /**
    * Statistic value.
    */
   private StatValue m_statValue;

   // constructors

   /**
    * Construct a saved statistic value.
    * @param statValue Statistic value.
    */
   public SavedStatValue(StatValue statValue)
   {
      m_statValue = statValue;
      save();
   }

   // operations

   /**
    * @see nexj.core.monitoring.jmx.StatValue#getType()
    */
   public String getType()
   {
      return m_statValue.getType();
   }

   /**
    * @see nexj.core.monitoring.jmx.StatValue#get()
    */
   public Object get()
   {
      return m_savedValue;
   }

   /**
    * @see nexj.core.monitoring.jmx.StatValue#set(java.lang.Object)
    */
   public void set(Object value)
   {
      throw new UnsupportedOperationException();
   }

   /**
    * @see nexj.core.monitoring.jmx.StatValue#getStatistic()
    */
   public Statistic getStatistic()
   {
      return m_statValue.getStatistic();
   }

   /**
    * Save value.
    */
   public void save()
   {
      m_savedValue = m_statValue.get();
   }

   /**
    * Reset value.
    */
   public void reset()
   {
      m_statValue.getStatistic().reset();
   }

   /**
    * @see java.lang.Object#clone()
    */
   public Object clone()
   {
      SavedStatValue savedStatValue;

      try
      {
         savedStatValue = (SavedStatValue)super.clone();
      }
      catch (CloneNotSupportedException e)
      {
         savedStatValue = null;
      }

      savedStatValue.m_statValue = (StatValue)m_statValue.clone();

      return savedStatValue;
   }
}
