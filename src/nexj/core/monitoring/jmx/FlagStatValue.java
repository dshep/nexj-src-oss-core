package nexj.core.monitoring.jmx;

import java.io.Serializable;

import nexj.core.monitoring.Flag;
import nexj.core.monitoring.Statistic;

/**
 * Flag statistic value.
 */
public class FlagStatValue implements StatValue, Serializable
{
   // constants

   /**
    * Serial version id.
    */
   private final static long serialVersionUID = 7447569016815156946L;

   // associations

   /**
    * Flag.
    */
   private Flag m_flag;

   // constructors

   /**
    * Construct an flag statistic value.
    * @param flag Flag.
    */
   public FlagStatValue(Flag flag)
   {
      m_flag = flag;
   }

   // operations

   /**
    * @see nexj.core.monitoring.jmx.StatValue#getType()
    */
   public String getType()
   {
      return "java.lang.Boolean";
   }

   /**
    * @see nexj.core.monitoring.jmx.StatValue#get()
    */
   public Object get()
   {
      return Boolean.valueOf(m_flag.get());
   }

   /**
    * @see nexj.core.monitoring.jmx.StatValue#set(java.lang.Object)
    */
   public void set(Object value)
   {
      m_flag.set(((Boolean)value).booleanValue());
   }

   /**
    * @see nexj.core.monitoring.jmx.StatValue#getStatistic()
    */
   public Statistic getStatistic()
   {
      return m_flag;
   }

   /**
    * @see java.lang.Object#clone()
    */
   public Object clone()
   {
      return new FlagStatValue((Flag)m_flag.clone());
   }
}
