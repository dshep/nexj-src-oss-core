package nexj.core.monitoring.jmx;

import java.io.Serializable;

import nexj.core.monitoring.Value;
import nexj.core.monitoring.Statistic;

/**
 * Value statistic value.
 */
public class ValueStatValue implements StatValue, Serializable
{
   // constants

   /**
    * Serial version id.
    */
   private final static long serialVersionUID = -7951367547153160389L;

   // associations

   /**
    * Value.
    */
   private Value m_value;

   // constructors

   /**
    * Construct an value statistic value.
    * @param value Value.
    */
   public ValueStatValue(Value value)
   {
      m_value = value;
   }

   // operations

   /**
    * @see nexj.core.monitoring.jmx.StatValue#getType()
    */
   public String getType()
   {
      return "java.lang.Number";
   }

   /**
    * @see nexj.core.monitoring.jmx.StatValue#get()
    */
   public Object get()
   {
      return m_value.get();
   }

   /**
    * @see nexj.core.monitoring.jmx.StatValue#set(java.lang.Object)
    */
   public void set(Object value)
   {
      m_value.set((Number)value);
   }

   /**
    * @see nexj.core.monitoring.jmx.StatValue#getStatistic()
    */
   public Statistic getStatistic()
   {
      return m_value;
   }

   /**
    * @see java.lang.Object#clone()
    */
   public Object clone()
   {
      return new ValueStatValue((Value)m_value.clone());
   }
}
