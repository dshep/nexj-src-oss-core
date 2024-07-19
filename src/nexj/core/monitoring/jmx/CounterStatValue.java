package nexj.core.monitoring.jmx;

import java.io.Serializable;

import nexj.core.meta.Primitive;
import nexj.core.monitoring.Counter;
import nexj.core.monitoring.Statistic;

/**
 * Counter statistic value.
 */
public class CounterStatValue implements StatValue, Serializable
{
   // constants

   /**
    * Serial version id.
    */
   private final static long serialVersionUID = 4671284667473401769L;

   // associations

   /**
    * Counter.
    */
   private Counter m_counter;

   // constructors

   /**
    * Construct an counter statistic value.
    * @param counter Counter.
    */
   public CounterStatValue(Counter counter)
   {
      m_counter = counter;
   }

   // operations

   /**
    * @see nexj.core.monitoring.jmx.StatValue#getType()
    */
   public String getType()
   {
      return "java.lang.Long";
   }

   /**
    * @see nexj.core.monitoring.jmx.StatValue#get()
    */
   public Object get()
   {
      return Primitive.createLong(m_counter.get());
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
      return m_counter;
   }

   /**
    * @see java.lang.Object#clone()
    */
   public Object clone()
   {
      return new CounterStatValue((Counter)m_counter.clone());
   }
}
