package nexj.core.monitoring.jmx;

import nexj.core.monitoring.Statistic;

/**
 * Statistic value.
 */
public interface StatValue extends Cloneable
{
   /**
    * Get the Java type of monitored value.
    */
   String getType();

   /**
    * Get the monitored value.
    * @return The monitored value of the type returned by getType.
    */
   Object get();

   /**
    * Set the monitored value.
    * @param value Monitored value of the type returned by getType.
    */
   void set(Object value);

   /**
    * Get statistic.
    * @return Statistic.
    */
   Statistic getStatistic();

   /**
    * @return Clone.
    */
   Object clone();
}
