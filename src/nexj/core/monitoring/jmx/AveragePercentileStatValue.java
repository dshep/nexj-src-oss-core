package nexj.core.monitoring.jmx;

import java.io.Serializable;

import nexj.core.meta.Primitive;
import nexj.core.monitoring.Average;
import nexj.core.monitoring.Statistic;

/**
 * Average percentile statistic value.
 */
public class AveragePercentileStatValue implements StatValue, Serializable
{
   // constants

   /**
    * Serial version id.
    */
   private final static long serialVersionUID = 997771217285153944L;

   // associations

   /**
    * Average.
    */
   private Average m_average;

   // constructors

   /**
    * Construct an average percentile statistic value.
    * @param average Average.
    */
   public AveragePercentileStatValue(Average average)
   {
      m_average = average;
   }

   // operations

   /**
    * @see nexj.core.monitoring.jmx.StatValue#getType()
    */
   public String getType()
   {
      return "java.lang.Double";
   }

   /**
    * @see nexj.core.monitoring.jmx.StatValue#get()
    */
   public Object get()
   {
      return Primitive.createDouble(m_average.getPercentile());
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
      return m_average;
   }

   /**
    * @see java.lang.Object#clone()
    */
   public Object clone()
   {
      return new AveragePercentileStatValue((Average)m_average.clone(true));
   }
}
