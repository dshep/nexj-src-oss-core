package nexj.core.monitoring.jmx;

import java.io.Serializable;

import nexj.core.meta.Primitive;
import nexj.core.monitoring.Average;
import nexj.core.monitoring.Statistic;

/**
 * Average mean statistic value.
 */
public class AverageMeanStatValue implements StatValue, Serializable
{
   // constants

   /**
    * Serial version id.
    */
   private final static long serialVersionUID = -8926482243524201291L;

   // associations

   /**
    * Average.
    */
   private Average m_average;

   // constructors

   /**
    * Construct an average mean statistic value.
    * @param average Average.
    */
   public AverageMeanStatValue(Average average)
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
      return Primitive.createDouble(m_average.getMean());
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
      return new AverageMeanStatValue((Average)m_average.clone(true));
   }
}
