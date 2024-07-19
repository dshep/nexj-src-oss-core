package nexj.core.monitoring;

import java.io.Serializable;

import nexj.core.meta.Primitive;

/**
 * Numeric value.
 */
public class Value implements Statistic, Serializable, Cloneable
{
   // constants

   /**
    * Serial version id.
    */
   private final static long serialVersionUID = 4940462324316292430L;

   // attributes

   /**
    * Value.
    */
   private Number m_value = Primitive.ZERO_INTEGER;

   // constructors

   /**
    * Construct a value (0).
    */
   public Value()
   {
   }

   /**
    * Construct a value.
    * @param value Value.
    */
   public Value(Number value)
   {
      m_value = value;
   }

   // operations

   /**
    * Set value.
    * @param value Value to set.
    */
   public synchronized void set(Number value)
   {
      m_value = value;
   }

   /**
    * Get value.
    * @return Value.
    */
   public synchronized Number get()
   {
      return m_value;
   }

   /**
    * @see nexj.core.monitoring.Statistic#accumulate(nexj.core.monitoring.Statistic)
    */
   public void accumulate(Statistic src)
   {
      m_value = ((Value)src).get();
   }

   /**
    * @see nexj.core.monitoring.Statistic#reset()
    */
   public void reset()
   {
      throw new UnsupportedOperationException();
   }

   /**
    * @see java.lang.Object#toString()
    */
   public synchronized String toString()
   {
      return "Value(" + m_value + ")";
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
