package nexj.core.monitoring;

import java.io.Serializable;

/**
 * Flag, i.e. true or false.
 */
public class Flag implements Statistic, Serializable, Cloneable
{
   // constants

   /**
    * Serial version id.
    */
   private final static long serialVersionUID = -7389688188188306716L;

   // attributes

   /**
    * Flag.
    */
   private boolean m_bFlag;

   // constructors

   /**
    * Construct a flag (false).
    */
   public Flag()
   {
   }

   /**
    * Construct a flag.
    * @param bFlag Flag.
    */
   public Flag(boolean bFlag)
   {
      m_bFlag = bFlag;
   }

   // operations

   /**
    * Set flag.
    * @param bFlag Flag to set.
    */
   public synchronized void set(boolean bFlag)
   {
      m_bFlag = bFlag;
   }

   /**
    * Get flag.
    * @return Flag.
    */
   public synchronized boolean get()
   {
      return m_bFlag;
   }

   /**
    * @see nexj.core.monitoring.Statistic#accumulate(nexj.core.monitoring.Statistic)
    */
   public void accumulate(Statistic src)
   {
      m_bFlag = ((Flag)src).get();
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
      return "Flag(" + m_bFlag + ")";
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
