// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

/**
 * Proxy class that converts the progress value to a specified range.
 */
public class ProgressProxy implements ProgressListener
{
   // attributes
   
   /**
    * Range start.
    */
   private double m_dStart;
   
   /**
    * Range end.
    */
   private double m_dEnd;
   
   // associations
   
   /**
    * The progress listener.
    */
   private ProgressListener m_listener;

   // constructors
   
   public ProgressProxy(ProgressListener listener)
   {
      m_dStart = 0.;
      m_dEnd = 1.;
      m_listener = listener;
   }

   // operations

   /**
    * Sets the progress listener.
    * @param listener The progress listener to set.
    */
   public void setListener(ProgressListener listener)
   {
      m_listener = listener;
   }

   /**
    * @return The progress listener.
    */
   public ProgressListener getListener()
   {
      return m_listener;
   }
   
   /**
    * Sets the progress range.
    * @param dStart Range start [0..1].
    * @param dEnd Range end [0..1].
    */
   public void setRange(double dStart, double dEnd)
   {
      assert dStart >= 0 && dStart <= 1;
      assert dEnd >= 0 && dEnd <= 1;
      assert dEnd >= dStart;
      
      m_dStart = dStart;
      m_dEnd = dEnd;
   }
   
   /**
    * Shifts the progress range. The old range end becomes the new range start.
    * @param dEnd The new range end.
    */
   public void shiftRange(double dEnd)
   {
      assert dEnd >= 0 && dEnd <= 1;
      assert dEnd >= m_dEnd;
      
      m_dStart = m_dEnd;
      m_dEnd = dEnd;
   }
   
   /**
    * @see nexj.core.util.ProgressListener#progress(java.lang.String, java.lang.Object[], double)
    */
   public void progress(String sMessageId, Object[] args, double dProgress)
   {
      if (m_listener != null)
      {
         m_listener.progress(sMessageId, args, m_dStart + (m_dEnd - m_dStart) * dProgress);
      }
   }
}
