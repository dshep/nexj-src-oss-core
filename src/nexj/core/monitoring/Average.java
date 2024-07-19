// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.monitoring;

import java.io.Serializable;
import java.util.Arrays;

/**
 * Observation with static distribution.
 */
public class Average implements Statistic, Cloneable, Serializable
{
   // constants

   /**
    * Serial version id.
    */
   private final static long serialVersionUID = -3208799612340664733L;

   // attributes

   /**
    * The sample count.
    */
   protected long m_lCount;

   /**
    * The accumulation count.
    */
   protected long m_lAccumulationCount;

   /**
    * The mean value.
    */
   protected double m_dMean;

   /**
    * Sum of squares of differences from the current mean.
    */
   protected double m_dMeanDiff2;

   /**
    * The computed percentile.
    */
   protected double m_dPercentile;

   /**
    * The quantile to compute.
    */
   protected double m_dQuantile;

   // associations

   /**
    * Value array for computing percentiles.
    */
   protected double[] m_dValueArray;

   // constructors

   /**
    * Constructs the sample with disabled percentile computation.
    */
   public Average()
   {
   }

   /**
    * Constructs the sample initialized for percentile computation.
    * @param dQuantile The quantile [0..1].
    * @param nCount The percentile sample count estimate.
    * Non-positive to disable percentile computation.
    */
   public Average(double dQuantile, int nCount)
   {
      if (dQuantile < 0 || dQuantile > 1)
      {
         throw new IllegalArgumentException("Quantile must be [0..1]");
      }
      
      m_dQuantile = dQuantile;

      if (nCount > 0)
      {
         m_dValueArray = new double[nCount];
      }
   }

   /**
    * Constructs the sample initialized for percentile computation.
    * @param nPercentile The percentile [0..100].
    * @param nCount The percentile sample count estimate.
    * Non-positive to disable percentile computation.
    */
   public Average(int nPercentile, int nCount)
   {
      this(nPercentile / 100.0, nCount);
   }

   // operations

   /**
    * Aggregates an observation.
    * @param dObservation The observation to add.
    */
   public synchronized void add(double dObservation)
   {
      // B. P. Welford (1962). "Note on a method for calculating corrected sums of squares and products".
      double dDiff = dObservation - m_dMean;

      m_dMean += dDiff / ++m_lCount;
      m_dMeanDiff2 += dDiff * (dObservation - m_dMean);

      if (m_dValueArray != null)
      {
         int nCount = (int)m_lCount;

         if (nCount >= m_dValueArray.length)
         {
            double[] dValueArray = new double[nCount << 1];

            System.arraycopy(m_dValueArray, 0, dValueArray, 0, nCount);
            m_dValueArray = dValueArray;
         }

         m_dValueArray[nCount - 1] = dObservation;
      }

      m_dPercentile = Double.NaN;
   }

   /**
    * @return The observation count.
    */
   public synchronized long getCount()
   {
      return m_lCount;
   }

   /**
    * @return The mean value of the observation.
    */
   public synchronized double getMean()
   {
      return m_dMean;
   }

   /**
    * @return The standard variance of the observation.
    */
   public synchronized double getStandardVariance()
   {
      if (m_lCount < 1)
      {
         return 0;
      }

      double dVariance =  m_dMeanDiff2 / m_lCount;

      return (dVariance < 0) ? 0 : dVariance;
   }

   /**
    * @return The standard deviation of the observation.
    */
   public double getStandardDeviation()
   {
      return Math.sqrt(getStandardVariance());
   }

   /**
    * @return The sample variance of the observation.
    */
   public synchronized double getSampleVariance()
   {
      if (m_lCount < 2)
      {
         return 0;
      }

      double dVariance = m_dMeanDiff2 / (m_lCount - 1);

      return (dVariance < 0) ? 0 : dVariance;
   }

   /**
    * @return The sample deviation of the observation.
    */
   public double getSampleDeviation()
   {
      return Math.sqrt(getSampleVariance());
   }

   /**
    * Computes the n-th percentile of the observation, as initialized in the constructor.
    * @return The percentile, or NaN if not supported.
    */
   public synchronized double getPercentile()
   {
      if (Double.isNaN(m_dPercentile) && m_dValueArray != null)
      {
         int nCount = (int)m_lCount;

         Arrays.sort(m_dValueArray, 0, nCount);

         double dIndex = m_dQuantile * (nCount - 1);
         int nIndex = (int)dIndex;

         if (nIndex == nCount - 1)
         {
            m_dPercentile = m_dValueArray[nIndex];
         }
         else
         {
      
            double dWeight = dIndex - nIndex;

            m_dPercentile = (1 - dWeight) * m_dValueArray[nIndex] + dWeight * m_dValueArray[nIndex + 1];
         }
      }

      return m_dPercentile;
   }

   /**
    * @return The observation average rate.
    * @param lDuration The time interval in ms.
    * @return The rate [observations/s].
    */
   public synchronized double getRate(long lDuration)
   {
      if (lDuration == 0)
      {
         return 0;
      }

      return (1000.0 * m_lCount) / lDuration;
   }

   /**
    * @see nexj.core.monitoring.Statistic#accumulate(nexj.core.monitoring.Statistic)
    */
   public void accumulate(Statistic src)
   {
      Average sample = (Average)src;

      long lCount;
      double dMean;
      double dMeanDiff2;
      double dPercentile;

      synchronized (sample)
      {
         lCount = sample.m_lCount;
         dMean = sample.m_dMean;
         dMeanDiff2 = sample.m_dMeanDiff2;
         dPercentile = sample.getPercentile();
      }

      if (lCount != 0)
      {
         synchronized (this)
         {
            ++m_lAccumulationCount;

            if (m_lCount == 0)
            {
               m_lCount = lCount;
               m_dMean = dMean;
               m_dMeanDiff2 = dMeanDiff2;
               m_dPercentile = dPercentile;
            }
            else
            {
               double dMeanDiff = dMean - m_dMean;
               long lTotalCount = lCount + m_lCount;

               m_dMean += (dMeanDiff / lTotalCount) * lCount;
               m_dMeanDiff2 += dMeanDiff2 + (dMeanDiff / lTotalCount) *
                  (dMeanDiff * m_lCount) * lCount;
               m_lCount = lTotalCount;

               // Moving average percentile update
               // Y. Bakshi, D. A. Hoeflin (2006). "Quantile Estimation: a Minimalist Approach".
               if (!Double.isNaN(dPercentile) && !Double.isNaN(getPercentile()))
               {
                  m_dValueArray = null;
                  m_dPercentile = (m_dPercentile * (m_lAccumulationCount - 1)  + dPercentile) / m_lAccumulationCount; 
               }
            }
         }
      }
   }

   /**
    * @see nexj.core.monitoring.Statistic#reset()
    */
   public synchronized void reset()
   {
      m_lCount = m_lAccumulationCount = 0;
      m_dMean = m_dMeanDiff2 = m_dPercentile = 0;
   }

   /**
    * Clones the object.
    * @param bFull Preserve all the resources (e.g. the percentile computation ability).
    * @return The cloned object. 
    */
   public Average clone(boolean bFull)
   {
      if (!bFull)
      {
         getPercentile();
      }

      try
      {
         Average obj = (Average)super.clone();

         if (!bFull)
         {
            obj.m_dValueArray = null;
         }
         else if (m_dValueArray != null)
         {
            obj.m_dValueArray = (double[])m_dValueArray.clone();
         }

         return obj;
      }
      catch (CloneNotSupportedException e)
      {
         return null;
      }
   }

   /**
    * @see java.lang.Object#toString()
    */
   public synchronized String toString()
   {
      return "Average(mean=" + getMean() + ", count=" + getCount() + ", percentile=" +
         getPercentile() + ", dev=" + getSampleDeviation() + ")";
   }
}
