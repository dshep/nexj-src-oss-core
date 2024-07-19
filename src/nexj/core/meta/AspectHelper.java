// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import java.util.regex.Pattern;

import nexj.core.util.ObjUtil;

/**
 * Aspect implementation helper.
 */
public abstract class AspectHelper implements Cloneable
{
   // attributes

   /**
    * Pointcut pattern count.
    */
   protected int m_nPointcutPatternCount;

   // associations

   /**
    * Pointcut pattern array: Pattern[3*n], sPattern[3*n+1], bInclusive[3*n+2].
    */
   protected Object[] m_pointcutPatternArray = new Object[3];

   // operations

   /**
    * @return The containing aspect.
    */
   protected abstract Aspect getContainer();
   
   /**
    * @see nexj.core.meta.Aspect#addPointcutPattern(java.lang.String, boolean)
    */
   public void addPointcutPattern(String sPattern, boolean bInclusive)
   {
      int i = m_nPointcutPatternCount * 3;

      if (i == m_pointcutPatternArray.length)
      {
         Object[] array = new Object[i << 1];

         System.arraycopy(m_pointcutPatternArray, 0, array, 0, i);
         m_pointcutPatternArray = array;
      }

      m_pointcutPatternArray[i] = Primitive.likePattern(sPattern, 0);
      m_pointcutPatternArray[i + 1] = sPattern;
      m_pointcutPatternArray[i + 2] = Boolean.valueOf(bInclusive);

      ++m_nPointcutPatternCount;
   }

   /**
    * @see nexj.core.meta.Aspect#getPointcutPattern(int)
    */
   public String getPointcutPattern(int nOrdinal)
   {
      if (nOrdinal >= m_nPointcutPatternCount)
      {
         throw new ArrayIndexOutOfBoundsException(nOrdinal);
      }

      return (String)m_pointcutPatternArray[3 * nOrdinal + 1]; 
   }

   /**
    * @see nexj.core.meta.Aspect#isPointcutPatternInclusive(int)
    */
   public boolean isPointcutPatternInclusive(int nOrdinal)
   {
      if (nOrdinal >= m_nPointcutPatternCount)
      {
         throw new ArrayIndexOutOfBoundsException(nOrdinal);
      }

      return ((Boolean)m_pointcutPatternArray[3 * nOrdinal + 2]).booleanValue(); 
   }

   /**
    * @see nexj.core.meta.Aspect#getPointcutPatternCount()
    */
   public int getPointcutPatternCount()
   {
      return m_nPointcutPatternCount;
   }

   /**
    * Determines if the patterns matches a given name.
    * @param sName The name to match. 
    */
   protected boolean isPatternMatching(String sName)
   {
      boolean bMatch = false;

      for (int i = (m_nPointcutPatternCount - 1) * 3; i >= 0; i -= 3)
      {
         if (((Pattern)m_pointcutPatternArray[i]).matcher(sName).matches())
         {
            bMatch = ((Boolean)m_pointcutPatternArray[i + 2]).booleanValue();

            if (!bMatch)
            {
               break;
            }
         }
      }

      return bMatch;
   }

   /**
    * @see nexj.core.meta.Aspect#isMatching(nexj.core.meta.Pointcut)
    */
   public boolean isMatching(Pointcut pointcut)
   {
      if (!pointcut.isPointcut() || pointcut == getContainer())
      {
         return false;
      }

      int i = pointcut.findAspectOverride(getContainer());

      if (i >= 0 && !pointcut.isAspectOverrideInclusive(i))
      {
         return false;
      }

      return isPatternMatching(pointcut.getName());
   }

   /**
    * @see nexj.core.meta.Aspect#addTo(nexj.core.meta.Pointcut)
    */
   public void addTo(Pointcut pointcut)
   {
      if (!pointcut.hasAspect(getContainer()) && isMatching(pointcut))
      {
         pointcut.addAspect(getContainer());
      }
   }

   public Object clone()
   {
      try
      {
         AspectHelper helper = (AspectHelper)super.clone();
         Object[] patternArray = new Object[m_pointcutPatternArray.length];

         System.arraycopy(m_pointcutPatternArray, 0, patternArray, 0, m_pointcutPatternArray.length);

         helper.m_pointcutPatternArray = patternArray;

         return helper;
      }
      catch (CloneNotSupportedException e)
      {
         ObjUtil.rethrow(e);

         return null;
      }
   }
}
