// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql.upgrade;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import nexj.core.meta.Aspect;
import nexj.core.meta.AspectHelper;
import nexj.core.meta.MetadataException;
import nexj.core.meta.Pointcut;
import nexj.core.util.Named;

/**
 * Upgrade step for applying/removing an aspect.
 */
public abstract class AspectUpgradeStep extends RelationalSchemaUpgradeStep implements Aspect
{
   // attributes

   /**
    * The aspect name.
    */
   protected String m_sAspectName;

   /**
    * The aspect override flag - true to remove conflicting aspect overrides. 
    */
   protected boolean m_bOverride;

   // associations

   /**
    * The aspect helper.
    */
   protected AspectHelper m_helper = new AspectHelper()
   {
      protected Aspect getContainer()
      {
         return getAspect();
      }

      /**
       * @see nexj.core.meta.AspectHelper#isMatching(nexj.core.meta.Pointcut)
       */
      public boolean isMatching(Pointcut pointcut)
      {
         if (m_bOverride)
         {
            if (!pointcut.isPointcut() || pointcut == getContainer())
            {
               return false;
            }

            return isPatternMatching(pointcut.getName());
         }

         return super.isMatching(pointcut);
      }
   };

   // operations

   /**
    * @see nexj.core.util.Named#getName()
    */
   public String getName()
   {
      return m_sAspectName;
   }

   /**
    * Sets the aspect name.
    * @param sAspectName The aspect name to set.
    */
   public void setAspectName(String sAspectName)
   {
      verifyNotReadOnly();
      m_sAspectName = sAspectName;
   }

   /**
    * @return The aspect name.
    */
   public String getAspectName()
   {
      return m_sAspectName;
   }

   /**
    * Sets the aspect override flag.
    * @param bOverride The aspect override flag to set.
    */
   public void setOverride(boolean bOverride)
   {
      verifyNotReadOnly();
      m_bOverride = bOverride;
   }

   /**
    * @return The aspect override flag.
    */
   public boolean isOverride()
   {
      return m_bOverride;
   }

   /**
    * Adds a pointcut to the step.
    * @param sName The pointcut pattern.
    * @param bInclusive True if the aspect is inclusive.
    */
   public void addPointcutPattern(String sPattern, boolean bInclusive)
   {
      verifyNotReadOnly();
      m_helper.addPointcutPattern(sPattern, bInclusive);
   }

   /**
    * @see nexj.core.meta.Aspect#getPointcutPattern(int)
    */
   public String getPointcutPattern(int nOrdinal)
   {
      return m_helper.getPointcutPattern(nOrdinal);
   }

   /**
    * @see nexj.core.meta.Aspect#isPointcutPatternInclusive(int)
    */
   public boolean isPointcutPatternInclusive(int nOrdinal)
   {
      return m_helper.isPointcutPatternInclusive(nOrdinal);
   }

   /**
    * @see nexj.core.meta.Aspect#getPointcutPatternCount()
    */
   public int getPointcutPatternCount()
   {
      return m_helper.getPointcutPatternCount();
   }

   /**
    * @see nexj.core.meta.Aspect#addTo(nexj.core.meta.Pointcut)
    */
   public void addTo(Pointcut pointcut)
   {
      m_helper.addTo(pointcut);
   }

   /**
    * @see nexj.core.meta.Aspect#isMatching(nexj.core.meta.Pointcut)
    */
   public boolean isMatching(Pointcut pointcut)
   {
      return m_helper.isMatching(pointcut);
   }

   /**
    * @see nexj.core.meta.Aspect#isAspect()
    */
   public boolean isAspect()
   {
      return true;
   }

   /**
    * Gets an iterator for the pointcuts matching the pointcut pattern.
    * @param itr The pointcut iterator to filter.
    * @return The matching pointcut iterator.
    */
   public Iterator getPointcutIterator(Iterator itr)
   {
      List list = new ArrayList();

      while (itr.hasNext())
      {
         Pointcut pointcut = (Pointcut)itr.next();

         if (m_helper.isMatching(pointcut))
         {
            list.add(pointcut);
         }
      }

      Collections.sort(list, Named.COMPARATOR);

      return list.iterator();
   }

   /**
    * @see nexj.core.meta.Aspect#applyTo(nexj.core.meta.Pointcut, int)
    */
   public void applyTo(Pointcut pointcut, int nPass) throws MetadataException
   {
      Aspect aspect = getAspect();

      if (m_bOverride && nPass == 0)
      {
         int i = pointcut.findAspectOverride(aspect);

         if (i >= 0 && !pointcut.isAspectOverrideInclusive(i))
         {
            pointcut.removeAspectOverride(aspect);
         }
      }

      aspect.applyTo(pointcut, nPass);
   }

   /**
    * @see nexj.core.meta.Aspect#removeFrom(nexj.core.meta.Pointcut)
    */
   public boolean removeFrom(Pointcut pointcut)
   {
      Aspect aspect = getAspect();

      if (aspect.removeFrom(pointcut))
      {
         if (m_bOverride)
         {
            int i = pointcut.findAspectOverride(aspect);

            if (i >= 0 && pointcut.isAspectOverrideInclusive(i))
            {
               pointcut.removeAspectOverride(aspect);
            }
         }

         return true;
      }

      return false;
   }

   /**
    * @return the aspect.
    */
   public abstract Aspect getAspect();

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuffer buf = new StringBuffer();

      buf.append(getTypeName());
      buf.append("(aspect=");
      buf.append(m_sAspectName);
      buf.append(", pointcuts=");

      for (int i = 0, n = m_helper.getPointcutPatternCount(); i < n; ++i)
      {
         if (i != 0)
         {
            buf.append(' ');
         }

         if (!m_helper.isPointcutPatternInclusive(i))
         {
            buf.append('!');
         }

         buf.append(m_helper.getPointcutPattern(i));
      }

      buf.append(')');

      return buf.toString();
   }
}
