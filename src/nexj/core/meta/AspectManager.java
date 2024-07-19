// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import nexj.core.util.ExceptionHolder;
import nexj.core.util.HashHolder;
import nexj.core.util.Logger;
import nexj.core.util.Named;
import nexj.core.util.UncheckedException;

/**
 * Class applying aspects.
 */
public abstract class AspectManager
{
   // associations

   /**
    * The set of all aspect names returned by the aspect iterator: String[].
    */
   protected Set m_aspectNameSet;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(AspectManager.class);

   // operations

   /**
    * @return The aspect iterator.
    */
   protected abstract Iterator getAspectIterator();

   /**
    * @return The pointcut iterator.
    */
   protected abstract Iterator getPointcutIterator();

   /**
    * Sets the properties on a metadata validation exception occurred
    * during an aspect application to a pointcut.
    * @param e The exception to update.
    * @param pointcut The pointcut.
    * @param aspect The aspect.
    */
   protected abstract void setProperties(MetadataMarker e, Pointcut pointcut, Aspect aspect);

   /**
    * Adds the aspects to the pointcuts.
    */
   public void addAspects() throws MetadataException
   {
      List aspectList = new ArrayList();

      m_aspectNameSet = new HashHolder();
      
      for (Iterator aspectItr = getAspectIterator(); aspectItr.hasNext();)
      {
         Aspect aspect = (Aspect)aspectItr.next();

         if (aspect.isAspect())
         {
            aspectList.add(aspect);
            m_aspectNameSet.add(aspect.getName());
         }
      }

      Collections.sort(aspectList, Named.COMPARATOR);

      for (Iterator pointcutItr = getPointcutIterator(); pointcutItr.hasNext();)
      {
         Pointcut pointcut = (Pointcut)pointcutItr.next();

         if (pointcut.isPointcut())
         {
            for (int i = 0, n = aspectList.size(); i < n; ++i)
            {
               ((Aspect)aspectList.get(i)).addTo(pointcut);
            }
         }
      }
   }

   /**
    * Applies the aspects to the pointcuts.
    * @param nPass The pass number, 0-based.
    */
   public void applyAspects(int nPass) throws MetadataException
   {
      MetadataCompoundValidationException eh = null;
      
      if (nPass == 0)
      {
         addAspects();
      }

      for (Iterator pointcutItr = getPointcutIterator(); pointcutItr.hasNext();)
      {
         Pointcut pointcut = (Pointcut)pointcutItr.next();

         for (int i = 0, n = pointcut.getAspectCount(); i < n; ++i)
         {
            Aspect aspect = pointcut.getAspect(i);

            if (m_aspectNameSet.contains(aspect.getName()))
            {
               try
               {
                  if (nPass == 0)
                  {
                     logApply(aspect, pointcut);
                  }
   
                  aspect.applyTo(pointcut, nPass);
               }
               catch (UncheckedException e)
               {
                  if (eh == null)
                  {
                     eh = new MetadataCompoundValidationException();
                  }
   
                  if (e instanceof MetadataValidationException)
                  {
                     eh.addException(e);
                  }
                  else if (e instanceof MetadataCompoundValidationException)
                  {
                     eh.addExceptions((ExceptionHolder)e);
                  }
                  else
                  {
                     MetadataValidationException x = new MetadataValidationException(e);
   
                     setProperties(x, pointcut, aspect);
                     eh.addException(x);
                  }
               }
            }
         }
      }

      if (eh != null)
      {
         throw eh;
      }
   }

   /**
    * Removes the aspects from the pointcuts.
    */
   public void removeAspects()
   {
      for (Iterator pointcutItr = getPointcutIterator(); pointcutItr.hasNext();)
      {
         Pointcut pointcut = (Pointcut)pointcutItr.next();

         for (Iterator aspectItr = getAspectIterator(); aspectItr.hasNext();)
         {
            Aspect aspect = (Aspect)aspectItr.next();
   
            if (aspect.isAspect() && aspect.removeFrom(pointcut))
            {
               logRemove(aspect, pointcut);
            }
         }
      }
   }

   /**
    * Logs the application of an aspect to a pointcut.
    * @param aspect The aspect, which is applied.
    * @param pointcut The target pointcut.
    */
   public static void logApply(Aspect aspect, Pointcut pointcut)
   {
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Applying aspect \"" + aspect.getName() + "\" to " + pointcut);
      }
   }

   /**
    * Logs the removal of an aspect from a pointcut.
    * @param aspect The aspect, which is removed.
    * @param pointcut The target pointcut.
    */
   public static void logRemove(Aspect aspect, Pointcut pointcut)
   {
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Removed aspect \"" + aspect.getName() + "\" from " + pointcut);
      }
   }
}
