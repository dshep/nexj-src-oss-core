// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql;

import java.util.Iterator;

import nexj.core.meta.Aspect;
import nexj.core.meta.AspectManager;
import nexj.core.meta.MetadataMarker;
import nexj.core.meta.MetadataObject;
import nexj.core.meta.Pointcut;

/**
 * Relational schema aspect manager.
 */
public abstract class RelationalSchemaAspectManager
{
   // associations

   /**
    * The table aspect manager.
    */
   protected AspectManager m_tableAspectManager = new AspectManager()
   {
      protected Iterator getAspectIterator()
      {
         return getTableAspectIterator();
      }

      protected Iterator getPointcutIterator()
      {
         return getTablePointcutIterator();
      }

      /**
       * @see nexj.core.meta.AspectManager#setProperties(MetadataMarker, nexj.core.meta.Pointcut, nexj.core.meta.Aspect)
       */
      protected void setProperties(MetadataMarker e, Pointcut pointcut, Aspect aspect)
      {
         ((MetadataObject)pointcut).setProperties(e);
         e.setProperty("aspect", aspect.getName());
      }
   };

   /**
    * The index aspect manager.
    */
   protected AspectManager m_indexAspectManager = new AspectManager()
   {
      protected Iterator getAspectIterator()
      {
         return getIndexAspectIterator();
      }

      protected Iterator getPointcutIterator()
      {
         return getIndexPointcutIterator();
      }

      protected void setProperties(MetadataMarker e, Pointcut pointcut, Aspect aspect)
      {
         ((MetadataObject)pointcut).setProperties(e);
         e.setProperty("aspect", aspect.getName());
      }
   };
   
   // operations

   /**
    * @return The table aspect iterator.
    */
   protected abstract Iterator getTableAspectIterator();

   /**
    * @return The table pointcut iterator.
    */
   protected abstract Iterator getTablePointcutIterator();

   /**
    * @return The index aspect iterator.
    */
   protected abstract Iterator getIndexAspectIterator();

   /**
    * @return The index pointcut iterator.
    */
   protected abstract Iterator getIndexPointcutIterator();

   /**
    * Applies the aspects.
    * @param nPass The pass number, 0-based.
    */
   public void applyAspects(int nPass)
   {
      switch (nPass)
      {
      case 0:
      case 1:
         m_tableAspectManager.applyAspects(nPass);
         break;

      case 2:
         m_indexAspectManager.applyAspects(0);
         break;
      }
   }

   /**
    * Applies all the aspect passes.
    */
   public void applyAspects()
   {
      for (int i = 0; i <= 2; ++i)
      {
         applyAspects(i);
      }
   }

   /**
    * Removes all the aspects.
    */
   public void removeAspects()
   {
      m_indexAspectManager.removeAspects();
      m_tableAspectManager.removeAspects();
   }
}
