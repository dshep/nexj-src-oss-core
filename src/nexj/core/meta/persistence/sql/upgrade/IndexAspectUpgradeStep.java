// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql.upgrade;

import java.util.Collections;
import java.util.Iterator;

import nexj.core.meta.Aspect;
import nexj.core.meta.persistence.sql.Index;
import nexj.core.meta.persistence.sql.RelationalSchema;
import nexj.core.meta.persistence.sql.RelationalSchemaAspectManager;
import nexj.core.util.EmptyIterator;

/**
 * Upgrade step for applying/removing index aspects.
 */
public abstract class IndexAspectUpgradeStep extends AspectUpgradeStep
{
   // associations

   /**
    * The index aspect.
    */
   protected Index m_index;

   // operations

   /**
    * @see nexj.core.meta.persistence.sql.upgrade.AspectUpgradeStep#getAspect()
    */
   public Aspect getAspect()
   {
      return m_index;
   }

   // inner classes

   /**
    * Aspect manager for an index aspect upgrade.
    */
   protected class IndexAspectUpgradeManager extends RelationalSchemaAspectManager
   {
      // associations

      /**
       * The relational schema.
       */
      protected RelationalSchema m_schema;

      // constructors

      public IndexAspectUpgradeManager(RelationalSchema schema)
      {
         m_schema = schema;
      }

      // operations

      protected Iterator getTableAspectIterator()
      {
         return EmptyIterator.getInstance();
      }

      protected Iterator getTablePointcutIterator()
      {
         return EmptyIterator.getInstance();
      }

      protected Iterator getIndexAspectIterator()
      {
         return Collections.singletonList(IndexAspectUpgradeStep.this).iterator();
      }

      protected Iterator getIndexPointcutIterator()
      {
         return getPointcutIterator(m_schema.getIndexIterator());
      }
   }
}
