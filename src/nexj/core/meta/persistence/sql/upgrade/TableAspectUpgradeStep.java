// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql.upgrade;

import java.util.Collections;
import java.util.Iterator;

import nexj.core.meta.Aspect;
import nexj.core.meta.persistence.sql.RelationalSchema;
import nexj.core.meta.persistence.sql.RelationalSchemaAspectManager;
import nexj.core.meta.persistence.sql.Table;
import nexj.core.util.EmptyIterator;


/**
 * Upgrade step for applying/removing table aspects.
 */
public abstract class TableAspectUpgradeStep extends AspectUpgradeStep
{
   // associations

   /**
    * The table aspect.
    */
   protected Table m_table;

   // operations

   /**
    * @see nexj.core.meta.persistence.sql.upgrade.AspectUpgradeStep#getAspect()
    */
   public Aspect getAspect()
   {
      return m_table;
   }

   /**
    * Undoes the effect of the upgrade step on a given pointcut.
    * @param table The pointcut.
    */
   public abstract void undo(Table table);

   // inner classes

   /**
    * Aspect manager for a table aspect upgrade.
    */
   protected abstract class TableAspectUpgradeManager extends RelationalSchemaAspectManager
   {
      // operations

      protected Iterator getTableAspectIterator()
      {
         return Collections.singletonList(TableAspectUpgradeStep.this).iterator();
      }

      protected Iterator getIndexAspectIterator()
      {
         return EmptyIterator.getInstance();
      }

      protected Iterator getIndexPointcutIterator()
      {
         return EmptyIterator.getInstance();
      }
   }

   /**
    * Aspect manager for a table aspect upgrade applying to a whole schema.
    */
   protected class SchemaTableAspectUpgradeManager extends TableAspectUpgradeManager
   {
      // associations

      /**
       * The relational schema.
       */
      protected RelationalSchema m_schema;

      // constructors

      public SchemaTableAspectUpgradeManager(RelationalSchema schema)
      {
         m_schema = schema;
      }

      // operations

      protected Iterator getTablePointcutIterator()
      {
         return getPointcutIterator(m_schema.getTableIterator());
      }
   }

   /**
    * Aspect manager for a table aspect upgrade applying to a whole schema.
    */
   protected class PointcutTableAspectUpgradeManager extends TableAspectUpgradeManager
   {
      // associations

      /**
       * The pointcut.
       */
      protected Table m_table;

      // constructors

      public PointcutTableAspectUpgradeManager(Table table)
      {
         m_table = table;
      }

      // operations

      protected Iterator getTablePointcutIterator()
      {
         return getPointcutIterator(Collections.singletonList(m_table).iterator());
      }
   }
}
