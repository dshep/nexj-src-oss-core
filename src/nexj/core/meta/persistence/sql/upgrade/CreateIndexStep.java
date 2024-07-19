// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql.upgrade;

import java.util.Collections;
import java.util.Iterator;

import nexj.core.meta.persistence.sql.Index;
import nexj.core.meta.persistence.sql.RelationalSchema;
import nexj.core.meta.persistence.sql.RelationalSchemaAspectManager;
import nexj.core.meta.persistence.sql.Table;
import nexj.core.util.EmptyIterator;

/**
 * Upgrade step for creating an index.
 */
public class CreateIndexStep extends TableUpgradeStep
{
   // associations

   /**
    * The index outline.
    */
   protected IndexOutline m_outline;

   /**
    * The created index.
    */
   protected Index m_index;

   // operations

   /**
    * Sets the index outline.
    * @param outline The index outline to set.
    */
   public void setOutline(IndexOutline outline)
   {
      verifyNotReadOnly();
      m_outline = outline;
   }

   /**
    * @return The index outline.
    */
   public IndexOutline getOutline()
   {
      return m_outline;
   }

   /**
    * @return The created index.
    */
   public Index getIndex()
   {
      return m_index;
   }

   /**
    * @see nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeStep#apply(nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeState)
    */
   public void apply(RelationalSchemaUpgradeState state)
   {
      final RelationalSchema schema = state.getSchema();
      final Table table = schema.getTable(m_sTableName);

      m_index = new Index(m_outline.getName(), m_outline.getType(), table);
      m_outline.copyTo(m_index);
      table.addIndex(m_index);

      new RelationalSchemaAspectManager()
      {
         protected Iterator getTableAspectIterator()
         {
            return schema.getTableIterator();
         }

         protected Iterator getTablePointcutIterator()
         {
            return EmptyIterator.getInstance();
         }

         protected Iterator getIndexAspectIterator()
         {
            return schema.getIndexIterator();
         }

         protected Iterator getIndexPointcutIterator()
         {
            return Collections.singletonList(m_index).iterator();
         }
      }.applyAspects(2);

      if (table.isAspect())
      {
         if (!m_index.isAspect())
         {
            for (Iterator itr = schema.getTableIterator(); itr.hasNext();)
            {
               Table pointcut = (Table)itr.next();

               if (pointcut.hasAspect(table))
               {
                  Index index = new Index(m_index.getName(pointcut), m_outline.getType(), pointcut);

                  m_outline.copyTo(index);
                  pointcut.addIndex(index);

                  if (m_outline.getRelatedTableName() != null)
                  {
                     schema.getTable(m_outline.getRelatedTableName()).addRelatedKey(index);
                  }
               }
            }
         }
      }
      else
      {
         if (m_outline.getRelatedTableName() != null)
         {
            schema.getTable(m_outline.getRelatedTableName()).addRelatedKey(m_index);
         }
      }
   }

   /**
    * @see nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeStep#undo(nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeState)
    */
   public void undo(RelationalSchemaUpgradeState state)
   {
      if (!state.removeIndex(m_outline.getName()) &&
         !state.containsTable(m_sTableName))
      {
         RelationalSchema schema = state.getSchema();
         Index index = schema.getIndex(m_outline.getName());

         schema.removeIndex(index);

         if (!index.isAspect() && index.getTable().isAspect())
         {
            for (Iterator itr = schema.getTableIterator(); itr.hasNext();)
            {
               Table pointcut = (Table)itr.next();

               if (pointcut.hasAspect(index.getTable()))
               {
                  String sName = index.getName(pointcut);

                  if (!state.removeIndex(sName))
                  {
                     schema.removeIndex(pointcut.getIndex(sName));
                  }
               }
            }
         }
      }
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "CreateIndex(table=" + m_sTableName + ", outline=" + m_outline + ')';
   }
}
