// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql.upgrade;

import java.util.Iterator;

import nexj.core.meta.persistence.sql.Index;
import nexj.core.meta.persistence.sql.RelationalSchema;
import nexj.core.meta.persistence.sql.Table;

/**
 * Upgrade step for dropping an index.
 */
public class DropIndexStep extends DropStep
{
   // associations

   /**
    * The dropped index.
    */
   protected Index m_index;

   // operations

   /**
    * @return The dropped index.
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
      RelationalSchema schema = state.getSchema();

      m_index = schema.getIndex(m_sName);
      schema.removeIndex(m_index);

      if (!m_index.isAspect() && m_index.getTable().isAspect())
      {
         for (Iterator itr = schema.getTableIterator(); itr.hasNext();)
         {
            Table pointcut = (Table)itr.next();

            if (pointcut.hasAspect(m_index.getTable()))
            {
               schema.removeIndex(pointcut.getIndex(m_index.getName(pointcut)));
            }
         }
      }
   }

   /**
    * @see nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeStep#undo(nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeState)
    */
   public void undo(RelationalSchemaUpgradeState state)
   {
      state.addIndex(m_sName, this);
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "DropIndex(name=" + m_sName + ')';
   }
}
