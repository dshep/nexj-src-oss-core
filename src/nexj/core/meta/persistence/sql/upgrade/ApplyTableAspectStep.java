// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql.upgrade;

import nexj.core.meta.MetadataException;
import nexj.core.meta.persistence.sql.RelationalSchema;
import nexj.core.meta.persistence.sql.SQLScriptHolder;
import nexj.core.meta.persistence.sql.Table;

/**
 * Upgrade step for applying a table aspect to matching tables. 
 */
public class ApplyTableAspectStep extends TableAspectUpgradeStep
{
   // associations

   /**
    * The SQL script holder.
    */
   protected SQLScriptHolder m_holder = new RelationalSchemaUpgradeSQLScriptHolder(this);

   // operations

   /**
    * @return The SQL script holder.
    */
   public SQLScriptHolder getScriptHolder()
   {
      return m_holder;
   }

   /**
    * @see nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeStep#apply(nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeState)
    */
   public void apply(RelationalSchemaUpgradeState state)
   {
      RelationalSchema schema = state.getSchema();

      m_table = schema.getTable(m_sAspectName);

      if (m_table.getType() != Table.ASPECT)
      {
         throw new MetadataException("err.meta.tableAspectUpgradeType", new Object[]{m_sAspectName});
      }

      new SchemaTableAspectUpgradeManager(schema).applyAspects();
      m_holder.validate(schema, m_table, state.getAdapters());
  }

   /**
    * @see nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeStep#undo(nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeState)
    */
   public void undo(RelationalSchemaUpgradeState state)
   {
      if (!state.containsTable(m_sAspectName))
      {
         RelationalSchema schema = state.getSchema();

         m_table = schema.getTable(m_sAspectName);

         if (m_table.getType() != Table.ASPECT)
         {
            throw new MetadataException("err.meta.tableAspectUpgradeType", new Object[]{m_sAspectName});
         }

         new SchemaTableAspectUpgradeManager(schema).removeAspects();
         state.addTableAspect(this);
      }
   }

   /**
    * @see nexj.core.meta.persistence.sql.upgrade.TableAspectUpgradeStep#undo(nexj.core.meta.persistence.sql.Table)
    */
   public void undo(Table table)
   {
      if (table.hasAspect(getAspect()))
      {
         new PointcutTableAspectUpgradeManager(table).removeAspects();
      }
   }
}
