// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql.upgrade;

import nexj.core.meta.persistence.sql.Table;

/**
 * Upgrade step for renaming a table.
 */
public class RenameTableStep extends RenameStep
{
   // associations

   /**
    * The table.
    */
   protected Table m_table;

   // operations

   /**
    * @return The table.
    */
   public Table getTable()
   {
      return m_table;
   }

   /**
    * @see nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeStep#apply(nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeState)
    */
   public void apply(RelationalSchemaUpgradeState state)
   {
      m_table = state.getSchema().getTable(m_sOldName);
      m_table.setName(m_sNewName);
   }

   /**
    * @see nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeStep#undo(nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeState)
    */
   public void undo(RelationalSchemaUpgradeState state)
   {
      if (!state.renameTable(m_sNewName, m_sOldName))
      {
         state.getSchema().getTable(m_sNewName).setName(m_sOldName);
      }
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "RenameTable(old=" + m_sOldName + ", new=" + m_sNewName + ')';
   }
}
