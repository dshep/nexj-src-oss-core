// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql.upgrade;

import nexj.core.meta.persistence.sql.Index;

/**
 * Upgrade step for renaming an index.
 */
public class RenameIndexStep extends RenameStep
{
   // associations

   /**
    * The renamed index.
    */
   protected Index m_index;

   // operations

   /**
    * @return The renamed index.
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
      m_index = state.getSchema().getIndex(m_sOldName);
      m_index.setName(m_sNewName);
   }

   /**
    * @see nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeStep#undo(nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeState)
    */
   public void undo(RelationalSchemaUpgradeState state)
   {
      if (!state.renameIndex(m_sNewName, m_sOldName))
      {
         Index index = state.getSchema().findIndex(m_sNewName);

         if (index == null) // valid to have phantom index if its table is dropped later in upgrade
         {
            state.addIndex(m_sOldName, this);

            return;
         }

         index.setName(m_sOldName);
      }
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "RenameIndex(old=" + m_sOldName + ", new=" + m_sNewName + ')';
   }
}
