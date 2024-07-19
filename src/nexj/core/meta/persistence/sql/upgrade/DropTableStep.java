// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql.upgrade;

import nexj.core.meta.persistence.sql.RelationalSchema;
import nexj.core.meta.persistence.sql.Table;

/**
 * Upgrade step for dropping a table.
 */
public class DropTableStep extends DropStep
{
   // associations

   /**
    * The dropped table.
    */
   protected Table m_table;

   // operations
   
   /**
    * @return The dropped table.
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
      RelationalSchema schema = state.getSchema();

      m_table = schema.getTable(m_sName);
      schema.removeTable(m_table);
   }

   /**
    * @see nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeStep#undo(nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeState)
    */
   public void undo(RelationalSchemaUpgradeState state)
   {
      state.addTable(m_sName, this);
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "DropTable(name=" + m_sName + ')';
   }
}
