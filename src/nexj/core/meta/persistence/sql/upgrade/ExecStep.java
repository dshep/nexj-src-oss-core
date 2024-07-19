// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql.upgrade;

import nexj.core.meta.persistence.sql.SQLScriptHolder;


/**
 * Upgrade step for executing SQL with special escapes.
 */
public class ExecStep extends RelationalSchemaUpgradeStep
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
      m_holder.validate(state.getSchema(), null, state.getAdapters());
   }

   /**
    * @see nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeStep#undo(nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeState)
    */
   public void undo(RelationalSchemaUpgradeState state)
   {
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "SQLExec(scripts=" + m_holder.getScriptCount() + ')';
   }
}
