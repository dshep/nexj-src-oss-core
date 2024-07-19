// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql.upgrade;

import java.util.Iterator;

import nexj.core.meta.persistence.sql.Column;
import nexj.core.meta.persistence.sql.RelationalSchema;
import nexj.core.meta.persistence.sql.Table;

/**
 * Upgrade step for dropping a column.
 */
public class DropColumnStep extends DropStep
{
   // attributes

   /**
    * The table name.
    */
   protected String m_sTableName;

   // associations

   /**
    * The dropped column.
    */
   protected Column m_column;

   // operations

   /**
    * Sets the table name.
    * @param sTableName The table name to set.
    */
   public void setTableName(String sTableName)
   {
      verifyNotReadOnly();
      m_sTableName = sTableName;
   }

   /**
    * @return The table name.
    */
   public String getTableName()
   {
      return m_sTableName;
   }

   /**
    * @return The dropped column.
    */
   public Column getColumn()
   {
      return m_column;
   }

   /**
    * @see nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeStep#apply(nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeState)
    */
   public void apply(RelationalSchemaUpgradeState state)
   {
      RelationalSchema schema = state.getSchema();
      Table table = schema.getTable(m_sTableName);

      m_column = table.getColumn(m_sName);
      table.removeColumn(m_column);

      if (table.isAspect())
      {
         for (Iterator itr = schema.getTableIterator(); itr.hasNext();)
         {
            Table pointcut = (Table)itr.next();

            if (pointcut.hasAspect(table))
            {
               pointcut.removeColumn(pointcut.getColumn(m_sName));
            }
         }
      }
   }

   /**
    * @see nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeStep#undo(nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeState)
    */
   public void undo(RelationalSchemaUpgradeState state)
   {
      state.addColumn(m_sTableName, m_sName, this, true);
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "DropColumn(table=" + m_sTableName + ", name=" + m_sName + ')';
   }
}
