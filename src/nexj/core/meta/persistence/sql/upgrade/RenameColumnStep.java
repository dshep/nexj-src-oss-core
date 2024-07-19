// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql.upgrade;

import java.util.Iterator;

import nexj.core.meta.persistence.sql.Column;
import nexj.core.meta.persistence.sql.RelationalSchema;
import nexj.core.meta.persistence.sql.Table;

/**
 * Upgrade step for renaming a column.
 */
public class RenameColumnStep extends RenameStep
{
   // attributes

   /**
    * The table name.
    */
   protected String m_sTableName;

   // associations

   /**
    * The renamed column.
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
    * Sets the renamed column.
    * @param column The renamed column to set.
    */
   public void setColumn(Column column)
   {
      verifyNotReadOnly();
      m_column = column;
   }

   /**
    * @return The renamed column.
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

      m_column = table.getColumn(m_sOldName);
      m_column.setName(m_sNewName);

      if (table.isAspect())
      {
         for (Iterator itr = schema.getTableIterator(); itr.hasNext();)
         {
            Table pointcut = (Table)itr.next();

            if (pointcut.hasAspect(table))
            {
               pointcut.getColumn(m_sOldName).setName(m_sNewName);
            }
         }
      }
   }

   /**
    * @see nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeStep#undo(nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeState)
    */
   public void undo(RelationalSchemaUpgradeState state)
   {
      if (!state.renameColumn(m_sTableName, m_sNewName, m_sOldName) &&
         !state.containsTable(m_sTableName))
      {
         RelationalSchema schema = state.getSchema();
         Table table = schema.getTable(m_sTableName);

         table.getColumn(m_sNewName).setName(m_sOldName);

         if (table.isAspect())
         {
            for (Iterator itr = schema.getTableIterator(); itr.hasNext();)
            {
               Table pointcut = (Table)itr.next();

               if (pointcut.hasAspect(table) && !state.renameColumn(pointcut.getName(), m_sNewName, m_sOldName))
               {
                  pointcut.getColumn(m_sNewName).setName(m_sOldName);
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
      return "RenameColumn(table=" + m_sTableName + ", old=" + m_sOldName + ", new=" + m_sNewName + ')';
   }
}
