// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql.upgrade;

import java.util.Iterator;

import nexj.core.meta.persistence.sql.Column;
import nexj.core.meta.persistence.sql.RelationalSchema;
import nexj.core.meta.persistence.sql.Table;

/**
 * Upgrade step for altering a column.
 */
public class AlterColumnStep extends TableUpgradeStep
{
   // associations

   /**
    * The column outline.
    */
   protected ColumnOutline m_outline;

   // associations

   /**
    * The altered column.
    */
   protected Column m_column;

   // operations

   /**
    * Sets the column outline.
    * @param outline The column outline to set.
    */
   public void setOutline(ColumnOutline outline)
   {
      verifyNotReadOnly();
      m_outline = outline;
   }

   /**
    * @return The column outline.
    */
   public ColumnOutline getOutline()
   {
      return m_outline;
   }

   /**
    * @return The altered column.
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

      m_column = table.getColumn(m_outline.getName());
      m_outline.copyTo(m_column);

      if (table.isAspect())
      {
         for (Iterator itr = schema.getTableIterator(); itr.hasNext();)
         {
            Table pointcut = (Table)itr.next();

            if (pointcut.hasAspect(table))
            {
               m_outline.copyTo(pointcut.getColumn(m_outline.getName()));
            }
         }
      }
   }

   /**
    * @see nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeStep#undo(nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeState)
    */
   public void undo(RelationalSchemaUpgradeState state)
   {
      state.addColumn(m_sTableName, m_outline.getName(), this, false);
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "AlterColumn(table=" + m_sTableName + ", outline=" + m_outline + ')';
   }
}
