// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql.upgrade;

import java.util.Iterator;

import nexj.core.meta.persistence.sql.Column;
import nexj.core.meta.persistence.sql.RelationalSchema;
import nexj.core.meta.persistence.sql.SQLScriptHolder;
import nexj.core.meta.persistence.sql.Table;


/**
 * Upgrade step for creating a column.
 */
public class CreateColumnStep extends TableUpgradeStep
{
   // associations

   /**
    * The column outline.
    */
   protected ColumnOutline m_outline;

   /**
    * The SQL script holder.
    */
   protected SQLScriptHolder m_holder = new RelationalSchemaUpgradeSQLScriptHolder(this);

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
    * @return The SQL script holder.
    */
   public SQLScriptHolder getScriptHolder()
   {
      return m_holder;
   }

   /**
    * @return The created column.
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

      m_column = new Column(m_outline.getName(), table);
      m_outline.copyTo(m_column);
      table.addColumn(m_column);

      m_holder.validate(schema, table, state.getAdapters());

      if (table.isAspect())
      {
         for (Iterator itr = schema.getTableIterator(); itr.hasNext();)
         {
            Table pointcut = (Table)itr.next();

            if (pointcut.hasAspect(table))
            {
               Column column = new Column(m_outline.getName(), table);
               m_outline.copyTo(column);
               pointcut.addColumn(column);
            }
         }
      }
   }

   /**
    * @see nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeStep#undo(nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeState)
    */
   public void undo(RelationalSchemaUpgradeState state)
   {
      String sColumnName = m_outline.getName();
      
      if (!state.removeColumn(m_sTableName, sColumnName) &&
         !state.containsTable(m_sTableName))
      {
         RelationalSchema schema = state.getSchema();
         Table table = schema.getTable(m_sTableName);

         table.removeColumn(table.getColumn(sColumnName));

         if (table.isAspect())
         {
            for (Iterator itr = schema.getTableIterator(); itr.hasNext();)
            {
               Table pointcut = (Table)itr.next();

               if (pointcut.hasAspect(table) && !state.removeColumn(pointcut.getName(), sColumnName))
               {
                  pointcut.removeColumn(pointcut.getColumn(sColumnName));
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
      return "CreateColumn(table=" + m_sTableName + ", outline=" + m_outline + ')';
   }
}
