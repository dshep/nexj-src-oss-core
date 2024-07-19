// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql.upgrade;

/**
 * Upgrade step for a table.
 */
public abstract class TableUpgradeStep extends RelationalSchemaUpgradeStep
{
   // attributes

   /**
    * The table name.
    */
   protected String m_sTableName;

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
}
