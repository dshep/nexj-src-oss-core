// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql.upgrade;

import nexj.core.meta.MetadataException;
import nexj.core.meta.persistence.sql.Table;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;

/**
 * Upgrade step for altering a table.
 */
public class AlterTableStep extends RelationalSchemaUpgradeStep
{
   // attributes

   /**
    * The table name.
    */
   protected String m_sName;

   /**
    * The table alias.
    */
   protected String m_sAlias;

   /**
    * The primary key name.
    */
   protected String m_sPrimaryKeyName;

   /**
    * The table type, one of the Table.* constants, or -1 if unchanged.
    */
   protected byte m_nType;

   // associations

   /**
    * Map of hints to enable/disable (lazy init).
    */
   protected Lookup/*<String, Boolean>*/ m_hintMap;

   /**
    * The altered table.
    */
   protected Table m_table;

   // operations

   /**
    * Sets the table name.
    * @param sName The table name to set.
    */
   public void setName(String sName)
   {
      verifyNotReadOnly();
      m_sName = sName;
   }

   /**
    * @return The table name.
    */
   public String getName()
   {
      return m_sName;
   }

   /**
    * Sets the table alias.
    * @param sAlias The table alias to set.
    */
   public void setAlias(String sAlias)
   {
      verifyNotReadOnly();
      m_sAlias = sAlias;
   }

   /**
    * @return The table alias.
    */
   public String getAlias()
   {
      return m_sAlias;
   }

   /**
    * Set the specified DDL hint.
    * @param sHint The hint to set (not null).
    * @param bEnabled is the hint enabled or disabled.
    */
   public void setHint(String sHint, boolean bEnabled)
   {
      verifyNotReadOnly();

      if (m_hintMap == null)
      {
         m_hintMap = new HashTab/*<String, Boolean>*/(2);
      }

      if (m_hintMap.put(sHint, Boolean.valueOf(bEnabled)) != null)
      {
         throw new MetadataException("err.meta.tableHintDup", new Object[]{sHint, m_sName});
      }
   }

   /**
    * Sets the primary key name.
    * @param sPrimaryKeyName The primary key name to set.
    */
   public void setPrimaryKeyName(String sPrimaryKeyName)
   {
      verifyNotReadOnly();
      m_sPrimaryKeyName = sPrimaryKeyName;
   }

   /**
    * @return The primary key name.
    */
   public String getPrimaryKeyName()
   {
      return m_sPrimaryKeyName;
   }

   /**
    * Sets the table type, one of the Table.* constants.
    * @param nType The table type, one of the Table.* constants to set.
    */
   public void setType(byte nType)
   {
      verifyNotReadOnly();
      m_nType = nType;
   }

   /**
    * @return The table type, one of the Table.* constants.
    */
   public byte getType()
   {
      return m_nType;
   }

   /**
    * @see nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeStep#apply(nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeState)
    */
   public void apply(RelationalSchemaUpgradeState state)
   {
      m_table = state.getSchema().getTable(m_sName);

      if (m_nType >= 0)
      {
         if ((m_table.getType() >= Table.VIEW) != (m_nType >= Table.VIEW))
         {
            throw new MetadataException("err.meta.alterTableType");
         }

         m_table.setType(m_nType);
      }

      m_table.setAlias(m_sAlias);

      if (m_hintMap != null)
      {
         for (Lookup.Iterator itr = m_hintMap.valueIterator(); itr.hasNext();)
         {
            if (((Boolean)itr.next()).booleanValue())
            {
               m_table.addHint(itr.getKey().toString());
            }
            else
            {
               m_table.removeHint(itr.getKey().toString());
            }
         }
      }

      if (m_sPrimaryKeyName != null)
      {
         m_table.setPrimaryKey(m_table.getIndex(m_sPrimaryKeyName));
         m_table.computePrimaryKeyParts();
      }
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
      StringBuilder buf = new StringBuilder(64);

      buf.append("AlterTable(name=");
      buf.append(m_sName);

      if (m_sAlias != null)
      {
         buf.append(", alias=");
         buf.append(m_sAlias);
      }

      if (m_nType >= 0)
      {
         buf.append(", type=");
         buf.append(new String[]{"managed", "external", "view", "aspect"}[m_nType]);
      }

      if (m_sPrimaryKeyName != null)
      {
         buf.append(", primaryKey=");
         buf.append(m_sPrimaryKeyName);
      }

      buf.append(')');

      return buf.toString();
   }
}
