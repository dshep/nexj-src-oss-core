// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql.upgrade;

import nexj.core.meta.MetadataException;

/**
 * Upgrade step for renaming an object.
 */
public abstract class RenameStep extends RelationalSchemaUpgradeStep
{
   // attributes
   
   /**
    * The old object name.
    */
   protected String m_sOldName;
   
   /**
    * The new object name.
    */
   protected String m_sNewName;

   // operations
   
   /**
    * Sets the old object name.
    * @param sOldName The old object name to set.
    */
   public void setOldName(String sOldName)
   {
      verifyNotReadOnly();
      m_sOldName = sOldName;
   }

   /**
    * @return The old object name.
    */
   public String getOldName()
   {
      return m_sOldName;
   }

   /**
    * Sets the new object name.
    * @param sNewName The new object name to set.
    */
   public void setNewName(String sNewName)
   {
      verifyNotReadOnly();

      if (sNewName != null && sNewName.equals(m_sOldName))
      {
         throw new MetadataException("err.meta.upgrade.rename", new Object[]{sNewName});
      }

      m_sNewName = sNewName;
   }

   /**
    * @return The new object name.
    */
   public String getNewName()
   {
      return m_sNewName;
   }
}
