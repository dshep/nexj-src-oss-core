// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql.upgrade;

/**
 * Upgrade step for dropping an object.
 */
public abstract class DropStep extends RelationalSchemaUpgradeStep
{
   // attributes
   
   /**
    * The object name.
    */
   protected String m_sName;

   // operations
   
   /**
    * Sets the object name.
    * @param sName The object name to set.
    */
   public void setName(String sName)
   {
      verifyNotReadOnly();
      m_sName = sName;
   }

   /**
    * @return The object name.
    */
   public String getName()
   {
      return m_sName;
   }
}
