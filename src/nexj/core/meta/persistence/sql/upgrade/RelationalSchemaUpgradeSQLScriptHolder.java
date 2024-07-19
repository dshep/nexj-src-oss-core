// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql.upgrade;

import nexj.core.meta.MetadataValidationException;
import nexj.core.meta.persistence.sql.SQLScriptHolder;

/**
 * The upgrade SQL script holder. 
 */
public class RelationalSchemaUpgradeSQLScriptHolder extends SQLScriptHolder
{
   // attributes

   /**
    * The relational schema upgrade step.
    */
   protected RelationalSchemaUpgradeStep m_step;

   // constructors

   /**
    * Constructs the script holder.
    * @param step The upgrade step.
    */
   public RelationalSchemaUpgradeSQLScriptHolder(RelationalSchemaUpgradeStep step)
   {
      m_step = step;
   }
   
   // operations

   /**
    * @see nexj.core.meta.persistence.sql.SQLScriptHolder#setProperties(nexj.core.meta.MetadataValidationException)
    */
   protected void setProperties(MetadataValidationException e)
   {
      m_step.setProperties(e);
   }
}
