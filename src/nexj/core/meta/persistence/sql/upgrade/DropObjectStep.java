// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql.upgrade;

import nexj.core.meta.persistence.sql.RelationalSchema;
import nexj.core.meta.persistence.sql.SQLObject;
import nexj.core.meta.persistence.sql.SQLScript;

/**
 * Upgrade step for dropping an SQL Object.
 */
public class DropObjectStep extends RelationalSchemaUpgradeStep
{
   // attributes

   /**
    * The object name.
    */
   protected String m_sName;

   // associations

   /**
    * The SQL script.
    */
   protected SQLScript m_script = new SQLScript();

   /**
    * The the SQL Object being dropped.
    */
   protected SQLObject m_object;

   // operations

   /**
    * @see nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeStep#apply(nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeState)
    */
   public void apply(RelationalSchemaUpgradeState state)
   {
      RelationalSchema schema = state.getSchema();

      m_object = new SQLObject(schema);
      m_object.setName(m_sName);
      m_object.setDropScript(m_script);
      m_script.validate(schema, null, state.getAdapters());
      m_object.setSchema(schema.getObject(m_sName).getSchema()); // schema of original
      schema.removeObject(m_object);
   }

   /**
    * @return The template of the SQL Object being dropped.
    */
   public SQLObject getObject()
   {
      return m_object;
   }

   /**
    * @param sName The name of the SQL Object being created.
    */
   public void setName(String sName)
   {
      verifyNotReadOnly();
      m_sName = sName;
   }

   /**
    * @return The SQL object name.
    */
   public String getName()
   {
      return m_sName;
   }

   /**
    * @return The SQL script.
    */
   public SQLScript getScript()
   {
      return m_script;
   }

   /**
    * @see nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeStep#undo(nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeState)
    */
   public void undo(RelationalSchemaUpgradeState state)
   {
      state.addObject(m_sName, this);
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "DropObject(name=" + m_sName + ')';
   }
}