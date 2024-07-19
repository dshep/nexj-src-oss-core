// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql;

import nexj.core.meta.ContextMetadata;
import nexj.core.meta.MetadataValidationException;
import nexj.core.util.ExceptionHolder;
import nexj.core.util.StringUtil;
import nexj.core.util.UncheckedException;

/**
 * Relational database managed object metadata.
 */
public class SQLObject extends RelationalObject
{
   // attributes

   /**
    * The object name, which must be unique for a given owner/schema and object type.
    * In contrast, the metadata object stores the full name: ownerName.objectName,
    * which is what is returned from getName().
    */
   protected String m_sObjectName;

   /**
    * The object owner/schema name.
    */
   protected String m_sOwnerName;

   // associations

   /**
    * The object creation script.
    */
   protected SQLScript m_create;

   /**
    * The object drop scripts.
    */
   protected SQLScript m_drop;

   // constructors

   /**
    * Constructs the object.
    * @param schema The relational schema.
    */
   public SQLObject(RelationalSchema schema)
   {
      super(schema);
   }

   // operations

   /**
    * @return The name of the object in the schema.
    */
   public String getObjectName()
   {
      return m_sObjectName;
   }

   /**
    * @return The name of the object owner. Can be null.  
    */
   public String getOwnerName()
   {
      return m_sOwnerName;
   }

   /**
    * Clones the table (deep copy).
    * @param schema The new schema instance.
    * @see nexj.core.meta.MetadataObject#clone()
    */
   public SQLObject clone(final RelationalSchema schema)
   {
      SQLObject obj = (SQLObject)super.clone();

      if (m_create != null)
      {
         obj.m_create = (SQLScript)m_create.clone();
      }

      if (m_drop != null)
      {
         obj.m_drop = (SQLScript)m_drop.clone();
      }

      obj.m_schema = schema;

      return obj;
   }

   /**
    * @return The script used for creating this SQL Object.
    */
   public SQLScript getCreateScript()
   {
      return m_create;
   }

   /**
    * @return The script used for dropping this SQL Object.
    */
   public SQLScript getDropScript()
   {
      return m_drop;
   }

   /**
    * @see nexj.core.meta.MetadataObject#makeReadOnly()
    */
   public void makeReadOnly()
   {
      super.makeReadOnly();

      if (m_create != null)
      {
         m_create.makeReadOnly();
      }

      if (m_drop != null)
      {
         m_drop.makeReadOnly();
      }
   }

   /**
    * Set the create script to use.
    * @param script The script to use when creating this SQL Object.
    */
   public void setCreateScript(SQLScript script)
   {
      verifyNotReadOnly();
      m_create = script;
   }

   /**
    * Set the drop script to use.
    * @param script The script to use when dropping this SQL Object.
    */
   public void setDropScript(SQLScript script)
   {
      verifyNotReadOnly();
      m_drop = script;
   }

   /**
    * @see nexj.core.meta.NamedMetadataObject#setName(java.lang.String)
    */
   public void setName(String sName)
   {
      sName = StringUtil.intern(sName); // make sure always use interned value from setName()
      m_sObjectName = null;
      m_sOwnerName = null;

      super.setName(sName);

      if (sName == null)
      {
         return;
      }

      int nOwnerEnd = sName.lastIndexOf('.');

      if (nOwnerEnd < 0) // if no owner/schema in the name then use the prefix from schema
      {
         String sPrefix = m_schema.getPrefix();

         if (sPrefix != null)
         {
            sName = sPrefix + sName; // the full name with possibly owner/schema and prefix
            nOwnerEnd = sName.lastIndexOf('.');
         }
      }

      if (nOwnerEnd > 0) // if have owner/schema
      {
         m_sOwnerName = sName.substring(0, nOwnerEnd);
      }

      m_sObjectName = sName.substring(nOwnerEnd + 1);
   }

   /**
    * @see nexj.core.meta.MetadataObject#validate(nexj.core.meta.ContextMetadata, nexj.core.util.ExceptionHolder)
    */
   public void validate(ContextMetadata metadata, ExceptionHolder warnings)
   {
      super.validate(metadata, warnings);

      try
      {
         m_create.validate(m_schema, null, null);
         m_drop.validate(m_schema, null, null);
      }
      catch (UncheckedException e)
      {
         MetadataValidationException x = new MetadataValidationException(e);

         setProperties(x);

         throw x;
      }
   }
}