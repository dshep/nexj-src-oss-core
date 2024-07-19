// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.file;


import nexj.core.meta.Aspect;
import nexj.core.meta.Attribute;
import nexj.core.meta.Component;
import nexj.core.meta.ContextMetadata;
import nexj.core.meta.MetadataException;
import nexj.core.meta.Primitive;
import nexj.core.meta.persistence.AttributeMapping;
import nexj.core.meta.persistence.GenericObjectKey;
import nexj.core.meta.persistence.GenericPersistenceMapping;
import nexj.core.meta.persistence.Key;
import nexj.core.meta.persistence.PersistenceMapping;
import nexj.core.scripting.Pair;
import nexj.core.util.ExceptionHolder;

/**
 * The metadata object describing the mapping of a metaclass' fields to
 * the file persistence layer.
 */
public class FileMapping extends GenericPersistenceMapping
{
   // constants

   /**
    * The lookup key for something stored in the file persistence adapter
    * always has just one part of type string.
    */
   protected final static Key KEY = new GenericObjectKey(new Primitive[]{Primitive.STRING});

   // associations

   /**
    * The component to use for generating unique keys for new items.
    */
   protected Component m_keyGenerator;


   // operations

   /**
    * @see nexj.core.meta.persistence.PersistenceMapping#getObjectKey()
    */
   public Key getObjectKey()
   {
      return KEY;
   }

   /**
    * @see nexj.core.meta.persistence.PersistenceMapping#create()
    */
   public PersistenceMapping create()
   {
      return new FileMapping();
   }

   /**
    * @see nexj.core.meta.persistence.PersistenceMapping#getSortKeys(nexj.core.meta.Attribute[], nexj.core.meta.persistence.PersistenceMapping[], Attribute[])
    */
   public Pair getSortKeys(Attribute[] assocs, PersistenceMapping[] mappings, Attribute[] restrictions)
   {
      return null;
   }

   /**
    * @see nexj.core.meta.persistence.PersistenceMapping#getUniqueKeys()
    */
   public Pair getUniqueKeys()
   {
      return null;
   }

   /**
    * Sets the primary key generator component.
    * @param keyGenerator The primary key generator component to set.
    */
   public void setKeyGenerator(Component keyGenerator)
   {
      verifyNotReadOnly();
      validateKeyGenerator(keyGenerator);
      m_keyGenerator = keyGenerator;
   }

   /**
    * @return The primary key generator component.
    */
   public Component getKeyGenerator()
   {
      return m_keyGenerator;
   }

   /**
    * @see nexj.core.meta.persistence.PersistenceMapping#setLockingAttribute(nexj.core.meta.Attribute)
    */
   public void setLockingAttribute(Attribute lockingAttribute)
   {
      super.setLockingAttribute(lockingAttribute);

      FilePrimitiveMapping lockingMapping = new FilePrimitiveMapping();

      lockingMapping.setAttribute(lockingAttribute);
      lockingMapping.setSysId(FilePrimitiveMapping.SYSID_LOCKING);
      addAttributeMapping(lockingMapping);
   }

   /**
    * @see nexj.core.meta.persistence.PersistenceMapping#deriveAttributeMapping(nexj.core.meta.Attribute, nexj.core.meta.Attribute, nexj.core.meta.persistence.AttributeMapping)
    */
   protected void deriveAttributeMapping(Attribute attribute, Attribute base, AttributeMapping baseMapping)
   {
      if (attribute.getType() == base.getType())
      {
         AttributeMapping mapping = (AttributeMapping)baseMapping.clone();

         mapping.setAttribute(attribute);
         addAttributeMapping(mapping);
      }
   }

   /**
    * @see nexj.core.meta.MetadataObject#validate(nexj.core.meta.ContextMetadata, ExceptionHolder)
    */
   public void validate(ContextMetadata metadata, ExceptionHolder warnings)
   {
      super.validate(metadata, warnings);

      if (!(m_metaclass instanceof Aspect))
      {
         if (m_lockingAttribute == null)
         {
            throw new MetadataException("err.meta.persistence.missingLockingAttribute",
               new Object[]{m_metaclass.getName()});
         }

         boolean bFound = false;

         for (int i = 0; i < m_mappingArray.length; i++)
         {
            FilePrimitiveMapping mapping = (FilePrimitiveMapping)m_mappingArray[i];

            if (mapping != null && mapping.getSysId() == FilePrimitiveMapping.SYSID_ID)
            {
               bFound = true;
               break;
            }
         }

         if (!bFound)
         {
            throw new MetadataException("err.meta.persistence.file.noFileNameAttribute",
               new Object[]{m_metaclass.getName()});
         }
      }
   }
}
