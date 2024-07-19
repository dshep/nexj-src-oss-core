// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql;

import nexj.core.meta.Aspect;
import nexj.core.meta.Attribute;
import nexj.core.meta.Metaclass;
import nexj.core.meta.MetadataException;
import nexj.core.meta.Primitive;
import nexj.core.meta.persistence.AttributeMapping;
import nexj.core.meta.persistence.ClassMapping;
import nexj.core.meta.persistence.Key;
import nexj.core.meta.persistence.PersistenceMapping;
import nexj.core.util.Named;
import nexj.core.util.ObjUtil;

/**
 * Relational mapping of an attribute referencing an object.
 */
public final class RelationalClassMapping extends ClassMapping
{
   // attributes

   /**
    * The table index specifying the association key of the source class.
    */
   private Index m_sourceKey;
   
   /**
    * The association key of the destination class.
    */
   private Key m_destinationKey;
   
   // operations

   /**
    * @see nexj.core.meta.persistence.AttributeMapping#setAttribute(nexj.core.meta.Attribute)
    */
   public void setAttribute(Attribute attribute)
   {
      super.setAttribute(attribute);
      
      if (attribute != null && m_destinationKey != null)
      {
         m_destinationKey.addAttribute(attribute);
      }
   }
   
   /**
    * @see nexj.core.meta.persistence.ClassMapping#isInner()
    */
   public boolean isInner()
   {
      return m_destinationKey.isObjectKey() && !m_sourceKey.isObjectKey();
   }

   /**
    * @see nexj.core.meta.persistence.ClassMapping#isUnique()
    */
   public boolean isUnique()
   {
      return m_destinationKey.isObjectKey();
   }

   /**
    * @see nexj.core.meta.persistence.ClassMapping#isPure()
    */
   public boolean isPure()
   {
      return m_destinationKey.isObjectKey() || m_sourceKey.isObjectKey();
   }

   /**
    * @see nexj.core.meta.persistence.ClassMapping#getKey(boolean)
    */
   public Key getKey(boolean bDestination)
   {
      return (bDestination) ? m_destinationKey : m_sourceKey;
   }

   /**
    * Sets the association key of the source class.
    * @param sourceKey The the association key of the source class to set.
    */
   public void setSourceKey(Index sourceKey)
   {
      verifyNotReadOnly();
      m_sourceKey = sourceKey;

      if (sourceKey != null)
      {
         if (sourceKey.isAspect())
         {
            throw new MetadataException("err.meta.persistence.sql.aspectKey",
               new Object[]{sourceKey.getName()});
         }

         if (m_attribute != null)
         {
            if (sourceKey.getTable().isAspect() !=
               (m_attribute.getMetaclass() instanceof Aspect &&
                  ((Aspect)m_attribute.getMetaclass()).isAspect()))
            {
               throw new MetadataException((sourceKey.getTable().isAspect()) ?
                  "err.meta.persistence.sql.classKeyAspectMismatch" :
                  "err.meta.persistence.sql.classAspectKeyMismatch",
                  new Object[]{m_attribute.getName(), m_attribute.getMetaclass().getName(),
                     sourceKey.getName(), sourceKey.getTable().getName()});
            }
         }

         sourceKey.setMapped();

         boolean bRequired = sourceKey.isObjectKeyPart() || m_attribute.isRequired();

         for (int i = 0; i < sourceKey.getIndexColumnCount(); ++i)
         {
            sourceKey.getIndexColumn(i).getColumn().setRequired(bRequired, false);
         }
      }
   }

   /**
    * @return The association key of the source class.
    */
   public Index getSourceKey()
   {
      return m_sourceKey;
   }

   /**
    * Sets the association key of the destination class.
    * @param destinationKey The the association key of the destination class to set.
    */
   public void setDestinationKey(Key destinationKey)
   {
      verifyNotReadOnly();
      m_destinationKey = destinationKey;

      if (destinationKey != null)
      {
         if (m_sourceKey == null)
         {
            Metaclass metaclass = m_persistenceMapping.getMetaclass();

            if (metaclass instanceof Aspect)
            {
               return;
            }

            throw new MetadataException("err.meta.persistence.sql.noSourceKey");
         }

         if (!PersistenceMapping.compatible(m_sourceKey, destinationKey))
         {
            throw new MetadataException("err.meta.incompatibleKeys",
               new Object[]{m_sourceKey.getName(), getKeyName(destinationKey)});
         }

         if (!m_sourceKey.isObjectKey() && !destinationKey.isObjectKey())
         {
            if (m_attribute.isCollection() && !m_sourceKey.isObjectKeyPart() ||
               !m_attribute.isCollection() && !destinationKey.isObjectKeyPart())
            {
               throw new MetadataException("err.meta.keyMismatch",
                  new Object[]{m_sourceKey.getName(), getKeyName(destinationKey)});
            }
         }

         destinationKey.addAttribute(m_attribute);
         destinationKey.setMapped();
      }
   }

   /**
    * @return The association key of the destination class.
    */
   public Key getDestinationKey()
   {
      return m_destinationKey;
   }

   /**
    * @see nexj.core.meta.persistence.AttributeMapping#isMultiplexed()
    */
   public boolean isMultiplexed()
   {
      return m_destinationKey != null && m_destinationKey.isMultiplexed();
   }

   /**
    * @see nexj.core.meta.persistence.AttributeMapping#isAliasOf(nexj.core.meta.persistence.AttributeMapping)
    */
   public boolean isAliasOf(AttributeMapping mapping)
   {
      if (!(mapping instanceof RelationalClassMapping))
      {
         return false;
      }
      
      RelationalClassMapping classMapping = (RelationalClassMapping)mapping;
      
      return classMapping.m_sourceKey == m_sourceKey &&
         classMapping.m_destinationKey == m_destinationKey;
   }

   /**
    * @see nexj.core.meta.persistence.AttributeMapping#getMaxLength(Primitive)
    */
   public int getMaxLength(Primitive type)
   {
      return 0;
   }

   /**
    * Gets a key name.
    * @param key The key.
    * @return The key name.
    */
   protected static String getKeyName(Key key)
   {
      if (key instanceof Named)
      {
         return ((Named)key).getName();
      }

      return ObjUtil.getShortClassName(key);
   }
   
   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return ObjUtil.getShortClassName(this) + " for " + getAttribute() + " to " + getMapping();
   }
}
