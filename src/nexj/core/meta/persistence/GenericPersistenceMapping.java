package nexj.core.meta.persistence;

import nexj.core.meta.Attribute;
import nexj.core.meta.MetadataException;
import nexj.core.scripting.Pair;
import nexj.core.util.ObjUtil;
import nexj.core.util.StringUtil;

/**
 * Generic persistence mapping implementation with support for attribute mapping storage.
 */
public abstract class GenericPersistenceMapping extends PersistenceMapping
{
   // associations

   /**
    * The attribute mapping array, the index corresponds to the attribute ordinal.
    */
   protected AttributeMapping[] m_mappingArray;

   // operations

   /**
    * @see nexj.core.meta.persistence.PersistenceMapping#addAttributeMapping(nexj.core.meta.persistence.AttributeMapping)
    */
   public void addAttributeMapping(AttributeMapping mapping)
   {
      verifyNotReadOnly();

      Attribute attribute = mapping.getAttribute();

      if (m_mappingArray == null)
      {
         m_mappingArray = new AttributeMapping[attribute.getMetaclass().getInstanceAttributeCount()];
      }

      if (m_mappingArray[attribute.getOrdinal()] != null)
      {
         throw new MetadataException("err.meta.attributeMappingDup",
            new Object[]{attribute.getName(), getMetaclass().getName()});
      }

      m_mappingArray[attribute.getOrdinal()] = mapping;
      mapping.setPersistenceMapping(this);
   }

   /**
    * @see nexj.core.meta.persistence.PersistenceMapping#getObjectKey()
    */
   public Key getObjectKey()
   {
      return null;
   }

   /**
    * @see nexj.core.meta.persistence.PersistenceMapping#getSortKeys(nexj.core.meta.Attribute[], nexj.core.meta.persistence.PersistenceMapping[], nexj.core.meta.Attribute[])
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
    * @see nexj.core.meta.persistence.PersistenceMapping#getAttributeMapping(nexj.core.meta.Attribute)
    */
   public AttributeMapping getAttributeMapping(Attribute attribute)
   {
      assert attribute.getMetaclass().isUpcast(m_metaclass) ||
         m_metaclass.isUpcast(attribute.getMetaclass());

      if (m_mappingArray == null || attribute.isStatic())
      {
         return null;
      }

      return m_mappingArray[attribute.getOrdinal()];
   }

   /**
    * @see nexj.core.meta.persistence.PersistenceMapping#addForeignKey(java.lang.String, nexj.core.meta.persistence.ClassMapping)
    */
   public Key addForeignKey(String sName, ClassMapping mapping) throws MetadataException
   {
      if (mapping != null)
      {
         mapping.setMapping(this);
      }

      return (StringUtil.isEmpty(sName)) ? getObjectKey() : getForeignKey(sName);
   }

   /**
    * Gets a foreign key by name.
    * @param sName The foreign key name.
    * @return The foreign key.
    * @throws MetadataException if the key cannot be found.
    */
   protected Key getForeignKey(String sName) throws MetadataException
   {
      throw new UnsupportedOperationException();
   }

   /**
    * @see nexj.core.meta.MetadataObject#makeReadOnly()
    */
   public void makeReadOnly()
   {
      if (m_mappingArray != null)
      {
         for (int i = 0; i < m_mappingArray.length; ++i)
         {
            if (m_mappingArray[i] != null)
            {
               m_mappingArray[i].makeReadOnly();
            }
         }
      }

      super.makeReadOnly();
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return ObjUtil.getShortClassName(this) + ' ' + getName() + " for " + getMetaclass();
   }
}
