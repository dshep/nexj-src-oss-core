// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nexj.core.meta.Attribute;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataObject;
import nexj.core.meta.Primitive;
import nexj.core.util.EmptyIterator;
import nexj.core.util.ObjUtil;

/**
 * The mapping descriptor of an individual attribute.
 */
public abstract class AttributeMapping extends MetadataObject implements Cloneable
{
   // associations

   /**
    * The containing persistence mapping.
    */
   protected PersistenceMapping m_persistenceMapping;

   /**
    * The attribute to which the mapping applies.
    */
   protected Attribute m_attribute;

   /**
    * The attribute denorm collection.
    */
   protected List m_denormList ; // of type AttributeDenorm

   // operations
   
   /**
    * Sets the containing persistence mapping.
    * @param persistenceMapping The containing persistence mapping to set.
    */
   public void setPersistenceMapping(PersistenceMapping persistenceMapping)
   {
      verifyNotReadOnly();
      m_persistenceMapping = persistenceMapping;
   }

   /**
    * @return The containing persistence mapping.
    */
   public PersistenceMapping getPersistenceMapping()
   {
      return m_persistenceMapping;
   }
   
   /**
    * Sets the associated attribute.
    * @param attribute The associated attribute to set.
    */
   public void setAttribute(Attribute attribute)
   {
      verifyNotReadOnly();
      m_attribute = attribute;

      if (attribute != null && attribute.isStatic())
      {
         throw new MetadataException("err.meta.persistence.staticAttribute",
            new Object[]{attribute.getName(), attribute.getMetaclass().getName()});
      }
   }

   /**
    * @return The associated attribute.
    */
   public Attribute getAttribute()
   {
      return m_attribute;
   }

   /**
    * Adds a new attribute denorm to the attribute mapping.
    * @param denorm The attribute denorm to add.
    */
   public void addDenorm(AttributeDenorm denorm)
   {
      verifyNotReadOnly();
      
      if (m_denormList == null)
      {
         m_denormList = new ArrayList(2);
      }

      if (!m_denormList.contains(denorm))
      {
         m_denormList.add(denorm);
         denorm.setMapping(this);
      }
   }

   /**
    * Gets an attribute denorm by ordinal number.
    * @param nOrdinal The denorm ordinal number (0-based).
    * @return The denorm object.
    */
   public AttributeDenorm getDenorm(int nOrdinal)
   {
      return (AttributeDenorm)m_denormList.get(nOrdinal);
   }

   /**
    * @return The attribute denorm count.
    */
   public int getDenormCount()
   {
      if (m_denormList == null)
      {
         return 0;
      }

      return m_denormList.size();
   }

   /**
    * @return An iterator for the contained attribute denorm objects.
    */
   public Iterator getDenormIterator()
   {
      if (m_denormList == null)
      {
         return EmptyIterator.getInstance();
      }

      return m_denormList.iterator();
   }

   /**
    * @return True if the attribute is denormalized.
    */
   public boolean isDenormalized()
   {
      return m_denormList != null && !m_denormList.isEmpty();
   }
   
   /**
    * Is underlying persistent storage reused for mapping to two or more
    * classes from different hierarchies (i.e. not deriving directly or
    * indirectly from each other). 
    * 
    * @return True if the underlying persistence storage item is multiplexed.
    */
   public abstract boolean isMultiplexed();

   /**
    * Determines if this mapping is an alias of another mapping.
    * This relation is reflexive, symmetric and transitive.
    * @param mapping The other mapping.
    * @return True if this mapping is an alias of another mapping.
    */
   public abstract boolean isAliasOf(AttributeMapping mapping);

   /**
    * @param type The mapped attribute type.
    * @return The maximum attribute data length. 0 for unlimited.
    */
   public abstract int getMaxLength(Primitive type);

   /**
    * @see nexj.core.meta.MetadataObject#clone()
    */
   public Object clone()
   {
      AttributeMapping mapping = (AttributeMapping)super.clone();
      int nDenormCount = getDenormCount();

      if (nDenormCount != 0)
      {
         List denormList = new ArrayList(nDenormCount);

         for (int i = 0; i < nDenormCount; ++i)
         {
            AttributeDenorm denorm = (AttributeDenorm)getDenorm(i).clone();

            denorm.setMapping(mapping);
            denormList.add(denorm);
         }

         mapping.m_denormList = denormList;
      }

      return mapping;
   }

   /**
    * Clones the mapping for a given attribute.
    * @param attribute The new attribute.
    * @return The new mapping. 
    */
   public AttributeMapping clone(Attribute attribute)
   {
      AttributeMapping mapping = (AttributeMapping)clone();

      mapping.m_attribute = attribute;

      return mapping;
   }

   /**
    * @see nexj.core.meta.MetadataObject#makeReadOnly()
    */
   public void makeReadOnly()
   {
      super.makeReadOnly();

      if (m_denormList instanceof ArrayList)
      {
         ((ArrayList)m_denormList).trimToSize();
      }
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return ObjUtil.getShortClassName(this) + " for " + getAttribute();
   }
}