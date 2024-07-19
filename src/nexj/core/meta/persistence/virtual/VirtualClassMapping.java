// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.virtual;

import nexj.core.meta.Attribute;
import nexj.core.meta.ContextMetadata;
import nexj.core.meta.Metaclass;
import nexj.core.meta.MetadataException;
import nexj.core.meta.Primitive;
import nexj.core.meta.persistence.AttributeMapping;
import nexj.core.meta.persistence.ClassMapping;
import nexj.core.meta.persistence.Key;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.ExceptionHolder;
import nexj.core.util.HashTab;
import nexj.core.util.LinkedHashTab;
import nexj.core.util.Lookup;

/**
 * Mapping for an association attribute in the virtual persistence adapter.
 */
public class VirtualClassMapping extends ClassMapping
{
   // associations

   /**
    * The association key of the source class.
    */
   protected Key m_sourceKey;

   /**
    * The association key of the destination class.
    */
   protected Key m_destinationKey;

   /**
    * The attributes on the associated class (and its associations) that
    * can be read homogeneously.
    * 
    * Key is always a Symbol.
    * Value may either be a Symbol (for a primitive attribute) or a Lookup
    * (for an association attribute).
    */
   protected Lookup m_compositionAttributeMap;

   // operations

   /**
    * Sets the attributes on the associated class that can be read homogeneously.
    * @param attributes List of attributes on the associated class.
    */
   public void setComposition(Pair attributes)
   {
      verifyNotReadOnly();

      if (attributes == null)
      {
         m_compositionAttributeMap = null;
      }
      else
      {
         m_compositionAttributeMap = new LinkedHashTab();
         getCompositionMap(attributes, m_compositionAttributeMap);
      }
   }

   /**
    * Converts a list of attributes to a composition map.
    * e.g. (a b (c d e) f) becomes: <Lookup(a=a, b=b, c=<Lookup(d=d, e=e)>, f=f)>
    * 
    * @param attributes The list of attributes.
    * @param attrMap The composition map.
    */
   protected static void getCompositionMap(Pair attributes, Lookup attrMap)
   {
      for (; attributes != null; attributes = attributes.getNext())
      {
         Object head = attributes.getHead();

         if (head instanceof Symbol)
         {
            attrMap.put(head, head);
         }
         else if (head instanceof Pair)
         {
            Pair subAttrList = (Pair)head;
            Lookup subAttrMap = new HashTab();

            if (!(subAttrList.getHead() instanceof Symbol))
            {
               throw new IllegalStateException();
            }

            attrMap.put(subAttrList.getHead(), subAttrMap);
            getCompositionMap(subAttrList.getNext(), subAttrMap);
         }
         else
         {
            throw new IllegalStateException();
         }
      }
   }

   /**
    * Converts a composition map to a list of attributes.
    * @param attrMap The composition map.
    * @return The list of attributes.
    */
   protected static Pair getAttributes(Lookup attrMap)
   {
      if (attrMap == null)
      {
         return null;
      }

      Pair list = null;
      Lookup.Iterator itr = attrMap.iterator();

      while (itr.hasNext())
      {
         Symbol attribute = (Symbol)itr.next();
         Object value = itr.getValue();

         if (value instanceof Lookup)
         {
            value = new Pair(attribute, getAttributes((Lookup)value));
         }

         list = new Pair(value, list);
      }

      return list;
   }

   /**
    * Looks up an attribute in the composition map. 
    * @param map The map in which to perform the lookup; null to look in the root map.
    * @param key The attribute to look up.
    * @return The lookup result; either the attribute symbol or a sub-map.
    */
   public Object findComposition(Lookup map, Attribute key)
   {
      if (map == null)
      {
         return m_compositionAttributeMap.get(key.getSymbol());
      }

      return map.get(key.getSymbol());
   }

   /**
    * @return True if the composition map is empty.
    */
   public boolean isEmptyComposition()
   {
      return m_compositionAttributeMap == null || m_compositionAttributeMap.size() == 0;
   }

   /**
    * Gets the list of attributes on the associated class that can be read homogeneously.
    * @return The list of attributes on the associated class that can be read homogeneously.
    */
   public Pair getCompositionAttributes()
   {
      return getAttributes(m_compositionAttributeMap);
   }

   /**
    * Sets the association source key.
    * @param sourceKey The source key.
    */
   public void setSourceKey(Key sourceKey)
   {
      verifyNotReadOnly();
      m_sourceKey = sourceKey;
   }

   /**
    * Sets the association destination key.
    * @param destinationKey The destination key.
    */
   public void setDestinationKey(Key destinationKey)
   {
      verifyNotReadOnly();
      m_destinationKey = destinationKey;
      destinationKey.addAttribute(m_attribute);
      destinationKey.setMapped();
   }

   /**
    * @see nexj.core.meta.persistence.ClassMapping#getKey(boolean)
    */
   public Key getKey(boolean bDestination)
   {
      if (bDestination)
      {
         return m_destinationKey;
      }

      return m_sourceKey;
   }

   /**
    * @see nexj.core.meta.persistence.ClassMapping#isInner()
    */
   public boolean isInner()
   {
      return m_destinationKey.isObjectKey() && !m_sourceKey.isObjectKey();
   }

   /**
    * @see nexj.core.meta.persistence.ClassMapping#isPure()
    */
   public boolean isPure()
   {
      return m_destinationKey.isObjectKey() || m_sourceKey.isObjectKey();
   }

   /**
    * @see nexj.core.meta.persistence.ClassMapping#isUnique()
    */
   public boolean isUnique()
   {
      return m_destinationKey.isObjectKey();
   }

   /**
    * @see nexj.core.meta.persistence.AttributeMapping#getMaxLength(nexj.core.meta.Primitive)
    */
   public int getMaxLength(Primitive type)
   {
      return 0;
   }

   /**
    * @see nexj.core.meta.persistence.AttributeMapping#isAliasOf(nexj.core.meta.persistence.AttributeMapping)
    */
   public boolean isAliasOf(AttributeMapping mapping)
   {
      if (!(mapping instanceof VirtualClassMapping))
      {
         return false;
      }

      VirtualClassMapping classMapping = (VirtualClassMapping)mapping;

      return classMapping.m_sourceKey == m_sourceKey &&
         classMapping.m_destinationKey == m_destinationKey;
   }

   /**
    * @see nexj.core.meta.persistence.AttributeMapping#isMultiplexed()
    */
   public boolean isMultiplexed()
   {
      return false;
   }

   /**
    * @see nexj.core.meta.MetadataObject#validate(nexj.core.meta.ContextMetadata, nexj.core.util.ExceptionHolder)
    */
   public void validate(ContextMetadata metadata, ExceptionHolder warnings)
   {
      super.validate(metadata, warnings);

      if (m_compositionAttributeMap != null)
      {
         validateCompositionMap(m_compositionAttributeMap, (Metaclass)m_attribute.getType());
      }
   }

   /**
    * Validates the composition attribute map.
    * @param attrMap The composition attribute map.
    * @param metaclass The attribute class.
    */
   private void validateCompositionMap(Lookup attrMap, Metaclass metaclass)
   {
      for (Lookup.Iterator itr = attrMap.iterator(); itr.hasNext();)
      {
         Symbol sym = (Symbol)itr.next();
         Object value = itr.getValue();
         Attribute attr = metaclass.findAttribute(sym);

         if (value instanceof Lookup)
         {
            if (attr == null || attr.getType().isPrimitive() ||
               !((metaclass = (Metaclass)attr.getType()).getPersistenceMapping() instanceof VirtualMapping))
            {
               throw new MetadataException("err.meta.persistence.virtual.invalidCompositeAttribute",
                  new Object[]{((Symbol)sym).getName(), m_attribute.getName(), m_attribute.getDeclarator().getName()});
            }

            validateCompositionMap((Lookup)value, metaclass);
         }
         else
         {
            if (attr == null || !(metaclass.getPersistenceMapping() instanceof VirtualMapping))
            {
               throw new MetadataException("err.meta.persistence.virtual.invalidCompositeAttribute",
                  new Object[]{((Symbol)sym).getName(), m_attribute.getName(), m_attribute.getDeclarator().getName()});
            }
         }
      }
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuilder buf = new StringBuilder(128);

      buf.append("Virtual class mapping for ");
      buf.append(m_attribute.getMetaclass().getName());
      buf.append('.');
      buf.append(m_attribute.getName());

      return buf.toString();
   }
}
