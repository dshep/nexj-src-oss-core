// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.virtual;

import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;
import java.util.Set;
import java.util.StringTokenizer;

import nexj.core.meta.Attribute;
import nexj.core.meta.ClassAspect;
import nexj.core.meta.Component;
import nexj.core.meta.ContextMetadata;
import nexj.core.meta.Metaclass;
import nexj.core.meta.MetadataException;
import nexj.core.meta.Primitive;
import nexj.core.meta.persistence.AttributeMapping;
import nexj.core.meta.persistence.GenericObjectKey;
import nexj.core.meta.persistence.GenericPersistenceMapping;
import nexj.core.meta.persistence.Key;
import nexj.core.meta.persistence.PersistenceMapping;
import nexj.core.persistence.virtual.VirtualAdapter;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.ExceptionHolder;
import nexj.core.util.HashHolder;
import nexj.core.util.IdentityHashTab;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;

/**
 * A persistence mapping for a class that is persisted to the virtual persistence adapter.
 */
public class VirtualMapping extends GenericPersistenceMapping
{
   // constants

   /**
    * The properties variable symbol.
    */
   public final static Symbol PROPERTIES = Symbol.define("properties");

   // attributes

   /**
    * True to inherit mapping scripts from the base class; false to ignore base mappings.
    */
   protected boolean m_bDerived;

   // associations

   /**
    * The primary key generator.
    */
   protected Component m_keyGenerator;

   /**
    * The text position map, used during parsing and compilation.
    */
   protected Lookup m_textPosMap = new IdentityHashTab();

   /**
    * The read mapping.
    */
   protected final ReadMapping m_readMapping = new ReadMapping();

   /**
    * The create mapping.
    */
   protected WorkMapping m_createMapping;

   /**
    * The update mapping.
    */
   protected UpdateMapping m_updateMapping;

   /**
    * The delete mapping.
    */
   protected WorkMapping m_deleteMapping;

   /**
    * The key describing the OID structure.
    */
   protected Key m_objectKey;

   /**
    * The sort keys for this class.
    */
   protected List m_sortKeyList = new ArrayList();

   /**
    * The logger for this mapping.
    */
   protected Logger m_logger;

   // operations

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
    * Gets the primary key generator.
    * @return The primary key generator component.
    */
   public Component getKeyGenerator()
   {
      return m_keyGenerator;
   }

   /**
    * @see nexj.core.meta.persistence.PersistenceMapping#setMetaclass(nexj.core.meta.Metaclass)
    */
   public void setMetaclass(Metaclass metaclass)
   {
      super.setMetaclass(metaclass);

      if (metaclass != null)
      {
         m_logger = Logger.getLogger(VirtualAdapter.class.getName() + '.' + metaclass.getName());
      }
   }

   /**
    * Gets the logger for this mapping.
    * @return The logger.
    */
   public Logger getLogger()
   {
      return m_logger;
   }

   /**
    * Sets the create mapping.
    * @param mapping The create mapping.
    */
   public void setCreateMapping(WorkMapping mapping)
   {
      verifyNotReadOnly();
      m_createMapping = mapping;
   }

   /**
    * Gets the create mapping.
    * @return The create mapping.
    */
   public WorkMapping getCreateMapping()
   {
      return m_createMapping;
   }

   /**
    * Sets the delete mapping.
    * @param mapping The delete mapping.
    */
   public void setDeleteMapping(WorkMapping mapping)
   {
      verifyNotReadOnly();
      m_deleteMapping = mapping;
   }

   /**
    * Gets the delete mapping.
    * @return The delete mapping.
    */
   public WorkMapping getDeleteMapping()
   {
      return m_deleteMapping;
   }

   /**
    * Gets the read mapping.
    * @return The read mapping. Not null.
    */
   public ReadMapping getReadMapping()
   {
      return m_readMapping;
   }

   /**
    * Sets the update mapping.
    * @param mapping The update mapping.
    */
   public void setUpdateMapping(UpdateMapping mapping)
   {
      verifyNotReadOnly();
      m_updateMapping = mapping;
   }

   /**
    * Gets the update mapping.
    * @return The update mapping.
    */
   public UpdateMapping getUpdateMapping()
   {
      return m_updateMapping;
   }

   /**
    * Compiles the virtual mapping code.
    * @param machine The virtual machine for compilation.
    */
   public void compile(Machine machine)
   {
      String sURLPrefix = "class:" + m_metaclass.getName() + ".persistence";

      try
      {
         machine.getGlobalEnvironment().defineVariable(Symbol.SYS_CURRENT_LOGGER, m_logger);

         m_readMapping.compile(machine, sURLPrefix + ".read", m_textPosMap);

         if (m_deleteMapping != null && m_metaclass == m_deleteMapping.getDeclarator())
         {
            m_deleteMapping.compile(machine, sURLPrefix + ".delete", m_textPosMap);
         }

         if (m_createMapping != null && m_metaclass == m_createMapping.getDeclarator())
         {
            m_createMapping.compile(machine, sURLPrefix + ".create", m_textPosMap);
         }

         if (m_updateMapping != null)
         {
            m_updateMapping.compile(machine, m_textPosMap);
         }
      }
      finally
      {
         m_textPosMap = null;
         machine.getGlobalEnvironment().removeVariable(Symbol.SYS_CURRENT_LOGGER);
      }
   }

   /**
    * Gets the position map, used during parsing and compilation.
    * @return The body text position map.
    */
   public Lookup getTextPositionMap()
   {
      return m_textPosMap;
   }

   /**
    * Compatible iff the classes are in the same inheritance hierarchy, they are on the same data source,
    * and the derived class is derived (transitively) from the base class.
    * @see nexj.core.meta.persistence.PersistenceMapping#isCompatible(nexj.core.meta.persistence.PersistenceMapping)
    */
   public boolean isCompatible(PersistenceMapping mapping)
   {
      if (mapping == this)
      {
         return true;
      }

      if (!(mapping instanceof VirtualMapping))
      {
         return false;
      }

      VirtualMapping other = (VirtualMapping)mapping;

      if (compatibleMappings(m_metaclass, other.getMetaclass()))
      {
    	  return true;
      }

      return compatibleMappings(other.getMetaclass(), m_metaclass);
   }

   /**
    * Helper function that compares two virtually-persisted metaclasses to see if their mappings
    * are compatible.
    * @param potentialBase The base class.
    * @param potentialDerived The derived class.
    * @return True if the mappings are compatible.
    */
   protected static boolean compatibleMappings(Metaclass potentialBase, Metaclass potentialDerived)
   {
      VirtualMapping baseMapping = (VirtualMapping)potentialBase.getPersistenceMapping();

      while (potentialDerived != null)
      {
         if (potentialDerived == potentialBase)
         {
            return true;
         }

         if (!(potentialDerived.getPersistenceMapping() instanceof VirtualMapping))
         {
            return false;
         }

         VirtualMapping otherMapping = (VirtualMapping)potentialDerived.getPersistenceMapping();

         if (otherMapping.getDataSource() != baseMapping.getDataSource())
         {
            return false;
         }

         if (!otherMapping.isDerived())
         {
            return false;
         }

         potentialDerived = potentialDerived.getBase();
      }

      return false;
   }

  /**
    * Sets the derived flag.
    * @param bDerived True to inherit mapping scripts from the base class; false to ignore base mappings.
    */
   public void setDerived(boolean bDerived)
   {
      verifyNotReadOnly();
      m_bDerived = bDerived;
   }

   /**
    * Gets the derived flag.
    * @return True to inherit mapping scripts from the base class; false to ignore base mappings.
    */
   public boolean isDerived()
   {
      return m_bDerived;
   }

   /**
    * @see nexj.core.meta.persistence.PersistenceMapping#deriveAttributeMapping(nexj.core.meta.Attribute, nexj.core.meta.Attribute, nexj.core.meta.persistence.AttributeMapping)
    */
   protected void deriveAttributeMapping(Attribute attribute, Attribute base, AttributeMapping baseMapping)
   {
      if (attribute.getType() == base.getType())
      {
         if (getAttributeMapping(attribute) == null)
         {
            AttributeMapping mapping = (AttributeMapping)baseMapping.clone();

            mapping.setAttribute(attribute);
            addAttributeMapping(mapping);
         }
      }
   }

   /**
    * @see nexj.core.meta.persistence.PersistenceMapping#resolveInheritance()
    */
   public void resolveInheritance() throws MetadataException
   {
      PersistenceMapping baseMapping = getBaseMapping();

      /*
       * Inherit the persistence mappings
       */
      if (baseMapping != null && baseMapping.getDataSource() == m_dataSource)
      {
         Metaclass baseClass = baseMapping.getMetaclass();

         // Inherit attribute mappings
         for (int i = 0, nCount = baseClass.getInstanceAttributeCount(); i < nCount; i++)
         {
            Attribute baseAttribute = baseClass.getInstanceAttribute(i);
            AttributeMapping baseAttrMapping = baseMapping.getAttributeMapping(baseAttribute);

            if (baseAttrMapping != null)
            {
               Attribute derivedAttribute = m_metaclass.getDerivedAttribute(baseAttribute);
               AttributeMapping derivedAttrMapping = getAttributeMapping(derivedAttribute);

               if (derivedAttrMapping == null)
               {
                  if (derivedAttribute.getType() == baseAttribute.getType())
                  {
                     derivedAttrMapping = (AttributeMapping)baseAttrMapping.clone(derivedAttribute);
                     derivedAttrMapping.setAttribute(derivedAttribute);
                     addAttributeMapping(derivedAttrMapping);
                  }
               }
            }
         }

         // Inherit mapping scripts
         if (m_bDerived)
         {
            resolveScriptInheritance((VirtualMapping)baseMapping);
         }
      }

      if (m_bDerived)
      {
         for (int i = 0, nCount = m_metaclass.getAspectCount(); i < nCount; i++)
         {
            ClassAspect aspect = (ClassAspect)m_metaclass.getAspect(i);
            VirtualMapping mapping = (VirtualMapping)aspect.findPersistenceMapping(m_dataSource);

            if (mapping != null)
            {
               resolveScriptInheritance(mapping);
            }
         }
      }

      super.resolveInheritance();

      // Only filter if requested (provide "class" variable to read mappings instead)
      m_bTypeCodeFiltered = m_bTypeCodeForced;
   }

   /**
    * Resolves inheritance for the mapping scripts.
    * @param base The base mapping.
    */
   protected void resolveScriptInheritance(VirtualMapping base)
   {
      if (m_createMapping == null)
      {
         m_createMapping = base.m_createMapping;
      }

      if (m_deleteMapping == null)
      {
         m_deleteMapping = base.m_deleteMapping;
      }

      m_readMapping.resolveInheritance(base.m_readMapping);

      if (m_updateMapping == null)
      {
         m_updateMapping = base.m_updateMapping;
      }
      else
      {
         m_updateMapping.resolveInheritance(base.m_updateMapping);
      }

      for (Lookup.Iterator it = base.m_textPosMap.iterator(); it.hasNext();)
      {
         it.next();
         m_textPosMap.put(it.getKey(), it.getValue());
      }
   }

   /**
    * @see nexj.core.meta.persistence.PersistenceMapping#resolveInheritance2()
    */
   public void resolveInheritance2()
   {
      super.resolveInheritance2();

      PersistenceMapping baseMapping = getBaseMapping();

      // Inherit the key generator
      if (baseMapping != null && baseMapping.getDataSource() == m_dataSource)
      {
         VirtualMapping mapping = (VirtualMapping)baseMapping;

         if (m_keyGenerator == null)
         {
            m_keyGenerator = mapping.getKeyGenerator();
         }
      }
   }

   /**
    * @see nexj.core.meta.persistence.GenericPersistenceMapping#getForeignKey(java.lang.String)
    */
   protected Key getForeignKey(String sName) throws MetadataException
   {
      StringTokenizer tok = new StringTokenizer(sName);

      if (!tok.hasMoreTokens())
      {
         throw new MetadataException("err.meta.persistence.virtual.invalidAttributeKey",
            new Object[]{sName, m_metaclass.getName()});
      }

      Attribute attr = m_metaclass.getAttribute(tok.nextToken());

      if (!tok.hasMoreTokens())
      {
         return new VirtualKey(attr);
      }

      ArrayList attrList = new ArrayList(4);

      do
      {
         if (!attr.getType().isPrimitive())
         {
            throw new MetadataException("err.meta.persistence.virtual.invalidAttributeKey",
               new Object[]{attr.getName(), m_metaclass.getName()});
         }

         attrList.add(attr);
         attr = (tok.hasMoreTokens()) ? m_metaclass.getAttribute(tok.nextToken()) : null;
      }
      while (attr != null);

      return new VirtualKey((Attribute[])attrList.toArray(new Attribute[attrList.size()]));
   }

   /**
    * @see nexj.core.meta.persistence.PersistenceMapping#create()
    */
   public PersistenceMapping create()
   {
      VirtualMapping mapping = new VirtualMapping();

      mapping.setDerived(true);

      return mapping;
   }

   /**
    * @see nexj.core.meta.persistence.PersistenceMapping#getObjectKey()
    */
   public Key getObjectKey()
   {
      if (m_objectKey == null)
      {
         PersistenceMapping baseMapping = getBaseMapping();

         if (baseMapping != null && baseMapping.getDataSource() == m_dataSource)
         {
            Key baseKey = baseMapping.getObjectKey();

            if (baseKey != null)
            {
               return m_objectKey = baseKey;
            }
         }

         throw new MetadataException("err.meta.persistence.virtual.emptyObjectKey",
               new Object[] {getMetaclass()});
      }

      return m_objectKey;
   }

   /**
    * Sets the types of the OID parts.
    * @param typeArray The types of the OID parts.
    */
   public void setKey(Primitive[] typeArray)
   {
      verifyNotReadOnly();
      m_objectKey = new GenericObjectKey(typeArray);
   }

   /**
    * Adds a sort key.
    * @param key The key definition.
    * @param bUnique True if the key is unique; false otherwise.
    */
   public void addSortKey(Pair key, boolean bUnique)
   {
      m_sortKeyList.add(new VirtualSortKey(key, bUnique, m_metaclass));
   }

   /**
    * Gets the list of sort keys.
    * @return A list of VirtualSortKey.
    */
   public List getSortKeys()
   {
      return m_sortKeyList;
   }

   /**
    * @see nexj.core.meta.persistence.PersistenceMapping#getSortKeys(nexj.core.meta.Attribute[], nexj.core.meta.persistence.PersistenceMapping[], nexj.core.meta.Attribute[])
    */
   public Pair getSortKeys(Attribute[] assocs, PersistenceMapping[] mappings, Attribute[] restrictions)
   {
      Pair sortKeys = null;
      Pair lastKey = null;
      BitSet filterSet = null;
      BitSet optionalSet = null;
      int nFilterCount = 0;

      // Create a set of attributes that participate in equality
      // comparisons due to the associations.
      if (assocs != null && assocs.length > 0)
      {
         filterSet = new BitSet(m_metaclass.getInstanceAttributeCount());

         for (int i = 0; i < assocs.length; i++)
         {
            PersistenceMapping mapping = mappings[i];

            if (mapping == null)
            {
               continue;
            }

            AttributeMapping attributeMapping = mapping.getAttributeMapping(assocs[i]);

            if (attributeMapping instanceof VirtualClassMapping)
            {
               VirtualClassMapping classMapping = (VirtualClassMapping)attributeMapping;

               if (classMapping.getMapping() == this && classMapping.getKey(true) instanceof VirtualKey)
               {
                  VirtualKey key = (VirtualKey)classMapping.getKey(true);

                  if (!key.isUnique())
                  {
                     for (int k = 0, nCount = key.getAttributeCount(); k < nCount; k++)
                     {
                        Attribute attr = key.getAttribute(k);

                        assert attr.getMetaclass() == m_metaclass;
                        assert !attr.getType().isPrimitive();

                        filterSet.set(attr.getOrdinal());
                     }
                  }
               }
            }
         }

         nFilterCount = filterSet.cardinality();
      }

      // Create a set of attributes that participate in equality
      // comparisons and can be optionally excluded from the index.
      if (restrictions != null && restrictions.length > 0)
      {
         optionalSet = new BitSet(m_metaclass.getInstanceAttributeCount());

         for (int i = 0; i < restrictions.length; i++)
         {
            optionalSet.set(restrictions[i].getOrdinal());
         }
      }

      // Collect the sort keys
      for (int nSortKey = 0; nSortKey < m_sortKeyList.size(); nSortKey++)
      {
         VirtualSortKey sortKey = (VirtualSortKey)m_sortKeyList.get(nSortKey);

         // Ignore sort keys that do not start with the columns specified in the
         // association filter, in arbitrary order
         if (nFilterCount > 0)
         {
            if (sortKey.getAttributeCount() < nFilterCount)
            {
               continue;
            }

            int i;

            for (i = 0; i < nFilterCount; i++)
            {
               Attribute attr = sortKey.getAttribute(i);

               if (attr == null || !filterSet.get(attr.getOrdinal()))
               {
                  break;
               }
            }

            if (i < nFilterCount)
            {
               continue;
            }
         }

         // Find out if the sort key includes the primary key
         Key pk = getObjectKey();
         boolean bMatch = false;
         boolean bAscending = true;

         for (int i = 0, k = -1; i < sortKey.getAttributeCount(); i++)
         {
            Attribute attr = sortKey.getAttribute(i);

            if (attr == null)
            {
               bMatch = true;
               bAscending = sortKey.isAscending(i);

               break;
            }

            if (optionalSet != null && optionalSet.get(attr.getOrdinal()))
            {
               continue;
            }

            if (getAttributeMapping(attr) instanceof VirtualPrimitiveMapping)
            {
               VirtualPrimitiveMapping mapping = (VirtualPrimitiveMapping)getAttributeMapping(attr);

               if (mapping.getObjectKeyPart() >= 0)
               {
                  if (k == -1)
                  {
                     if (mapping.getObjectKeyPart() == 0)
                     {
                        k = 1;
                        bAscending = sortKey.isAscending(i);

                        continue;
                     }

                     break;
                  }

                  if (mapping.getObjectKeyPart() == k)
                  {
                     if ((pk.isPartAscending(0) ^ pk.isPartAscending(k)) != (bAscending ^ sortKey.isAscending(i)))
                     {
                        break;
                     }
                     else
                     {
                        if (k == pk.getPartCount() - 1)
                        {
                           bMatch = true;

                           break;
                        }
                     }
                  }
                  else
                  {
                     break;
                  }
               }
            }
         }

         Pair key = null;

         // If the above is true, then append the primary key
         // at the end of the sort key
         if (bMatch)
         {
            Pair[] objKeyAttributeArray = new Pair[pk.getPartCount()];

            for (int i = 0; i < m_metaclass.getInstanceAttributeCount(); i++)
            {
               Attribute attr = m_metaclass.getInstanceAttribute(i);

               if (getAttributeMapping(attr) instanceof VirtualPrimitiveMapping)
               {
                  VirtualPrimitiveMapping mapping = (VirtualPrimitiveMapping)getAttributeMapping(attr);

                  if (mapping.getObjectKeyPart() >= 0)
                  {
                     objKeyAttributeArray[mapping.getObjectKeyPart()] = new Pair(
                        attr.getSymbol(),
                        Boolean.valueOf(bAscending ^ !pk.isPartAscending(mapping.getObjectKeyPart()))
                     );
                  }
               }
            }

            boolean bObjKeyFullyMapped = true;

            for (int i = 0; i < objKeyAttributeArray.length; i++)
            {
               if (objKeyAttributeArray[i] == null)
               {
                  bObjKeyFullyMapped = false;

                  break;
               }
            }

            if (bObjKeyFullyMapped)
            {
               key = Pair.fromArray(objKeyAttributeArray);
            }
            else
            {
               key = new Pair(new Pair(new Pair(Symbol.AT),
                  Boolean.valueOf(bAscending)));
            }
         }
         else
         {
            pk = null;
         }

         int nCount;

         // Advance, stopping at first non-ignorable non-primitive-mapped attribute
         for (nCount = nFilterCount; nCount < sortKey.getAttributeCount(); nCount++)
         {
            Attribute attr = sortKey.getAttribute(nCount);

            if (attr == null || !attr.getType().isPrimitive())
            {
               if (optionalSet == null || attr == null || !optionalSet.get(attr.getOrdinal()))
               {
                  break;
               }
            }
         }

         bMatch = true;

         // Verify that remainder of index is the primary key, or that there is no remaining index
         if (pk != null)
         {
            if (nCount == sortKey.getAttributeCount())
            {
               key = null;
            }
            else
            {
               for (int i = nCount; i < sortKey.getAttributeCount(); i++)
               {
                  Attribute attr = sortKey.getAttribute(i);

                  if (attr != null)
                  {
                     AttributeMapping mapping = getAttributeMapping(attr);

                     if (mapping instanceof VirtualPrimitiveMapping)
                     {
                        if (((VirtualPrimitiveMapping)mapping).getObjectKeyPart() >= 0)
                        {
                           continue;
                        }
                     }

                     bMatch = false;

                     break;
                  }
               }
            }
         }

         if (!bMatch)
         {
            continue;
         }

         // Add non-ignorable primitive-mapped columns to the sort key
         for (int i = nCount - 1; i >= nFilterCount; i--)
         {
            Attribute attr = sortKey.getAttribute(i);

            if (attr.getType().isPrimitive())
            {
               if (optionalSet == null || !optionalSet.get(attr.getOrdinal()))
               {
                  key = new Pair(
                     new Pair(attr.getSymbol(), Boolean.valueOf(sortKey.isAscending(i))),
                     key
                  );
               }
            }
         }

         // Append the new sort key to the end of the sort key list
         if (key != null)
         {
            key = new Pair(
               Boolean.valueOf(pk != null || sortKey.isUnique() && nCount == sortKey.getAttributeCount()),
               key
            );

            if (lastKey == null)
            {
               sortKeys = lastKey = new Pair(key);
            }
            else
            {
               Pair pair;

               // Filter out the duplicates
               for (pair = sortKeys; pair != null; pair = pair.getNext())
               {
                  if (pair.getHead().equals(key))
                  {
                     break;
                  }
               }

               if (pair == null)
               {
                  key = new Pair(key);
                  lastKey.setTail(key);
                  lastKey = key;
               }
            }
         }
      }

      return sortKeys;
   }

   /**
    * @see nexj.core.meta.persistence.PersistenceMapping#getUniqueKeys()
    */
   public Pair getUniqueKeys()
   {
      Set keySet = new HashHolder(m_sortKeyList.size());  // eliminates duplicates
      BitSet keyAttrSet = new BitSet(m_metaclass.getInstanceAttributeCount());
      Pair uniqueKeys = null;

      for (int nSortKey = m_sortKeyList.size() - 1; nSortKey >= 0; nSortKey--)
      {
         VirtualSortKey sortKey = (VirtualSortKey)m_sortKeyList.get(nSortKey);

         if (!sortKey.isUnique())
         {
            continue;
         }

         for (int i = sortKey.getAttributeCount() - 1; i >= 0 ; i--)
         {
            Attribute attr = sortKey.getAttribute(i);

            if (attr == null)
            {
               keyAttrSet.clear();

               break;
            }
            else
            {
               AttributeMapping mapping = getAttributeMapping(attr);

               if (mapping instanceof VirtualPrimitiveMapping)
               {
                  if (((VirtualPrimitiveMapping)mapping).getObjectKeyPart() >= 0)
                  {
                     keyAttrSet.clear();

                     break;
                  }
               }

               keyAttrSet.set(attr.getOrdinal());
            }
         }

         if (!keyAttrSet.isEmpty() && keySet.add(keyAttrSet))
         {
            Pair key = null;

            for (int i = keyAttrSet.nextSetBit(0); i >= 0; i = keyAttrSet.nextSetBit(i + 1))
            {
               key = new Pair(m_metaclass.getInstanceAttribute(i).getSymbol(), key);
            }

            uniqueKeys = new Pair(key, uniqueKeys);
            keyAttrSet.clear();
         }
      }

      return uniqueKeys;
   }

   /**
    * @see nexj.core.meta.MetadataObject#validate(nexj.core.meta.ContextMetadata, nexj.core.util.ExceptionHolder)
    */
   public void validate(ContextMetadata metadata, ExceptionHolder warnings)
   {
      super.validate(metadata, warnings);

      if (m_objectKey == null || m_objectKey.getPartCount() == 0)
      {
         throw new MetadataException("err.meta.persistence.virtual.emptyObjectKey",
            new Object[] {getMetaclass()});
      }
   }

   /**
    * @see nexj.core.meta.MetadataObject#makeReadOnly()
    */
   public void makeReadOnly()
   {
      super.makeReadOnly();

      if (m_createMapping != null)
      {
         m_createMapping.makeReadOnly();
      }

      m_readMapping.makeReadOnly();

      if (m_updateMapping != null)
      {
         m_updateMapping.makeReadOnly();
      }

      if (m_deleteMapping != null)
      {
         m_deleteMapping.makeReadOnly();
      }
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return ObjUtil.getShortClassName(this) + " on " + m_metaclass;
   }
}
