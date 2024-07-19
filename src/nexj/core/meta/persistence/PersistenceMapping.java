// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence;

import nexj.core.meta.Attribute;
import nexj.core.meta.ClassAspect;
import nexj.core.meta.Component;
import nexj.core.meta.Metaclass;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataLookupException;
import nexj.core.meta.MetadataMarker;
import nexj.core.meta.MetadataObject;
import nexj.core.meta.MetadataValidationException;
import nexj.core.meta.Primitive;
import nexj.core.meta.PrimitivePrivilege;
import nexj.core.meta.PrivilegeSet;
import nexj.core.persistence.PersistenceHook;
import nexj.core.scripting.Pair;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.Named;
import nexj.core.util.ObjUtil;
import nexj.core.util.UncheckedException;
import nexj.core.util.Undefined;

/**
 * Metadata describing the mapping between the metaclass attributes
 * and the underlying persistent storage.
 */
public abstract class PersistenceMapping extends MetadataObject implements Named
{
   // constants

   /**
    * No caching.
    */
   public final static byte CACHING_NONE = 0;

   /**
    * Instance caching.
    */
   public final static byte CACHING_INSTANCE = 1;

   /**
    * Class caching.
    */
   public final static byte CACHING_CLASS = 2;

   /**
    * No fragment replication.
    */
   public final static byte REPLICATION_NONE = 0;

   /**
    * Unicast fragment replication - default fragment to/from the designated fragment.
    */
   public final static byte REPLICATION_UNICAST = 1;

   /**
    * Broadcast fragment replication - default fragment to all other fragments.
    */
   public final static byte REPLICATION_BROADCAST = 2;

   // attributes

   /**
    * True if a query for this class should be filtered by type code.
    */
   protected boolean m_bTypeCodeFiltered;

   /**
    * True if the class object is determined by the type code.
    */
   protected boolean m_bTypeCodeDispatched;

   /**
    * True if the type code filtering should be always applied.
    */
   protected boolean m_bTypeCodeForced;

   /**
    * True if the read privilege varies between this class
    * and any of the subclasses.
    */
   protected boolean m_bTypeCodePrivileged;

   /**
    * The caching mode (one of the CACHING_* constants).
    */
   protected byte m_nCaching;

   /**
    * The fragment replication model (one of the REPLICATION_* constants).
    */
   protected byte m_nFragmentReplication;

   // associations

   /**
    * The data source.
    */
   protected DataSource m_dataSource;

   /**
    * The metaclass for which this persistence mapping applies.
    */
   protected Metaclass m_metaclass;

   /**
    * The attribute used for optimistic locking. Can be null.
    */
   protected Attribute m_lockingAttribute;

   /**
    * The type code attribute. Can be null.
    */
   protected Attribute m_typeCodeAttribute;
   
   /**
    * Map of type code value to a derived class: Metaclass[Object].
    */
   protected TypeCodeMap m_typeCodeMap;

   /**
    * The fragment name attribute.
    */
   protected Attribute m_fragmentAttribute;

   /**
    * The persistence hook component.
    */
   protected Component m_hook;

   // operations

   /**
    * Sets the data source.
    * @param dataSource The data source to set.
    */
   public void setDataSource(DataSource dataSource)
   {
      verifyNotReadOnly();
      m_dataSource = dataSource;
   }

   /**
    * @return The data source.
    */
   public DataSource getDataSource()
   {
      return m_dataSource;
   }

   /**
    * @see nexj.core.util.Named#getName()
    */
   public String getName()
   {
      return m_dataSource.getName();
   }

   /**
    * Sets the containing metaclass.
    * @param metaclass The containing metaclass to set.
    */
   public void setMetaclass(Metaclass metaclass)
   {
      verifyNotReadOnly();
      m_metaclass = metaclass;
   }

   /**
    * @return The containing metaclass.
    */
   public Metaclass getMetaclass()
   {
      return m_metaclass;
   }
   
   /**
    * Sets the optimistic locking attribute.
    * @param lockingAttribute The optimistic locking attribute to set.
    */
   public void setLockingAttribute(Attribute lockingAttribute)
   {
      verifyNotReadOnly();
      m_lockingAttribute = lockingAttribute;
      
      if (lockingAttribute != null &&
         lockingAttribute.getDeclarator() == lockingAttribute.getMetaclass())
      {
         if (!lockingAttribute.getType().isPrimitive())
         {
            throw new MetadataException("err.meta.lockingAttributeType",
               new Object[]{lockingAttribute.getName(), m_metaclass.getName()});
         }

         if (!lockingAttribute.isRequired())
         {
            throw new MetadataException("err.meta.lockingAttributeNonRequired",
               new Object[]{lockingAttribute.getName(), m_metaclass.getName()});
         }

         if (lockingAttribute.getInitializer() == Undefined.VALUE)
         {
            throw new MetadataException("err.meta.lockingAttributeUninitialized",
               new Object[]{lockingAttribute.getName(), m_metaclass.getName()});
         }
      }
   }

   /**
    * @return The optimistic locking attribute.
    */
   public Attribute getLockingAttribute()
   {
      return m_lockingAttribute;
   }
   
   /**
    * Sets the type code attribute.
    * @param typeCodeAttribute The type code attribute to set.
    */
   public void setTypeCodeAttribute(Attribute typeCodeAttribute)
   {
      verifyNotReadOnly();
      m_typeCodeAttribute = typeCodeAttribute;
      
      if (typeCodeAttribute != null)
      {
         if (!typeCodeAttribute.getType().isPrimitive() &&
            typeCodeAttribute.getDeclarator() == typeCodeAttribute.getMetaclass())
         {
            throw new MetadataException("err.meta.typeCodeAttrType", 
               new Object[]{typeCodeAttribute.getName(), m_metaclass.getName()});
         }
         
         if (!typeCodeAttribute.isRequired())
         {
            throw new MetadataException("err.meta.typeCodeAttrNonRequired", 
               new Object[]{typeCodeAttribute.getName(), m_metaclass.getName()});
         }

         if (typeCodeAttribute.getInitializer() != Undefined.VALUE && 
            typeCodeAttribute.getValue() != Undefined.VALUE)
         {
            throw new MetadataException("err.meta.typeCodeAttrInitializer",
               new Object[]{typeCodeAttribute.getName(), m_metaclass.getName()});
         }
      }
   }

   /**
    * @return The type code attribute.
    */
   public Attribute getTypeCodeAttribute()
   {
      return m_typeCodeAttribute;
   }

   /**
    * Adds a type code to metaclass mapping.
    * @param typeCode The type code value.
    * @param metaclass The metaclass, must be this class
    * or a derived (directly or indirectly) one.
    */
   protected void addTypeCode(Object typeCode, Metaclass metaclass)
   {
      verifyNotReadOnly();

      if (m_typeCodeMap == null)
      {
         m_typeCodeMap = new TypeCodeMap(4);
      }

      Object oldValue = m_typeCodeMap.put(typeCode, metaclass);

      if (oldValue != null)
      {
         m_typeCodeMap.put(typeCode, oldValue);

         throw new MetadataException("err.meta.typeCodeDup",
            new Object[]{typeCode, m_metaclass.getName()});
      }

      if (!m_bTypeCodePrivileged)
      {
         m_bTypeCodePrivileged = !ObjUtil.equal(m_metaclass.getReadPrivilege(),
            metaclass.getReadPrivilege());
      }
   }

   /**
    * Finds this or a derived metaclass by type code.
    * @param typeCode The type code.
    * @return The found metaclass, or null if not found.
    */
   public Metaclass findMetaclassByTypeCode(Object typeCode) throws MetadataLookupException
   {
      Metaclass metaclass = null;
      
      if (m_typeCodeMap != null && typeCode != null)
      {
         metaclass = (Metaclass)m_typeCodeMap.get(typeCode);
      }

      return metaclass;
   }
   
   /**
    * Gets this or a derived metaclass by type code.
    * @param typeCode The type code.
    * @return The found metaclass.
    * @throws MetadataLookupException if the type code is invalid.
    */
   public Metaclass getMetaclassByTypeCode(Object typeCode) throws MetadataLookupException
   {
      Metaclass metaclass = findMetaclassByTypeCode(typeCode);
      
      if (metaclass == null)
      {
         throw new MetadataLookupException("err.meta.typeCodeLookup",
            String.valueOf(typeCode), m_metaclass);
      }

      return metaclass;
   }

   /**
    * @return An iterator over the type codes in the dispatch map.
    */
   public Lookup.Iterator getTypeCodeIterator()
   {
      if (m_typeCodeMap == null)
      {
         return HashTab.EMPTY_ITERATOR;
      }

      return m_typeCodeMap.iterator();
   }

   /**
    * Creates an iterator that takes into account the class read privileges.
    * @param privilegeSet The privilege set, or null for all type codes.
    * @return An iterator over the type codes in the dispatch map.
    */
   public Lookup.Iterator getTypeCodeIterator(PrivilegeSet privilegeSet)
   {
      if (m_typeCodeMap == null)
      {
         return TypeCodeMap.EMPTY_ITERATOR;
      }
      
      if (privilegeSet == null)
      {
         return m_typeCodeMap.iterator();
      }

      return m_typeCodeMap.iterator(privilegeSet);
   }

   /**
    * @return The type code count in the dispatch map.
    */
   public int getTypeCodeCount()
   {
      if (m_typeCodeMap == null)
      {
         return 0;
      }
      
      return m_typeCodeMap.size();
   }

   /**
    * Sets the flag for filtering the query tye type code.
    * @param bTypeCodeFiltered True if a query for this class should be filtered by type code.
    */
   public void setTypeCodeFiltered(boolean bTypeCodeFiltered)
   {
      verifyNotReadOnly();
      m_bTypeCodeFiltered = bTypeCodeFiltered;
   }

   /**
    * @return True if a query for this class should be filtered by type code.
    */
   public boolean isTypeCodeFiltered()
   {
      return m_bTypeCodeFiltered;
   }

   /**
    * Sets the flag for determining the class object by type code.
    * @param bTypeCodeDispatched True if the class object is determined by the type code.
    */
   public void setTypeCodeDispatched(boolean bTypeCodeDispatched)
   {
      verifyNotReadOnly();
      m_bTypeCodeDispatched = bTypeCodeDispatched;
   }

   /**
    * @return True if the class object is determined by the type code.
    */
   public boolean isTypeCodeDispatched()
   {
      return m_bTypeCodeDispatched;
   }
   
   /**
    * Sets a type code forced filtering flag.
    * @param bForsed True if a query for this class should be always filtered.
    */
   public void setTypeCodeForced(boolean bForced)
   {
      verifyNotReadOnly();
      m_bTypeCodeForced = bForced;
   }
   
   /**
    * @return True if a query for this class should be always filtered.
    */
   public boolean isTypeCodeForced()
   {
      return m_bTypeCodeForced;
   }

   /**
    * Determines whether a privilege set does not contain the read privilege
    * required by a subclass dispatched by the type code.
    * @param privilegeSet The privilege set.
    * @return True if the set does not contain the privilege.
    */
   public boolean isTypeCodePrivileged(PrivilegeSet privilegeSet)
   {
      if (m_bTypeCodePrivileged)
      {
         for (Lookup.Iterator itr = m_typeCodeMap.iterator(); itr.hasNext();)
         {
            itr.next();
            
            PrimitivePrivilege privilege = ((Metaclass)itr.getValue()).getReadPrivilege();
            
            if (privilege != null && !privilegeSet.contains(privilege))
            {
               return true;
            }
         }
      }

      return false;
   }

   /**
    * Sets the fragment name attribute.
    * @param fragmentAttribute The fragment name attribute to set.
    */
   public void setFragmentAttribute(Attribute fragmentAttribute)
   {
      verifyNotReadOnly();

      if (fragmentAttribute != null && fragmentAttribute.getType() != Primitive.STRING)
      {
         throw new MetadataException("err.meta.fragmentAttributeType", 
            new Object[]{fragmentAttribute.getName(), m_metaclass.getName()});
      }

      m_fragmentAttribute = fragmentAttribute;
   }

   /**
    * @return The fragment name attribute.
    */
   public Attribute getFragmentAttribute()
   {
      return m_fragmentAttribute;
   }

   /**
    * Sets the fragment replication model (one of the REPLICATION_* constants).
    * @param nFragmentReplication The fragment replication model (one of REPLICATION_* constants) to set.
    */
   public void setFragmentReplication(byte nFragmentReplication)
   {
      verifyNotReadOnly();
      m_nFragmentReplication = nFragmentReplication;
   }

   /**
    * @return The fragment replication model (one of the REPLICATION_* constants).
    */
   public byte getFragmentReplication()
   {
      return m_nFragmentReplication;
   }

   /**
    * Sets the caching mode (one of the CACHING_* constants).
    * @param nCaching The caching mode (one of the CACHING_* constants) to set.
    */
   public void setCaching(byte nCaching)
   {
      verifyNotReadOnly();
      m_nCaching = nCaching;
   }

   /**
    * @return The caching mode (one of the CACHING_* constants).
    */
   public byte getCaching()
   {
      return m_nCaching;
   }

   /**
    * @return True if the actual persistence mapping is determined dynamically.
    */
   public boolean isDynamic()
   {
      return false;
   }

   /**
    * Sets the persistence hook component.
    * @param hook The persistence hook component to set.
    */
   public void setHook(Component hook)
   {
      verifyNotReadOnly();
      m_hook = hook;
      
      if (hook != null)
      {
         Object instance = hook.getInstance(null);

         if (!(instance instanceof PersistenceHook))
         {
            throw new MetadataException("err.meta.invalidPersistenceHook", new Object[]{hook.getName()});
         }
      }
   }

   /**
    * @return The persistence hook component.
    */
   public Component getHook()
   {
      return m_hook;
   }

   /**
    * Determines if this persistence mapping cannot be used in the same
    * physical query as another mapping, requiring a heterogenous query. 
    * @param mapping The other mapping. Can be null.
    * @return True if it cannot be used in the same query.
    */
   public boolean isHeterogenous(PersistenceMapping mapping)
   {
      return mapping == null || mapping.m_dataSource != m_dataSource;
   }

   /**
    * Determines if this persistence mapping can be used in the same
    * class inheritance branch, i.e. if subclasses can be persisted
    * in the same physical storage object, requiring OID uniqueness
    * within the inheritance branch.
    * @param mapping The other mapping.
    * @return True if the mappings are compatible.
    */
   public boolean isCompatible(PersistenceMapping mapping)
   {
      return this == mapping;
   }

   /**
    * Computes the inherited state. Override in custom persistence mappings to
    * provide persistence-adapter-specific mapping inheritance.
    * 
    * Note that it is possible that this mapping is on a different data source than the
    * base metaclass persistence mapping.
    * 
    * This happens because the framework allows a metaclass to be based on a metaclass
    * from a different data source or even a different persistence adapter. This is a
    * feature that allows for re-use of the attributes and events defined in the base
    * class, and even polymorphic substitution.
    * 
    * When extending a class in a different data source, it is expected that the metadata
    * developer will fully specify the adapter-specific portions of the persistence
    * mapping. Therefore there will not be any mapping inheritance.
    * 
    * In this case, attributes and events, as well as elements that are common to all
    * persistence mappings will be inherited. But the adapter-specific mappings will
    * not be inherited.
    * 
    * @throws MetadataException if an error is detected.
    */
   public void resolveInheritance() throws MetadataException
   {
      verifyNotReadOnly();

      // Inherit attributes from the aspects

      int nAspectCount = m_metaclass.getAspectCount();

      if (nAspectCount != 0)
      {
         for (int nAttribute = 0, nAttributeCount = m_metaclass.getInstanceAttributeCount();
            nAttribute < nAttributeCount; ++nAttribute)
         {
            Attribute attribute = m_metaclass.getInstanceAttribute(nAttribute);
   
            for (int nAspect = 0; nAspect < nAspectCount; ++nAspect)
            {
               ClassAspect aspect = (ClassAspect)m_metaclass.getAspect(nAspect);
               Attribute aspectAttribute = aspect.findAttribute(attribute.getName());

               if (aspectAttribute != null)
               {
                  PersistenceMapping aspectMapping = aspect.findPersistenceMapping(m_dataSource);

                  if (aspectMapping != null)
                  {
                     AttributeMapping aspectAttrMapping = aspectMapping.getAttributeMapping(aspectAttribute);

                     if (aspectAttrMapping != null)
                     {
                        deriveAttributeMapping(attribute, aspectAttribute, aspectAttrMapping);
                     }

                     break;
                  }
               }
            }
         }
      }

      completeDerivation();

      PersistenceMapping baseMapping = getBaseMapping();

      if (baseMapping != null)
      {
         if (m_typeCodeAttribute == null && baseMapping.m_typeCodeAttribute != null)
         {
            m_typeCodeAttribute = m_metaclass.getDerivedAttribute(baseMapping.m_typeCodeAttribute);
         }

         if (m_lockingAttribute == null && baseMapping.m_lockingAttribute != null)
         {
            m_lockingAttribute = m_metaclass.getDerivedAttribute(baseMapping.m_lockingAttribute);
         }

         if (m_fragmentAttribute == null && baseMapping.m_fragmentAttribute != null)
         {
            m_fragmentAttribute = m_metaclass.getDerivedAttribute(baseMapping.m_fragmentAttribute);
         }

         if (m_nFragmentReplication == REPLICATION_NONE)
         {
            m_nFragmentReplication = baseMapping.getFragmentReplication();
         }

         if (m_nCaching == CACHING_NONE)
         {
            m_nCaching = baseMapping.getCaching();
         }
      }

      if (nAspectCount != 0)
      {
         for (int nAspect = 0; nAspect < nAspectCount; ++nAspect)
         {
            ClassAspect aspect = (ClassAspect)m_metaclass.getAspect(nAspect);
            PersistenceMapping aspectMapping = aspect.findPersistenceMapping(m_dataSource);

            if (aspectMapping != null)
            {
               if (m_typeCodeAttribute == null && aspectMapping.getTypeCodeAttribute() != null)
               {
                  m_typeCodeAttribute = m_metaclass.getAttribute(aspectMapping.getTypeCodeAttribute().getName());

                  if (aspectMapping.isTypeCodeForced())
                  {
                     m_bTypeCodeForced = true;
                  }
               }

               if (m_lockingAttribute == null && aspectMapping.getLockingAttribute() != null)
               {
                  m_lockingAttribute = m_metaclass.getAttribute(aspectMapping.getLockingAttribute().getName());
               }

               if (m_fragmentAttribute == null && aspectMapping.getFragmentAttribute() != null)
               {
                  m_fragmentAttribute = m_metaclass.getAttribute(aspectMapping.getFragmentAttribute().getName());
               }

               if (m_nFragmentReplication == REPLICATION_NONE)
               {
                  m_nFragmentReplication = aspectMapping.getFragmentReplication();
               }

               if (m_nCaching == CACHING_NONE)
               {
                  m_nCaching = aspectMapping.getCaching();
               }
            }
         }
      }

      if (baseMapping != null)
      {
         if (baseMapping.m_typeCodeAttribute != null)
         {
            if (m_typeCodeAttribute.getOrdinal() != baseMapping.m_typeCodeAttribute.getOrdinal())
            {
               throw new MetadataException("err.meta.typeCodeAttrMismatch",
                  new Object[]{m_metaclass.getName(), m_metaclass.getBase().getName()});
            }

            if (baseMapping.m_bTypeCodeForced)
            {
               m_bTypeCodeForced = true;
            }
         }

         if (baseMapping.m_lockingAttribute != null)
         {
            if (m_lockingAttribute.getOrdinal() != baseMapping.m_lockingAttribute.getOrdinal())
            {
               throw new MetadataException("err.meta.lockingAttrMismatch",
                  new Object[]{m_metaclass.getName(), m_metaclass.getBase().getName()});
            }
         }
      }

      // Validate the type code attribute

      if (m_typeCodeAttribute != null)
      {
         if (getAttributeMapping(m_typeCodeAttribute) == null)
         {
            throw new MetadataException("err.meta.unmappedTypeCodeAttr",
               new Object[]{m_typeCodeAttribute.getName(), m_metaclass.getName()});
         }

         Object typeCode = m_typeCodeAttribute.getValue();

         if (typeCode != Undefined.VALUE)
         {
            if (typeCode == null || Primitive.primitiveOf(typeCode) == Primitive.ANY)
            {
               MetadataValidationException e = new MetadataValidationException(
                  "err.meta.typeCodeValue",  new Object[]{m_typeCodeAttribute.getName(), m_metaclass.getName()});

               m_typeCodeAttribute.setProperties(e);

               throw e;
            }

            typeCode = ((Primitive)m_typeCodeAttribute.getType()).convert(typeCode);
         }

         try
         {
            if (typeCode != Undefined.VALUE)
            {
               addTypeCode(typeCode, m_metaclass);
            }

            for (; baseMapping != null; baseMapping = baseMapping.getBaseMapping())
            {
               if (baseMapping.m_dataSource != m_dataSource ||
                  baseMapping.m_typeCodeAttribute == null)
               {
                  break;
               }

               if (!m_bTypeCodeFiltered)
               {
                  if (baseMapping.m_typeCodeAttribute.getValue() != Undefined.VALUE ||
                     baseMapping.getMetaclass().getDerivedCount() != 1)
                  {
                     m_bTypeCodeFiltered = true;
                  }
               }

               if (typeCode != Undefined.VALUE)
               {
                  baseMapping.addTypeCode(typeCode, m_metaclass);
                  baseMapping.m_bTypeCodeDispatched = true;
               }
            }
         }
         catch (UncheckedException x)
         {
            MetadataValidationException e = new MetadataValidationException(x);

            m_typeCodeAttribute.setProperties(e);

            throw e;
         }

         if (!m_bTypeCodeFiltered)
         {
            if (m_bTypeCodeForced || getAttributeMapping(m_typeCodeAttribute).isMultiplexed())
            {
               m_bTypeCodeFiltered = true;
            }
         }
      }

      // Validate the locking attribute

      if (m_lockingAttribute != null)
      {
         if (getAttributeMapping(m_lockingAttribute) == null)
         {
            throw new MetadataException("err.meta.unmappedLockingAttribute",
               new Object[]{m_lockingAttribute.getName(), m_metaclass.getName()});
         }
      }
   }

   /**
    * Template method for deriving an attribute mapping from a base attribute mapping.
    * Used to support aspects.
    * @param attribute The attribute, for which to derive the mapping.
    * @param base The base attribute.
    * @param baseMapping The base attribute mapping.
    */
   protected void deriveAttributeMapping(Attribute attribute, Attribute base, AttributeMapping baseMapping)
   {
   }

   /**
    * Template method for post-processing the derivation.
    */
   protected void completeDerivation()
   {
   }

   /**
    * Second pass of inheritance resolution, invoked after the component metadata has been read.
    */
   public void resolveInheritance2()
   {
      if (m_typeCodeMap == null)
      {
         m_bTypeCodeFiltered = false;
      }

      PersistenceMapping baseMapping = getBaseMapping();

      if (baseMapping != null && baseMapping.getHook() != null &&
         m_hook == null && isCompatible(baseMapping))
      {
         m_hook = baseMapping.getHook();
      }
   }

   /**
    * @return The persistence mapping of the base class, or null if not available.
    */
   public PersistenceMapping getBaseMapping()
   {
      PersistenceMapping mapping = null;
      Metaclass base = m_metaclass.getBase();

      if (base != null)
      {
         mapping = base.getPersistenceMapping();

         if (mapping != null)
         {
            mapping = mapping.findMapping(this);
         }
      }

      return mapping;
   }

   /**
    * Finds a mapping of the same type as a given mapping.
    * @param mapping The mapping to match against.
    * @return The matching mapping, or null if not found.
    */
   public PersistenceMapping findMapping(PersistenceMapping mapping)
   {
      return (getClass() == mapping.getClass()) ? this : null;
   }

   /**
    * Adds a new attribute mapping to the persistence mapping.
    * @param mapping The attribute mapping to add.
    * @throws MetadataException if an attribute mapping
    * with the same name already exists.
    */
   public abstract void addAttributeMapping(AttributeMapping mapping);

   /**
    * Gets an attribute mapping for a given attribute.
    * @param attr The attribute for which to get the mapping.
    * @return The attribute mapping object. Can be null for unmapped attributes.
    */
   public abstract AttributeMapping getAttributeMapping(Attribute attribute);

   /**
    * Gets a compatible attribute mapping for a given attribute.
    * @param attribute The attribute for which to find the mapping.
    * @param compatible The compatible mapping which should correspond to the attribute type.
    *    Can be null to skip this test.
    * @return The attribute mapping object. Can be null if not found.
    */
   public ClassMapping findClassMapping(Attribute attribute, PersistenceMapping compatible)
   {
      ClassMapping mapping = (ClassMapping)getAttributeMapping(attribute);

      if (compatible != null && mapping != null)
      {
         if (!mapping.getMapping().isCompatible(compatible))
         {
            mapping = null;
         }
      }

      return mapping;
   }

   /**
    * @return A key describing the OID structure.
    */
   public abstract Key getObjectKey();

   /**
    * Adds a foreign key with a given name to this mapping
    * and sets the association mapping in a class mapping.
    * @param sName The name of the foreign key to add. Can be null.
    * @param mapping The class mapping to update. Can be null.
    * @return The added foreign key.
    * @throws MetadataException if the key cannot be added.
    */
   public abstract Key addForeignKey(String sName, ClassMapping mapping) throws MetadataException;

   /**
    * @return True if this is a context mapping.
    */
   public boolean isContext()
   {
      return this != m_metaclass.getPersistenceMapping();
   }

   /**
    * Computes the available sort keys given the associations
    * which restrict the instances of the class.
    * @param assocs Array of associations that point to this class. Null means no restriction.
    * @param mappings Array of persistence mappings corresponding to the assocs.
    * @param restrictions Array of attributes that restrict the instances of this class. Null means no restriction.
    * @return List of pairs in the following format:
    * (
    *    (bUnique1 (attr1_1 . bAsc1_1) ... (attr1_N . bAsc1_N))
    *    ...
    *    (bUniqueM (attrM_1 . bAscM_1) ... (attrM_L . bAscM_L))
    * )
    */
   public abstract Pair getSortKeys(Attribute[] assocs, PersistenceMapping[] mappings, Attribute[] restrictions);

   /**
    * Computes the unique secondary keys.
    * - Attribute order in the unique secondary keys does not matter.
    * - Each unique secondary key should have a different set of attributes than the other unique
    * secondary keys.
    * - Association attributes may be present in a unique secondary key.
    * - A key that contains the primary key is not considered a secondary key and should not be returned
    * by this method.
    * 
    * @return List of pairs in the following format:
    * (
    *    (attr1_1 ... attr1_N)
    *    ...
    *    (attrM_1 ... attrM_L)
    * ) 
    */
   public abstract Pair getUniqueKeys();
   
   /**
    * @return A new instance of the same persistence mapping class.
    */
   public abstract PersistenceMapping create();

   /**
    * Validates a key generator.
    * @param keyGenerator The key generator. Can be null.
    * @throws MetadataException if the key generator is not valid.
    */
   protected void validateKeyGenerator(Component keyGenerator) throws MetadataException
   {
      if (keyGenerator != null)
      {
         Object value = keyGenerator.getConstantValue("VALUE_COUNT", null);

         if (value instanceof Number)
         {
            int nPartCount = getObjectKey().getPartCount();
            int nValueCount = ((Number)value).intValue();

            if (nValueCount > 0 && nValueCount != nPartCount)
            {
               throw new MetadataException("err.meta.persistence.oidValueCount",
                  new Object[]{m_metaclass.getName(), keyGenerator.getName(),
                     Primitive.createInteger(nPartCount), Primitive.createInteger(nValueCount)});
            }
         }
      }
   }

   /**
    * Makes the visitor to visit this (or the underlying) persistence mapping. 
    */
   public void visit(Visitor visitor)
   {
      visitor.visit(this);
   }

   /**
    * @see nexj.core.meta.MetadataObject#setProperties(nexj.core.meta.MetadataMarker)
    */
   public void setProperties(MetadataMarker marker)
   {
      m_metaclass.setProperties(marker);
      marker.setProperty("item", "persistenceMapping");
   }

   /**
    * Determines if two keys are compatible,
    * i.e. if they can participate in a natural join.
    * @param key1 The first key.
    * @param key2 The second key.
    * @return true if the keys are compatible.
    */
   public static boolean compatible(Key key1, Key key2)
   {
      if (key1.getPartCount() == 0 ||
         key1.getPartCount() != key2.getPartCount())
      {
         return false;
      }
      
      for (int i = 0; i < key1.getPartCount(); ++i)
      {
         if (key1.getPartType(i) != key2.getPartType(i))
         {
            return false;
         }
      }
      
      return true;
   }

   // inner classes

   /**
    * Hash table which can filter out privileged type codes.
    */
   protected static class TypeCodeMap extends HashTab
   {
      private final static long serialVersionUID = 5357965718892706927L;

      /**
       * Constructs the table.
       * @param nCount The estimated entry count.
       */
      public TypeCodeMap(int nCount)
      {
         super(nCount);
      }

      /**
       * Returns an iterator over the type codes for classes
       * with read privileges in the privilege set.
       * @param privilegeSet The privilege set.
       * @return The iterator.
       */
      public Lookup.Iterator iterator(PrivilegeSet privilegeSet)
      {
         return new TypeCodeIterator(privilegeSet);
      }

      /**
       * Privilege filtering iterator.
       */
      protected class TypeCodeIterator extends GenericHashTabIterator
      {
         protected PrivilegeSet m_privilegeSet;

         /**
          * Constructs the iterator.
          * @param privilegeSet The privilege set.
          */
         public TypeCodeIterator(PrivilegeSet privilegeSet)
         {
            super(false);
            m_privilegeSet = privilegeSet;
            incr();
         }

         /**
          * @see nexj.core.util.GenericHashTab.GenericHashTabIterator#incr()
          */
         protected boolean incr()
         {
            m_nCur = m_nNext;
            m_nNext += 2;

            while (m_nNext < m_table.length)
            {
               if (m_table[m_nNext] != null && m_table[m_nNext] != EMPTY)
               {
                  PrimitivePrivilege privilege = ((Metaclass)m_table[m_nNext + 1]).getReadPrivilege();

                  if (privilege == null || m_privilegeSet.contains(privilege))
                  {
                     return true;
                  }
               }

               m_nNext += 2;
            }

            return false;
         }
      }
   }

   /**
    * Interface implemented by persistence mapping visitors.
    */
   public interface Visitor
   {
      /**
       * Visits a persistence mapping.
       * @param mapping The mapping to visit.
       */
      void visit(PersistenceMapping mapping);
   }
}
