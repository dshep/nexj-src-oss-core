// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.virtual;

import java.util.BitSet;
import java.util.List;

import nexj.core.meta.Attribute;
import nexj.core.meta.Component;
import nexj.core.meta.Metaclass;
import nexj.core.meta.TypeMismatchException;
import nexj.core.meta.persistence.AttributeMapping;
import nexj.core.meta.persistence.ClassMapping;
import nexj.core.meta.persistence.Key;
import nexj.core.meta.persistence.Schema;
import nexj.core.meta.persistence.SchemaUpgrade;
import nexj.core.meta.persistence.virtual.VirtualClassMapping;
import nexj.core.meta.persistence.virtual.VirtualKey;
import nexj.core.meta.persistence.virtual.VirtualMapping;
import nexj.core.meta.persistence.virtual.VirtualPrimitiveMapping;
import nexj.core.meta.persistence.virtual.VirtualSortKey;
import nexj.core.meta.upgrade.UpgradeState;
import nexj.core.persistence.Cursor;
import nexj.core.persistence.Field;
import nexj.core.persistence.GenericPersistenceAdapter;
import nexj.core.persistence.OID;
import nexj.core.persistence.OIDGenerator;
import nexj.core.persistence.OIDHolder;
import nexj.core.persistence.PersistenceException;
import nexj.core.persistence.Query;
import nexj.core.persistence.SchemaVersion;
import nexj.core.persistence.Source;
import nexj.core.persistence.Work;
import nexj.core.runtime.Instance;
import nexj.core.runtime.UnitOfWork;
import nexj.core.runtime.ValidationException;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.PropertyMap;
import nexj.core.util.Undefined;

/**
 * The virtual persistence adapter.
 */
public class VirtualAdapter extends GenericPersistenceAdapter
{
   // constants

   /**
    * The field type used for OID items.
    */
   public final static String OID = "<OID_TYPE>";

   // associations

   /**
    * The virtual work item lookup key.
    */
   protected VirtualWork m_workKey;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(VirtualAdapter.class);

   // operations

   /**
    * @see nexj.core.persistence.PersistenceAdapter#execute(nexj.core.persistence.Work[], int, int)
    */
   public void execute(Work[] workArray, int nStart, int nEnd) throws PersistenceException
   {
      for (int nBatch = nStart; nBatch < nEnd;)
      {
         if (nStart < nEnd &&
            (nStart == nBatch || workArray[nStart].compareTo(workArray[nBatch]) == 0))
         {
            nStart++;
         }
         else
         {
            if (nBatch == nStart)
            {
               nStart++;
            }

            ((VirtualWork)workArray[nBatch]).execute(workArray, nBatch, nStart);
            nBatch = nStart;
         }
      }
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#addDependency(nexj.core.runtime.UnitOfWork, nexj.core.persistence.Work, nexj.core.runtime.Instance, nexj.core.meta.persistence.Key, nexj.core.meta.persistence.Key, boolean)
    */
   public void addDependency(UnitOfWork uow, Work work, Instance instance, Key dstKey, Key srcKey, boolean bSuccessor)
      throws PersistenceException
   {
      if (instance.getUnitOfWork() != uow)
      {
         return;
      }

      int nType;

      switch (instance.getState())
      {
         case Instance.NEW:
            nType = VirtualWork.CREATE;
            break;

         case Instance.DIRTY:
            nType = VirtualWork.UPDATE;
            break;

         case Instance.DELETED:
            if (instance.getOID() != null)
            {
               nType = VirtualWork.DELETE;
               break;
            }

         default:
            return;
      }

      VirtualWork virtualWork = findWork(uow, instance);

      if (virtualWork == null)
      {
         virtualWork = addWork(uow, nType, instance);
      }

      if (bSuccessor)
      {
         if (nType == VirtualWork.CREATE)
         {
            work.addSuccessor(virtualWork, dstKey, srcKey);
         }
      }
      else
      {
         virtualWork.addSuccessor(work, dstKey, srcKey);
      }
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#addWork(nexj.core.runtime.UnitOfWork, nexj.core.runtime.Instance)
    */
   public void addWork(UnitOfWork uow, Instance instance) throws PersistenceException
   {
      switch (instance.getState())
      {
         case Instance.NEW:
            addCreate(uow, instance);
            break;

         case Instance.DIRTY:
            addUpdate(uow, instance);
            break;

         case Instance.DELETED:
            getWork(uow, VirtualWork.DELETE, instance);
            break;

         default:
            throw new IllegalStateException();
      }
   }

   /**
    * Creates a work item for the specific instance.
    * @param uow The unit of work to which to add the work items.
    * @param nType The type of work to create, one of VirtualWork.CREATE, VirtualWork.UPDATE,
    * or VirtualWork.DELETE.
    * @param instance The instance.
    * @return The work item that was created.
    */
   public VirtualWork addWork(UnitOfWork uow, int nType, Instance instance)
   {
      VirtualWork work;

      switch (nType)
      {
         case VirtualWork.CREATE:
            work = new VirtualCreate(instance, this);
            break;

         case VirtualWork.UPDATE:
            work = new VirtualUpdate(instance, this);
            break;

         case VirtualWork.DELETE:
            work = new VirtualDelete(instance, this);
            break;

         default:
            throw new IllegalStateException();
      }

      uow.addWork(work);

      return work;
   }

   /**
    * Looks up or creates the work items for the specified instance.
    * @param uow The unit of work in which to lookup/add the work items.
    * @param nType The type of work to create, one of VirtualWork.CREATE, VirtualWork.UPDATE,
    * or VirtualWork.DELETE. 
    * @param instance The instance.
    * @return The work item.
    */
   public VirtualWork getWork(UnitOfWork uow, int nType, Instance instance)
   {
      VirtualWork work = findWork(uow, instance);

      if (work == null)
      {
         work = addWork(uow, nType, instance);
      }

      return work;
   }

   /**
    * Finds a work item for the given instance in the unit of work.
    * @param uow The unit of work.
    * @param instance The instance.
    * @return The work item; null if nothing was found.
    */
   protected VirtualWork findWork(UnitOfWork uow, Instance instance)
   {
      if (m_workKey == null)
      {
         m_workKey = new VirtualWork(instance, this)
         {
            public int getType()
            {
               return -1;
            }

            public void execute(Work[] workArray, int nStart, int nEnd)
            {
               throw new UnsupportedOperationException();
            }
         };
      }
      else
      {
         m_workKey.setData(instance);
      }

      return (VirtualWork)uow.findWork(m_workKey);
   }

   /**
    * Adds creation work items for the instance.
    * @param uow The unit of work.
    * @param instance The instance for which to add the work items.
    */
   protected void addCreate(UnitOfWork uow, Instance instance)
   {
      Metaclass metaclass = instance.getMetaclass();
      int nCount = metaclass.getInstanceAttributeCount();
      VirtualMapping mapping = (VirtualMapping)instance.getPersistenceMapping();
      OID oid = instance.getOID();
      VirtualWork work = getWork(uow, VirtualWork.CREATE, instance);

      if (oid == null)
      {
         Component keyGen = mapping.getKeyGenerator();

         if (keyGen != null)
         {
            oid = ((OIDGenerator)keyGen.getInstance(uow.getInvocationContext())).generateOID(instance, this);
         }
         else
         {
            // Compute OID from attribute values
            Object[] valueArray = null;

            for (int i = 0; i < nCount; i++)
            {
               Attribute attribute = metaclass.getInstanceAttribute(i);

               if (attribute.getType().isPrimitive())
               {
                  VirtualPrimitiveMapping attrMapping = (VirtualPrimitiveMapping)mapping.getAttributeMapping(attribute);

                  if (attrMapping != null)  // ignore non-persisted attributes
                  {
                     int nKeyPart = attrMapping.getObjectKeyPart();

                     if (nKeyPart >= 0)
                     {
                        Object value = instance.getValueDirect(i);

                        if (value == null)
                        {
                           valueArray = null;

                           break;
                        }

                        if (valueArray == null)
                        {
                           valueArray = new Object[mapping.getObjectKey().getPartCount()];
                        }

                        valueArray[nKeyPart] = value;
                     }
                  }
               }
            }

            if (valueArray != null)
            {
               oid = new OID(valueArray);
            }
         }

         instance.setOID(oid);
      }

      addCreateDependencies(uow, instance, work);
   }

   /**
    * Adds update work items for the instance.
    * @param uow The unit of work.
    * @param instance The instance for which to add the work items.
    */
   protected void addUpdate(UnitOfWork uow, Instance instance)
   {
      VirtualWork work = getWork(uow, VirtualWork.UPDATE, instance);

      addUpdateDependencies(uow, instance, work);
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#getFields(nexj.core.persistence.Source)
    */
   public Field[] getFields(Source source) throws PersistenceException
   {
      Object item = source.getItem();

      if (item instanceof Field[])
      {
         return (Field[])item;
      }

      return null;
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#getValues(nexj.core.persistence.OID, nexj.core.persistence.Source)
    */
   public Object[] getValues(OID oid, Source source) throws PersistenceException
   {
      if (source.getItem() instanceof Field[])
      {
         return oid.getValueArray();
      }

      return null;
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#getOID(nexj.core.util.PropertyMap, java.lang.Object)
    */
   public OID getOID(PropertyMap instance, Object item)
   {
      if (instance instanceof OIDHolder)
      {
         return ((OIDHolder)instance).getOID();
      }

      return null;
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#getValue(nexj.core.persistence.OID, nexj.core.persistence.Source)
    */
   public Object getValue(OID oid, Source source)
   {
      if (source instanceof Field)
      {
         Field field = (Field)source;
         Attribute attribute = field.getAttribute();

         if (attribute.getType().isPrimitive())
         {
            VirtualPrimitiveMapping attrMapping = (VirtualPrimitiveMapping)field.getAttributeMapping();
            int nKeyPart = attrMapping.getObjectKeyPart();

            if (nKeyPart >= 0)
            {
               return oid.getValue(nKeyPart);
            }
         }
      }

      return Undefined.VALUE;
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#getValue(nexj.core.util.PropertyMap, nexj.core.persistence.Source)
    */
   public Object getValue(PropertyMap instance, Source source)
   {
      if (source instanceof Field)
      {
         Field field = (Field)source;
         Object item = field.getItem();

         if (item == VirtualAdapter.OID)
         {
            if (instance instanceof OIDHolder)
            {
               return ((OIDHolder)instance).getOID();
            }

            throw new TypeMismatchException("getValue");
         }
      }

      return Undefined.VALUE;
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#getVersion(nexj.core.meta.persistence.Schema)
    */
   public SchemaVersion getVersion(Schema schema) throws PersistenceException
   {
      return null;
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#mapQuery(nexj.core.persistence.Query)
    */
   public void mapQuery(final Query root) throws PersistenceException
   {
      if (s_logger.isDumpEnabled())
      {
         s_logger.dump("mapQuery: " + root);
      }

      if (root.isOutput())
      {
         root.visit(new Query.Visitor()
         {
            public boolean visit(Query query)
            {
               Attribute attribute = query.getAttribute();

               root.addOutputQuery(query);

               if (query.isRoot())
               {
                  if (!(query.getPersistenceMapping() instanceof VirtualMapping))
                  {
                     // Hetero join from Virtual to non-Virtual
                     setParentItem(query, attribute, (ClassMapping)query.getAttributeMapping());

                     return true;
                  }

                  if (query.getParent() != null && !(query.getParent().getPersistenceMapping() instanceof VirtualMapping))
                  {
                     // Hetero join from non-Virtual to Virtual
                     query.setItem(createOIDItem(query));
                     query.setMapping(query.getPersistenceMapping());
                     setChildItem(query, attribute, (ClassMapping)query.getAttributeMapping());

                     return true;
                  }
               }

               query.setItem(createOIDItem(query));
               query.setMapping(query.getPersistenceMapping());

               if (attribute != null)
               {
                  boolean bRoot = query.isRoot();

                  if (!bRoot)
                  {
                     query.makeRoot(m_context);
                  }

                  ClassMapping mapping = (ClassMapping)query.getAttributeMapping();
                  boolean bHeterogeneous = !(mapping instanceof VirtualClassMapping) ||
                     !isComposition(query, (VirtualClassMapping)mapping);

                  if (bHeterogeneous)
                  {
                     if (!bRoot)
                     {
                        query.addRoot(query);
                     }

                     setParentItem(query, attribute, mapping);
                     setChildItem(query, attribute, mapping);
                     query.setOutput(!mapping.isInner());
                  }
                  else
                  {
                     query.setGenerator(new VirtualJoinTab());
                     query.setParentItem(createOIDItem(query));
                     query.setChildItem(null);
                     query.setOutput(true); // no additional RPC cost to querying composition mappings
                  }
               }

               return true;
            }

            private void setChildItem(Query query, Attribute attribute, ClassMapping mapping)
            {
               if (mapping.getKey(true).isObjectKey())
               {
                  // Parent has FK
                  query.setChildItem(createOIDItem(query));
               }
               else
               {
                  // Child has FK
                  query.setChildItem(createKeyItem((VirtualKey)mapping.getKey(true), query));
               }
            }

            private void setParentItem(Query query, Attribute attribute, ClassMapping mapping)
            {
               Key srcKey = mapping.getKey(false);

               if (srcKey.isObjectKey())
               {
                  // Child has FK
                  query.setParentItem(createOIDItem(query.getParent()));
               }
               else
               {
                  // Parent has FK
                  query.setParentItem(createKeyItem((VirtualKey)srcKey, query.getParent()));
               }
            }

            public boolean postVisit(Query query)
            {
               return true;
            }

            public boolean isEligible(Query query)
            {
               // Visit only direct children of root. Visit only output queries, not where queries.
               return query.isOutput() && query.getParent() == root;
            }
         }, Query.VISIT_QUERY);
      }

      // Prevent expansion to OID components for association attributes in where clauses
      root.visit(new Query.Visitor()
      {
         public boolean visit(Query query)
         {
            if (query != root)
            {
               query.setAdapter(VirtualAdapter.this);
            }

            return true;
         }

         public boolean postVisit(Query query)
         {
            return true;
         }

         public boolean isEligible(Query query)
         {
            return query.getParent() == root;
         }
      }, Query.VISIT_WHERE);
   }

   /**
    * Creates an item that can be used to retrieve the object key of the instances
    * selected by query.
    * @param query The query.
    * @return The item that can be used to retrieve the OID.
    */
   protected static Object createOIDItem(Query query)
   {
      return OID;
   }

   /**
    * Creates an item that can be used to retrieve the value of the key
    * from the instances selected by the query.
    * @param key The key.
    * @param query The query.
    * @return The item that can be used to retrieve the foreign key value.
    */
   protected static Object createKeyItem(VirtualKey key, Query query)
   {
      if (key.isOIDAttribute())
      {
         return new Field(query, key.getAttribute(0), true);
      }

      int nCount = key.getAttributeCount();
      Field[] fkFieldArray = new Field[nCount];

      for (int i = 0; i < nCount; i++)
      {
         fkFieldArray[i] = new Field(query, key.getAttribute(i), true);
      }

      return fkFieldArray;
   }

   /**
    * Determines if the query can be read homogeneously.
    * @param query The query.
    * @param mapping The mapping.
    * @return True if the query may be read homogeneously; false otherwise.
    */
   protected static boolean isComposition(Query query, VirtualClassMapping mapping)
   {
      if (mapping.isEmptyComposition())
      {
         return false;
      }

      return isComposition(query, mapping, null);
   }

   /**
    * Determines if the query can be read homogeneously.
    * @param query The query.
    * @param mapping The mapping.
    * @param map The composition map; null for the root composition map.
    * @return True if the query may be read homogeneously; false otherwise.
    */
   protected static boolean isComposition(Query query, VirtualClassMapping mapping, Lookup map)
   {
      for (Field field = query.getFirstOutputField(); field != null; field = field.getNext())
      {
         if (mapping.findComposition(map, field.getAttribute()) == null)
         {
            return false;
         }
      }

      for (int nQuery = 1; nQuery < query.getOutputQueryCount(); nQuery++)
      {
         Query subQuery = query.getOutputQuery(nQuery);
         Object subMap = mapping.findComposition(map, subQuery.getAttribute());

         if (!(subMap instanceof Lookup) || !isComposition(subQuery, mapping, (Lookup)subMap))
         {
            return false;
         }
      }

      return true;
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#openCursor(nexj.core.persistence.Query)
    */
   public Cursor openCursor(Query query) throws PersistenceException
   {
      return new VirtualCursor(query);
   }

   /**
    * @see nexj.core.persistence.GenericPersistenceAdapter#isUnique(nexj.core.persistence.Query, java.util.List)
    */
   public boolean isUnique(Query query, List sourceList)
   {
      BitSet attrSet = new BitSet(query.getMetaclass().getInstanceAttributeCount());
      int nCount = sourceList.size();

      for (int nSource = 0; nSource < nCount; nSource++)
      {
         Source source = (Source)sourceList.get(nSource);

         if (source instanceof Field)
         {
            attrSet.set(((Field)source).getAttribute().getOrdinal());
         }
         else
         {
            Query assoc = source.getQuery();

            if (assoc.getParent() == query)
            {
               attrSet.set(assoc.getAttribute().getOrdinal());
            }
         }
      }

      if (!attrSet.isEmpty())
      {
         List sortKeyList = ((VirtualMapping)query.getPersistenceMapping()).getSortKeys();

         loop:
            for (int nSortKey = 0, nSortKeyCount = sortKeyList.size(); nSortKey < nSortKeyCount; nSortKey++)
            {
               VirtualSortKey sortKey = (VirtualSortKey)sortKeyList.get(nSortKey);

               if (sortKey.isUnique())
               {
                  for (int i = 0, n = sortKey.getAttributeCount(); i < n; i++)
                  {
                     Attribute attr = sortKey.getAttribute(i);

                     if (attr == null || !attrSet.get(attr.getOrdinal()))
                     {
                        continue loop;
                     }
                  }

                  return true;
               }
            }
      }

      return false;
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#setVersion(nexj.core.meta.persistence.Schema, nexj.core.persistence.SchemaVersion)
    */
   public void setVersion(Schema schema, SchemaVersion version) throws PersistenceException
   {
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#upgrade(nexj.core.meta.persistence.SchemaUpgrade, nexj.core.meta.upgrade.UpgradeState, nexj.core.persistence.SchemaVersion)
    */
   public void upgrade(SchemaUpgrade upgrade, UpgradeState state, SchemaVersion version) throws PersistenceException
   {
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#validate(nexj.core.meta.persistence.AttributeMapping, java.lang.Object)
    */
   public void validate(AttributeMapping mapping, Object value) throws ValidationException
   {
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#addDenorm(nexj.core.runtime.UnitOfWork, nexj.core.runtime.Instance, nexj.core.meta.persistence.AttributeMapping)
    */
   public void addDenorm(UnitOfWork uow, Instance instance, AttributeMapping mapping) throws PersistenceException
   {
   }
}
