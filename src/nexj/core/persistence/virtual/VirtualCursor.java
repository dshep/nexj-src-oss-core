// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.virtual;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import nexj.core.meta.Attribute;
import nexj.core.meta.Metaclass;
import nexj.core.meta.persistence.DataSourceFragment;
import nexj.core.meta.persistence.virtual.ReadMapping;
import nexj.core.meta.persistence.virtual.VirtualDataSourceFragment;
import nexj.core.meta.persistence.virtual.VirtualMapping;
import nexj.core.meta.persistence.virtual.VirtualPrimitiveMapping;
import nexj.core.persistence.Cursor;
import nexj.core.persistence.Field;
import nexj.core.persistence.GenericCursor;
import nexj.core.persistence.OID;
import nexj.core.persistence.OIDHolder;
import nexj.core.persistence.PersistenceException;
import nexj.core.persistence.Query;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InstanceArrayList;
import nexj.core.runtime.InstanceList;
import nexj.core.runtime.InstanceRef;
import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Pair;
import nexj.core.util.EmptyIterator;
import nexj.core.util.Logger;
import nexj.core.util.PropertyIterator;
import nexj.core.util.SingletonIterator;
import nexj.core.util.StringUtil;
import nexj.core.util.Undefined;

/**
 * Cursor for reading from a virtual-mapped class.
 */
public class VirtualCursor extends GenericCursor
{
   // associations

   /**
    * The persistence adapter.
    */
   protected VirtualAdapter m_adapter;

   /**
    * The data source fragment.
    */
   protected VirtualDataSourceFragment m_fragment;

   /**
    * Iterator over the cursor's results.
    */
   protected Iterator m_resultIterator;

   /**
    * The arguments to the read function.
    */
   protected Object[] m_readArgArray;

   /**
    * The cursor close function.
    */
   protected Function m_closeFunction;

   /**
    * The current result.
    */
   protected TransferObject m_result;

   /**
    * The class logger.
    */
   private final static Logger s_logger = Logger.getLogger(VirtualAdapter.class);

   /**
    * The long-running query logger.
    */
   private final static Logger s_lrqLogger = Logger.getLogger(VirtualAdapter.class.getName() + ".LRQ"); 

   // constructors

   /**
    * Constructs the cursor and allocates the necessary resources.
    * @param query The query.
    */
   public VirtualCursor(Query query)
   {
      super(query);
   }

   /**
    * Constructs a cursor over an existing result set. Does not execute the query.
    * @param query The query.
    * @param resultIterator The result set iterator.
    */
   public VirtualCursor(Query query, Iterator resultIterator)
   {
      m_resultIterator = resultIterator;
      init(query);
   }

   // operations

   /**
    * @see nexj.core.persistence.GenericCursor#init(nexj.core.meta.persistence.DataSourceFragment)
    */
   protected void init(DataSourceFragment fragment)
   {
      m_adapter = (VirtualAdapter)m_query.getAdapter();
      m_fragment = (VirtualDataSourceFragment)fragment;
      m_lWarningTimeout = m_fragment.getWarningTimeout();
   }

   /**
    * @see nexj.core.persistence.GenericCursor#disconnect()
    */
   protected void disconnect()
   {
      if (m_closeFunction != null)
      {
         m_adapter.getInvocationContext().getMachine().invoke(m_closeFunction, (Pair)null);
      }

      m_closeFunction = null;
      m_resultIterator = null;
      m_result = null;
   }

   /**
    * @see nexj.core.persistence.GenericCursor#query()
    */
   protected void query() throws PersistenceException
   {
      // Do not execute query when cursor is constructed for an existing result set
      if (m_resultIterator != null)
      {
         return;
      }

      if (s_logger.isDumpEnabled())
      {
         s_logger.dump("Output queries:");

         for (int nQuery = 0; nQuery < m_query.getOutputQueryCount(); nQuery++)
         {
            s_logger.dump(nQuery + ": " + m_query.getOutputQuery(nQuery));
         }
      }

      Machine machine = m_adapter.getInvocationContext().getMachine();
      VirtualMapping mapping = (VirtualMapping)m_query.getPersistenceMapping();
      ReadMapping readMapping = mapping.getReadMapping();

      assert mapping.getMetaclass() == m_query.getMetaclass();

      m_readArgArray = readMapping.getFunctionArgs(m_query, m_fragment);

      if (m_bDebug)
      {
         logQuery(Logger.DEBUG);
      }

      long lStartTime = getCurrentTime();
      Pair readClosePair = (Pair)machine.invoke(readMapping.getFunction(), m_readArgArray);

      logDuration(lStartTime, true);
      m_resultIterator = getIterator(readClosePair.getHead(), mapping.getMetaclass());
      m_closeFunction = (Function)readClosePair.getTail();
   }

   /**
    * Gets an iterator over the result of a read mapping.
    * @param result The result Collection, list, Iterator, or TransferObject.
    * @param metaclass The class of the read mapping.
    * @return An iterator over the TransferObjects returned by the mapping.
    * @throws PersistenceException If "result" argument is of the wrong type.
    */
   protected static Iterator getIterator(Object result, Metaclass metaclass)
   {
      if (result instanceof TransferObject)
      {
         return new SingletonIterator(result);
      }

      if (result instanceof Collection)
      {
         return ((Collection)result).iterator();
      }

      if (result instanceof Pair)
      {
         return Pair.getIterator((Pair)result);
      }

      if (result instanceof Iterator)
      {
         return (Iterator)result;
      }

      if (result == null)
      {
         return EmptyIterator.getInstance();
      }

      throw new PersistenceException("err.persistence.virtual.invalidReadResult",
         new Object[]{metaclass.getName(), result.getClass().getCanonicalName()});
   }

   /**
    * @see nexj.core.persistence.GenericCursor#fetch()
    */
   protected boolean fetch() throws PersistenceException
   {
      if (m_resultIterator.hasNext())
      {
         m_result = (TransferObject)m_resultIterator.next();

         if (s_logger.isDumpEnabled())
         {
            s_logger.dump("Result: " + String.valueOf(m_result));
         }

         TransferObject tobj = m_result;

         if (tobj != null)
         {
            OID oid = tobj.getOID();
            VirtualMapping mapping = (VirtualMapping)m_query.getPersistenceMapping();
            Metaclass metaclass = mapping.getMetaclass();

            assert metaclass == m_query.getMetaclass();

            if (oid == null)
            {
               PersistenceException ex = new PersistenceException("err.persistence.virtual.missingOID",
                  new Object[]{metaclass.getName()});

               ex.setValue("result", tobj);

               throw ex;
            }

            // Set the attributes that are mapped to the OID key parts
            for (int i = 0, nCount = metaclass.getInstanceAttributeCount(); i < nCount; i++)
            {
               Attribute attribute = metaclass.getInstanceAttribute(i);

               if (attribute.getType().isPrimitive())
               {
                  VirtualPrimitiveMapping attrMapping = (VirtualPrimitiveMapping)mapping.getAttributeMapping(attribute);

                  if (attrMapping != null)
                  {
                     int nKeyPart = attrMapping.getObjectKeyPart();

                     if (nKeyPart >= 0)
                     {
                        tobj.setValue(attribute.getName(), oid.getValue(nKeyPart));
                     }
                  }
               }
            }

            // Stores raw data from composition-mapped attributes for use in VirtualCursor.internalComplete
            for (int nQuery = 1, nQueryCount = m_query.getOutputQueryCount(); nQuery < nQueryCount; nQuery++)
            {
               Query query = m_query.getOutputQuery(nQuery);
               Object generator = query.getGenerator();

               if (generator instanceof VirtualJoinTab)
               {
                  ((VirtualJoinTab)generator).addChildObjects(oid, tobj.findValue(query.getAttribute().getName()));
               }
            }
         }

         return true;
      }

      return false;
   }

   /**
    * Takes the homogeneous data that were stored during fetch() and joins them to the results.
    * @see nexj.core.persistence.GenericCursor#completeQuery()
    */
   protected void completeQuery()
   {
      for (int nQuery = 1, nQueryCount = m_query.getOutputQueryCount(); nQuery < nQueryCount; nQuery++)
      {
         Query query = m_query.getOutputQuery(nQuery);
         Object generator = query.getGenerator();

         if (!(generator instanceof VirtualJoinTab))
         {
            continue;
         }

         VirtualJoinTab joinTab = (VirtualJoinTab)generator;

         for (Iterator itr = joinTab.getParentOIDIterator(); itr.hasNext(); )
         {
            OID parentOID = (OID)itr.next();
            List childList = joinTab.getChildObjects(parentOID);

            if (childList == null)
            {
               continue;
            }

            Set containerSet = query.getParentInstances(parentOID);

            // composition data are stored *in* a parent, so must have one and only one
            assert containerSet.size() == 1;

            Instance container = (Instance)containerSet.iterator().next();
            Cursor cursor = new VirtualCursor(query, childList.iterator());  // creates Instances
            Attribute assoc = query.getAttribute();

            if (assoc.isCollection())
            {
               // Add composition instances to collection on parent
               Instance childInst;
               InstanceList childInstList = (InstanceList)container.getOldValueDirect(
                  assoc.getOrdinal());

               while ((childInst = cursor.next()) != null)
               {
                  childInstList.setLazy(false);
                  childInstList.add(childInst, InstanceList.DIRECT | InstanceList.REPLACE);
               }
            }
            else
            {
               assert childList.size() == 1;

               container.setOldValueDirect(assoc.getOrdinal(), cursor.next());

               assert cursor.next() == null;
            }
         }

         joinTab.clear();
         query.completeParentInstances();
      }

      // Process heterogeneous joins
      super.completeQuery();
   }

   /**
    * @see nexj.core.persistence.GenericCursor#getKey(java.lang.Object)
    */
   protected OID getKey(Object item)
   {
      if (m_result == null)
      {
         // Cursor will skip instance if no result
         return null;
      }

      if (item == VirtualAdapter.OID)
      {
         return m_result.getOID();
      }

      if (item instanceof Field)
      {
         Field field = (Field)item;

         Object value = m_result.findValue(field.getAttribute().getName());

         if (value == null || value instanceof OID)
         {
            return (OID)value;
         }

         // Retrieve key from a composition mapping
         if (value instanceof OIDHolder)
         {
            return ((OIDHolder)value).getOID();
         }

         // Convert non-OID to a single-part OID
         return new OID(new Object[]{value});
      }

      if (item instanceof Field[])
      {
         Field[] fieldArray = (Field[])item;
         int nCount = fieldArray.length;
         Object[] partArray = new Object[nCount];

         for (int i = 0; i < nCount; i++)
         {
            Object value = m_result.findValue(fieldArray[i].getAttribute().getName());

            if (value == null)
            {
               return null;
            }

            partArray[i] = value;
         }

         return new OID(partArray);
      }

      return null;
   }

   /**
    * @see nexj.core.persistence.GenericCursor#getRawValue(nexj.core.persistence.Field)
    */
   protected Object getRawValue(Field field) throws PersistenceException
   {
      return m_result.findValue(field.getAttribute().getName());
   }

   /**
    * Gets attributes that are not in the read attributes list.
    * @see nexj.core.persistence.GenericCursor#addInstance(nexj.core.runtime.Instance, nexj.core.meta.Metaclass, nexj.core.persistence.Query, boolean)
    */
   protected void addInstance(Instance instance, InstanceRef ref, Metaclass metaclass, Query query, boolean bOverwrite)
   {
      super.addInstance(instance, ref, metaclass, query, bOverwrite);

      // Apply processing only to root query node (e.g. m_result is invalid for child queries)
      if (query != m_query)
      {
         return;
      }

      // Make the instance non-lazy if possible
      if (instance.isLazy())
      {
         Field field = query.getTypeCodeField();

         if (field != null)
         {
            metaclass = query.getPersistenceMapping().findMetaclassByTypeCode(getValue(field));
         }
         else if (!StringUtil.isEmpty(m_result.getClassName()))
         {
            assert metaclass.getName().equals(m_result.getClassName());
         }
         else
         {
            return;
         }

         instance.setMetaclass(metaclass);
      }

      for (PropertyIterator itr = m_result.getIterator(); itr.hasNext(); )
      {
         String sAttribute = (String)itr.next();
         Attribute attribute = metaclass.getAttribute(sAttribute);

         if (!attribute.isPersistent())
         {
            continue;
         }

         int nOrdinal = attribute.getOrdinal();

         if (query.findAssoc(Query.ASSOC_QUERY, attribute, null, false) != null)
         {
            continue;
         }

         Object value = itr.getValue();

         if (attribute.getType().isPrimitive())
         {
            // All attributes returned by the read mapping should be set
            if (bOverwrite || instance.getOldValueDirect(nOrdinal) == Undefined.VALUE)
            {
               instance.setOldValueDirect(nOrdinal, value);
            }
         }
         else if (attribute.isCollection())
         {
            // A composition attribute
            Cursor cursor = new VirtualCursor(getCompositionQuery(attribute, query), getIterator(value, metaclass));

            try
            {
               InstanceList instanceList = new InstanceArrayList();

               instanceList.setAssociation(instance, attribute, true);
               instanceList.setLazy(false);
               instance.setOldValueDirect(nOrdinal, instanceList);

               Instance assoc;

               while ((assoc = cursor.next()) != null)
               {
                  if (!instanceList.contains(assoc))
                  {
                     instanceList.add(assoc, InstanceList.DIRECT);
                  }
               }
            }
            finally
            {
               cursor.close();
            }
         }
         else if (value instanceof TransferObject)
         {
            Cursor cursor = new VirtualCursor(getCompositionQuery(attribute, query), new SingletonIterator(value));

            try
            {
               instance.setOldValueDirect(nOrdinal, cursor.next());
            }
            finally
            {
               cursor.close();
            }
         }
         else if (value instanceof OID)
         {
            // Associate an instance, creating a lazy instance if not loaded
            OID oid = (OID)value;
            Metaclass assocClass = (Metaclass)attribute.getType();
            InstanceRef assocRef = m_query.getInvocationContext().findInstanceRef(assocClass, oid);
            Instance assoc = (assocRef == null) ? null : assocRef.getInstance();

            if (assoc == null)
            {
               assoc = new Instance(assocClass, true, m_query.getInvocationContext());
               assocRef = assoc.cache(oid);
            }

            if (bOverwrite || instance.getOldValueDirect(nOrdinal) == Undefined.VALUE)
            {
               instance.setOldValueDirect(nOrdinal, assoc);
            }

            // Share-lock the assoc instance
            if (m_uow != null)
            {
               m_uow.lock(assocRef, false);
            }
         }
      }
   }

   /**
    * Gets a composition child query.
    * @param attribute The composition attribute.
    * @param parent The parent query.
    * @return The composition query.
    */
   protected static Query getCompositionQuery(Attribute attribute, Query parent)
   {
      Query query = new Query((Metaclass)attribute.getType(), parent.getInvocationContext());

      query.setOutput(Query.OUTPUT_EAGER);

      if (parent.getSecurity() == Query.SEC_ALL)
      {
         query.setSecurity(Query.SEC_ALL);
      }

      if ((parent.getRestriction() & Query.RESTRICTION_PARENT) != 0)
      {
         query.addRestriction(parent.getRestriction() & ~Query.RESTRICTION_PARENT);
      }

      query.setCursor(false);
      query.prepare(false);

      return query;
   }

   /**
    * @see nexj.core.persistence.GenericCursor#getMetaclass(nexj.core.persistence.Query)
    */
   protected Metaclass getMetaclass(Query query)
   {
      Metaclass root = super.getMetaclass(query);

      // m_result is invalid for child query nodes
      if (query != m_query)
      {
         return root;
      }

      String sClassName = m_result.getClassName();

      if (sClassName == null || root.getName().equals(sClassName))
      {
         return root;
      }

      Metaclass derived = root.getMetadata().findMetaclass(sClassName);

      if (root.isUpcast(derived))
      {
         return derived;
      }

      throw new PersistenceException("err.persistence.virtual.resultTypeMismatch",
         new Object[]{String.valueOf(m_result.getOID()), root.getName(), sClassName});
   }

   /**
    * @see nexj.core.persistence.GenericCursor#getType()
    */
   protected String getType()
   {
      return "Virtual";
   }

   /**
    * @see nexj.core.persistence.GenericCursor#getLRQLogger()
    */
   protected Logger getLRQLogger()
   {
      return s_lrqLogger;
   }

   /**
    * @see nexj.core.persistence.GenericCursor#getLogger()
    */
   protected Logger getLogger()
   {
      return s_logger;
   }

   /**
    * @see nexj.core.persistence.GenericCursor#logQuery(int)
    */
   protected void logQuery(int nLevel)
   {
      if (s_logger.isLevelEnabled(nLevel))
      {
         s_logger.log(nLevel, "Virtual read with the following arguments:");
         ((VirtualMapping)m_query.getPersistenceMapping()).getReadMapping()
            .logFunctionArgs(m_readArgArray, s_logger, nLevel);
      }
   }
}
