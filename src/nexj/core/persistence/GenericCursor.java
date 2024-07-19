// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;

import nexj.core.meta.Attribute;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Primitive;
import nexj.core.meta.persistence.DataSourceFragment;
import nexj.core.runtime.DataVolumeException;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InstanceArrayList;
import nexj.core.runtime.InstanceList;
import nexj.core.runtime.InstanceRef;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.UnitOfWork;
import nexj.core.scripting.Function;
import nexj.core.scripting.Pair;
import nexj.core.util.HashTab2D;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.Lookup2D;
import nexj.core.util.ObjUtil;
import nexj.core.util.PrintWriter;
import nexj.core.util.Printable;
import nexj.core.util.StackTrace;
import nexj.core.util.Undefined;

/**
 * Generic reusable cursor implementation.
 */
public abstract class GenericCursor implements Cursor, Printable
{
   // attributes

   /**
    * The query performance warning timeout in milliseconds.
    */
   protected long m_lWarningTimeout;

   /**
    * The currently retrieved record count (needed for retrieving top-optimized plural queries using binary search).
    */
   protected int m_nRecordCount;

   /**
    * The last page offset.
    */
   protected int m_nRecordOffset;

   /**
    * For plural limited queries, this is the value that was used as max count during the last query.
    */
   protected int m_nLastMaxCount;

   /**
    * The total count of retrieved instances (without the skipped ones).
    */
   protected int m_nTotalCount;

   /**
    * The count of instances to skip before returning the result.
    */
   protected int m_nOffset;

   /**
    * The maximum count of instances to retrieve.
    */
   protected int m_nMaxCount;

   /**
    * The limit of the number of instances retrieved in a single unlimited next().
    */
   protected int m_nLimit;

   /**
    * The query timeout in seconds (0 for unlimited, negative to use the default).
    */
   protected int m_nTimeout;

   /**
    * The number of Cartesian product rows in a step(Lookup) page.
    */
   protected int m_nPageSize = 128;

   /**
    * True if the end of the recordset has been reached.
    */
   protected boolean m_bEOF;

   /**
    * True if the end of the current page has been reached.
    */
   protected boolean m_bEOP;

   /**
    * True if debug logging is enabled.
    */
   protected boolean m_bDebug;

   /**
    * True if the generated query has already been logged.
    */
   protected boolean m_bQueryLogged;

   /**
    * True if the stack trace has already been logged.
    */
   protected boolean m_bStackLogged;

   // associations

   /**
    * The root query.
    */
   protected Query m_query;

   /**
    * The root class object.
    */
   protected Metaclass m_metaclass;

   /**
    * Array of instances corresponding to the current record, indexed by query node ordinal number.
    */
   protected Instance[] m_instanceArray;

   /**
    * The root instance list, read by the cursor.
    */
   protected InstanceList m_instanceList;

   /**
    * The set of query-instance pairs retrieved by the cursor, used to ignore the already read instances.
    */
   protected Lookup2D m_instanceSet = new HashTab2D();

   /**
    * Map of instances and annotation names to functions: Function[Instance][String].
    */
   protected Lookup2D m_annotationMap;

   /**
    * The output map: Instance[Query].
    */
   protected Lookup m_outputMap;

   /**
    * The instance list retrieved from the cache.
    */
   protected InstanceList m_cachedInstanceList;

   /**
    * The UOW for read locking.
    */
   protected UnitOfWork m_uow;

   /**
    * Iterator over the Cartesian product of the heterogeneous joins.
    */
   protected QueryCartesianProductIterator m_stepIterator;

   /**
    * The class logger.
    */
   protected Logger m_logger;

   /**
    * The long-running query logger.
    */
   protected Logger m_lrqLogger;

   // constructors

   /**
    * Constructs the cursor and allocates the necessary resources.
    * @param query The query.
    */
   protected GenericCursor(Query query)
   {
      init(query);
   }

   /**
    * Constructs the cursor, performing no initialization.
    */
   protected GenericCursor()
   {
   }

   // operations

   /**
    * Template method to initialize the cursor.
    * @param query The query with which to initialize the cursor.
    */
   protected void init(Query query)
   {
      assert query.isRoot();

      m_query = query;
      m_metaclass = query.getMetaclass();

      if (!query.isIdentity())
      {
         while (m_metaclass.getBase() != null)
         {
            m_metaclass = m_metaclass.getBase();
         }
      }

      m_nTimeout = query.getTimeout();

      if (m_nTimeout < 0 && (query.isCursor() || query.isLocking()))
      {
         m_nTimeout = Query.TIMEOUT_UNLIMITED;
      }

      m_logger = getLogger();
      m_lrqLogger = getLRQLogger();
      m_bDebug = m_logger.isDebugEnabled();

      init(query.getFragment());

      if (m_lWarningTimeout > 0 && !m_bDebug)
      {
         if (m_lrqLogger != null && m_lrqLogger.isWarnEnabled())
         {
            if (m_lrqLogger.isInfoEnabled())
            {
               if (m_lrqLogger.isDebugEnabled())
               {
                  m_lWarningTimeout >>= 1;

                  if (m_lrqLogger.isDumpEnabled())
                  {
                     m_lWarningTimeout >>= 1;
                  }
               }

               if (m_lWarningTimeout == 0)
               {
                  m_lWarningTimeout = 1;
               }
            }
            else
            {
               m_lWarningTimeout <<= 1;
            }
         }
         else
         {
            m_lWarningTimeout = 0;
         }
      }

      m_nLimit = m_query.getLimit();

      if (m_query.getWhere() != null && m_query.getWhere().isConstant())
      {
         // True where clause has been folded by Query
         m_bEOF = true;
      }
      else
      {
         run();
      }
   }

   /**
    * The cursor type name, used e.g. in log messages.
    */
   protected abstract String getType();

   /**
    * @return The class logger.
    */
   protected abstract Logger getLogger();

   /**
    * @return The long-running query logger. Can be null.
    */
   protected abstract Logger getLRQLogger();

   /**
    * Template method to initialize the cursor, including the adapter and timeouts.
    * @param fragment The fragment to set. Can be null if the data source does not support fragments.
    */
   protected abstract void init(DataSourceFragment fragment);

   /**
    * @return True if the duration log is enabled.
    */
   private boolean isDurationLogEnabled()
   {
      return m_bDebug || m_lWarningTimeout > 0;
   }

   /**
    * @return The current time offset in milliseconds from 1-Jan-1970 UTC,
    * or 0 if the duration log is disabled.
    */
   protected long getCurrentTime()
   {
      if (isDurationLogEnabled())
      {
         return System.currentTimeMillis();
      }

      return 0;
   }

   /**
    * Logs the operation duration.
    * @param lStartTime The start time.
    * @param bQuery True for query, false for retrieval.
    */
   protected void logDuration(long lStartTime, boolean bQuery)
   {
      if (isDurationLogEnabled())
      {
         long lDuration = System.currentTimeMillis() - lStartTime;

         if (m_bDebug || lDuration > m_lWarningTimeout)
         {
            int nLevel;

            if (m_bDebug)
            {
               nLevel = Logger.DEBUG;
            }
            else
            {
               nLevel = Logger.WARN;

               if (!m_bQueryLogged)
               {
                  logQuery(nLevel);
                  m_bQueryLogged = true;
               }
            }

            if (m_logger.isLevelEnabled(nLevel))
            {
               StringBuilder buf = new StringBuilder(128);

               if (bQuery)
               {
                  buf.append(getType() + " execution time:");
               }
               else
               {
                  buf.append("Retrieved ");
                  buf.append(Math.max(m_instanceList.getCount() - m_nOffset, 0));

                  if (m_nOffset > 0)
                  {
                     buf.append(", skipped ");
                     buf.append(m_nOffset);
                  }

                  buf.append(" instance(s) of ");
                  buf.append(m_query.getMetaclass().getName());

                  int nRecordCount = m_nRecordCount - m_nRecordOffset;

                  if (m_instanceList.getCount() < nRecordCount && nRecordCount != 0)
                  {
                     buf.append(" from ");
                     buf.append(nRecordCount);
                     buf.append(" record(s)");
                  }

                  buf.append(" in");
               }

               buf.append(' ');
               buf.append(lDuration);
               buf.append(" ms");

               boolean bPerfProblem = (lDuration > ((m_lWarningTimeout > 0) ? m_lWarningTimeout : 100));

               if (bPerfProblem)
               {
                  buf.append(" (potential performance problem)");

                  if (m_bStackLogged)
                  {
                     m_logger.log(nLevel, buf);
                  }
                  else
                  {
                     StackTrace t = new StackTrace();

                     m_query.getInvocationContext().getMachine().updateStackTrace(t);
                     m_logger.log(nLevel, buf, t);
                     m_bStackLogged = true;
                  }
               }
               else
               {
                  m_logger.log(nLevel, buf);
               }
            }
         }
      }
   }

   /**
    * Logs a query timeout.
    * @param lStartTime The start time offset in milliseconds from 1-Jan-1970 UTC,
    * or 0 if the duration log is disabled.
    * @param bQuery True to log the query.
    */
   protected void logTimeout(long lStartTime, boolean bQuery)
   {
      if (lStartTime != 0 && !m_query.isLocking())
      {
         long lDuration = System.currentTimeMillis() - lStartTime;

         if (bQuery)
         {
            logQuery(Logger.WARN);
         }

         if (m_logger.isWarnEnabled())
         {
            StackTrace t = new StackTrace();

            m_query.getInvocationContext().getMachine().updateStackTrace(t);
            m_logger.warn(getType() + " execution time: " + lDuration + " ms (potential performance problem)", t);
         }
      }
   }

   /**
    * Logs the generated query.
    * @param nLevel The log level.
    * @see Logger
    */
   protected abstract void logQuery(int nLevel);

   /**
    * Executes the query.
    */
   protected abstract void query() throws PersistenceException;

   /**
    * Runs the query.
    */
   protected void run() throws PersistenceException
   {
      m_bQueryLogged = false;
      m_nRecordCount = 0;
      m_nRecordOffset = 0;
      m_nLastMaxCount = m_query.getMaxCount();
      m_nOffset = Math.max(0, m_query.getOffset());

      if (m_query.isCached())
      {
         m_cachedInstanceList = m_query.readCached();
         m_bQueryLogged = true;
         m_nOffset = 0;
      }
      else
      {
         m_instanceArray = new Instance[m_query.getOutputQueryCount()];
         m_cachedInstanceList = null;

         InvocationContext context = m_query.getInvocationContext();
         int nRPC = context.getRPCCount() - context.getPersistCount();

         try
         {
            query();
         }
         finally
         {
            context.addPersistCount(context.getRPCCount() - context.getPersistCount() - nRPC);
         }
      }
   }

   /**
    * Fetches the next record of data.
    * @return True if the row has been retrieved, false for EOF.
    */
   protected abstract boolean fetch() throws PersistenceException;

   /**
    * Gets an unconverted value corresponding to a field from the current record.
    * This is a template method invoked by GenericCursor.getValue(Field).
    * @param field The field for which to get the value.
    */
   protected abstract Object getRawValue(Field field) throws PersistenceException;

   /**
    * Gets a value corresponding to a field from the current record.
    * @param field The field for which to get the value.
    */
   protected final Object getValue(Field field) throws PersistenceException
   {
      assert field.isPersistent();

      Object value = getRawValue(field);

      if (field.getConverter() != null)
      {
         value = field.getConverter().getForwardFunction().invoke(value);
      }

      return value;
   }

   /**
    * Gets a OID represented by given fields.
    * This is a helper method that getKey() can use for convenience (i.e. if getKey() happens
    * to have available an array of Field that needs converting to an OID).
    * @param keyFieldArray The array of key fields.
    * @param The resulting OID, or null, if any of the field values is null.
    */
   protected final OID getKey(Field[] keyFieldArray) throws PersistenceException
   {
      Object[] keyValueArray = new Object[keyFieldArray.length];

      for (int i = 0; i < keyFieldArray.length; ++i)
      {
         if ((keyValueArray[i] = getValue(keyFieldArray[i])) == null)
         {
            return null;
         }
      }

      return new OID(keyValueArray);
   }

   /**
    * Gets a OID represented by given fields.
    * @param item The mapping item, supplied by Query.getItem(), Query.getParentItem(), or
    * Query.getChildItem().
    * @return The resulting OID, or null, if any of the field values is null.
    */
   protected abstract OID getKey(Object item);

   /**
    * Gets the class object corresponding to a query node for the current record.
    * @param query The query node.
    * @return The class object.
    */
   protected Metaclass getMetaclass(Query query)
   {
      if (query == m_query)
      {
         return m_metaclass;
      }

      return (Metaclass)m_instanceArray[query.getParent().getOrdinal()]
         .getMetaclass().getDerivedAttribute(query.getAttribute()).getType();
   }

   /**
    * Releases the data source connection.
    */
   protected abstract void disconnect();

   /**
    * Releases any resources needed before EOF.
    */
   protected void release()
   {
      disconnect();
      m_instanceArray = null;
      m_instanceSet = null;
      m_annotationMap = null;
      m_outputMap = null;
      m_cachedInstanceList = null;
      m_uow = null;
      m_stepIterator = null;
   }

   /**
    * Releases the resources allocated by the cursor.
    */
   public void close()
   {
      m_bEOF = true;
      release();
      m_annotationMap = null;
      m_instanceList = null;
   }

   /**
    * Discards an attribute value, which could change when an instance is updated.
    * @param instance The instance from which to discard the value.
    * @param attribute The attribute, which value to discard.
    */
   protected static void discard(Instance instance, Attribute attribute)
   {
      if (attribute.getMetaclass().isUpcast(instance.getMetaclass()))
      {
         instance.discard(attribute, false);
      }
   }

   /**
    * Discards the query associations, which could change when an instance is updated.
    * @param instance The instance from which to discard the value.
    * @param query The query from which to take the associations.
    */
   protected static void discard(Instance instance, Query query)
   {
      for (Iterator itr = query.getAssocIterator(Query.ASSOC_QUERY); itr.hasNext();)
      {
         Query assoc = (Query)itr.next();

         if (assoc.isOutput())
         {
            discard(instance, assoc.getAttribute());
         }
      }
   }

   /**
    * Makes one step through the instances.
    * @param bDiscardExtra True to discard the last instance if the maximum number of instances has been reached.
    * @return True if the step has been executed, false if the end of the data has been reached.
    */
   protected boolean internalStep(boolean bDiscardExtra)
   {
      if (m_bEOF)
      {
         return false;
      }

      if (m_bEOP)
      {
         m_bEOP = false;
      }
      else if (bDiscardExtra &&
         m_nMaxCount >= 0 &&
         !m_query.isPlural() &&
         m_nRecordCount - m_nRecordOffset == m_nOffset + m_nMaxCount)
      {
         m_bEOF = true;
         m_bEOP = false;

         return false;
      }
      else if (fetch())
      {
         ++m_nRecordCount;
      }
      else if (!m_query.isPlural() // Not a multi-row-per-instance query
         || m_nLastMaxCount <= 0 // No max requested or no rows actually expected
         || !m_query.isLimited() // Not limited by adapter
         || m_nRecordCount < m_nLastMaxCount) // Retrieved less records than requested, so this has to be the last possible iteration
      {
         // Original recordset finished and no need to initialize for plural query
         m_bEOF = true;

         return false;
      }
      else
      {
         // Save the query count so it could be reset to original state after SQL generation
         int nQueryMaxCountSaved = m_query.getMaxCount();

         try
         {
            // Increment the max count for this run (exponentially double).
            // On overflow, the resulting negative means unlimited count.
            m_query.setMaxCount(m_nLastMaxCount << 1);

            // Requery with the new max count
            run();
         }
         finally
         {
            // Reset to original request
            m_query.setMaxCount(nQueryMaxCountSaved);
         }

         if (!fetch())
         {
            // Unable to get more on the second try => we are definitely done
            m_bEOF = true;

            return false;
         }

         ++m_nRecordCount;
         bDiscardExtra = true;
         // Skip everything past max count no matter what was originally requested,
         // as this iteration has a manually set max count with requerying.
      }

      // The UOW for read locking
      m_uow = m_query.getInvocationContext().getUnitOfWork();

      if (m_uow != null && !m_uow.isDirty())
      {
         m_uow = null;
      }

      boolean bSkipInstance = (m_instanceList.getCount() < m_nOffset);

      for (int nQuery = 0, nQueryCount = m_query.getOutputQueryCount(); nQuery < nQueryCount; ++nQuery)
      {
         Query query = m_query.getOutputQuery(nQuery);
         Attribute assoc = query.getAttribute();
         boolean bIdentity = query.isIdentity();
         boolean bParentIdentity;
         Instance container;
         Attribute reverse;
         OID oid;

         // First output query must be main query node.
         assert (nQuery == 0) == (query == m_query);

         // Query ordinal is the output query ordinal (unless query is joined heterogeneously).
         assert nQuery == query.getOrdinal() || query.getRoot() != m_query;

         // Query ordinal is, by definition, the output query ordinal of its root.
         assert query.getRoot().getOutputQuery(query.getOrdinal()) == query;

         // Get the query class instance OID

         if (query == m_query)
         {
            container = null;
            reverse = null;
            oid = (bIdentity) ? getKey(query.getItem()) : null;
            bParentIdentity = true;
         }
         else
         {
            assert bIdentity;

            Query parent = query.getParent();

            bParentIdentity = parent.isIdentity();
            container = m_instanceArray[parent.getOrdinal()];

            if (container == null ||
               !assoc.getMetaclass().isUpcast(container.getMetaclass()) && bParentIdentity)
            {
               if (query.getRoot() == m_query)
               {
                  m_instanceArray[query.getOrdinal()] = null;
               }

               continue;
            }

            if (bParentIdentity)
            {
               assoc = container.getMetaclass().getDerivedAttribute(assoc);
               reverse = assoc.getReverse();
   
               if (reverse != null && reverse.isCollection())
               {
                  reverse = null;
               }
            }
            else
            {
               reverse = null;
            }

            oid = getKey(query.getParentItem());
         }

         InstanceList instanceList;
         boolean bAdd = true;

         if (container == null)
         {
            instanceList = m_instanceList;
         }
         else
         {
            Object value;

            if (bParentIdentity)
            {
               value = container.getOldValueDirect(assoc.getOrdinal());
   
               if (value == Undefined.VALUE)
               {
                  if (assoc.isCollection())
                  {
                     instanceList = new InstanceArrayList();
                     instanceList.setAssociation(container, assoc, true);
                     instanceList.setLazy(false);
                     container.setOldValueDirect(assoc.getOrdinal(), instanceList);
                  }
                  else
                  {
                     instanceList = null;
   
                     if (oid == null)
                     {
                        container.setOldValueDirect(assoc.getOrdinal(), null);
                     }
                  }
               }
               else
               {
                  if (assoc.isCollection() && !(value instanceof Undefined))
                  {
                     instanceList = (InstanceList)value;
                     instanceList.setLazy(false);
                  }
                  else
                  {
                     instanceList = null;
                     bAdd = false;
                  }
               }
            }
            else
            {
               instanceList = null;
            }
         }

         Metaclass metaclass;

         if (bIdentity)
         {
            if (oid == null)
            {
               if (query.getRoot() == m_query)
               {
                  m_instanceArray[query.getOrdinal()] = null;
               }

               continue;
            }

            if (query.getRoot() != m_query && !query.isLazy())
            {
               if (!bSkipInstance)
               {
                  query.addParentInstance(container, oid);
               }

               continue;
            }

            Field field = query.getTypeCodeField();

            if (field != null)
            {
               metaclass = query.getPersistenceMapping().findMetaclassByTypeCode(getValue(field));

               if (metaclass == null)
               {
                  m_instanceArray[query.getOrdinal()] = null;

                  continue;
               }
            }
            else
            {
               metaclass = getMetaclass(query);
            }
         }
         else
         {
            metaclass = getMetaclass(query);
         }

         InstanceRef ref = m_query.getInvocationContext().findInstanceRef(metaclass, oid);
         Instance instance = (ref == null) ? null : ref.getInstance();

         // Check if the instance has already been retrieved
         if (instance != null)
         {
            // Already retrieved instance
            if (instance.isLazy())
            {
               if (query.isLazy())
               {
                  if (!metaclass.isUpcast(instance.getLazyMetaclass()))
                  {
                     instance.setLazyMetaclass(metaclass);
                  }
               }
               else
               {
                  instance.setMetaclass(metaclass);
               }
            }

            if (instanceList != null)
            {
               bAdd = !instanceList.contains(instance);

               if (bAdd &&
                  container == null &&
                  m_nMaxCount >= 0 &&
                  instanceList.getCount() == m_nOffset + m_nMaxCount)
               {
                  m_bEOF = bDiscardExtra;
                  m_bEOP = !bDiscardExtra;

                  return false;
               }
            }

            if (bAdd)
            {
               switch (instance.getState())
               {
                  case Instance.DELETED:
                     bAdd = false;
                     break;

                  case Instance.DIRTY:
                     if (reverse != null)
                     {
                        Object reverseValue = instance.getValueDirect(reverse.getOrdinal());

                        if (reverseValue != Undefined.VALUE && reverseValue != container)
                        {
                           bAdd = false;
                        }
                     }

                     break;
               }

               if (!bAdd && instanceList == null)
               {
                  if (container.getValueDirect(assoc.getOrdinal()) == Undefined.VALUE)
                  {
                     // Set the new value to null, old value to the retrieved instance
                     container.setValueDirect(assoc.getOrdinal(), null);
                  }

                  bAdd = true;
               }
            }

            if (m_instanceSet.get(query, instance) == null)
            {
               // Merge the not yet retrieved attribute values

               if (!bSkipInstance)
               {
                  // Overwrite only if the instance is clean
                  boolean bOverwrite = (instance.getUnitOfWork() == null);
                  Field lockingField = query.getLockingField();

                  if (lockingField != null)
                  {
                     Attribute lockingAttribute = lockingField.getAttribute();
                     Object oldValue = instance.getNewValueDirect(lockingAttribute.getOrdinal());

                     if (oldValue != Undefined.VALUE)
                     {
                        // Check the old lock value
                        Primitive primitive = (Primitive)lockingAttribute.getType();
                        Object value = getValue(lockingField);

                        if (((Boolean)primitive.findNEFunction(primitive).invoke(oldValue, value)).booleanValue())
                        {
                           // The lock values do not match
                           // If the instance is dirty/deleted or share-locked, throw an exception
                           if (!bOverwrite || ref.isLocked())
                           {
                              throw new OptimisticLockException(instance);
                           }

                           // Discard all the instance attributes
                           for (int i = 0, n = metaclass.getInstanceAttributeCount(); i != n; ++i)
                           {
                              Attribute attribute = metaclass.getInstanceAttribute(i);

                              if (attribute.isPersistent())
                              {
                                 discard(instance, attribute);
                              }
                           }

                           instance.setOldValueDirect(lockingAttribute.getOrdinal(), value);
                        }
                        else
                        {
                           if (bOverwrite)
                           {
                              discard(instance, query);

                              if (reverse != null && bAdd && !instance.isLazy())
                              {
                                 instance.setValueDirect(reverse.getOrdinal(), container);
                              }

                              bOverwrite = false;
                           }
                        }
                     }
                     else
                     {
                        // If the instance is clean, discard the
                        // instance assocs referenced in this query
                        if (bOverwrite)
                        {
                           discard(instance, query);
                        }
                     }
                  }
                  else
                  {
                     // If the instance is clean, discard the
                     // instance assocs referenced in this query
                     if (bOverwrite)
                     {
                        discard(instance, query);
                     }
                  }

                  if (bOverwrite && reverse != null && bAdd && !instance.isLazy())
                  {
                     instance.setValueDirect(reverse.getOrdinal(), container);
                  }

                  addInstance(instance, ref, metaclass, query, bOverwrite);
               }
            }
         }
         else
         {
            // New instance

            if (container == null &&
               m_nMaxCount >= 0 &&
               instanceList.getCount() == m_nOffset + m_nMaxCount)
            {
               m_bEOF = bDiscardExtra;
               m_bEOP = !bDiscardExtra;

               return false;
            }

            instance = new Instance(metaclass, query.isLazy(), m_query.getInvocationContext());
            ref = instance.cache(oid);

            if (!bSkipInstance)
            {
               addInstance(instance, ref, metaclass, query, true);
            }
         }

         if (bAdd)
         {
            if (instanceList != null)
            {
               instanceList.add(instance, InstanceList.DIRECT);
            }
            else if (bParentIdentity)
            {
               container.setOldValueDirect(assoc.getOrdinal(), instance);
            }
            else
            {
               container.setAnnotation(assoc.getName(), instance);
            }

            if (reverse != null && !instance.isLazy() && instance.getOldValueDirect(reverse.getOrdinal()) == Undefined.VALUE)
            {
               instance.setOldValueDirect(reverse.getOrdinal(), container);
            }
         }

         // If query is heterogeneous then this node is finished--the lazy instance has been created.
         if (query.getRoot() != m_query)
         {
            continue;
         }

         if (bSkipInstance)
         {
            break;
         }

         m_instanceArray[query.getOrdinal()] = instance;

         if (m_outputMap != null)
         {
            m_outputMap.put(query, instance);
         }

         if (query.isJoin() && bAdd)
         {
            query.join(instance, getKey(query.getChildItem()));
         }
      }

      return true;
   }

   /**
    * Sets the primitive attribute values on the instance.
    * @param instance The instance to receive the values.
    * @param ref The instance reference.
    * @param metaclass The class being read.
    * @param query The query providing the values.
    * @param bOverwrite True to overwrite values in the instance.
    */
   protected void addInstance(Instance instance, InstanceRef ref, Metaclass metaclass, Query query, boolean bOverwrite)
   {
      for (Field field = query.getFirstOutputField(); field != null; field = field.getNext())
      {
         Attribute attribute = field.getAttribute();

         // Consider only this class and inherited attributes
         if (attribute != null && attribute.getMetaclass().isUpcast(metaclass))
         {
            int nOrdinal = attribute.getOrdinal();

            if (bOverwrite || instance.getOldValueDirect(nOrdinal) == Undefined.VALUE)
            {
               instance.setOldValueDirect(nOrdinal, getValue(field));
            }
         }

         String sAnnotation = field.getAnnotation();

         if (sAnnotation != null)
         {
            if (field.isPersistent())
            {
               instance.setAnnotation(sAnnotation, getValue(field));
            }
            else
            {
               Object mapping = field.getMapping();

               if (mapping == null)
               {
                  instance.setAnnotation(sAnnotation, field.getItem());
               }
               else if (mapping instanceof Source)
               {
                  if (mapping instanceof Field)
                  {
                     instance.setAnnotation(sAnnotation, getValue((Field)mapping));
                  }
                  else
                  {
                     Query assoc = (Query)mapping;
                     OID oid = getKey(assoc.getItem());
                     Instance ainst = null;

                     if (oid != null)
                     {
                        InstanceRef aref = m_query.getInvocationContext().findInstanceRef(assoc.getMetaclass(), oid);

                        if (aref != null)
                        {
                           ainst = aref.getInstance();
                        }

                        if (ainst == null)
                        {
                           ainst = new Instance(assoc.getMetaclass(), assoc.isLazy(), m_query.getInvocationContext());
                           ainst.cache(oid);
                        }
                     }

                     instance.setAnnotation(sAnnotation, ainst);
                  }
               }
               else
               {
                  if (m_annotationMap == null)
                  {
                     m_annotationMap = new HashTab2D();
                  }

                  m_annotationMap.put(instance, sAnnotation, mapping);
               }
            }
         }
      }

      // Set the readable attribute value if it is already in the where clause
      if (query.isSecure() && metaclass.getReadAccessAttribute() != null && !instance.isLazy())
      {
         int nOrdinal = metaclass.getReadAccessAttribute().getOrdinal();

         if (bOverwrite || instance.getOldValueDirect(nOrdinal) == Undefined.VALUE)
         {
            instance.setOldValueDirect(nOrdinal, Boolean.TRUE);
         }
      }

      // Share-lock the instance
      if (m_uow != null)
      {
         m_uow.lock(ref, false);
      }

      m_instanceSet.put(query, instance, instance);
   }

   /**
    * Retrieves the next page of instances into m_instanceList.
    * @param nMaxCount The maximum number of instances to retrieve.
    * @param bStep True to execute only one internal step.
    */
   protected void internalNext(int nMaxCount, boolean bStep)
   {
      long lStartTime = (bStep) ? 0 : getCurrentTime();
      boolean bDiscardExtra = false;

      m_nRecordOffset = m_nRecordCount;

      if (m_query.getMaxCount() >= 0)
      {
         m_nMaxCount = m_query.getMaxCount() - m_nTotalCount;

         if (m_nMaxCount < 0)
         {
            m_nMaxCount = 0;
         }

         if (nMaxCount >= 0 && nMaxCount < m_nMaxCount)
         {
            m_nMaxCount = nMaxCount;
         }
         else
         {
            bDiscardExtra = true;
         }
      }
      else
      {
         m_nMaxCount = nMaxCount;
      }

      if (m_instanceList == null)
      {
         if (m_cachedInstanceList != null && (m_nMaxCount < 0 || m_nMaxCount > m_cachedInstanceList.size()))
         {
            m_instanceList = m_cachedInstanceList;
            m_bEOF = true;
         }
         else
         {
            m_instanceList = new InstanceArrayList((m_nMaxCount >= 0) ? m_nMaxCount + 1 : 9);
         }
      }
      else
      {
         m_instanceList.clear();
      }

      if (m_cachedInstanceList != null)
      {
         if (m_instanceList != m_cachedInstanceList)
         {
            int nCount = m_cachedInstanceList.size() - m_nTotalCount;

            if (nCount <= 0)
            {
               m_bEOF = true;
            }
            else
            {
               if (m_nMaxCount > 0 && m_nMaxCount < nCount)
               {
                  nCount = m_nMaxCount;
               }

               if (nCount > m_nLimit && m_nLimit > 0 && nMaxCount < 0)
               {
                  throw new DataVolumeException("err.persistence.instanceCount",
                     new Object[]{m_query.getMetaclass().getName(), Primitive.createInteger(m_nLimit)});
               }

               for (int i = m_nTotalCount; --nCount >= 0; ++i)
               {
                  m_instanceList.add(m_cachedInstanceList.getInstance(i), InstanceList.DIRECT);
               }
            }
         }
      }
      else
      {
         while (internalStep(bDiscardExtra) && !bStep)
         {
            int nCount = m_instanceList.getCount();

            if (nCount > m_nLimit && m_nLimit > 0 && nMaxCount < 0)
            {
               throw new DataVolumeException("err.persistence.instanceCount",
                  new Object[]{m_query.getMetaclass().getName(), Primitive.createInteger(m_nLimit)});
            }
         }
      }

      m_nTotalCount += Math.max(m_instanceList.getCount() - m_nOffset, 0);

      if (!bStep)
      {
         completeQuery();
         logDuration(lStartTime, false);
      }

      if (m_bEOF)
      {
         release();
      }
      else
      {
         if (m_instanceSet != null)
         {
            m_instanceSet.clear();
         }
      }

      // Remove the empty elements according to the query offset

      InstanceList instanceList;

      if (m_nOffset > 0)
      {
         int nCount = m_instanceList.getCount() - m_nOffset;

         if (nCount < 0)
         {
            nCount = 0;
         }

         instanceList = new InstanceArrayList(nCount);

         for (int i = m_nOffset; nCount > 0; ++i, --nCount)
         {
            instanceList.add(m_instanceList.getInstance(i), InstanceList.DIRECT);
         }

         m_instanceList = instanceList;
         m_nOffset = 0;
      }

      if (m_bDebug)
      {
         m_logger.dump(m_instanceList);
      }
   }

   /**
    * Completes the query.
    */
   protected void completeQuery()
   {
      m_query.complete();

      if (m_annotationMap != null)
      {
         for (Lookup2D.Iterator itr = m_annotationMap.valueIterator(); itr.hasNext();)
         {
            Function fun = (Function)itr.next();
            Instance instance = (Instance)itr.getKey1();

            instance.setAnnotation((String)itr.getKey2(),
               instance.getContext().getMachine().invoke(fun, instance, (Pair)null));
         }

         if (m_bEOF)
         {
            m_annotationMap = null;
         }
         else
         {
            m_annotationMap.clear();
         }
      }
   }

   /**
    * Retrieves the next page of instances.
    * @param nMaxCount The maximum number of instances to retrieve.
    * @return The list of the retrieved instances.
    */
   public InstanceList next(int nMaxCount)
   {
      if (m_bEOF)
      {
         return new InstanceArrayList(0);
      }

      internalNext(nMaxCount, false);

      InstanceList instanceList = m_instanceList;

      m_instanceList = null;

      return instanceList;
   }

   /**
    * Retrieves the next instance.
    * @return The next instance, or null if none.
    */
   public Instance next()
   {
      if (m_bEOF)
      {
         return null;
      }

      internalNext(1, false);

      if (m_instanceList.isEmpty())
      {
         return null;
      }

      return m_instanceList.remove(0, InstanceList.DIRECT);
   }

   /**
    * Makes one step through the instances.
    * @param map The query to instance output map.
    * @return True if the step has been made, or false if EOF has been reached.
    */
   public boolean step(Lookup map)
   {
      if (map != null)
      {
         map.clear();
      }

      // Advance to next row in Cartesian product using results stored in the Cartesian product iterator
      if (m_stepIterator != null && m_stepIterator.hasNext())
      {
         m_stepIterator.step(map);

         return true;
      }

      if (!m_query.isHeterogeneous() && map != null)
      {
         m_outputMap = map;
      }

      if (!m_bEOF)
      {
         internalNext(1, true);
      }

      // Store the new instance and its sub-collections in the Cartesian product iterator
      if (!m_bEOF)
      {
         if (m_query.isHeterogeneous())
         {
            ArrayList stepList = new ArrayList(m_nPageSize);

            // Read a page of steps
            for (int i = 1; i < m_nPageSize && !m_bEOF; i++)
            {
               stepList.add(m_instanceArray);
               m_instanceArray = new Instance[m_instanceArray.length];
               internalNext(1, true);
            }

            if (!m_bEOF)
            {
               stepList.add(m_instanceArray);
            }

            completeQuery();  // aggregate heterogeneous joins across the page to reduce RPCs

            m_stepIterator = new QueryCartesianProductIterator(m_query, stepList);
            m_stepIterator.step(map);

            return true;
         }

         completeQuery();
      }

      m_outputMap = null;

      return !m_bEOF;
   }

   /**
    * @see java.lang.Object#finalize()
    */
   protected void finalize() throws Throwable
   {
      close();
   }

   /**
    * @see nexj.core.util.Printable#printOn(nexj.core.util.PrintWriter)
    */
   public void printOn(PrintWriter writer) throws IOException
   {
      writer.write(ObjUtil.getShortClassName(this));
      writer.write("(total=");
      writer.print(m_nTotalCount);
      writer.write(", query=");
      writer.print(m_query);
      writer.write(')');
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return PrintWriter.toString(this);
   }
}
