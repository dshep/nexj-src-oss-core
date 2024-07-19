// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import javax.transaction.Status;
import javax.transaction.Synchronization;
import javax.transaction.SystemException;
import javax.transaction.Transaction;

import nexj.core.integration.Sender;
import nexj.core.integration.sync.DefaultSyncEngine;
import nexj.core.meta.Accessor;
import nexj.core.meta.Component;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Metadata;
import nexj.core.meta.Primitive;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.channel.jms.MessageQueue;
import nexj.core.meta.integration.channel.queueing.ObjectQueue;
import nexj.core.meta.persistence.AttributeMapping;
import nexj.core.persistence.OID;
import nexj.core.persistence.PersistenceAdapter;
import nexj.core.persistence.PersistenceException;
import nexj.core.persistence.PersistenceHook;
import nexj.core.persistence.Work;
import nexj.core.rpc.RPCException;
import nexj.core.rpc.RPCUtil;
import nexj.core.rpc.Request;
import nexj.core.rpc.TransferObject;
import nexj.core.rpc.jms.JMSSender;
import nexj.core.rpc.queueing.ObjectSender;
import nexj.core.rpc.timer.PersistentTimer;
import nexj.core.runtime.InstanceRef.Lock;
import nexj.core.runtime.license.License;
import nexj.core.runtime.license.LicenseException;
import nexj.core.scripting.Function;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.EmptyIterator;
import nexj.core.util.GenericHashTab;
import nexj.core.util.HashDeque;
import nexj.core.util.HashHolder;
import nexj.core.util.HashTab;
import nexj.core.util.HashTab2D;
import nexj.core.util.Holder;
import nexj.core.util.IdentityHashTab;
import nexj.core.util.LinkedHashTab;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.Lookup2D;
import nexj.core.util.LookupDeque;
import nexj.core.util.ObjUtil;
import nexj.core.util.PropertyIterator;
import nexj.core.util.UncheckedException;
import nexj.core.util.Undefined;

/**
 * Tracks the changes made to object instances
 * and orders the persistence operations.
 */
public class UnitOfWork implements InvocationContextAware, Synchronization
{
   // constants

   /**
    * The JMS system queue.
    */
   protected final static String JMS_SYSTEM_QUEUE = "SystemQueue";

   /**
    * The JMS system topic.
    */
   protected final static String JMS_SYSTEM_TOPIC = "SystemTopic";

   /**
    * The system object queue.
    */
   protected final static String SYSTEM_QUEUE = "ObjectSystemQueue";

   /**
    * The transaction is managed by the unit of work.
    */
   public final static byte TX_MANAGED = 0;

   /**
    * The transaction is managed externally.
    */
   public final static byte TX_EXTERNAL = 1;

   /**
    * No transaction association is allowed.
    */
   public final static byte TX_NONE = 2;

   /**
    * Default caching mode.
    */
   public final static byte CACHE_DEFAULT = 0x00;

   /**
    * The cache is unpartitioned.
    */
   public final static byte CACHE_UNPARTITIONED = 0x01;

   /**
    * Not committing.
    */
   protected final static byte COMMIT_NONE = 0;

   /**
    * Invoking pending events.
    */
   protected final static byte COMMIT_EVENTS = 1;

   /**
    * Validate the changed instances.
    */
   protected final static byte COMMIT_VALIDATION = 2;

   /**
    * Work items in progress.
    */
   protected final static byte COMMIT_WORK = 3;

   /**
    * Replicating changes to subscribers.
    */
   protected final static byte COMMIT_SYNC = 4;

   /**
    * Cache preparation in progress.
    */
   protected final static byte COMMIT_CACHE = 5;

   /**
    * Rolling back.
    */
   protected final static byte COMMIT_UNDO = 6;

   /**
    * The instance state tracking information is not removed for deleted instances.
    */
   protected final static int CHANGE_KEPT = 0x100;

   /**
    * The asynchronous JMS message factory.
    */
   public final static JMSMessageFactory JMS_MESSAGE_FACTORY = new JMSMessageFactory();

   /**
    * The asynchronous ObjectQueue message factory.
    */
   public final static ObjectMessageFactory OBJECT_MESSAGE_FACTORY = new ObjectMessageFactory();

   /**
    * Broadcast to all properties.
    */
   protected final static TransferObject BROADCAST_ALL_PROPERTIES = new TransferObject(1);

   /**
    * Broadcast to others properties.
    */
   protected final static TransferObject BROADCAST_OTHER_PROPERTIES = new TransferObject(1);

   static
   {
      BROADCAST_ALL_PROPERTIES.setValue(ObjectSender.RECEIVER, Symbol.define("all"));
      BROADCAST_OTHER_PROPERTIES.setValue(ObjectSender.RECEIVER, Symbol.define("other"));
   }

   // attributes

   /**
    * The transaction mode, one of the TX_* constants.
    */
   protected byte m_nTxMode;

   /**
    * The auto-commit flag.
    * True if the UOW should be committed even on failed (complete(false)) invocation context.
    */
   protected boolean m_bAutoCommit;

   /**
    * True if the unit of work is in progress.
    */
   protected boolean m_bDirty;

   /**
    * True if changes have been detected during persistence operations.
    */
   protected boolean m_bSuspect;

   /**
    * True if the transaction will be committed.
    */
   protected boolean m_bTransient;

   /**
    * The raw mode flag.
    * True for data synchronization.
    */
   protected boolean m_bRaw;

   /**
    * The locking flag.
    * True to share-lock instance references.
    */
   protected boolean m_bLocking;

   /**
    * Commit stage, one of the COMMIT_* constants.
    */
   protected byte m_nCommitStage;

   /**
    * The maximum changed instance count (negative for unlimited).
    */
   protected int m_nMaxChangeCount;

   /**
    * The maximum pending event invocation iterations (negative for unlimited).
    */
   protected int m_nMaxEventCount = 1000;

   /**
    * The transaction timestamp.
    */
   protected long m_lTimestamp;

   /**
    * The unit of work timeout in milliseconds (0 means infinity).
    */
   protected long m_lTimeout;

   /**
    * The next timeout.
    */
   protected long m_lNextTimeout;

   /**
    * The current fragment name.
    * Used only during commit.
    */
   protected String m_sFragmentName;

   // associations

   /**
    * The JTA transaction.
    */
   protected Transaction m_transaction;

   /**
    * The first instance reference share-lock.
    * All the instances read in a UOW are organized
    * in a linked list for optimistic share-locking.
    */
   protected InstanceRef.Lock m_readLock;

   /**
    * The set of instances for committing.
    */
   protected HashDeque m_instanceSet;

   /**
    * The set of instances for validation.
    */
   protected Set m_validationSet;

   /**
    * Maps a modified collection to the old collection,
    * if it has been instantiated, or null: InstanceList[InstanceList].
    */
   protected Lookup m_collectionMap;

   /**
    * Maps a modified collection to the pre-collection (the state before any updates since
    * the last life cycle event invocation), if it has been instantiated: InstanceList[InstanceList].
    */
   protected Lookup m_preCollectionMap;

   /**
    * The collection of instances with modified OIDs.
    */
   protected List m_oidChangeList;

   /**
    * The instance change state map.
    */
   protected Lookup m_stateMap;

   /**
    * Work item set.
    */
   protected Holder m_workSet;

   /**
    * Instance replication set: [Instance, sFragmentName]
    */
   protected Lookup2D m_replicationSet;

   /**
    * Synchronization command map from an arbitrary key: Function[Object].
    */
   protected LookupDeque m_synchronizerMap;

   /**
    * Map from an arbitrary key to a (function . args) pair, where function is a scheme function
    * taking as its first argument the transaction status (int).
    */
   protected LookupDeque m_compensatorMap;

   /**
    * Denormalization resolution list: Instance[2*n], AttributeMapping[2*n+1].
    */
   protected List m_denormList;

   /**
    * Map of collections of asynchronous messages to send: Object[][Channel].
    */
   protected Lookup m_messageMap;

   /**
    * Cache map: Object[Object].
    */
   protected Lookup m_cacheMap;

   /**
    * Set of keys to be removed from the cache: [Object].
    */
   protected Holder m_uncacheSet;

   /**
    * Transient cache map: Object[Object].
    * @see InvocationContext
    */
   protected Lookup m_transientMap;

   /**
    * Local cache map: Object[Object].
    * Invalidated upon commit/rollback.
    */
   protected Lookup m_localMap;

   /**
    * The property map.
    * Invalidated upon commit/rollback.
    */
   protected GenericSerializablePropertyMap m_propertyMap;

   /**
    * Managed resource list: Resource[].
    */
   protected List m_resourceList;

   /**
    * The unit of work that has to be completed before this one.
    */
   protected UnitOfWork m_prev;

   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(UnitOfWork.class);

   /**
    * Constructor for the UncacheCommand, if available.
    */
   protected static Constructor s_uncacheConstructor;

   // Load the constructor of UncacheCommand, if available.

   static
   {
      try
      {
         s_uncacheConstructor = Class.forName("nexj.core.runtime.platform.generic.UncacheCommand")
            .getConstructor(new Class[] {Holder.class, long.class});
      }
      catch (Throwable t) {} // UncacheCommand is not available
   }

   // constructors

   /**
    * Constructs the unit of work.
    * @param context The invocation context.
    * @param transaction The JTA transaction. Can be null.
    * @param nTxMode The transaction mode, one of the TX_* constants.
    */
   public UnitOfWork(InvocationContext context, Transaction transaction, byte nTxMode)
   {
      assert transaction == null || nTxMode != TX_NONE;

      m_context = context;
      m_nMaxChangeCount = context.getMaxUnitOfWorkChangeCount();
      m_lTimestamp = System.currentTimeMillis();
      m_nTxMode = nTxMode;
      associateWithTransaction(transaction);
   }

   // operations

   /**
    * Sets the invocation context.
    * @param context The invocation context to set.
    */
   public void setInvocationContext(InvocationContext context)
   {
      m_context = context;
   }

   /**
    * @return The invocation context.
    */
   public InvocationContext getInvocationContext()
   {
      return m_context;
   }

   /**
    * Sets the unit of work that has to be completed before this one.
    * @param prev The unit of work that has to be completed before this one to set.
    */
   public void setPrevious(UnitOfWork prev)
   {
      m_prev = prev;
   }

   /**
    * @return The unit of work that has to be completed before this one. Can be null.
    */
   public UnitOfWork getPrevious()
   {
      return m_prev;
   }

   /**
    * Sets the maximum changed instance count.
    * @param nMaxChangeCount The maximum changed instance count to set (negative for unlimited).
    */
   public void setMaxChangeCount(int nMaxChangeCount)
   {
      m_nMaxChangeCount = nMaxChangeCount;
   }

   /**
    * @return The maximum changed instance count (negative for unlimited).
    */
   public int getMaxChangeCount()
   {
      return m_nMaxChangeCount;
   }

   /**
    * Sets the maximum pending event invocation count.
    * @param nMaxEventCount The maximum pending event invocation count to set (negative for unlimited).
    */
   public void setMaxEventCount(int nMaxEventCount)
   {
      m_nMaxEventCount = nMaxEventCount;
   }

   /**
    * @return The maximum pending event invocation count (negative for unlimited).
    */
   public int getMaxEventCount()
   {
      return m_nMaxEventCount;
   }

   /**
    * Sets the auto-commit flag.
    * @param bAutoCommit The auto-commit flag to set.
    */
   public void setAutoCommit(boolean bAutoCommit)
   {
      m_bAutoCommit = bAutoCommit;
   }

   /**
    * @return The auto-commit flag.
    */
   public boolean isAutoCommit()
   {
      return m_bAutoCommit;
   }

   /**
    * @return The transaction mode, on of the TX_* constants.
    */
   public byte getTxMode()
   {
      return m_nTxMode;
   }

   /**
    * @return True if the unit of work can be associated with a new transaction.
    */
   public boolean isReusable()
   {
      return (!m_bDirty || m_bTransient) && m_bTransient == m_context.isTransient() && m_nTxMode != TX_NONE;
   }

   /**
    * @return True if the unit of work is in progress.
    */
   public boolean isDirty()
   {
      return m_bDirty;
   }

   /**
    * Sets the transient flag.
    * @param bTransient The transient flag to set.
    */
   public void setTransient(boolean bTransient)
   {
      m_bTransient = bTransient;
   }

   /**
    * @return The transient flag.
    */
   public boolean isTransient()
   {
      return m_bTransient;
   }

   /**
    * Sets the raw mode flag.
    * @param bRaw The raw mode flag to set.
    */
   public void setRaw(boolean bRaw)
   {
      m_bRaw = bRaw;
   }

   /**
    * @return The raw mode flag.
    */
   public boolean isRaw()
   {
      return m_bRaw;
   }

   /**
    * Sets the locking flag.
    * @param bLocking The locking flag to set.
    */
   public void setLocking(boolean bLocking)
   {
      m_bLocking = bLocking;
   }

   /**
    * @return The locking flag.
    */
   public boolean isLocking()
   {
      return m_bLocking;
   }

   /**
    * Sets the JTA transaction.
    * @param transaction The JTA transaction to set.
    */
   public void setTransaction(Transaction transaction)
   {
      assert transaction == null || m_nTxMode != TX_NONE;

      if (transaction != m_transaction)
      {
         if (m_resourceList != null)
         {
            for (int i = m_resourceList.size() - 1; i >= 0; --i)
            {
               Resource res = (Resource)m_resourceList.get(i);

               if (res.getTransaction() != transaction && res.getRef() == 0)
               {
                  res.release();
                  m_resourceList.remove(i);
               }
            }
         }

         associateWithTransaction(transaction);
      }
   }

   /**
    * @return The JTA transaction. Can be null.
    */
   public Transaction getTransaction()
   {
      return m_transaction;
   }

   /**
    * @return The transaction status (one of Status.STATUS_* constants).
    * @see javax.transaction.Status
    */
   public int getStatus()
   {
      if (m_transaction == null)
      {
         return Status.STATUS_NO_TRANSACTION;
      }

      try
      {
         return m_transaction.getStatus();
      }
      catch (Throwable t)
      {
         return Status.STATUS_UNKNOWN;
      }
   }

   /**
    * Associates the UOW with a JTA transaction.
    * @param transaction The transaction to associate with.
    */
   protected void associateWithTransaction(Transaction transaction)
   {
      m_transaction = transaction;

      if (transaction != null)
      {
         m_bDirty = true;

         try
         {
            if (transaction.getStatus() == Status.STATUS_ACTIVE)
            {
               transaction.registerSynchronization(this);
            }
         }
         catch (Throwable t)
         {
            ObjUtil.rethrow(t);
         }
      }
   }

   /**
    * Sets the unit of work timeout in milliseconds (0 means infinity).
    * @param lTimeout The unit of work timeout in milliseconds (0 means infinity) to set.
    */
   public void setTimeout(long lTimeout)
   {
      m_lTimeout = lTimeout;
   }

   /**
    * @return The unit of work timeout in milliseconds (0 means infinity).
    */
   public long getTimeout()
   {
      return m_lTimeout;
   }

   /**
    * Checks transaction validity.
    */
   public void checkTransaction() throws TransactionException
   {
      if (m_transaction != null)
      {
         try
         {
            switch (m_transaction.getStatus())
            {
               case Status.STATUS_MARKED_ROLLBACK:
               case Status.STATUS_ROLLING_BACK:
               case Status.STATUS_ROLLEDBACK:
                  throw new RollbackException();
            }
         }
         catch (SystemException e)
         {
         }
      }

      if (m_lTimeout > 0 && System.currentTimeMillis() - m_lTimestamp > m_lTimeout)
      {
         throw new UnitOfWorkTimeoutException(m_lTimeout);
      }
   }

   /**
    * Checks the software license.
    * @throws LicenseException if the license has expired.
    */
   public void checkLicense() throws LicenseException
   {
      License.check(m_lTimestamp);
   }

   /**
    * @return The transaction start time.
    */
   public long getTime()
   {
      return m_lTimestamp;
   }

   /**
    * Adds a share-lock to the UOW.
    * Does not add it to the instance reference.
    * @param lock The lock to add.
    */
   protected void addLock(Lock lock)
   {
      assert lock.m_uow == this;

      if (m_readLock != null)
      {
         m_readLock.m_prev = lock;
      }

      lock.m_next = m_readLock;
      m_readLock = lock;
   }

   /**
    * Removes a share-lock from the UOW.
    * Does not remove it from the instance reference.
    * @param lock The lock to remove.
    */
   protected void removeLock(Lock lock)
   {
      assert lock.m_uow == this;

      if (lock.m_next != null)
      {
         lock.m_next.m_prev = lock.m_prev;
      }

      if (lock.m_prev != null)
      {
         lock.m_prev.m_next = lock.m_next;
      }
      else
      {
         assert lock == m_readLock;

         m_readLock = lock.m_next;
      }
   }

   /**
    * Share-locks an instance reference according to the locking flag.
    * @param ref The instance reference to lock. Can be null.
    * @param bForce True to override the UOW locking flag.
    */
   public void lock(InstanceRef ref, boolean bForce)
   {
      if ((bForce | m_bLocking) && ref != null)
      {
         ref.lock(this);
      }
   }

   /**
    * Share-locks an instance.
    * @param instance The instance to lock.
    */
   public void lock(Instance instance)
   {
      if (instance.getOID() != null)
      {
         InstanceRef ref = m_context.findInstanceRef(instance.getLazyMetaclass(), instance.getOID());

         if (ref != null)
         {
            ref.lock(this);
         }
      }
   }

   /**
    * Read-unlocks an instance.
    * @param instance The instance to lock.
    */
   public void unlock(Instance instance)
   {
      if (instance.getOID() != null)
      {
         InstanceRef ref = m_context.findInstanceRef(instance.getLazyMetaclass(), instance.getOID());

         if (ref != null)
         {
            Lock lock = ref.unlink(this);

            if (lock != null)
            {
               removeLock(lock);
            }
         }
      }
   }

   /**
    * Unlocks all the instances locked by the UOW.
    */
   protected void unlock()
   {
      while (m_readLock != null)
      {
         m_readLock.m_ref.unlink(this);
         m_readLock = m_readLock.m_next;
      }

      m_bLocking = false;
   }

   /**
    * Adds an instance to the change set.
    * @param instance The instance to add.
    */
   protected void addChange(Instance instance)
   {
      if (m_nCommitStage > COMMIT_VALIDATION)
      {
         if (m_nCommitStage == COMMIT_CACHE ||
            !instance.isCommitPending() ||
            instance.getState() != Instance.NEW)
         {
            if (instance.getUnitOfWork() == null)
            {
               instance.rollback();
            }

            throw new WorkStateException("err.runtime.committing");
         }

         m_bSuspect = true;
      }

      checkTransaction();

      if (m_instanceSet == null)
      {
         m_instanceSet = new HashDeque(16);
      }

      if (m_nMaxChangeCount >= 0 &&
         m_instanceSet.size() >= m_nMaxChangeCount &&
         !m_instanceSet.contains(instance))
      {
         if (instance.getUnitOfWork() == null)
         {
            instance.rollback();
         }

         throw new DataVolumeException("err.runtime.changeCount",
            new Object[]{Primitive.createInteger(m_nMaxChangeCount)});
      }

      m_bDirty = true;

      if (m_instanceSet.add(instance))
      {
         instance.setUnitOfWork(this);
         accumulateChange(instance);
      }

      if (m_nCommitStage == COMMIT_VALIDATION)
      {
         if (m_validationSet == null)
         {
            m_validationSet = new HashHolder();
         }

         m_validationSet.add(instance);
      }
   }

   /**
    * Removes an instance from the change set.
    * @param instance The instance to remove.
    */
   protected void removeChange(Instance instance)
   {
      if (m_instanceSet != null)
      {
         m_instanceSet.remove(instance);
         accumulateChange(instance);

         if (m_validationSet != null)
         {
            m_validationSet.remove(instance);
         }
      }
   }

   /**
    * @return The changed instance count.
    */
   public int getInstanceCount()
   {
      if (m_instanceSet == null)
      {
         return 0;
      }

      return m_instanceSet.size();
   }

   /**
    * @return The changed instance iterator.
    */
   public Iterator getInstanceIterator()
   {
      if (m_instanceSet == null)
      {
         return EmptyIterator.getInstance();
      }

      return m_instanceSet.iterator();
   }

   /**
    * Adds a collection to the change set.
    * @param list The collection to add.
    */
   public void addChange(InstanceList list)
   {
      if (m_collectionMap == null)
      {
         m_collectionMap = new IdentityHashTab();
      }

      if (!m_collectionMap.contains(list))
      {
         m_collectionMap.put(list, null);
      }
   }

   /**
    * @return The changed collection count.
    */
   protected int getCollectionCount()
   {
      if (m_collectionMap == null)
      {
         return 0;
      }

      return m_collectionMap.size();
   }

   /**
    * @return The changed collection iterator.
    */
   protected Lookup.Iterator getCollectionIterator()
   {
      if (m_collectionMap == null)
      {
         return GenericHashTab.EMPTY_ITERATOR;
      }

      return m_collectionMap.iterator();
   }

   /**
    * Gets an old (before UOW) version of a collection.
    * @param list The collection, which old version to get.
    * @return The old collection.
    */
   protected InstanceList getOldCollection(InstanceList list)
   {
      if (list.isDirty())
      {
         InstanceList oldList = (m_collectionMap == null) ? null : (InstanceList)m_collectionMap.get(list);

         if (oldList == null)
         {
            oldList = (InstanceList)list.clone();
            oldList.complete(null, false, false);

            if (m_collectionMap == null)
            {
               m_collectionMap = new IdentityHashTab();
            }

            m_collectionMap.put(list, oldList);
         }

         return oldList;
      }

      return list;
   }

   /**
    * Gets a previous (before updates since the last life cycle event invocation)
    * version of a collection.
    * @param list The collection, which previous version to get.
    * @return The pre-collection.
    */
   protected InstanceList getPreCollection(InstanceList list)
   {
      if (list.isDirty())
      {
         InstanceList preList = (m_preCollectionMap == null) ? null : (InstanceList)m_preCollectionMap.get(list);

         if (preList == null)
         {
            preList = (InstanceList)list.clone();
            preList.complete(null, false, true);

            if (m_collectionMap == null)
            {
               m_collectionMap = new IdentityHashTab();
            }

            if (!m_collectionMap.contains(list))
            {
               m_collectionMap.put(list, null);
            }

            if (m_preCollectionMap == null)
            {
               m_preCollectionMap = new IdentityHashTab();
            }

            m_preCollectionMap.put(list, preList);
         }

         return preList;
      }

      return list;
   }

   /**
    * Removes a previous version of a collection.
    * @param list The collection, which previous version to remove.
    */
   protected void removePreCollection(InstanceList list)
   {
      if (m_preCollectionMap != null)
      {
         m_preCollectionMap.remove(list);
      }
   }

   /**
    * Removes the instance from the instance map and
    * adds it to the OID change list.
    * @param instance The instance to add.
    */
   public void changeOID(Instance instance)
   {
      if (m_nCommitStage != COMMIT_UNDO)
      {
         if (m_oidChangeList == null)
         {
            m_oidChangeList = new ArrayList(20);
         }

         m_oidChangeList.add(instance);
         m_oidChangeList.add(instance.getOID());
      }
   }

   /**
    * Accumulates an instance change in the given map.
    * @param map The destination map.
    * @param instance The instance to accumulate.
    * @param nState The state to accumulate.
    */
   public static void accumulateChange(Lookup map, Instance instance, byte nState)
   {
      Integer oldState = (Integer)map.put(instance, Primitive.createInteger(nState));

      if (oldState != null)
      {
         if (oldState.byteValue() == Instance.NEW)
         {
            switch (instance.getState())
            {
               case Instance.CLEAN:
               case Instance.DIRTY:
                  map.put(instance, oldState);
                  break;

               case Instance.DELETED:
                  if ((oldState.intValue() & CHANGE_KEPT) == 0)
                  {
                     map.remove(instance);
                  }

                  break;
            }
         }
         else if (nState == Instance.CLEAN)
         {
            map.put(instance, oldState);
         }
      }
   }

   /**
    * Accumulates an instance change if its class object is tracked.
    * @param instance The instance, which changes to accumulate.
    */
   public void accumulateChange(Instance instance)
   {
      assert instance.getState() != Instance.INIT;

      if (m_context.isTracked(instance))
      {
         if (m_stateMap == null)
         {
            m_stateMap = new HashTab();
         }

         accumulateChange(m_stateMap, instance, instance.getState());
      }
   }

   /**
    * Accumulates the instance changes in the invocation context.
    */
   public void accumulateChanges()
   {
      if (m_stateMap != null)
      {
         for (Lookup.Iterator itr = m_stateMap.iterator(); itr.hasNext();)
         {
            itr.next();
            m_context.accumulateChange((Instance)itr.getKey(), ((Number)itr.getValue()).byteValue());
         }
      }
   }

   /**
    * Keeps a deletion state on a new instance.
    * @param instance The new instance.
    */
   public void keepChange(Instance instance)
   {
      if (m_stateMap != null)
      {
         Integer state = (Integer)m_stateMap.get(instance);

         if (state != null &&
            state.byteValue() == Instance.NEW &&
            (state.intValue() & CHANGE_KEPT) == 0)
         {
            m_stateMap.put(instance, Primitive.createInteger(state.byteValue() | CHANGE_KEPT));
         }
      }
   }

   /**
    * Computes the hiddenness of the tracked instances.
    */
   public void computeHiddenness()
   {
      if (m_stateMap != null)
      {
         for (Lookup.Iterator itr = m_stateMap.iterator(); itr.hasNext();)
         {
            ((Instance)itr.next()).computeHiddenness();
         }
      }
   }

   /**
    * Adds a denormalization fixup to the UOW.
    * @param instance The source instance.
    * @param mapping The denormalized attribute mapping.
    */
   public void addDenorm(Instance instance, AttributeMapping mapping)
   {
      if (m_denormList == null)
      {
         m_denormList = new ArrayList(8);
      }

      m_denormList.add(instance);
      m_denormList.add(mapping);
   }

   /**
    * Adds a replication fragment for a given instance.
    * @param instance The instance to replicate.
    * @param sFragmentName The fragment name.
    */
   public void addReplicationFragment(Instance instance, String sFragmentName)
   {
      if (m_replicationSet == null)
      {
         m_replicationSet = new HashTab2D();
      }

      if (sFragmentName == null)
      {
         sFragmentName = "";
      }

      m_replicationSet.put(instance, sFragmentName, null);
   }

   /**
    * Adds a synchronization command to execute before committing.
    * @param key The key associated with the command.
    * @param function The function to execute.
    */
   public void addSynchronizer(Object key, Function function)
   {
      assert key != null;
      assert function != null;

      if (m_synchronizerMap == null)
      {
         m_synchronizerMap = new LinkedHashTab();
      }

      m_synchronizerMap.put(key, function);
   }

   /**
    * Adds an xa transaction completion listener.
    * @param key The key associated with the command.
    * @param function A function whose first argument is transaction status (int), to be executed on transaction completion.
    */
   public void addCompensator(Object key, Function function)
   {
      assert key != null;
      assert function != null;

      if (m_compensatorMap == null)
      {
         m_compensatorMap = new LinkedHashTab();
      }

      m_compensatorMap.put(key, function);
   }

   /**
    * Caches a wrapped object.
    * @param key The cache key. Cannot be null.
    * @param cached The wrapped object.
    * @param nMode Combination of CACHE_* flags.
    */
   protected void cache(Object key, Cached cached, byte nMode)
   {
      assert key != null;
      assert cached != null;

      if (m_cacheMap == null)
      {
         m_cacheMap = new HashTab();
      }

      if ((nMode & CACHE_UNPARTITIONED) == 0)
      {
         key = m_context.getPartitionedKey(key);
      }

      m_cacheMap.put(key, cached);

      if (m_nCommitStage == COMMIT_CACHE)
      {
         cached.prepare();
      }
   }

   /**
    * Caches a reference to an immutable object.
    * The same as cacheReference(), but the key is not committed.
    * The difference from cacheLocal() is that the global cache name space is used.
    * @param key The cache key. Cannot be null.
    * @param ref The object reference to cache.
    * @param nMode Combination of CACHE_* flags.
    */
   public void cacheTemporary(Object key, Object ref, byte nMode)
   {
      cache(key, new CachedTemporary(ref), nMode);
   }

   /**
    * Caches a reference to an immutable object.
    * @param key The cache key. Cannot be null.
    * @param ref The object reference to cache.
    */
   public void cacheReference(Object key, Object ref)
   {
      cache(key, new CachedReference(ref), CACHE_DEFAULT);
   }

   /**
    * Caches a copy of a serializable object.
    * @param key The cache key. Cannot be null.
    * @param obj The object to serialize and cache.
    * Must implement java.io.Serializable.
    */
   public void cacheCopy(Object key, Object obj)
   {
      cache(key, new CachedCopy(obj), CACHE_DEFAULT);
   }

   /**
    * Caches an instance or a collection of instances.
    * @param key The cache key. Cannot be null.
    * @param instance The instance or the collection to cache.
    * Must implement Instance or InstanceList.
    * @param attributes The attribute list: (attr1 ... (assoc2 attr21 ... attr 2N) ...).
    */
   public void cacheInstance(Object key, Object instance, Pair attributes)
   {
      cache(key, new CachedInstance(instance, attributes), CACHE_DEFAULT);
   }

   /**
    * Retrieves an object from the cache.
    * @param key The cache key.
    * @return The cached object, or null if not found.
    */
   public Object getCached(Object key)
   {
      return getCached(key, CACHE_DEFAULT);
   }

   /**
    * Retrieves an object from the cache.
    * @param key The cache key.
    * @param nMode Combination of CACHE_* flags.
    * @return The cached object, or null if not found.
    */
   public Object getCached(Object key, byte nMode)
   {
      assert key != null;

      if ((nMode & CACHE_UNPARTITIONED) == 0)
      {
         key = m_context.getPartitionedKey(key);
      }

      Object obj = null;

      if (m_cacheMap != null)
      {
         obj = m_cacheMap.get(key);
      }

      if (obj == null)
      {
         if (m_uncacheSet != null && m_uncacheSet.contains(key))
         {
            return null;
         }

         obj = m_context.getGlobalCache().get(key);
      }

      if (obj instanceof Cached)
      {
         obj = ((Cached)obj).get(m_context);
      }

      return obj;
   }

   /**
    * Removes from the cache an object corresponding to a given key.
    * @param key The cache key to invalidate. Cannot be null.
    */
   public void uncache(Object key)
   {
      uncache(key, CACHE_DEFAULT);
   }

   /**
    * Removes from the cache an object corresponding to a given key.
    * @param key The cache key to invalidate. Cannot be null.
    * @param nMode Combination of CACHE_* flags.
    */
   public void uncache(Object key, byte nMode)
   {
      assert key != null;

      if ((nMode & CACHE_UNPARTITIONED) == 0)
      {
         key = m_context.getPartitionedKey(key);
      }

      if (m_cacheMap != null)
      {
         m_cacheMap.remove(key);
      }

      if (m_uncacheSet == null)
      {
         m_uncacheSet = new HashHolder();
      }

      m_uncacheSet.add(key);
   }

   /**
    * Adds data to the transient cache.
    * @see InvocationContext
    * @param key The cache key.
    * @param value The cache value.
    */
   public void cacheTransient(Object key, Object value)
   {
      assert key != null;
      assert value != null;
      assert value != Undefined.VALUE;

      if (m_transientMap == null)
      {
         m_transientMap = new HashTab();
      }

      m_transientMap.put(key, value);
   }

   /**
    * Gets data from the transient cache.
    * @see InvocationContext
    * @param key The cache key.
    * @return The cached value, or null if none.
    */
   public Object getCachedTransient(Object key)
   {
      assert key != null;

      if (m_transientMap != null)
      {
         Object value = m_transientMap.get(key);

         if (value == Undefined.VALUE)
         {
            return null;
         }

         if (value != null)
         {
            return value;
         }
      }

      return m_context.getCachedTransient(key);
   }

   /**
    * Removes an entry from the transient cache.
    * @see InvocationContext
    * @param key The cache key.
    */
   public void uncacheTransient(Object key)
   {
      assert key != null;

      if (m_transientMap == null)
      {
         m_transientMap = new HashTab();
      }

      m_transientMap.put(key, Undefined.VALUE);
   }

   /**
    * Adds data to the local UOW cache.
    * @param key The cache key.
    * @param value The cache value.
    */
   public void cacheLocal(Object key, Object value)
   {
      assert key != null;
      assert value != null;

      if (m_localMap == null)
      {
         m_localMap = new HashTab();
      }

      m_localMap.put(key, value);
   }

   /**
    * Gets data from the local cache.
    * @param key The cache key.
    * @return The cached value, or null if none.
    */
   public Object getCachedLocal(Object key)
   {
      assert key != null;

      if (m_localMap != null)
      {
         return m_localMap.get(key);
      }

      return null;
   }

   /**
    * Removes an entry from the local cache.
    * @param key The cache key.
    */
   public void uncacheLocal(Object key)
   {
      assert key != null;

      if (m_localMap != null)
      {
         m_localMap.remove(key);
      }
   }

   /**
    * Sets a UOW property value.
    * The UOW properties are preserved across asynchronous invocations.
    * @param sName The property name.
    * @param value The property value.
    */
   public void setValue(String sName, Object value)
   {
      assert sName != null;

      if (value != null)
      {
         if (m_propertyMap == null)
         {
            m_propertyMap = new GenericSerializablePropertyMap();
         }

         m_propertyMap.setValue(sName, value);
      }
      else if (m_propertyMap != null)
      {
         m_propertyMap.removeValue(sName);
      }
   }

   /**
    * Gets a UOW property value.
    * @param sName The property name.
    * @return The UOW property value, or null if not found.
    */
   public Object getValue(String sName)
   {
      assert sName != null;

      if (m_propertyMap != null)
      {
         return m_propertyMap.getValue(sName);
      }

      return null;
   }

   /**
    * @return The current fragment name.
    */
   public String getFragmentName()
   {
      if (m_sFragmentName != null)
      {
         return (m_sFragmentName.length() == 0) ? null : m_sFragmentName;
      }

      return m_context.getFragmentName();
   }

   /**
    * Gets the current fragment name.
    * @param bFragmented The fragmentation flag.
    * @return The current fragment name, or null if bFragmented is false.
    */
   public String getFragmentName(boolean bFragmented)
   {
      return (bFragmented) ? getFragmentName() : null;
   }

   /**
    * Caches a resource in the UOW.
    * @param resource The resource to cache.
    */
   public void addResource(Resource resource)
   {
      if (m_resourceList == null)
      {
         m_resourceList = new ArrayList(4);
      }

      resource.setTransaction(m_transaction);
      m_resourceList.add(resource);
   }

   /**
    * Gets a resource from the UOW. Always returns resources with non-zero
    * ref count. The resource's transaction must be the same as the JTA
    * transaction of this UOW.
    * @param rm The resource manager.
    * @param sFragmentName The fragment name. Can be null.
    * @param type The resource type. Can be null.
    * @param bShared True to return a shareable resource.
    * @return The resource, or null if not found.
    */
   public Resource findResource(ResourceManager rm, String sFragmentName, Class type, boolean bShared)
   {
      if (m_resourceList != null)
      {
         for (int i = m_resourceList.size() - 1; i >= 0; --i)
         {
            Resource res = (Resource)m_resourceList.get(i);

            if (res.getTransaction() == m_transaction &&
               (type == null || res.getClass() == type) &&
               res.getResourceManager() == rm &&
               !res.isReleased() &&
               ObjUtil.equal(res.getFragmentName(), sFragmentName) &&
               (bShared && res.isShareable() || res.getRef() == 0))
            {
               if (!bShared)
               {
                  res.setShareable(false);
               }

               return res;
            }
         }
      }

      return null;
   }

   /**
    * Releases the cached resources.
    * @param bForce True to close resources that are in use.
    */
   protected void releaseResources(boolean bForce)
   {
      if (m_resourceList != null)
      {
         for (int i = m_resourceList.size() - 1; i >= 0; --i)
         {
            Resource res = (Resource)m_resourceList.get(i);
            int nRefCount = res.getRef();

            if (bForce && nRefCount != 0)
            {
               if (s_logger.isDebugEnabled())
               {
                  s_logger.debug("Resource " + res + " still in use");
               }
            }

            if (bForce || nRefCount == 0)
            {
               res.release();
               m_resourceList.remove(i);
            }
         }
      }
   }

   /**
    * @return The count of messages added to this UOW.
    */
   public int getMessageCount()
   {
      int nCount = 0;

      if (m_messageMap != null)
      {
         for (Iterator itr = m_messageMap.valueIterator(); itr.hasNext();)
         {
            nCount += ((Collection)itr.next()).size();
         }
      }

      return nCount;
   }

   /**
    * Adds an asynchronous message to the unit of work.
    * @param accessor The instance or metaclass to send.
    * @param event The event to invoke on the instance.
    * @param args The event arguments, or null if none.
    * @param correlator The correlator instance or metaclass, or null if none.
    * @param corEvent The correlator event, or null if none.
    * @param nPriority The message priority, 0..9, or -1 to use the default value.
    * @param lTTL The message time to live in ms, or -1 to use the default value.
    * @param bBroadcast True to send the message as a topic.
    * @param bNoLocal True to exclude the local node from the broadcast.
    */
   public void addMessage(Accessor accessor, Symbol event, Object[] args,
      Accessor correlator, Symbol corEvent, int nPriority, long lTTL,
      boolean bBroadcast, boolean bNoLocal)
   {
      addMessage(null, accessor, event, args, correlator, corEvent, nPriority, lTTL, bNoLocal, (bBroadcast) ?
            ((bNoLocal) ? BROADCAST_OTHER_PROPERTIES : BROADCAST_ALL_PROPERTIES) : null);
   }

   /**
    * Selects a factory and sets channel-specific properties for an asynchronous message to a secure channel.
    * @param channel The channel.
    * @param bNoLocal True to exclude the local node from the broadcast.
    * @param properties The property set.
    * @return The message factory.
    */
   protected MessageFactory getSecureMessageFactory(Channel channel, boolean bNoLocal,
         TransferObject properties)
   {
      if (properties == null)
      {
         properties = new TransferObject(1);
      }

      if (channel instanceof MessageQueue)
      {
         MessageQueue mq = (MessageQueue)channel;

         if (!bNoLocal && mq.isLoopback() && mq.isBroadcast())
         {
            properties.setValue(JMSSender.NODE, "");
         }

         return JMS_MESSAGE_FACTORY;
      }
      else if (channel instanceof ObjectQueue)
      {
         return OBJECT_MESSAGE_FACTORY;
      }
      else
      {
         throw new RPCException("err.rpc.msg.channel", new Object[]{channel.getName()});
      }
   }

   /**
    * Adds an asynchronous message to the unit of work.
    * @param sChannel The destination channel name.
    * @param accessor The instance or metaclass to send.
    * @param event The event to invoke on the instance.
    * @param args The event arguments, or null if none.
    * @param correlator The correlator instance or metaclass, or null if none.
    * @param corEvent The correlator event, or null if none.
    * @param nPriority The message priority, 0..9, or -1 to use the default value.
    * @param lTTL The message time to live in ms, or -1 to use the default value.
    * @param bNoLocal True to exclude the local node from the broadcast.
    */
   public void addMessage(String sChannel, Accessor accessor, Symbol event, Object[] args,
      Accessor correlator, Symbol corEvent, int nPriority, long lTTL, boolean bNoLocal)
   {
      addMessage(sChannel, accessor, event, args, correlator, corEvent, nPriority, lTTL, bNoLocal, null);
   }
   
   /**
    * Adds an asynchronous message to the unit of work.
    * @param sChannel The destination channel name.
    * @param accessor The instance or metaclass to send.
    * @param event The event to invoke on the instance.
    * @param args The event arguments, or null if none.
    * @param correlator The correlator instance or metaclass, or null if none.
    * @param corEvent The correlator event, or null if none.
    * @param nPriority The message priority, 0..9, or -1 to use the default value.
    * @param lTTL The message time to live in ms, or -1 to use the default value.
    * @param bNoLocal True to exclude the local node from the broadcast.
    * @param properties The message properties, can be null.
    */
   public void addMessage(String sChannel, Accessor accessor, Symbol event, Object[] args,
         Accessor correlator, Symbol corEvent, int nPriority, long lTTL, boolean bNoLocal,
         TransferObject properties)
   {
      accessor.getMetaclass().getSelector(event).getMember((args == null) ? 0 : args.length);

      if (correlator != null)
      {
         correlator.getMetaclass().getSelector(corEvent).getMember(0);
      }

      TransferObject clonedProperties = (properties == null) ? new TransferObject(3) : (TransferObject)properties.clone();

      if (m_context.getPrincipal() != InvocationContext.ANONYMOUS_PRINCIPAL)
      {
         clonedProperties.setValue(ObjectSender.USER, m_context.getPrincipal().getName());
      }

      clonedProperties.setValue(ObjectSender.PROTECTED, Boolean.FALSE);

      Channel channel = getChannel(sChannel, properties);
      MessageFactory mf = getSecureMessageFactory(channel, bNoLocal, clonedProperties);

      addMessage(channel, new AccessorEnvelope(mf, accessor, event, args,
         correlator, corEvent, clonedProperties, nPriority, lTTL));
   }

   /**
    * Adds an asynchronous message to the unit of work.
    * @param sChannel The destination channel name. Use null for system queue channel.
    * @param obj The object to send.
    * @param properties The message properties.
    * @param nPriority The message priority, 0..9, or -1 to use the default value.
    * @param lTTL The message time to live in ms, or -1 to use the default value.
    * @param bNoLocal True to exclude the local node from the broadcast.
    */
   public Channel addMessage(String sChannel, Object obj, TransferObject properties, int nPriority, long lTTL, boolean bNoLocal)
   {
      Channel channel = getChannel(sChannel, properties);

      addMessage(channel, obj, properties, nPriority, lTTL, bNoLocal);

      return channel;
   }

   /**
    * Adds an asynchronous message to the unit of work.
    * @param channel The destination channel.
    * @param obj The object to send.
    * @param properties The message properties.
    * @param nPriority The message priority, 0..9, or -1 to use the default value.
    * @param lTTL The message time to live in ms, or -1 to use the default value.
    * @param bNoLocal True to exclude the local node from the broadcast.
    */
   public void addMessage(Channel channel, Object obj, TransferObject properties, int nPriority, long lTTL, boolean bNoLocal)
   {
      TransferObject clonedProperties = (properties == null) ? new TransferObject(3) : (TransferObject)properties.clone();

      MessageFactory mf = getSecureMessageFactory(channel, bNoLocal, clonedProperties);

      addMessage(channel, new ObjectEnvelope(mf, obj, clonedProperties, nPriority, lTTL));
   }

   /**
    * Adds an asynchronous message to the unit of work.
    * @param channel The destination channel.
    * @param msg The message.
    */
   public void addMessage(Channel channel, TransferObject msg)
   {
      addMessage(channel, (Object)msg);
   }

   /**
    * Adds an asynchronous message to the unit of work.
    * @param sChannel The destination channel name.
    * @param msg The message.
    */
   public void addMessage(String sChannel, TransferObject msg)
   {
      addMessage(m_context.getMetadata().getChannel(sChannel), (Object)msg);
   }

   /**
    * Adds an asynchronous message to the unit of work.
    * @param channel The destination channel.
    * @param msg The message to add.
    */
   protected void addMessage(Channel channel, Object msg) throws RPCException, WorkStateException
   {
      if (channel.getSender() == null)
      {
         throw new RPCException("err.rpc.notSender", new Object[]{channel.getName()});
      }

      if (m_nCommitStage == COMMIT_CACHE)
      {
         throw new WorkStateException("err.runtime.committing");
      }

      checkTransaction();

      List list;

      if (m_messageMap == null)
      {
         m_messageMap = new HashTab();
         list = new ArrayList();
         m_messageMap.put(channel, list);
      }
      else
      {
         list = (List)m_messageMap.get(channel);

         if (list == null)
         {
            list = new ArrayList();
            m_messageMap.put(channel, list);
         }
      }

      list.add(msg);
      m_bDirty = true;
   }

   /**
    * Get channel by name.
    * @param sChannel Channel name. Use null for system queue channel.
    * @param properties Properties.
    */
   protected Channel getChannel(String sChannel, TransferObject properties)
   {
      Metadata metadata = m_context.getMetadata();
      Channel channel;

      if (sChannel == null)
      {
         channel = metadata.findChannel(SYSTEM_QUEUE);
      }
      else
      {
         channel = metadata.getChannel(sChannel);
      }

      if (channel == null)
      {
         channel = metadata.getChannel((properties == null || !properties.hasValue("receiver")) ?
               JMS_SYSTEM_QUEUE : JMS_SYSTEM_TOPIC);
      }

      return channel;
   }

   /**
    * Adjust the next timeout.
    * @param lTimeout The next timeout.
    * @see java.util.Date#getTime()
    */
   public void setNextTimeout(long lTimeout)
   {
      if (m_lNextTimeout == 0 || lTimeout < m_lNextTimeout)
      {
         m_lNextTimeout = lTimeout;
      }
   }

   /**
    * Adds a work item to the set.
    * @param work The work item to add.
    */
   public void addWork(Work work)
   {
      if (!m_workSet.add(work))
      {
         throw new IllegalStateException("Duplicate work item");
      }
   }

   /**
    * Finds a work item in the the set.
    * @param key The lookup key, must be equal (.equals() == true) to the work item.
    * @return The found work item.
    */
   public Work findWork(Work key)
   {
      return (Work)m_workSet.get(key);
   }

   /**
    * Invokes the pending events on the instances.
    * @param bIterate True to iterate until no more events are generated.
    * @param bCommit True to invoke the commit events.
    */
   public void invokePendingEvents(boolean bIterate, boolean bCommit)
   {
      if (m_instanceSet != null)
      {
         Instance[] changeArray = new Instance[m_instanceSet.size()];

         for (int nEventCount = 0;; ++nEventCount)
         {
            int nCount = 0;

            for (Iterator itr = m_instanceSet.iterator(); itr.hasNext();)
            {
               Instance instance = (Instance)itr.next();

               if (instance.isEventPending())
               {
                  changeArray[nCount++] = instance;
               }
            }

            if (nCount == 0)
            {
               if (bCommit)
               {
                  for (Iterator itr = m_instanceSet.iterator(); itr.hasNext();)
                  {
                     Instance instance = (Instance)itr.next();

                     if (instance.isCommitPending())
                     {
                        changeArray[nCount++] = instance;
                     }
                  }
               }

               if (nCount == 0)
               {
                  break;
               }

               if (m_nMaxEventCount >= 0 && nEventCount >= m_nMaxEventCount)
               {
                  throw new IterationCountException("err.runtime.eventCount",
                     new Object[]{Primitive.createInteger(m_nMaxEventCount)});
               }

               for (int i = 0; i != nCount; ++i)
               {
                  Instance instance = changeArray[i];

                  instance.invoke("commit", (Pair)null);
                  instance.uncache();
               }
            }
            else
            {
               if (m_nMaxEventCount >= 0 && nEventCount >= m_nMaxEventCount)
               {
                  throw new IterationCountException("err.runtime.eventCount",
                     new Object[]{Primitive.createInteger(m_nMaxEventCount)});
               }

               for (int i = 0; i != nCount; ++i)
               {
                  changeArray[i].invokePendingEvent();
               }

               if (!bIterate)
               {
                  break;
               }
            }

            nCount = m_instanceSet.size();

            if (nCount > changeArray.length)
            {
               changeArray = new Instance[nCount];
            }
         }
      }
   }

   /**
    * Validates the instances in the validation set.
    */
   protected void validate() throws ValidationException
   {
      for (;;)
      {
         m_nCommitStage = COMMIT_VALIDATION;

         Set validationSet = m_validationSet;

         if (validationSet == null)
         {
            break;
         }

         m_validationSet = null;

         ValidationException e = null;
         boolean bFull = !m_bRaw;

         m_validationSet = null;

         for (Iterator itr = validationSet.iterator(); itr.hasNext();)
         {
            try
            {
               ((Instance)itr.next()).validate(bFull);
            }
            catch (ValidationException x)
            {
               if (e == null)
               {
                  e = x;
               }
               else
               {
                  e.addException(x);
               }
            }
         }

         if (e != null)
         {
            throw e;
         }

         m_nCommitStage = COMMIT_EVENTS;
         invokePendingEvents(true, true);
      }
   }

   /**
    * Executes presorted work items, batching them for each engine.
    * @param workArray The work item array.
    * @param nStart The start index.
    * @param nEnd The end index (exclusive).
    */
   protected void execute(Work[] workArray, int nStart, int nEnd)
   {
      while (nStart < nEnd)
      {
         Work work = workArray[nStart];
         int nStartSaved = nStart;

         for (++nStart; nStart < nEnd; ++nStart)
         {
            if (!workArray[nStart].isBatchableWith(work))
            {
               break;
            }
         }

         int nRPC = m_context.getRPCCount() - m_context.getPersistCount();

         try
         {
            work.getAdapter().execute(workArray, nStartSaved, nStart);
         }
         catch (UncheckedException e)
         {
            List instanceList = new ArrayList(nStart - nStartSaved);

            for (int i = nStartSaved; i < nStart; ++i)
            {
               instanceList.add(workArray[i].getInstance());
            }

            e.setValue("committedInstances", instanceList);

            throw e;
         }
         finally
         {
            m_context.addPersistCount(m_context.getRPCCount() - m_context.getPersistCount() - nRPC);
         }
      }
   }

   /**
    * @return True if the UOW is committing the data.
    */
   public boolean isCommitting()
   {
      return m_nCommitStage != COMMIT_NONE;
   }

   /**
    * Ensure that the transaction is still committing.
    */
   protected void checkState() throws WorkStateException
   {
      if (!isCommitting())
      {
         throw new WorkStateException("err.runtime.rolledback");
      }
   }

   /**
    * Commits the changes to the persistent storage.
    * @param bEndTx True to commit also the distributed transaction.
    */
   public void commit(boolean bEndTx)
   {
      if (isCommitting())
      {
         throw new WorkStateException("err.runtime.recursiveCommit");
      }

      // Committing a transient UoW should not do any work
      if (m_bTransient && (m_bDirty || m_transaction != null))
      {
         throw new WorkStateException("err.runtime.transientCommit");
      }

      try
      {
         m_nCommitStage = COMMIT_EVENTS;
         invokePendingEvents(true, true);
         checkState();
         m_validationSet = m_instanceSet;
         validate();
         DefaultSyncEngine.processChanges(this);
         checkState();
         computeHiddenness();

         if (m_context.getTester() != null)
         {
            m_context.getMachine().invoke(m_context.getTester(),
               new Object[]{this, Boolean.TRUE, Boolean.valueOf(bEndTx)});
         }

         checkState();
         m_nCommitStage = COMMIT_WORK;

         if (m_instanceSet != null)
         {
            m_workSet = new HashHolder(m_instanceSet.size());

            if (s_logger.isDebugEnabled())
            {
               s_logger.debug("Committing " + m_instanceSet.size() + " instance(s)");

               if (s_logger.isDumpEnabled())
               {
                  for (Iterator itr = m_instanceSet.iterator(); itr.hasNext();)
                  {
                     s_logger.dump(itr.next());
                  }
               }
            }

            for (Iterator itr = m_instanceSet.iterator(); itr.hasNext();)
            {
               Instance instance = (Instance)itr.next();

               if (instance.getPersistenceMapping() != null)
               {
                  PersistenceAdapter adapter = instance.getAdapter();

                  adapter.addWork(this, instance);

                  Component hook = instance.getPersistenceMapping().getHook();

                  if (hook != null)
                  {
                     ((PersistenceHook)hook.getInstance(m_context)).addWork(this, instance);
                  }
               }
            }

            if (m_denormList != null)
            {
               for (int i = 0, n = m_denormList.size(); i < n; i += 2)
               {
                  Instance instance = (Instance)m_denormList.get(i);

                  instance.getAdapter().addDenorm(this, instance, (AttributeMapping)m_denormList.get(i + 1));
               }

               m_denormList = null;
            }

            // Remove the empty items
            for (Iterator itr = m_workSet.iterator(); itr.hasNext();)
            {
               Work work = (Work)itr.next();

               if (work.isEmpty())
               {
                  itr.remove();
                  work.ignore();
               }
            }

            // Sort the work items topologically
            Work[] workArray = new Work[m_workSet.size()];
            int nStart = 0;
            int nEnd = 0;

            // Get the root items
            for (Iterator itr = m_workSet.iterator(); itr.hasNext();)
            {
               Work work = (Work)itr.next();

               if (work.getPredCount() == 0)
               {
                  workArray[nEnd++] = work;
               }
            }

            while (nStart < nEnd)
            {
               // Sort the items that are at the same distance from the root,
               // so that similar work items are batched together.
               Arrays.sort(workArray, nStart, nEnd);

               int nNewEnd;

               for (nNewEnd = nEnd; nStart < nEnd; ++nStart)
               {
                  Work work = workArray[nStart];

                  for (int i = 0, nCount = work.getSuccessorCount(); i < nCount; ++i)
                  {
                     Work succ = work.getSuccessor(i);

                     if (succ.decPredCount() == 0)
                     {
                        workArray[nNewEnd++] = succ;
                     }
                  }
               }

               nEnd = nNewEnd;
            }

            if (nEnd != workArray.length)
            {
               // TODO: Break the cycles
               throw new UnsupportedOperationException("Circular dependencies in units of work are not supported yet");
            }

            // Execute the work items while batching them for each engine
            execute(workArray, 0, nEnd);

            // Execute the work items for replicated instances against their fragments
            if (m_replicationSet != null)
            {
               Set fragmentSet = new HashSet();

               for (Lookup2D.Iterator itr = m_replicationSet.valueIterator(); itr.hasNext();)
               {
                  itr.next();
                  fragmentSet.add(itr.getKey2());
               }

               fragmentSet.remove((m_context.getFragmentName() == null) ? "" : m_context.getFragmentName());

               if (!fragmentSet.isEmpty())
               {
                  Work[] replicaArray = new Work[nEnd];

                  for (Iterator itr = fragmentSet.iterator(); itr.hasNext();)
                  {
                     String sFragmentName = (String)itr.next();

                     nStart = 0;

                     for (int i = 0; i < nEnd; ++i)
                     {
                        Work work = workArray[i];

                        if (m_replicationSet.contains(work.getInstance(), sFragmentName))
                        {
                           replicaArray[nStart++] = work;
                           work.setFragmentName(sFragmentName);
                        }
                     }

                     if (nStart > 0)
                     {
                        if (s_logger.isDebugEnabled())
                        {
                           s_logger.debug("Replicating " + nStart + " work item(s) to fragment \"" + sFragmentName + "\"");
                        }

                        m_sFragmentName = sFragmentName;
                        execute(replicaArray, 0, nStart);
                     }
                  }

                  m_sFragmentName = null;
               }
            }
         }

         if (!m_bRaw)
         {
            m_nCommitStage = COMMIT_SYNC;

            if (m_synchronizerMap != null)
            {
               if (s_logger.isDebugEnabled())
               {
                  s_logger.debug("Invoking " + m_synchronizerMap.size() + " synchronizer(s)");
               }

               for (Iterator itr = m_synchronizerMap.valueIterator(); itr.hasNext();)
               {
                  m_context.getMachine().invoke((Function)itr.next(), (Pair)null);
               }
            }

            // Synchronize changes to all interested subscribers
            DefaultSyncEngine.submitSyncCommands(m_context);
            m_nCommitStage = COMMIT_WORK;
         }

         // Invalidate the cluster cache, except in this node.
         if (m_uncacheSet != null && m_context.getGlobalCache().isDistributed())
         {
            m_nCommitStage = COMMIT_EVENTS;

            if (s_uncacheConstructor == null)
            {
               throw new UnsupportedOperationException("Application server clustering not supported");
            }

            try
            {
               addMessage((String)null, s_uncacheConstructor.newInstance(new Object[] {m_uncacheSet,
                     Primitive.createLong(m_lTimestamp)}), BROADCAST_OTHER_PROPERTIES, 2, 0, true);
            }
            catch (Throwable t)
            {
               ObjUtil.rethrow(t);
            }

            m_nCommitStage = COMMIT_WORK;
         }

         checkInstances();

         m_nCommitStage = COMMIT_CACHE;

         // Send the accumulated messages
         if (m_messageMap != null)
         {
            for (Lookup.Iterator itr = m_messageMap.iterator(); itr.hasNext();)
            {
               Channel channel = (Channel)itr.next();
               Sender sender = (Sender)channel.getSender().getInstance(m_context);
               List list = (List)itr.getValue();

               for (int i = 0, n = list.size(); i != n; ++i)
               {
                  Object msg = list.get(i);

                  if (msg instanceof Envelope)
                  {
                     list.set(i, ((Envelope)msg).getMessage());
                  }
               }

               if (s_logger.isDebugEnabled())
               {
                  s_logger.debug("Sending " + list.size() + " message(s) on channel \"" + channel.getName() + "\"");
               }

               sender.send(list);
            }
         }

         if (bEndTx || m_transaction == null)
         {
            prepareCache();
         }

         // Commit the JTA transaction
         if (bEndTx)
         {
            releaseResources(false);

            if (m_transaction != null && m_nTxMode != TX_EXTERNAL)
            {
               m_context.commitTransaction(m_transaction);
               m_transaction = null;
            }
         }

         commitInstances();

         if (m_transaction == null)
         {
            commitCache();
         }

         // Commit the items to the transient cache
         if (m_transientMap != null)
         {
            m_context.cacheTransient(m_transientMap.iterator());
            m_transientMap = null;
         }

         m_localMap = null;
         m_propertyMap = null;

         // Notify the timer about the new timeout
         if (m_lNextTimeout != 0)
         {
            ((PersistentTimer)m_context.getComponentInstance("System.Timer")).notify(m_lNextTimeout);
            m_lNextTimeout = 0;
         }

         m_bDirty = (m_transaction != null);
         m_compensatorMap = (m_bDirty) ? m_compensatorMap : null;

         accumulateChanges();
         unlock();

         if (s_logger.isDebugEnabled())
         {
            s_logger.debug((bEndTx) ? "Commit completed" : "Pre-commit completed");
         }

         m_lTimestamp = System.currentTimeMillis();
      }
      finally
      {
         m_validationSet = null;
         m_workSet = null;
         m_denormList = null;
         m_sFragmentName = null;
         m_bSuspect = false;

         if (bEndTx)
         {
            releaseResources(false);
         }

         m_nCommitStage = COMMIT_NONE;
      }

      m_context.commitLog();
   }

   /**
    * Commits the changes to the persistent storage.
    */
   public void commit()
   {
      commit(true);
   }

   /**
    * Checks for illegal instance modifications.
    */
   protected void checkInstances() throws WorkStateException
   {
      if (m_bSuspect)
      {
         boolean bChanged = false;

         // Reset the instance change state and filter the changes
         for (Iterator itr = m_instanceSet.iterator(); itr.hasNext();)
         {
            Instance instance = (Instance)itr.next();

            if (instance.isEventPending())
            {
               bChanged = true;
               s_logger.error("Non-transient work item added during commit: " + instance);
            }
         }

         if (bChanged)
         {
            throw new WorkStateException("err.runtime.committing");
         }
      }
   }

   /**
    * Commits the modified instances and collections.
    */
   protected void commitInstances()
   {
      if (m_instanceSet != null)
      {
         // Reset the instance change state and filter the changes
         for (Iterator itr = m_instanceSet.iterator(); itr.hasNext();)
         {
            ((Instance)itr.next()).commit();
            // TODO: Filter the changed instances
         }

         m_instanceSet = null;
         m_oidChangeList = null;
         m_replicationSet = null;
      }

      if (m_collectionMap != null)
      {
         for (Iterator itr = m_collectionMap.iterator(); itr.hasNext();)
         {
            ((InstanceList)itr.next()).complete(this, true, false);
         }

         m_collectionMap = null;
      }

      m_preCollectionMap = null;
      m_synchronizerMap = null;
      m_messageMap = null;
   }

   /**
    * Prepares the cached items for commit.
    */
   protected void prepareCache()
   {
      if (m_cacheMap != null && m_cacheMap.size() != 0)
      {
         Cached[] cachedArray = new Cached[m_cacheMap.size()];
         int i = 0;

         for (Iterator itr = m_cacheMap.valueIterator(); itr.hasNext(); )
         {
            cachedArray[i++] = (Cached)itr.next();
         }

         for (i = 0; i < cachedArray.length; ++i)
         {
            cachedArray[i].prepare();
         }
      }
   }

   /**
    * Checks for unprepared cache items.
    */
   protected void checkCache()
   {
      if (m_cacheMap != null && m_cacheMap.size() != 0)
      {
         int nUnpreparedCount = 0;

         for (Iterator itr = m_cacheMap.valueIterator(); itr.hasNext(); )
         {
            if (!((Cached)itr.next()).isPrepared())
            {
               itr.remove();
               ++nUnpreparedCount;
            }
         }

         if (nUnpreparedCount != 0)
         {
            s_logger.error("Unable to commit to global cache: skipping " + nUnpreparedCount + " unprepared item(s)");

            if (m_cacheMap.size() == 0)
            {
               m_cacheMap = null;
            }
         }
      }
   }

   /**
    * Commits the items to the global cache.
    */
   protected void commitCache()
   {
      if (m_cacheMap != null && m_cacheMap.size() != 0)
      {
         for (Iterator itr = m_cacheMap.valueIterator(); itr.hasNext();)
         {
            if (itr.next() instanceof CachedTemporary)
            {
               itr.remove();
            }
         }
      }

      if (m_uncacheSet != null && m_uncacheSet.size() != 0 ||
         m_cacheMap != null && m_cacheMap.size() != 0)
      {
         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Committing to global cache: invalidating " +
               ((m_uncacheSet != null) ? m_uncacheSet.size() : 0) + " item(s), caching " +
               ((m_cacheMap != null) ? m_cacheMap.size() : 0) + " item(s), timestamp=" +
               m_lTimestamp);
         }

         m_context.getGlobalCache().update(m_uncacheSet, m_cacheMap, m_lTimestamp);

         m_uncacheSet = null;
         m_cacheMap = null;
      }
   }

   /**
    * Rolls back the changes.
    * @param bEndTx True to rollback also the distributed transaction.
    */
   public void rollback(boolean bEndTx)
   {
      if (m_nCommitStage == COMMIT_UNDO)
      {
         throw new WorkStateException("err.runtime.recursiveRollback");
      }

      try
      {
         m_nCommitStage = COMMIT_UNDO;

         if (m_context.getTester() != null)
         {
            m_context.getMachine().invoke(m_context.getTester(),
               new Object[]{this, Boolean.FALSE, Boolean.valueOf(bEndTx)});
         }

         if (bEndTx)
         {
            releaseResources(false);

            if (m_transaction != null)
            {
               m_context.rollbackTransaction(m_transaction, m_nTxMode == TX_EXTERNAL);
               m_transaction = null;
            }
         }

         if (m_instanceSet != null)
         {
            // Roll back the changed OIDs
            if (m_oidChangeList != null)
            {
               for (int i = m_oidChangeList.size() - 2; i >= 0; i -= 2)
               {
                  Instance instance = (Instance)m_oidChangeList.get(i);

                  instance.setOID((OID)m_oidChangeList.get(i + 1));
               }
            }

            for (Iterator itr = m_instanceSet.iterator(); itr.hasNext();)
            {
               ((Instance)itr.next()).rollback();
            }

            m_instanceSet = null;
            m_replicationSet = null;
         }

         if (m_collectionMap != null)
         {
            for (Iterator itr = m_collectionMap.iterator(); itr.hasNext();)
            {
               ((InstanceList)itr.next()).complete(this, false, false);
            }

            m_collectionMap = null;
         }

         m_preCollectionMap = null;

         unlock();

         m_denormList = null;
         m_synchronizerMap = null;
         m_messageMap = null;
         m_oidChangeList = null;
         m_cacheMap = null;
         m_uncacheSet = null;
         m_transientMap = null;
         m_localMap = null;
         m_propertyMap = null;
         m_lNextTimeout = 0;
         m_nCommitStage = COMMIT_NONE;
         m_bDirty = (m_transaction != null);
         m_compensatorMap = (m_bDirty) ? m_compensatorMap : null;
         m_bSuspect = false;

         if (s_logger.isDebugEnabled())
         {
            s_logger.debug((bEndTx) ? "Rollback completed" : "Undo completed");
         }

         m_lTimestamp = System.currentTimeMillis();
      }
      finally
      {
         m_nCommitStage = COMMIT_NONE;
      }

      m_context.commitLog();
   }

   /**
    * Rolls back the changes.
    */
   public void rollback()
   {
      rollback(true);
   }

   /**
    * @see javax.transaction.Synchronization#beforeCompletion()
    */
   public void beforeCompletion()
   {
   }

   /**
    * @see javax.transaction.Synchronization#afterCompletion(int)
    */
   public void afterCompletion(int nTxStatus)
   {
      if (nTxStatus == Status.STATUS_COMMITTED)
      {
         m_transaction = null;
         commitInstances();
         checkCache();
         commitCache();
      }

      if (m_compensatorMap != null)
      {
         Context contextSaved = ThreadContextHolder.getContext();

         try
         {
            ThreadContextHolder.setContext(m_context);

            for (Lookup.Iterator iter = m_compensatorMap.valueIterator(); iter.hasNext();)
            {
               Function function = (Function)iter.next();

               try
               {
                  m_context.getMachine().invoke(function, Primitive.createInteger(nTxStatus), (Object[])null);
               }
               catch (Throwable t)
               {
                  s_logger.error("Error in compensator " + function + " with key \"" + iter.getKey() + '"', t);
               }
            }
         }
         finally
         {
            ThreadContextHolder.setContext(contextSaved);
            m_compensatorMap = null;
         }
      }
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuffer buf = new StringBuffer(64);

      buf.append(super.toString());
      buf.append("(transient=");
      buf.append(m_bTransient);
      buf.append(", dirty=");
      buf.append(m_bDirty);
      buf.append(", time=");
      buf.append(m_lTimestamp);
      buf.append(", changes=");
      buf.append(getInstanceCount());
      buf.append(", messages=");
      buf.append(getMessageCount());
      buf.append(", txMode=");

      switch (m_nTxMode)
      {
         case TX_NONE:
            buf.append("none");
            break;

         case TX_MANAGED:
            buf.append("managed");
            break;

         case TX_EXTERNAL:
            buf.append("external");
            break;
      }

      buf.append(", tx=");
      buf.append(m_transaction);
      buf.append(')');

      return buf.toString();
   }

   // inner classes

   /**
    * Asynchronous message factory interface.
    */
   protected interface MessageFactory
   {
      /**
       * Creates a message.
       * @param properties The message properties.
       * @param nPriority The message priority.
       * @param lTTL The delivery delay in milliseconds.
       */
      public TransferObject createMessage(TransferObject properties, int nPriority, long lTTL);

      /**
       * Serializes a unit of work state into the message.
       * @param uow The unit of work.
       * @param tobj The message to modify.
       */
      public void serializeState(UnitOfWork uow, TransferObject tobj);
   }

   /**
    * Base class for asynchronous message envelopes.
    */
   protected class Envelope
   {
      /**
       * The transfer object containing the message.
       */
      protected TransferObject m_tobj;

      /**
       * The factory for building the message.
       */
      protected MessageFactory m_mf;

      /**
       * @param mf The message factory.
       * @param properties The message properties.
       * @param nPriority The message priority.
       * @param lTTL The delivery delay in milliseconds.
       */
      public Envelope(MessageFactory mf, TransferObject properties, int nPriority, long lTTL)
      {
         m_mf = mf;
         m_tobj = m_mf.createMessage(properties, nPriority, lTTL);
      }

      /**
       * @return The message.
       */
      public TransferObject getMessage()
      {
         m_mf.serializeState(UnitOfWork.this, m_tobj);

         return m_tobj;
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         return "Envelope: " + m_tobj;
      }
   }

   /**
    * Factory for Asynchronous JMS messages.
    */
   protected static class JMSMessageFactory implements MessageFactory
   {
      // operations

      /**
       * Builds the message.
       * @param properties The message sender properties.
       * @param nPriority The message priority.
       * @param lTTL The message time to live in milliseconds.
       * @return The message.
       */
      public TransferObject createMessage(TransferObject properties, int nPriority, long lTTL)
      {
         TransferObject tobj = new TransferObject(4);

         if (properties != null)
         {
            tobj.setValue(JMSSender.PROPERTIES, properties);
         }

         if (nPriority >= 0)
         {
            tobj.setValue(JMSSender.PRIORITY, Primitive.createInteger(nPriority));
         }

         if (lTTL >= 0)
         {
            tobj.setValue(JMSSender.TTL, Primitive.createLong(lTTL));
         }

         return tobj;
      }

      /**
       * Serializes a unit of work state into the message.
       * @param uow The unit of work.
       * @param tobj The message to modify.
       */
      public void serializeState(UnitOfWork uow, TransferObject tobj)
      {
         GenericSerializablePropertyMap propertyMap = uow.m_propertyMap;

         if (propertyMap != null && propertyMap.getValueCount() != 0)
         {
            TransferObject properties = (TransferObject)tobj.findValue(JMSSender.PROPERTIES);

            if (properties == null || !properties.hasValue(JMSSender.STATE))
            {
               if (properties == null)
               {
                  properties = new TransferObject(1);
                  tobj.setValue(JMSSender.PROPERTIES, properties);
               }

               properties.setValue(JMSSender.STATE, propertyMap.serializeValues(uow.getInvocationContext()));
            }
         }
      }
   }

   /**
    * Factory for Asynchronous ObjectQueue messages.
    */
   protected static class ObjectMessageFactory implements MessageFactory
   {
      // operations

      /**
       * Builds the message.
       * @param properties The message sender properties.
       * @param nPriority The message priority.
       * @param lTTL The message time to live in milliseconds.
       * @return The message.
       */
      public TransferObject createMessage(TransferObject properties, int nPriority, long lTTL)
      {
         TransferObject tobj = new TransferObject(
            "ObjectQueueMessage", ((properties == null) ? 0 : properties.getValueCount()) + 3);

         if (properties != null)
         {
            for (PropertyIterator iter = properties.getIterator(); iter.hasNext();)
            {
               iter.next();
               tobj.setValue(iter.getName(), iter.getValue());
            }
         }

         if (nPriority >= 0)
         {
            tobj.setValue(ObjectSender.PRIORITY, Primitive.createInteger(nPriority));
         }

         if (lTTL >= 0)
         {
            tobj.setValue(ObjectSender.DELAY, Primitive.createLong(lTTL));
         }

         return tobj;
      }

      /**
       * Serializes a unit of work state into the message.
       * @param uow The unit of work.
       * @param tobj The message to modify.
       */
      public void serializeState(UnitOfWork uow, TransferObject tobj)
      {
         GenericSerializablePropertyMap propertyMap = uow.m_propertyMap;

         if (propertyMap != null && propertyMap.getValueCount() != 0)
         {
            if (!tobj.hasValue(ObjectSender.STATE))
            {
               tobj.setValue(ObjectSender.STATE, propertyMap.serializeValues(uow.getInvocationContext()));
            }
         }
      }
   }

   /**
    * Envelope containing an object.
    */
   protected class ObjectEnvelope extends Envelope
   {
      // constructors
      public ObjectEnvelope(MessageFactory mf, Object obj, TransferObject properties, int nPriority, long lTTL)
      {
         super(mf, properties, nPriority, lTTL);
         m_tobj.setValue(JMSSender.BODY, obj);
      }
   }

   /**
    * Envelope containing an instance or metaclass invocation.
    */
   protected class AccessorEnvelope extends Envelope
   {
      // associations

      /**
       * The accessor to invoke.
       */
      protected Accessor m_accessor;

      /**
       * The event to invoke on that instance.
       */
      protected Symbol m_event;

      /**
       * The event arguments. Can be null.
       */
      protected Object[] m_arguments;

      /**
       * The instance to invoke after this message has been processed.
       */
      protected Accessor m_correlator;

      /**
       * The event to invoke on the correlated instance.
       */
      protected Symbol m_corEvent;

      // constructors

      public AccessorEnvelope(MessageFactory mf, Accessor accessor, Symbol event, Object[] arguments,
         Accessor correlator, Symbol corEvent, TransferObject properties, int nPriority, long lTTL)
      {
         super(mf, properties, nPriority, lTTL);

         m_accessor = accessor;
         m_event = event;
         m_arguments = arguments;
         m_correlator = correlator;
         m_corEvent = corEvent;
      }

      // operations

      public TransferObject getMessage()
      {
         Lookup identityMap = new HashTab();
         TransferObject tobj;

         if (m_accessor instanceof Metaclass)
         {
            tobj = new TransferObject(((Metaclass)m_accessor).getName(), 0);
         }
         else
         {
            tobj = (TransferObject)RPCUtil.transferState(m_accessor, null, identityMap, RPCUtil.TF_ALL);
         }

         tobj.setEventName(m_event.getName());

         TransferObject correlator;

         if (m_correlator != null)
         {
            if (m_correlator instanceof Metaclass)
            {
               correlator = new TransferObject(((Metaclass)m_correlator).getName(), 0);
            }
            else
            {
               correlator = (TransferObject)RPCUtil.transferState(m_correlator, null, identityMap, RPCUtil.TF_TOP);
            }

            correlator.setEventName(m_corEvent.getName());
         }
         else
         {
            correlator = null;
         }

         Request request = new Request();
         Object[] arguments = null;

         request.setLocale(m_context.getLocale());
         request.setTimeZone(m_context.getTimeZone());
         request.setCorrelator(correlator);

         if (m_arguments != null)
         {
            arguments = new Object[m_arguments.length];

            for (int i = 0; i < arguments.length; ++i)
            {
               arguments[i] = RPCUtil.transferState(m_arguments[i], null, identityMap, RPCUtil.TF_ALL);
            }
         }

         request.addInvocation(tobj, arguments);
         m_tobj.setValue(Sender.BODY, request);

         return super.getMessage();
      }
   }

   /**
    * Interface implemented by cached item holders.
    */
   protected abstract static class Cached implements java.io.Serializable
   {
      /**
       * Serialization version.
       */
      private final static long serialVersionUID = 3211459364895597597L;

      /**
       * Flag that the object needs conversion.
       */
      protected transient boolean m_bConvert;

      /**
       * The cached item.
       */
      protected Object m_obj;

      /**
       * Prepares the object for caching.
       */
      public void prepare()
      {
         if (m_bConvert)
         {
            convert();
            m_bConvert = false;
         }
      }

      /**
       * @return True is the object has been prepared for caching.
       */
      public boolean isPrepared()
      {
         return !m_bConvert;
      }

      /**
       * Converts the object to internal format.
       * @param obj The object to convert.
       * @return The converted object.
       */
      protected abstract void convert();

      /**
       * Creates and returns the cached object.
       * @param context The invocation context.
       * @return The cached object.
       */
      public abstract Object get(InvocationContext context);

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         return ObjUtil.getShortClassName(this) + '(' + m_obj + ')';
      }
   }

   /**
    * Temporary cached reference holder.
    */
   protected class CachedTemporary extends Cached
   {
      // constants

      /**
       * Serialization version.
       */
      private final static long serialVersionUID = -6474669167188365034L;

      // constructors

      /**
       * Constructs the temporary cached reference.
       * @param ref The reference to cache.
       */
      public CachedTemporary(Object ref)
      {
         m_obj = ref;
      }

      // operations

      /**
       * @see nexj.core.runtime.UnitOfWork.Cached#convert()
       */
      protected void convert()
      {
      }

      /**
       * @see nexj.core.runtime.UnitOfWork.Cached#get(nexj.core.runtime.InvocationContext)
       */
      public Object get(InvocationContext context)
      {
         return m_obj;
      }
   }

   /**
    * Cached reference holder.
    */
   protected static class CachedReference extends Cached
   {
      // constants

      /**
       * Serialization version.
       */
      private final static long serialVersionUID = -984418065820365444L;

      // constructors

      /**
       * Constructs the cached reference.
       * @param ref The reference to cache.
       */
      public CachedReference(Object ref)
      {
         m_obj = ref;
      }

      // operations

      /**
       * @see nexj.core.runtime.UnitOfWork.Cached#convert()
       */
      protected void convert()
      {
      }

      /**
       * @see nexj.core.runtime.UnitOfWork.Cached#get(nexj.core.runtime.InvocationContext)
       */
      public Object get(InvocationContext context)
      {
         return m_obj;
      }
   }

   /**
    * Cached serialized copy holder.
    */
   protected final static class CachedCopy extends Cached
   {
      // constants

      /**
       * Serialization version.
       */
      private final static long serialVersionUID = -4966771213290983615L;

      // constructors

      /**
       * Constructs the cached object copy.
       * @param obj The cached object.
       */
      public CachedCopy(Object obj)
      {
         m_obj = obj;
         m_bConvert = true;
      }

      // operations

      /**
       * @see nexj.core.runtime.UnitOfWork.Cached#convert()
       */
      protected void convert()
      {
         try
         {
            ByteArrayOutputStream aos = new ByteArrayOutputStream(256);
            ObjectOutputStream os = new ObjectOutputStream(aos);

            os.writeObject(m_obj);
            os.flush();
            m_obj = aos.toByteArray();
         }
         catch (Exception e)
         {
            throw new PersistenceException("err.runtime.cacheSerialize", e);
         }
      }

      /**
       * @see nexj.core.runtime.UnitOfWork.Cached#get(nexj.core.runtime.InvocationContext)
       */
      public Object get(InvocationContext context)
      {
         if (m_bConvert)
         {
            return m_obj;
         }

         try
         {
            ByteArrayInputStream ais = new ByteArrayInputStream((byte[])m_obj);
            ObjectInputStream is = new ObjectInputStream(ais);

            return is.readObject();
         }
         catch (Exception e)
         {
            throw new PersistenceException("err.runtime.cacheDeserialize", e);
         }
      }
   }

   /**
    * Cached instance graph holder.
    */
   protected final static class CachedInstance extends Cached
   {
      // constants

      /**
       * Serialization version.
       */
      private final static long serialVersionUID = -4386876106769224104L;

      // attributes

      /**
       * The attribute list to serialize.
       */
      protected transient Pair m_attributes;

      /**
       * Constructs the holder.
       * @param instance The instance to hold.
       * @param attributes The list of attributes to convert.
       */
      public CachedInstance(Object instance, Pair attributes)
      {
         m_obj = instance;
         m_attributes = attributes;
         m_bConvert = true;
      }

      // operations

      /**
       * @see nexj.core.runtime.UnitOfWork.Cached#convert()
       */
      protected void convert()
      {
         m_obj = RPCUtil.transfer(m_obj, m_attributes, RPCUtil.TF_ALL | RPCUtil.TF_CACHE);
         m_attributes = null;
      }

      /**
       * @see nexj.core.runtime.UnitOfWork.Cached#get(nexj.core.runtime.InvocationContext)
       */
      public Object get(InvocationContext context)
      {
         if (m_bConvert)
         {
            return m_obj;
         }

         return RPCUtil.instantiateDirect(m_obj,
            new IdentityHashTab((m_obj instanceof Collection) ? ((Collection)m_obj).size() : 2),
            context);
      }
   }
}