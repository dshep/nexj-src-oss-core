// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import nexj.core.meta.Accessor;
import nexj.core.meta.Attribute;
import nexj.core.meta.Event;
import nexj.core.meta.Member;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Primitive;
import nexj.core.meta.Selector;
import nexj.core.meta.Type;
import nexj.core.meta.TypeConversionException;
import nexj.core.meta.Typed;
import nexj.core.meta.persistence.DataSource;
import nexj.core.meta.persistence.DataSourceFragment;
import nexj.core.meta.persistence.PersistenceMapping;
import nexj.core.persistence.AssociationIntegrityException;
import nexj.core.persistence.ConstraintViolationException;
import nexj.core.persistence.LazyLocation;
import nexj.core.persistence.OID;
import nexj.core.persistence.OIDHolder;
import nexj.core.persistence.OptimisticLockException;
import nexj.core.persistence.PersistenceAdapter;
import nexj.core.persistence.PersistenceException;
import nexj.core.persistence.PersistenceResolver;
import nexj.core.persistence.Query;
import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Pair;
import nexj.core.scripting.ScriptingException;
import nexj.core.scripting.Symbol;
import nexj.core.util.Binary;
import nexj.core.util.Invalid;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.Named;
import nexj.core.util.ObjUtil;
import nexj.core.util.PrintWriter;
import nexj.core.util.Printable;
import nexj.core.util.PropertyHashTab;
import nexj.core.util.PropertyIterator;
import nexj.core.util.PropertyMap;
import nexj.core.util.StackTrace;
import nexj.core.util.StringId;
import nexj.core.util.Undefined;

/**
 * A generic persistent object instance.
 * It is a closure and implements the Function interface.
 * The first argument of that function is the method symbol.
 */
public final class Instance implements Accessor, InvocationContextHolder, ContextHolder, PropertyMap, OIDHolder, InstanceHolder, LazyLocation, Function, Typed, Named, Printable
{
   // constants

   /**
    * Initial state of the instance - not registered with the UOW.
    */
   public final static byte INIT = 0;

   /**
    * Clean instance - has an OID and is registered in the UOW instance table.
    */
   public final static byte CLEAN = 1;

   /**
    * New instance - has no OID and is registered in the UOW change table.
    */
   public final static byte NEW = 2;

   /**
    * Dirty instance - has an OID and is registered in the UOW instance and change tables.
    */
   public final static byte DIRTY = 3;

   /**
    * Deleted instance - has an OID and is registered in the UOW instance and change tables.
    */
   public final static byte DELETED = 4;

   /**
    * The state event is pending in the UOW.
    */
   private final static byte EVENT_PENDING = 0x01;

   /**
    * The state event has been already invoked once in the UOW.
    */
   private final static byte EVENT_INVOKED = 0x02;

   /**
    * The commit event has already been invoked once in the UOW.
    */
   private final static byte EVENT_COMMIT = 0x04;

   /**
    * The security flag when a pending event is flagged.
    */
   private final static byte EVENT_SECURE = 0x08;

   /**
    * Temporary flag for load attribute computation,
    * used to avoid allocating storage on each load invocation.
    */
   private final static byte LOAD_READ = 0x10;

   /**
    * Set if the instance has been restored from cache.
    */
   private final static byte CACHE_BUSY = 0x20;

   /**
    * Set if all the instance state is updateable.
    */
   private final static byte UPDATEABLE = 0x40;

   /**
    * Set if the instance visibility has been reduced.
    */
   private final static byte HIDDEN = (byte)0x80;

   /**
    * Maximum loading depth for circular dependency detection.
    */
   private final static int MAX_LOADING_DEPTH = 1000;

   /**
    * State names, indexed by state number.
    */
   private final static String[] STATE_NAME_ARRAY = new String[]{"INIT", "CLEAN", "NEW", "DIRTY", "DELETED"};

   // attributes

   /**
    * The instance state (INIT, CLEAN, NEW, DIRTY, DELETED).
    */
   private byte m_nState;

   /**
    * Combination of pending system event (EVENT_*) and other flags.
    */
   private byte m_nFlags;

   /**
    * The loading depth counter.
    */
   private short m_nLoadingDepth;

   // associations

   /**
    * The object state. The values are indexed by the attribute ordinals.
    */
   private Object[] m_valueArray;

   /**
    * The object state before updating - a copy of m_valueArray.
    */
   private Object[] m_oldValueArray;

   /**
    * The object state before updating since the last life cycle event -
    * a copy of m_valueArray.
    */
   private Object[] m_preValueArray;

   /**
    * The annotation name to value map: Object[String].
    */
   private PropertyHashTab m_annotationMap;

   /**
    * This is the instance class.
    */
   private Metaclass m_metaclass;

   /**
    * The object OID.
    */
   private OID m_oid;

   /**
    * The object invocation context.
    */
   private InvocationContext m_context;

   /**
    * The object's current unit of work. Can be null.
    */
   private UnitOfWork m_uow;

   /**
    * Bit set for overridden calculated values.
    * The bit offset corresponds to the attribute ordinal number.
    */
   private byte[] m_overrideArray;

   /**
    * The class logger.
    */
   private final static Logger s_logger = Logger.getLogger(Instance.class);

   // constructors

   /**
    * Creates an instance of a given class with a specified state.
    * @param metaclass The metaclass of which to create an instance.
    * @param nState One of the Instance.* state constants.
    * @param context The invocation context.
    */
   public Instance(Metaclass metaclass, byte nState, InvocationContext context)
   {
      this(metaclass, false, context);
      setState(nState);
   }

   /**
    * Creates an undefined instance of a given class.
    * The instance state has to be set separately.
    * @param metaclass The metaclass of which to create an instance.
    * @param context The invocation context.
    */
   public Instance(Metaclass metaclass, InvocationContext context)
   {
      this(metaclass, false, context);
   }

   /**
    * Creates an undefined instance of a given class.
    * The instance state has to be set separately.
    * @param metaclass The metaclass of which to create an instance.
    * @param bLazy True to construct a lazy-loaded instance with deferred class identification.
    * @param context The invocation context.
    */
   public Instance(Metaclass metaclass, boolean bLazy, InvocationContext context)
   {
      assert metaclass != null;

      m_metaclass = metaclass;
      m_context = context;

      if (!bLazy)
      {
         m_valueArray = new Object[metaclass.getInstanceAttributeCount()];
         Arrays.fill(m_valueArray, Undefined.VALUE);
      }
   }

   // operations

   /**
    * Sets the metaclass of a lazy-loaded instance.
    */
   public void setMetaclass(Metaclass metaclass)
   {
      assert isLazy();

      m_metaclass = metaclass;
      m_valueArray = new Object[metaclass.getInstanceAttributeCount()];
      Arrays.fill(m_valueArray, Undefined.VALUE);
   }

   /**
    * @return The instance class.
    * @see nexj.core.meta.Accessor#getMetaclass()
    */
   public Metaclass getMetaclass()
   {
      load();

      return m_metaclass;
   }

   /**
    * Sets the tentative class of a lazy-loaded instance.
    * The instance remains lazy-loaded.
    */
   public void setLazyMetaclass(Metaclass metaclass)
   {
      assert isLazy();

      m_metaclass = metaclass;
   }

   /**
    * @see nexj.core.meta.Accessor#getLazyMetaclass()
    */
   public Metaclass getLazyMetaclass()
   {
      return m_metaclass;
   }

   /**
    * @see nexj.core.meta.Accessor#isLazy()
    */
   public boolean isLazy()
   {
      return m_valueArray == null;
   }

   /**
    * @return True if the instance is missing.
    */
   public boolean isMissing()
   {
      if (isLazy())
      {
         try
         {
            load((Pair)null, false, false);
         }
         catch (AssociationIntegrityException ex)
         {
            return true;
         }
      }

      return false;
   }

   /**
    * @see nexj.core.runtime.InstanceHolder#getInstance()
    */
   public Instance getInstance()
   {
      return this;
   }

   /**
    * @see nexj.core.meta.Typed#getType()
    */
   public Type getType()
   {
      return getMetaclass();
   }

   /**
    * @see nexj.core.meta.OIDHolder#setOID(nexj.core.meta.OID)
    */
   public void setOID(OID oid)
   {
      if (m_oid != null)
      {
         if (m_oid.equals(oid))
         {
            m_oid = oid;
            return;
         }

         m_context.removeInstance(this);

         if (m_uow != null && oid != null)
         {
            m_uow.changeOID(this);
         }
      }

      m_oid = oid;

      if (oid != null)
      {
         m_context.addInstance(this);
      }
   }

   /**
    * @see nexj.core.meta.OIDHolder#getOID()
    */
   public OID getOID()
   {
      return m_oid;
   }

   /**
    * @see nexj.core.persistence.LazyLocation#getLazyClassName()
    */
   public String getLazyClassName()
   {
      return m_metaclass.getName();
   }

   /**
    * @see nexj.core.persistence.LazyLocation#getLazyCaption()
    */
   public String getLazyCaption()
   {
      return m_metaclass.getCaption();
   }

   /**
    * Caches an instance in initial state in the invocation context and transitions it to clean state.
    * @param oid The instance OID.
    * @return The instance reference.
    */
   public InstanceRef cache(OID oid)
   {
      assert m_nState == INIT;
      assert m_uow == null;
      assert m_oid == null;
      assert (m_nFlags & EVENT_PENDING) == 0;

      m_oid = oid;

      InstanceRef ref = m_context.addInstance(this);

      m_nState = CLEAN;

      return ref;
   }

   /**
    * @return The Invocation context.
    */
   public InvocationContext getInvocationContext()
   {
      return m_context;
   }

   /**
    * @see nexj.core.runtime.ContextHolder#getContext()
    */
   public Context getContext()
   {
      return m_context;
   }

   /**
    * Sets the current unit of work.
    * @param uow The current unit of work to set.
    */
   public void setUnitOfWork(UnitOfWork uow)
   {
      m_uow = uow;
   }

   /**
    * @return The current unit of work.
    */
   public UnitOfWork getUnitOfWork()
   {
      return m_uow;
   }

   /**
    * @return The persistence mapping for this instance. Can be null.
    */
   public PersistenceMapping getPersistenceMapping()
   {
      PersistenceMapping mapping = m_metaclass.getPersistenceMapping();

      if (mapping != null && mapping.isDynamic())
      {
         mapping = ((PersistenceResolver)mapping.getDataSource().getComponent().getInstance(m_context)).getMapping(this);
      }

      return mapping;
   }

   /**
    * @return The data source fragment name of the instance. Can be null.
    */
   public String getFragmentName()
   {
      PersistenceMapping mapping = m_metaclass.getPersistenceMapping();

      if (mapping == null)
      {
         return null;
      }

      String sName = null;

      if (mapping.isDynamic())
      {
         sName = ((PersistenceResolver)mapping.getDataSource().getComponent().getInstance(m_context)).getFragmentName(this);
      }

      if (sName == null)
      {
         sName = m_context.getUnitOfWork().getFragmentName(mapping.getDataSource().getFragmentCount() != 1);
      }

      if (sName != null && sName.length() == 0)
      {
         sName = null;
      }

      return sName;
   }

   /**
    * @return The data source fragment of the instance. Can be null.
    */
   public DataSourceFragment getFragment()
   {
      PersistenceMapping mapping = getPersistenceMapping();

      if (mapping == null)
      {
         return null;
      }

      return mapping.getDataSource().getFragment(getFragmentName());
   }

   /**
    * @return The persistence adapter for this instance.
    */
   public PersistenceAdapter getAdapter()
   {
      return (PersistenceAdapter)getPersistenceMapping()
         .getDataSource().getComponent().getInstance(m_context);
   }

   /**
    * Clones the values into previous values.
    */
   private Object[] cloneValues()
   {
      Object[] valueArray = (Object[])m_valueArray.clone();

      for (int i = 0, n = m_metaclass.getInstanceAttributeCount(); i < n; ++i)
      {
         if (m_metaclass.getInstanceAttribute(i).isCalculated())
         {
            valueArray[i] = Undefined.VALUE;
         }
      }

      return valueArray;
   }

   /**
    * Commits the instance.
    */
   public void commit()
   {
      assert m_oid != null || getPersistenceMapping() == null || m_nState == DELETED;

      switch (m_nState)
      {
         case INIT:
            m_nState = CLEAN;

            break;

         case NEW:
         case DIRTY:
            uncache();
            m_uow.removeChange(this);
            m_nState = CLEAN;
            m_oldValueArray = null;
            m_preValueArray = null;
            m_overrideArray = null;

            break;

         case DELETED:
            uncache();
            m_uow.removeChange(this);
            m_context.removeInstance(this);

            if (m_preValueArray != null)
            {
               System.arraycopy(m_valueArray, 0, m_preValueArray, 0, m_valueArray.length);
            }

            break;
      }

      m_nFlags &= HIDDEN;
      m_uow = null;
   }

   /**
    * Rolls the instance back to CLEAN state.
    */
   public void rollback()
   {
      switch (m_nState)
      {
         case INIT:
         case NEW:
            m_nState = DELETED;
            setOID(null);

            break;

         case DIRTY:
            m_nState = CLEAN;
            m_valueArray = m_oldValueArray;
            m_oldValueArray = null;
            m_overrideArray = null;

            break;

         case DELETED:
            m_nState = CLEAN;

            if (m_oldValueArray != null)
            {
               m_valueArray = m_oldValueArray;
               m_oldValueArray = null;
               m_overrideArray = null;
            }

            break;
      }

      m_preValueArray = null;
      m_nFlags &= HIDDEN;
      m_uow = null;
   }

   /**
    * Transitions the instance to CLEAN state.
    */
   public void setClean()
   {
      assert m_oid != null || getPersistenceMapping() == null;

      switch (m_nState)
      {
         case INIT:
            m_nState = CLEAN;

            break;

         case NEW:
            m_uow.removeChange(this);
            m_nState = CLEAN;

            break;

         case DIRTY:
         case DELETED:
            m_uow.removeChange(this);
            m_nState = CLEAN;
            m_oldValueArray = null;
            m_overrideArray = null;

            break;
      }

      m_preValueArray = null;
      m_nFlags &= HIDDEN;
      m_uow = null;
   }

   /**
    * Transitions the instance to NEW state.
    */
   public void setNew()
   {
      assert !isLazy();

      switch (m_nState)
      {
         case INIT:
            assert m_oid == null;

            m_context.manageTransaction(m_metaclass.getCreateTransactionMode());
            m_nState = NEW;
            setEventPending(true);
            m_context.getUnitOfWork().addChange(this);

            break;

         case CLEAN:
            assert m_oid != null;

            m_context.removeInstance(this);
            m_context.manageTransaction(m_metaclass.getCreateTransactionMode());
            m_nState = NEW;
            setEventPending(true);
            m_context.getUnitOfWork().addChange(this);

            break;

         case NEW:
            return;

         default:
            throw new IllegalStateException("Cannot create an already initialized instance");
      }

      m_preValueArray = cloneValues();
   }

   /**
    * Transitions the instance to DIRTY state.
    */
   public void setDirty()
   {
      load();

      switch (m_nState)
      {
         case INIT:
            assert m_oid != null;

            m_context.manageTransaction(m_metaclass.getUpdateTransactionMode());
            m_oldValueArray = (Object[])m_valueArray.clone();
            m_preValueArray = cloneValues();
            m_context.addInstance(this);
            m_nState = DIRTY;
            setEventPending(true);
            m_context.getUnitOfWork().addChange(this);

            break;

         case CLEAN:
            assert m_oid != null || m_metaclass.getPersistenceMapping() == null;

            m_context.manageTransaction(m_metaclass.getUpdateTransactionMode());
            m_oldValueArray = (Object[])m_valueArray.clone();
            m_preValueArray = cloneValues();
            m_nState = DIRTY;
            setEventPending(true);
            m_context.getUnitOfWork().addChange(this);

            break;

         case NEW:
            setEventPending(true);
            m_context.getUnitOfWork().addChange(this);

         case DIRTY:
            break;

         default:
            throw new IllegalStateException("Cannot modify an already deleted instance");
      }
   }

   /**
    * Transitions the instance to REMOVED state.
    */
   public void setDeleted()
   {
      load();

      switch (m_nState)
      {
         case INIT:
            assert getPersistenceMapping() == null || m_oid != null;

            m_context.addInstance(this);
            m_nState = DELETED;
            setEventPending(true);
            m_context.getUnitOfWork().addChange(this);

            break;

         case CLEAN:
            assert m_oid != null;

            m_nState = DELETED;
            setEventPending(true);
            m_context.getUnitOfWork().addChange(this);

            break;

         case NEW:
            m_nState = DELETED;
            setEventPending(true);
            m_uow.removeChange(this);

            break;

         case DIRTY:
            setEventPending(true);
            m_uow.addChange(this);

         case DELETED:
            m_nState = DELETED;
            m_uow.accumulateChange(this);

            break;
      }

      if (m_preValueArray != null)
      {
         System.arraycopy(m_valueArray, 0, m_preValueArray, 0, m_valueArray.length);
      }
   }

   /**
    * Transitions instance state (CLEAN, NEW, DIRTY, DELETED).
    * The state after this method invocation is not necessarily nState.
    * @param nState The instance state to set.
    */
   public void setState(byte nState)
   {
      switch (nState)
      {
         case INIT:
            if (m_nState != INIT)
            {
               throw new IllegalStateException("Cannot go back to INIT state");
            }

            break;

         case CLEAN:
            setClean();

            break;

         case NEW:
            setNew();

            break;

         case DIRTY:
            setDirty();

            break;

         case DELETED:
            setDeleted();

            break;

         default:
            throw new IllegalArgumentException("Invalid state: " + nState);
      }
   }

   /**
    * @return The instance state (INIT, CLEAN, NEW, DIRTY, DELETED).
    */
   public byte getState()
   {
      return m_nState;
   }

   /**
    * Moves the instance to the back of the change queue.
    */
   public void requeue()
   {
      if (m_nState > CLEAN)
      {
         m_context.getUnitOfWork().addChange(this);
      }
   }

   /**
    * Suspends the pending system event.
    * Not the same as setEventpending(false), as it does not change any internal state.
    */
   public void suspendEvent()
   {
      m_nFlags &= ~EVENT_PENDING;
   }

   /**
    * Sets the system event pending flag.
    * @param bEventPending The system event pending flag to set.
    */
   public void setEventPending(boolean bEventPending)
   {
      if (bEventPending)
      {
         if ((m_nFlags & EVENT_PENDING) != 0)
         {
            if (m_context.isSecure())
            {
               m_nFlags |= EVENT_SECURE;
            }
         }
         else
         {
            m_nFlags |= EVENT_PENDING;

            if (m_context.isSecure())
            {
               m_nFlags |= EVENT_SECURE;
            }
            else
            {
               m_nFlags &= ~EVENT_SECURE;
            }
         }
      }
      else
      {
         suspendEvent();

         if (m_preValueArray != null)
         {
            int nValueCount = m_valueArray.length;

            // Minimize the work
            if (nValueCount * 3 < m_uow.getCollectionCount())
            {
               for (int i = 0; i < nValueCount; ++i)
               {
                  Object value = m_valueArray[i];

                  if (value instanceof InstanceList)
                  {
                     ((InstanceList)value).finish();
                  }

                  Object pre = m_preValueArray[i];

                  if (pre != value && pre instanceof InstanceList)
                  {
                     ((InstanceList)pre).finish();
                  }

                  if (m_oldValueArray != null)
                  {
                     Object old = m_oldValueArray[i];

                     if (old != pre && old != value && old instanceof InstanceList)
                     {
                        ((InstanceList)old).finish();
                     }
                  }
               }
            }
            else
            {
               for (Iterator itr = m_uow.getCollectionIterator(); itr.hasNext();)
               {
                  InstanceList list = (InstanceList)itr.next();

                  if (list.getContainer() == this)
                  {
                     list.finish();
                  }
               }
            }

            System.arraycopy(m_valueArray, 0, m_preValueArray, 0, m_valueArray.length);
         }
      }
   }

   /**
    * @return The system event pending flag.
    */
   public boolean isEventPending()
   {
      return (m_nFlags & EVENT_PENDING) != 0;
   }

   /**
    * Sets the system event invocation flag and clears the pending event flag if the invocation flag is true.
    * @param bEventInvoked The system event invocation flag to set.
    */
   public void setEventInvoked(boolean bEventInvoked)
   {
      if (bEventInvoked)
      {
         setEventPending(false);
         m_nFlags |= EVENT_INVOKED;
      }
      else
      {
         m_nFlags &= ~EVENT_INVOKED;
      }
   }

   /**
    * @return The system event invocation flag.
    */
   public boolean isEventInvoked()
   {
      return (m_nFlags & EVENT_INVOKED) != 0;
   }

   /**
    * @return The pending event name, null if none.
    */
   public String getPendingEventName()
   {
      if ((m_nFlags & EVENT_PENDING) != 0)
      {
         switch (m_nState)
         {
            case NEW:
               if ((m_nFlags & EVENT_INVOKED) != 0)
               {
                  return "update";
               }

               return "create";

            case DIRTY:
               return "update";

            case DELETED:
               return "delete";
         }
      }

      return null;
   }

   /**
    * Invokes a system event.
    * @param sEvent The system event name. Can be null.
    */
   protected void invokeSystemEvent(String sEvent)
   {
      if (sEvent != null)
      {
         boolean bSecure = (m_nFlags & EVENT_SECURE) != 0;

         if (bSecure == m_context.isSecure())
         {
            invoke(sEvent);
         }
         else
         {
            boolean bSecureSaved = m_context.isSecure();

            try
            {
               m_context.setSecure(bSecure);
               invoke(sEvent);
            }
            finally
            {
               m_context.setSecure(bSecureSaved);
            }
         }
      }
   }

   /**
    * Invokes the pending event.
    */
   public void invokePendingEvent()
   {
      invokeSystemEvent(getPendingEventName());
   }

   /**
    * Invokes a suspended system event.
    * @return True if the event has been invoked.
    */
   public boolean invokeSuspendedEvent()
   {
      if ((m_nFlags & EVENT_PENDING) != 0)
      {
         invokePendingEvent();

         return true;
      }

      if ((m_nFlags & EVENT_INVOKED) == 0)
      {
         switch (m_nState)
         {
            case NEW:
               invokeSystemEvent("create");

               return true;

            case DIRTY:
               invokeSystemEvent("update");

               return true;
         }
      }

      return false;
   }

   /**
    * Sets the commit pending flag.
    * @param bCommitPending The commit pending flag to set.
    */
   public void setCommitPending(boolean bCommitPending)
   {
      if (bCommitPending)
      {
         m_nFlags &= ~EVENT_COMMIT;
      }
      else
      {
         m_nFlags |= EVENT_COMMIT;
      }
   }

   /**
    * @return True if a commit event should be invoked for the instance.
    */
   public boolean isCommitPending()
   {
      return (m_nFlags & EVENT_COMMIT) == 0 && (m_nState == NEW || m_nState == DIRTY);
   }

   /**
    * Marks the instance as cached.
    */
   public void setCached(boolean bCached)
   {
      if (bCached)
      {
         if (!isLazy())
         {
            m_nFlags |= CACHE_BUSY;
         }
      }
      else
      {
         m_nFlags &= ~CACHE_BUSY;
      }
   }

   /**
    * @return True if the instance is marked as cached.
    */
   public boolean isCached()
   {
      return (m_nFlags & CACHE_BUSY) != 0;
   }

   /**
    * @return True if the visibility has been reduced.
    */
   public boolean isHidden()
   {
      return (m_nFlags & HIDDEN) != 0;
   }

   /**
    * Computes the hiddenness flag.
    */
   public void computeHiddenness()
   {
      if ((m_nFlags & HIDDEN) == 0)
      {
         switch (m_nState)
         {
            case NEW:
               m_nFlags |= HIDDEN;
               break;

            case DIRTY:
            case DELETED:
               if (m_context.getGeneration() != InvocationContext.GEN_NEW)
               {
                  break;
               }

               Attribute attribute = m_metaclass.getReadAccessAttribute();

               if (m_nState == DIRTY && attribute != null && !attribute.isStatic() &&
                  getValueDirect(attribute.getOrdinal()) == Undefined.VALUE)
               {
                  break;
               }

               if (isReadable())
               {
                  if (m_nState == DELETED)
                  {
                     m_nFlags |= HIDDEN;
                  }
               }
               else
               {
                  try
                  {
                     m_context.setGeneration(InvocationContext.GEN_OLD);

                     if (isReadable())
                     {
                        m_nFlags |= HIDDEN;
                     }
                  }
                  finally
                  {
                     m_context.setGeneration(InvocationContext.GEN_NEW);
                  }
               }

               break;
         }
      }
   }

   /**
    * Sets the loading flag.
    * @param bLoading The loading flag to set.
    */
   public void setLoading(boolean bLoading)
   {
      if (bLoading)
      {
         ++m_nLoadingDepth;
      }
      else
      {
         --m_nLoadingDepth;
      }
   }

   /**
    * @return The loading flag.
    */
   public boolean isLoading()
   {
      return m_nLoadingDepth != 0;
   }

   /**
    * Invokes the load event for the specified attribute.
    * @param symbol The attribute symbol.
    */
   public void load(Symbol symbol)
   {
      ((Event)m_metaclass.getSelector("load").getMember(1)).invoke(
         new Object[]{this, symbol}, m_context.getMachine());
   }

   /**
    * Discards the value of a given attribute.
    * @param attribute The attribute which value to discard.
    * @param bReverseCollection True to remove an item from a reverse collection.
    */
   public void discard(Attribute attribute, boolean bReverseCollection)
   {
      assert m_uow == null;

      if (!isLazy())
      {
         attribute = m_metaclass.getInstanceAttribute(attribute.getOrdinal());

         Attribute reverse = attribute.getReverse();
         byte nGenerationSaved = m_context.getGeneration();

         try
         {
            for (byte nGeneration = nGenerationSaved;;)
            {
               Object oldValue = getValueDirect(attribute.getOrdinal());

               if (!(oldValue instanceof Undefined))
               {
                  attribute.invalidateDependency(this, Undefined.VALUE);

                  if (oldValue != null)
                  {
                     if (attribute.isCollection())
                     {
                        InstanceList list = (InstanceList)oldValue;

                        list.setLazy(true);

                        for (int i = list.getCount() - 1; i >= 0; --i)
                        {
                           Instance instance = list.getInstance(i);

                           if (instance == null)
                           {
                              list.remove(i, InstanceList.DIRECT);
                           }
                           else if (instance.getUnitOfWork() == null && !instance.isLazy())
                           {
                              if (reverse != null)
                              {
                                 if (!(instance.getValueDirect(reverse.getOrdinal()) instanceof Undefined))
                                 {
                                    instance.setValueDirect(reverse.getOrdinal(), Undefined.VALUE);
                                    instance.getMetaclass().getInstanceAttribute(reverse.getOrdinal())
                                       .invalidateDependency(instance, Undefined.VALUE);
                                 }
                              }

                              list.remove(i, InstanceList.DIRECT);
                           }
                        }
                     }
                     else
                     {
                        if (reverse != null)
                        {
                           Instance instance = (Instance)oldValue;

                           if (instance.getUnitOfWork() == null && !instance.isLazy())
                           {
                              if (reverse.isCollection())
                              {
                                 if (bReverseCollection)
                                 {
                                    Object reverseValue = instance.getValueDirect(reverse.getOrdinal());

                                    if (reverseValue != Undefined.VALUE && reverseValue != null)
                                    {
                                       InstanceList list = (InstanceList)reverseValue;

                                       list.setLazy(true);

                                       if (list.remove(this, InstanceList.DIRECT))
                                       {
                                          instance.getMetaclass().getInstanceAttribute(reverse.getOrdinal())
                                             .invalidateDependency(instance, Undefined.VALUE);
                                       }
                                    }
                                 }
                              }
                              else
                              {
                                 instance.setValueDirect(reverse.getOrdinal(), Undefined.VALUE);
                              }
                           }
                        }

                        setValueDirect(attribute.getOrdinal(), Undefined.VALUE);
                     }
                  }
                  else
                  {
                     setValueDirect(attribute.getOrdinal(), Undefined.VALUE);
                  }
               }

               if (++nGeneration > InvocationContext.GEN_OLD)
               {
                  break;
               }

               m_context.setGeneration(nGeneration);
            }
         }
         finally
         {
            m_context.setGeneration(nGenerationSaved);
         }
      }
   }

   /**
    * Sets/add an association.
    * @param nOrdinal The ordinal number of the association to set.
    * @param instance The instance to add.
    * @param bDirect True to disable side effects.
    */
   public void associate(int nOrdinal, Instance instance, boolean bDirect)
   {
      load();

      Attribute attribute = m_metaclass.getInstanceAttribute(nOrdinal);

      if (attribute.isCollection())
      {
         Object value = getValueDirect(nOrdinal);
         InstanceList list;

         if (value instanceof Undefined)
         {
            list = new InstanceArrayList();

            setValueDirect(nOrdinal, list);
            list.setAssociation(this, attribute, true);
         }
         else
         {
            list = (InstanceList)value;
         }

         if (list.add(instance, InstanceList.REPLACE | InstanceList.DIRECT | InstanceList.WEAK | InstanceList.TRACK))
         {
            if (!bDirect)
            {
               attribute.invalidateDependency(this, Invalid.VALUE);
            }
         }
      }
      else if (bDirect)
      {
         Object value = getValueDirect(nOrdinal);

         if (value instanceof Undefined)
         {
            setValueDirect(nOrdinal, instance);
         }
      }
      else
      {
         assign(attribute, instance, false);
      }
   }

   /**
    * Removes an association.
    * @param nOrdinal The ordinal number of the association to remove.
    * @param instance The instance to remove.
    * @param bDirect True to disable side effects.
    */
   public void dissociate(int nOrdinal, Instance instance, boolean bDirect)
   {
      load();

      Attribute attribute = m_metaclass.getInstanceAttribute(nOrdinal);

      if (attribute.isCollection())
      {
         Object value;

         if (bDirect)
         {
            value = getValueDirect(nOrdinal);
         }
         else
         {
            value = getValue(nOrdinal);
            attribute.invalidateDependency(this, Invalid.VALUE);
         }

         if (!(value instanceof Undefined))
         {
            InstanceList list = (InstanceList)value;

            list.remove(instance, (bDirect) ? InstanceList.DIRECT : InstanceList.DIRECT | InstanceList.TRACK);
         }
      }
      else
      {
         if (bDirect)
         {
            setValueDirect(nOrdinal, null);
         }
         else
         {
            attribute.invalidateDependency(this, Invalid.VALUE);
            assign(attribute, null, false);
         }
      }
   }

   /**
    * @return True if the instance is readable to the current user.
    */
   public boolean isReadable()
   {
      load();

      if (!m_metaclass.isReadable(m_context.getPrivilegeSet()))
      {
         return false;
      }

      Attribute attribute = m_metaclass.getReadAccessAttribute();

      if (attribute != null)
      {
         if (m_context.isSecure())
         {
            Object value = (attribute.isStatic()) ?
               m_metaclass.getValue(attribute.getOrdinal()) :
                  getValue(attribute.getOrdinal());

            if (!Boolean.TRUE.equals(value))
            {
               return false;
            }
         }
      }

      return true;
   }

   /**
    * Checks if a given attribute is readable.
    * @param attribute The attribute to check.
    * @return True if readable.
    */
   public boolean isReadable(Attribute attribute)
   {
      load();

      return attribute.isReadable(m_context.getPrivilegeSet());
   }

   /**
    * Checks if a given attribute is updateable.
    * @param attribute The attribute to check.
    * @return True if updateable.
    */
   public boolean isUpdateable(Attribute attribute)
   {
      if ((m_nFlags & UPDATEABLE) != 0)
      {
         return true;
      }

      load();

      if (!m_context.isSecure())
      {
         return !attribute.isReadOnly();
      }

      if (!attribute.isUpdateable(m_context.getPrivilegeSet()))
      {
         return false;
      }

      if (m_nState != NEW)
      {
         Attribute accessAttribute = attribute.getAccessAttribute();

         if (accessAttribute != null)
         {
            int nOrdinal = accessAttribute.getOrdinal();

            if (accessAttribute.isStatic())
            {
               return !Boolean.FALSE.equals(m_metaclass.getValue(nOrdinal));
            }

            return !Boolean.FALSE.equals(getValue(nOrdinal)) ||
               !Boolean.FALSE.equals(getOldValue(nOrdinal));
         }
      }

      return true;
   }

   /**
    * Checks the access rights to a given attribute.
    * @param attribute The attribute to check.
    * @throws SecurityViolationException if the access is denied.
    */
   public void checkUpdateAccess(Attribute attribute) throws SecurityViolationException
   {
      if (!isUpdateable(attribute))
      {
         throw new SecurityViolationException(
            (attribute.isReadOnly()) ?
               "err.runtime.attributeReadOnlyAccess" :
               "err.runtime.attributeUpdateAccess",
            new Object[]{attribute.getName(), m_metaclass.getName()});
      }
   }

   /**
    * Checks the access rights to a given attribute.
    * @param nOrdinal The attribute ordinal number.
    * @throws SecurityViolationException if the access is denied.
    */
   public void checkUpdateAccess(int nOrdinal) throws SecurityViolationException
   {
      load();

      checkUpdateAccess(m_metaclass.getInstanceAttribute(nOrdinal));
   }

   /**
    * Checks the access rights to a given attribute.
    * @param nOrdinal The attribute ordinal number.
    * @param instance The instance which is going to be assigned.
    * @throws SecurityViolationException if the access is denied.
    */
   public void checkUpdateAccess(int nOrdinal, Instance instance) throws SecurityViolationException
   {
      load();

      Attribute attribute = m_metaclass.getInstanceAttribute(nOrdinal);

      if (attribute.isCollection())
      {
         Object value = getValueDirect(nOrdinal);

         if (value instanceof InstanceList)
         {
            InstanceList list = (InstanceList)value;

            if (!list.isLazy() && list.contains(instance))
            {
               return;
            }
         }
      }
      else
      {
         if (getValueDirect(nOrdinal) == instance)
         {
            return;
         }
      }

      if (!isUpdateable(attribute))
      {
         if (attribute.isCollection())
         {
            InstanceList list = (InstanceList)getValue(nOrdinal);

            if (list == null || !list.contains(instance))
            {
               checkUpdateAccess(attribute);
            }
         }
         else
         {
            if (getValue(nOrdinal) != instance)
            {
               checkUpdateAccess(attribute);
            }
         }
      }
   }

   /**
    * Initializes the aliased associations for a newly assigned collection.
    * @param list The newly assigned collection.
    */
   private void setAliases(InstanceList list)
   {
      assert !isLazy();

      Attribute attribute = list.getAttribute();
      Attribute alias = attribute.getAlias();

      if (alias != null)
      {
         while (alias.getOrdinal() != attribute.getOrdinal())
         {
            Object value = getValueDirect(alias.getOrdinal());

            if (value != null && !(value instanceof Undefined))
            {
               if (value instanceof InstanceList)
               {
                  InstanceList aliasList = (InstanceList)value;

                  if (aliasList.getCount() != 0)
                  {
                     list.setLoading();
                  }

                  if (list.getCount() != 0)
                  {
                     aliasList.setLoading();
                  }
               }
               else
               {
                  list.setLoading();
               }
            }

            alias = alias.getAlias();
         }
      }
   }

   /**
    * Updates the aliased associations of a given attribute.
    * @param attribute The attribute for which to update the associations.
    */
   protected void updateAliases(Attribute attribute)
   {
      assert !isLazy();

      Attribute alias = attribute.getAlias();

      if (alias != null)
      {
         while (alias.getOrdinal() != attribute.getOrdinal())
         {
            Object value = getValueDirect(alias.getOrdinal());

            if (value != null && !(value instanceof Undefined))
            {
               if (value instanceof InstanceList)
               {
                  ((InstanceList)value).setLoading();
               }
            }

            alias = alias.getAlias();
         }
      }
   }

   /**
    * Assigns a value without side effects,
    * except for instance state management.
    * This method is for INTERNAL USE ONLY.
    * @param attribute The attribute object.
    * @param value The attribute value.
    * @param bAssoc True to set the reverse assoc.
    */
   public void assign(Attribute attribute, Object value, boolean bAssoc)
   {
      assert !isLazy();

      int nOrdinal = attribute.getOrdinal();
      Object oldValue = getValueDirect(nOrdinal);

      if (!ObjUtil.equal(oldValue, value))
      {
         if (!isLoading())
         {
            if (!attribute.isCollection() &&
               (m_metaclass.getPersistenceMapping() == null ||
                  m_metaclass.getPersistenceMapping().getLockingAttribute() != attribute))
            {
               if (m_nState == DIRTY || m_nState == NEW)
               {
                  if (m_uow != m_context.getUnitOfWork() && m_context.isUnitOfWorkGlobal())
                  {
                     throw new TransactionException("err.runtime.wrongTransaction",
                        new Object[]{getLazyClassName()});
                  }

                  setEventPending(true);
               }
               else
               {
                  setDirty();
               }
            }

            if (attribute.isCalculated())
            {
               setOverridden(nOrdinal);
            }
         }

         if (attribute.isCollection())
         {
            InstanceList list = (InstanceList)value;

            if (list != null && attribute.getReverse() != null)
            {
               list.checkUpdateAccess(attribute.getReverse(), this);
            }

            setValueDirect(nOrdinal, list);

            if (list != null)
            {
               list.setAssociation(this, attribute, false);
               setAliases(list);
            }
         }
         else if (bAssoc)
         {
            Attribute reverse = attribute.getReverse();

            if (reverse != null)
            {
               int nRevOrdinal = reverse.getOrdinal();

               if (oldValue instanceof Instance && attribute.isReverseOf(reverse))
               {
                  Instance instance = (Instance)oldValue;

                  instance.checkUpdateAccess(nRevOrdinal);
                  instance.dissociate(nRevOrdinal, this, false);
               }

               Instance instance = (Instance)value;

               if (instance != null)
               {
                  instance.checkUpdateAccess(nRevOrdinal, this);
               }

               setValueDirect(nOrdinal, instance);

               if (instance != null)
               {
                  instance.associate(nRevOrdinal, this, false);
               }
            }
            else
            {
               setValueDirect(nOrdinal, value);
            }
         }
         else
         {
            setValueDirect(nOrdinal, value);
         }
      }
   }

   /**
    * @see nexj.core.meta.Accessor#setValue(int, java.lang.Object)
    */
   public void setValue(int nOrdinal, Object value)
   {
      assert !isLazy();

      Attribute attribute = m_metaclass.getInstanceAttribute(nOrdinal);

      value = convert(attribute, value);

      Object oldValue = getValueDirect(nOrdinal);

      if (!ObjUtil.equal(oldValue, value))
      {
         if (!isLoading())
         {
            checkUpdateAccess(attribute);

            if (oldValue == Undefined.VALUE && attribute.isReverseInverseDependencyPersistent())
            {
               oldValue = getValue(nOrdinal);
            }

            if (m_nState == DIRTY || m_nState == NEW)
            {
               if (m_uow != m_context.getUnitOfWork() && m_context.isUnitOfWorkGlobal())
               {
                  throw new TransactionException("err.runtime.wrongTransaction",
                     new Object[]{getLazyClassName()});
               }

               if (!isEventPending())
               {
                  m_context.getUnitOfWork().addChange(this);
               }

               setEventPending(true);
            }
            else
            {
               setDirty();
            }

            if (attribute.isCalculated())
            {
               setOverridden(nOrdinal);
            }

            attribute.invalidateDependency(this, Invalid.VALUE);
         }

         if (attribute.isCollection())
         {
            if (attribute.getType().isPrimitive())
            {
               setValueDirect(nOrdinal, (List)value);
            }
            else
            {
               InstanceList list = (InstanceList)value;

               if (list != null && !isLoading())
               {
                  list.checkUpdateAccess(attribute.getReverse(), this);
               }

               setValueDirect(nOrdinal, list);

               if (list != null)
               {
                  list.setAssociation(this, attribute, isLoading());
                  setAliases(list);
               }
            }
         }
         else
         {
            Attribute reverse = attribute.getReverse();

            if (reverse != null)
            {
               int nRevOrdinal = reverse.getOrdinal();

               if (oldValue instanceof Instance && attribute.isReverseOf(reverse))
               {
                  Instance instance = (Instance)oldValue;

                  if (!instance.isMissing())
                  {
                     if (!isLoading())
                     {
                        instance.checkUpdateAccess(nRevOrdinal);
                     }

                     instance.dissociate(nRevOrdinal, this, isLoading());
                  }
               }

               Instance instance = (Instance)value;

               if (instance != null && !isLoading())
               {
                  instance.checkUpdateAccess(nRevOrdinal, this);
               }

               setValueDirect(nOrdinal, instance);

               if (instance != null)
               {
                  instance.associate(nRevOrdinal, this, isLoading());
               }
            }
            else
            {
               setValueDirect(nOrdinal, value);
            }
         }
      }
   }

   /**
    * @see nexj.core.meta.Accessor#setValue(java.lang.String, java.lang.Object)
    */
   public void setValue(String sName, Object value)
   {
      load();

      Attribute attribute = m_metaclass.getAttribute(sName);

      if (attribute.isStatic())
      {
         m_metaclass.setValue(attribute.getOrdinal(), value);
      }
      else
      {
         setValue(attribute.getOrdinal(), value);
      }
   }

   /**
    * @see nexj.core.meta.Accessor#getValue(int)
    */
   public Object getValue(int nOrdinal)
   {
      assert !isLazy();

      Object value = getValueDirect(nOrdinal);
      Attribute attribute = m_metaclass.getInstanceAttribute(nOrdinal);

      if (!(value instanceof Undefined) && attribute.isCached() &&
         (!attribute.isCalculated() || attribute.isPersistent() ||
            m_context.getGeneration() == InvocationContext.GEN_NEW || m_nState >= NEW))
      {
         return value;
      }

      ++m_nLoadingDepth;

      try
      {
         if (m_nLoadingDepth > MAX_LOADING_DEPTH)
         {
            throw new IterationCountException("err.persistence.circularLoading",
               new Object[]{attribute.getName(), m_metaclass.getName()});
         }

         if (m_nState == NEW && value == Undefined.VALUE && attribute.getInitializer() != Undefined.VALUE)
         {
            return setCalcValue(attribute,
               m_context.getMachine().invoke(attribute.getInitializerFunction(), new Pair(this)));
         }

         if (attribute.getValueFunction() != null)
         {
            if (m_nState == CLEAN || m_nState == DIRTY)
            {
               // Optimize the lazy-loading and possibly evaluate in the persistence engine
               if (!isPersistentDependencyLoaded(attribute))
               {
                  load(new Pair(attribute.getSymbol()), true, false);

                  if (attribute.isCached())
                  {
                     value = getValueDirect(nOrdinal);

                     if (!(value instanceof Undefined))
                     {
                        return value;
                     }
                  }
               }
            }

            return setCalcValue(attribute,
               m_context.getMachine().invoke(attribute.getValueFunction(), this, (Object[])null));
         }

         if (attribute.isCollection() && attribute.isPersistent() &&
            (m_nState == CLEAN || m_nState == DIRTY))
         {
            InstanceList list = new InstanceArrayList(0);

            setDefaultValue(nOrdinal, list);
            list.setAssociation(this, attribute, true);

            return list;
         }

         load(attribute.getSymbol());

         return getValueDirect(nOrdinal);
      }
      finally
      {
         --m_nLoadingDepth;
      }
   }

   /**
    * @see nexj.core.meta.Accessor#getValue(java.lang.String)
    */
   public Object getValue(String sName)
   {
      load();

      Attribute attribute = m_metaclass.findAttribute(sName);

      if (attribute == null)
      {
         Object value = findAnnotation(sName, Undefined.VALUE);

         if (value != Undefined.VALUE)
         {
            return value;
         }

         // This will throw an exception
         attribute = m_metaclass.getAttribute(sName);
      }

      if (attribute.isStatic())
      {
         return m_metaclass.getValue(attribute.getOrdinal());
      }

      return getValue(attribute.getOrdinal());
   }

   /**
    * @see nexj.core.meta.Accessor#getValue(java.lang.String, java.lang.String)
    */
   public Object getValue(String sName, String sFallbackName)
   {
      load();

      Attribute attribute = m_metaclass.findAttribute(sName);

      if (attribute == null)
      {
         if (sFallbackName == null)
         {
            return null;
         }

         attribute = m_metaclass.getAttribute(sFallbackName);
      }

      if (attribute.isStatic())
      {
         return m_metaclass.getValue(attribute.getOrdinal());
      }

      return getValue(attribute.getOrdinal());
   }

   /**
    * Converts a value to the type of a given attribute.
    * @param attribute The attribute.
    * @param value The value.
    * @return The converted value.
    */
   protected Object convert(Attribute attribute, Object value)
   {
      Type type = attribute.getType();

      if (attribute.isCollection())
      {
         if (value != null)
         {
            if (type.isPrimitive())
            {
               List list = (List)value;
               List convertedList = null;

               for (int i = 0, n = list.size(); i < n; ++i)
               {
                  Object original = list.get(i);

                  if (convertedList == null)
                  {
                     Primitive primitive = Primitive.primitiveOf(original);

                     if (type != primitive)
                     {
                        convertedList = new ArrayList(list.size());

                        for (int k = 0; k < i; ++k)
                        {
                           convertedList.add(list.get(k));
                        }

                        convertedList.add(((Primitive)type).getConverter(primitive).invoke(original));
                     }
                  }
                  else
                  {
                     convertedList.add(type.convert(original));
                  }
               }

               return (convertedList != null) ? convertedList : list;
            }

            InstanceList list;

            if (value instanceof InstanceList)
            {
               list = (InstanceList)value;
            }
            else if (value instanceof Collection)
            {
               list = new InstanceArrayList((Collection)value);
            }
            else if (value instanceof Pair)
            {
               Pair pair = (Pair)value;

               list = new InstanceArrayList(Pair.length(pair));

               do
               {
                  list.add(type.convert(pair.getHead()));
                  pair = pair.getNext();
               }
               while (pair != null);

               return list;
            }
            else if (value instanceof Object[])
            {
               list = new InstanceArrayList(Arrays.asList((Object[])value));
            }
            else
            {
               throw new TypeConversionException(type);
            }

            for (int i = 0, n = list.getCount(); i < n; ++i)
            {
               type.convert(list.get(i));
            }

            return list;
         }
      }

      return type.convert(value);
   }

   /**
    * Converts a calculated value and assigns reverse association attributes.
    * @param attribute The instance attribute.
    * @param value The value to set.
    * @return The converted value.
    */
   protected Object setCalcValue(Attribute attribute, Object value)
   {
      value = convert(attribute, value);

      if (m_context.getGeneration() == InvocationContext.GEN_NEW || m_nState >= NEW)
      {
         if (attribute.isCollection())
         {
            setValueDirect(attribute.getOrdinal(), value);

            if (!attribute.getType().isPrimitive())
            {
               if (value != null)
               {
                  InstanceList list = (InstanceList)value;

                  if (list.getContainer() == null)
                  {
                     list.setLazy(false);
                     list.setAssociation(this, attribute, true);
                  }

                  setAliases(list);
               }
            }
         }
         else
         {
            Attribute reverse = attribute.getReverse();

            if (reverse != null)
            {
               int nRevOrdinal = reverse.getOrdinal();

               Instance instance = (Instance)value;

               setValueDirect(attribute.getOrdinal(), instance);

               if (instance != null)
               {
                  instance.associate(nRevOrdinal, this, true);
               }
            }
            else
            {
               setValueDirect(attribute.getOrdinal(), value);
            }
         }
      }

      return value;
   }

   /**
    * Sets a default value for an uninitialized attribute.
    * @param nOrdinal The attribute ordinal number.
    * @param value The value to set. 
    */
   protected void setDefaultValue(int nOrdinal, Object value)
   {
      assert !isLazy();

      byte nGen = m_context.getGeneration();

      switch (nGen)
      {
         case InvocationContext.GEN_NEW:
            m_valueArray[nOrdinal] = value;
            // do GEN_PRE as well
         case InvocationContext.GEN_PRE:
            if (m_preValueArray != null)
            {
               if (m_preValueArray[nOrdinal] instanceof Undefined)
               {
                  m_preValueArray[nOrdinal] = value;
               }

               if (nGen == InvocationContext.GEN_PRE && m_valueArray[nOrdinal] instanceof Undefined)
               {
                  m_valueArray[nOrdinal] = value;
               }
            }
            else if (nGen == InvocationContext.GEN_PRE)
            {
               m_valueArray[nOrdinal] = value;
            }
            // do GEN_OLD as well
         case InvocationContext.GEN_OLD:
            if (m_oldValueArray != null)
            {
               if (m_oldValueArray[nOrdinal] instanceof Undefined)
               {
                  m_oldValueArray[nOrdinal] = value;
               }

               if (nGen != InvocationContext.GEN_NEW)
               {
                  if (m_valueArray[nOrdinal] instanceof Undefined)
                  {
                     m_valueArray[nOrdinal] = value;
                  }

                  if (nGen == InvocationContext.GEN_OLD && m_preValueArray[nOrdinal] instanceof Undefined)
                  {
                     m_preValueArray[nOrdinal] = value;
                  }
               }
            }
            else if (nGen == InvocationContext.GEN_OLD)
            {
               m_valueArray[nOrdinal] = value;
            }

            break;

         default:
            throw new IllegalStateException("Invalid generation");
      }
   }

   /**
    * @see nexj.core.meta.Accessor#setValueDirect(int, java.lang.Object)
    */
   public void setValueDirect(int nOrdinal, Object value)
   {
      assert !isLazy();

      switch (m_context.getGeneration())
      {
         case InvocationContext.GEN_NEW:
            m_valueArray[nOrdinal] = value;
            break;

         case InvocationContext.GEN_PRE:
            setPreValueDirect(nOrdinal, value);
            break;

         case InvocationContext.GEN_OLD:
            setOldValueDirect(nOrdinal, value);
            break;

         default:
            throw new IllegalStateException("Invalid generation");
      }
   }

   /**
    * @see nexj.core.meta.Accessor#getValueDirect(int)
    */
   public Object getValueDirect(int nOrdinal)
   {
      assert !isLazy();

      switch (m_context.getGeneration())
      {
         case InvocationContext.GEN_NEW:
            return m_valueArray[nOrdinal];

         case InvocationContext.GEN_PRE:
            return getPreValueDirect(nOrdinal);

         case InvocationContext.GEN_OLD:
            return getOldValueDirect(nOrdinal);

         default:
            throw new IllegalStateException("Invalid generation");
      }
   }

   /**
    * @see nexj.core.meta.Accessor#invalidate(int, Object)
    */
   public void invalidate(int nOrdinal, Object value)
   {
      load();

      if (m_nState == CLEAN)
      {
         Attribute attribute = m_metaclass.getInstanceAttribute(nOrdinal);

         if (!attribute.isCollection() && value != Undefined.VALUE)
         {
            if (attribute.isPersistent())
            {
               setDirty();
            }
            else
            {
               if (m_uow != null)
               {
                  m_uow.accumulateChange(this);
               }
               else if (m_context.getUnitOfWork() != null)
               {
                  m_context.getUnitOfWork().accumulateChange(this);
               }
               else
               {
                  m_context.accumulateChange(this, Instance.CLEAN);
               }
            }
         }
      }
      else if (isOverridden(nOrdinal))
      {
         return;
      }

      setValueDirect(nOrdinal, value);
   }

   /**
    * Gets an old (before update) value by ordinal number.
    * @param nOrdinal The attribute ordinal number.
    * @return The old value.
    */
   public Object getOldValue(int nOrdinal)
   {
      assert !isLazy();

      byte nGenerationSaved = m_context.getGeneration();

      try
      {
         m_context.setGeneration(InvocationContext.GEN_OLD);

         return getValue(nOrdinal);
      }
      finally
      {
         m_context.setGeneration(nGenerationSaved);
      }
   }

   /**
    * Gets an object attribute value by name.
    * @param sName The attribute name.
    * @return The attribute value.
    */
   public Object getOldValue(String sName)
   {
      load();

      Attribute attribute = m_metaclass.getAttribute(sName);

      if (attribute.isStatic())
      {
         return m_metaclass.getValue(attribute.getOrdinal());
      }

      return getOldValue(attribute.getOrdinal());
   }

   /**
    * Sets an old (before update) value by ordinal number, without side effects.
    * @param nOrdinal The attribute ordinal number.
    * @param value The value to set.
    */
   public void setOldValueDirect(int nOrdinal, Object value)
   {
      assert !isLazy();

      if (m_oldValueArray != null)
      {
         if (m_valueArray[nOrdinal] == Undefined.VALUE)
         {
            m_valueArray[nOrdinal] = value;
         }

         if (m_preValueArray[nOrdinal] == Undefined.VALUE)
         {
            m_preValueArray[nOrdinal] = value;
         }

         m_oldValueArray[nOrdinal] = value;
      }
      else
      {
         m_valueArray[nOrdinal] = value;
      }
   }

   /**
    * Gets an old (before update) value by ordinal number, without side effects.
    * @param nOrdinal The attribute ordinal number.
    * @return The old value.
    */
   public Object getOldValueDirect(int nOrdinal)
   {
      assert !isLazy();

      Object value = (m_oldValueArray != null) ? m_oldValueArray[nOrdinal] : m_valueArray[nOrdinal];

      if (value instanceof InstanceList)
      {
         InstanceList list = (InstanceList)value;

         if (list.isDirty())
         {
            value = m_context.getUnitOfWork().getOldCollection(list);

            if (m_oldValueArray != null)
            {
               m_oldValueArray[nOrdinal] = value;
            }
         }
      }

      return value;
   }

   /**
    * Sets a previous (since the last life cycle event) value by ordinal number.
    * @param nOrdinal The attribute ordinal number.
    * @return The previous value.
    */
   public Object getPreValue(int nOrdinal)
   {
      assert !isLazy();

      byte nGenerationSaved = m_context.getGeneration();

      try
      {
         m_context.setGeneration(InvocationContext.GEN_PRE);

         return getValue(nOrdinal);
      }
      finally
      {
         m_context.setGeneration(nGenerationSaved);
      }
   }

   /**
    * Sets a previous (since the last life cycle event) value by name.
    * @param sName The attribute name.
    * @return The previous value.
    */
   public Object getPreValue(String sName)
   {
      load();

      Attribute attribute = m_metaclass.getAttribute(sName);

      if (attribute.isStatic())
      {
         return m_metaclass.getValue(attribute.getOrdinal());
      }

      return getPreValue(attribute.getOrdinal());
   }

   /**
    * Sets a previous (since the last life cycle event) value by ordinal number,
    * without side effects.
    * @param nOrdinal The attribute ordinal number.
    * @param value The value to set.
    */
   public void setPreValueDirect(int nOrdinal, Object value)
   {
      assert !isLazy();

      if (m_preValueArray != null)
      {
         m_preValueArray[nOrdinal] = value;
      }
      else
      {
         m_valueArray[nOrdinal] = value;
      }
   }

   /**
    * Gets a previous (since the last life cycle event) value by ordinal number,
    * without side effects.
    * @param nOrdinal The attribute ordinal number.
    * @return The previous value.
    */
   public Object getPreValueDirect(int nOrdinal)
   {
      assert !isLazy();

      Object value = (m_preValueArray != null) ? m_preValueArray[nOrdinal] : m_valueArray[nOrdinal];

      if (value instanceof InstanceList)
      {
         InstanceList list = (InstanceList)value;

         if (list.isDirty())
         {
            value = m_context.getUnitOfWork().getPreCollection(list);

            if (m_preValueArray != null)
            {
               m_preValueArray[nOrdinal] = value;
            }
         }
      }

      return value;
   }

   /**
    * Sets a new (after update) value by ordinal number, without side effects.
    * @param nOrdinal The attribute ordinal number.
    * @param value The value to set.
    */
   public void setNewValueDirect(int nOrdinal, Object value)
   {
      assert !isLazy();

      m_valueArray[nOrdinal] = value;
   }

   /**
    * Gets a new (after update) value by ordinal number, without side effects.
    * @param nOrdinal The attribute ordinal number.
    * @return The new value.
    */
   public Object getNewValueDirect(int nOrdinal)
   {
      assert !isLazy();

      return m_valueArray[nOrdinal];
   }

   /**
    * Initializes the annotation map optimized for a given number annotations.
    * @param nCount The estimated annotation count.
    */
   public void initAnnotations(int nCount)
   {
      if (m_annotationMap == null)
      {
         m_annotationMap = new PropertyHashTab(nCount);
      }
   }

   /**
    * Sets an annotation.
    * @param sName The annotation name.
    * @param value The annotation value.
    */
   public void setAnnotation(String sName, Object value)
   {
      if (m_annotationMap == null)
      {
         m_annotationMap = new PropertyHashTab();
      }

      m_annotationMap.put(sName, value);
   }

   /**
    * Finds an annotation by name.
    * @param sName The annotation name.
    * @return The annotation, or null if not found.
    */
   public Object findAnnotation(String sName)
   {
      return findAnnotation(sName, null);
   }

   /**
    * Finds an annotation by name.
    * @param sName The annotation name.
    * @return The annotation, or null if not found.
    */
   public Object findAnnotation(String sName, Object defaultValue)
   {
      if (m_annotationMap == null)
      {
         return defaultValue;
      }

      return m_annotationMap.findValue(sName, defaultValue);
   }

   /**
    * @return The annotation iterator.
    */
   public PropertyIterator getAnnotationIterator()
   {
      if (m_annotationMap == null)
      {
         return PropertyHashTab.EMPTY_ITERATOR;
      }

      return m_annotationMap.getIterator();
   }

   /**
    * @return The annotation count.
    */
   public int getAnnotationCount()
   {
      if (m_annotationMap == null)
      {
         return 0;
      }

      return m_annotationMap.size();
   }

   /**
    * @see nexj.core.meta.Accessor#invoke(java.lang.String, java.lang.Object[])
    */
   public Object invoke(String sName, Object[] args)
   {
      load();

      Event event = (Event)m_metaclass.getSelector(sName).getMember((args != null) ? args.length : 0);

      return event.invoke((event.isStatic()) ? (Accessor)m_metaclass : this, args, m_context.getMachine());
   }

   /**
    * @see nexj.core.meta.Accessor#invoke(java.lang.String, nexj.core.scripting.Pair)
    */
   public Object invoke(String sName, Pair args)
   {
      load();

      Event event = (Event)m_metaclass.getSelector(sName).getMember(Pair.length(args));

      return event.invoke((event.isStatic()) ? (Accessor)m_metaclass : this, args, m_context.getMachine());
   }

   /**
    * @see nexj.core.meta.Accessor#invoke(java.lang.String)
    */
   public Object invoke(String sName)
   {
      return invoke(sName, (Object[])null);
   }

   /**
    * @see nexj.core.util.PropertyMap#findValue(java.lang.String)
    */
   public Object findValue(String sName)
   {
      return findValue(sName, null);
   }

   /**
    * @see nexj.core.util.PropertyMap#findValue(java.lang.String, java.lang.Object)
    */
   public Object findValue(String sName, Object defaultValue)
   {
      if (isLazy())
      {
         return findAnnotation(sName, defaultValue);
      }

      Attribute attribute = m_metaclass.findAttribute(sName);

      if (attribute == null)
      {
         return findAnnotation(sName, defaultValue);
      }

      Object value;

      if (attribute.isStatic())
      {
         value = m_metaclass.getValueDirect(attribute.getOrdinal());
      }
      else
      {
         value = getValueDirect(attribute.getOrdinal());
      }

      if (value instanceof Undefined)
      {
         value = defaultValue;
      }

      return value;
   }

   /**
    * @see nexj.core.util.PropertyMap#hasValue(java.lang.String)
    */
   public boolean hasValue(String sName)
   {
      if (isLazy())
      {
         return false;
      }

      Attribute attribute = m_metaclass.findAttribute(sName);

      if (attribute == null)
      {
         return false;
      }

      if (attribute.isStatic())
      {
         return !(m_metaclass.getValueDirect(attribute.getOrdinal()) instanceof Undefined);
      }

      return !(getValueDirect(attribute.getOrdinal()) instanceof Undefined);
   }

   /**
    * @see nexj.core.util.PropertyMap#getValueCount()
    */
   public int getValueCount()
   {
      if (isLazy())
      {
         return 0;
      }

      int nCount = 0;

      for (int i = 0, n = m_valueArray.length; i < n; ++i)
      {
         if (!(getValueDirect(i) instanceof Undefined))
         {
            ++nCount;
         }
      }

      return nCount;
   }

   /**
    * @see nexj.core.util.PropertyMap#getIterator()
    */
   public PropertyIterator getIterator()
   {
      return new PropertyIterator()
      {
         private int m_nNext = -1;
         private int m_nCur;

         {
            incr();
         }

         private void incr()
         {
            m_nCur = m_nNext++;

            if (m_valueArray != null)
            {
               while (m_nNext < m_valueArray.length)
               {
                  if (!(getValueDirect(m_nNext) instanceof Undefined))
                  {
                     break;
                  }

                  ++m_nNext;
               }
            }
         }

         public boolean hasNext()
         {
            return m_valueArray != null && m_nNext < m_valueArray.length;
         }

         public Object next()
         {
            if (m_valueArray != null && m_nNext < m_valueArray.length)
            {
               incr();

               return m_metaclass.getInstanceAttribute(m_nCur).getName();
            }

            throw new java.util.NoSuchElementException();
         }

         public void remove()
         {
            Instance.this.setValueDirect(m_nCur, Undefined.VALUE);
         }

         public String getName()
         {
            return m_metaclass.getInstanceAttribute(m_nCur).getName();
         }

         public Object getValue()
         {
            return Instance.this.getValue(m_nCur);
         }

         public void setValue(Object value)
         {
            Instance.this.setValue(m_nCur, value);
         }
      };
   }

   /**
    * @see nexj.core.util.PropertyMap#getClassName()
    */
   public String getClassName()
   {
      load();

      return m_metaclass.getName();
   }

   /**
    * @see nexj.core.scripting.Function#invoke(int, nexj.core.scripting.Machine)
    */
   public boolean invoke(int nArgCount, Machine machine)
   {
      if (nArgCount != 0)
      {
         Object sym = machine.getArg(0, nArgCount);

         if (sym instanceof Symbol)
         {
            if (isLazy() && !Symbol._OID.equals(sym))
            {
               load();
            }

            Selector sel = m_metaclass.findSelector(sym.toString());
            Member member = (sel == null) ? null : sel.findMember(nArgCount - 1);

            if (member == null)
            {
               Object value = Undefined.VALUE;

               if (nArgCount == 1)
               {
                  value = findAnnotation(sym.toString(), Undefined.VALUE);
               }

               if (value == Undefined.VALUE)
               {
                  // This should throw an exception
                  m_metaclass.getSelector(sym.toString()).getMember(nArgCount - 1);
               }

               machine.returnValue(value, nArgCount);

               return false;
            }

            if (member.isAttribute())
            {
               if (nArgCount == 1)
               {
                  machine.returnValue(
                     (member.isStatic()) ?
                        m_metaclass.getValue(((Attribute)member).getOrdinal()) :
                        getValue(((Attribute)member).getOrdinal()),
                     nArgCount);
               }
               else if (nArgCount == 2)
               {
                  Object value = machine.getArg(1, nArgCount);

                  if (member.isStatic())
                  {
                     m_metaclass.setValue(((Attribute)member).getOrdinal(), value);
                  }
                  else
                  {
                     setValue(((Attribute)member).getOrdinal(), value);
                  }

                  machine.returnValue(value, nArgCount);
               }
               else
               {
                  throw new ScriptingException("err.scripting.maxArgCount",
                     new Object[]{toString(),
                        Primitive.ONE_INTEGER,
                        Primitive.createInteger(nArgCount - 1)});
               }

               return false;
            }
            else
            {
               if (member.isStatic())
               {
                  machine.setArg(0, nArgCount, m_metaclass);
               }
               else
               {
                  machine.setArg(0, nArgCount, this);
               }

               if (m_metaclass.getLogger().isDebugEnabled())
               {
                  ((Event)member).dump(nArgCount, machine);
               }

               return ((Event)member).getFunction().invoke(nArgCount, machine);
            }
         }
      }
      else
      {
         throw new ScriptingException("err.scripting.minArgCount",
            new Object[]{m_metaclass.getName(),
               Primitive.ONE_INTEGER,
               Primitive.createInteger(nArgCount)});
      }

      throw new ScriptingException("err.scripting.funCall");
   }

   /**
    * Marks a calculated value as explicitly overridden.
    * @param nOrdinal The attribute ordinal number.
    */
   private void setOverridden(int nOrdinal)
   {
      if (m_overrideArray == null)
      {
         m_overrideArray = new byte[(m_metaclass.getInstanceAttributeCount() + 7) >> 3];
      }

      m_overrideArray[nOrdinal >> 3] |= 1 << (nOrdinal & 7);
   }

   /**
    * Checks whether a calculated value has been explicitly overridden.
    * @param nOrdinal The attribute ordinal number.
    */
   public boolean isOverridden(int nOrdinal)
   {
      return m_overrideArray != null && (m_overrideArray[nOrdinal >> 3] & 1 << (nOrdinal & 7)) != 0;
   }

   /**
    * Checks whether a value has been updated but not yet committed.
    * @param nOrdinal The attribute ordinal number.
    * @return True if the value has been updated.
    */
   public boolean isDirty(int nOrdinal)
   {
      assert !isLazy();

      if (m_nState == DELETED)
      {
         return false;
      }

      if (m_oldValueArray == null)
      {
         return m_nState == NEW;
      }

      Object value = m_valueArray[nOrdinal];

      if (value instanceof Undefined)
      {
         return value != Undefined.VALUE;
      }

      if (value instanceof InstanceList &&
         ((InstanceList)value).isDirty())
      {
         return true;
      }

      if (m_metaclass.getInstanceAttribute(nOrdinal).isCalculated() &&
         !isOverridden(nOrdinal))
      {
         return false;
      }

      return !ObjUtil.equal(value, m_oldValueArray[nOrdinal]);
   }

   /**
    * Checks whether a value has been updated but not yet committed.
    * @param sName The attribute name.
    * @return True if the value has been updated.
    */
   public boolean isDirty(String sName)
   {
      if (isLazy())
      {
         return false;
      }

      Attribute attribute = m_metaclass.getAttribute(sName);

      if (attribute.isStatic())
      {
         return false;
      }

      return isDirty(attribute.getOrdinal());
   }

   /**
    * Checks whether a value has been updated since last life cycle event invocation.
    * @param nOrdinal The attribute ordinal number.
    * @return True if the value has been updated.
    */
   public boolean isUpdated(int nOrdinal)
   {
      assert !isLazy();

      Object value = m_valueArray[nOrdinal];

      if (value instanceof Undefined)
      {
         return value != Undefined.VALUE;
      }

      if (value instanceof InstanceList &&
         ((InstanceList)value).isUpdated())
      {
         return true;
      }

      if (m_preValueArray == null)
      {
         return false;
      }

      if (m_metaclass.getInstanceAttribute(nOrdinal).isCalculated() &&
         !isOverridden(nOrdinal))
      {
         return false;
      }

      return !ObjUtil.equal(value, m_preValueArray[nOrdinal]);
   }

   /**
    * Checks whether a value has been updated since last life cycle event invocation.
    * @param sName The attribute name.
    * @return True if the value has been updated.
    */
   public boolean isUpdated(String sName)
   {
      if (isLazy())
      {
         return false;
      }

      Attribute attribute = m_metaclass.getAttribute(sName);

      if (attribute.isStatic())
      {
         return false;
      }

      return isUpdated(attribute.getOrdinal());
   }

   /**
    * Verifies that the instance contains valid data.
    * @param bFull True to perform full validation, false to perform only static validation.
    * @throws ValidationException if inconsistencies are found.
    */
   public void validate(boolean bFull) throws ValidationException
   {
      assert !isLazy();

      if (m_nState == NEW || m_nState == DIRTY)
      {
         PersistenceMapping persistenceMapping = getPersistenceMapping();
         PersistenceAdapter adapter = (persistenceMapping == null) ? null :
            (PersistenceAdapter)persistenceMapping.getDataSource()
               .getComponent().getInstance(m_context);
         ValidationException e = null;

         for (int i = 0, nCount = m_metaclass.getInstanceAttributeCount(); i != nCount; ++i)
         {
            Attribute attribute = m_metaclass.getInstanceAttribute(i);
            Object value = getValueDirect(i);

            if (value instanceof Undefined)
            {
               if ((m_nState == NEW || value == Invalid.VALUE) &&
                  (attribute.isPersistent() ||
                     (attribute.isRequired() ||
                        attribute.getValidationFunction() != null ||
                        m_nState == NEW && attribute.getInitializer() != Undefined.VALUE ||
                        attribute.isConstrained() && attribute.getEnumeration() != attribute.getType()) && bFull))
               {
                  value = getValue(i);
               }
               else
               {
                  continue;
               }
            }

            if (m_nState == DIRTY && !isDirty(i))
            {
               continue;
            }

            Type type = attribute.getType();

            if (attribute.isRequired())
            {
               boolean bMissing = (value == null);

               if (!bMissing)
               {
                  if (type.isPrimitive())
                  {
                     bMissing = (type == Primitive.STRING && value.equals(""));
                  }
                  else
                  {
                     bMissing = (attribute.isCollection() && ((InstanceList)value).isEmpty());
                  }
               }

               if (bMissing)
               {
                  e = createValidationException(e);

                  ValidationException x = new ValidationException("err.validation.requiredAttribute",
                     new Object[]{new StringId(attribute.getCaption()), new StringId(m_metaclass.getCaption())});

                  x.setClassName(m_metaclass.getName());
                  x.setOIDHolder(this);
                  e.addException(attribute.getName(), x);
               }
            }

            if (attribute.getMaxLength() > 0)
            {
               int nLength = 0;

               if (value instanceof String)
               {
                  nLength = ((String)value).length();
               }
               else if (value instanceof Binary)
               {
                  nLength = ((Binary)value).getData().length;
               }

               if (nLength > attribute.getMaxLength())
               {
                  e = createValidationException(e);

                  ValidationException x = new ValidationException("err.validation.maxDataLength",
                     new Object[]{Primitive.createInteger(attribute.getMaxLength()),
                        new StringId(attribute.getCaption()), new StringId(m_metaclass.getCaption())});

                  x.setClassName(m_metaclass.getName());
                  x.setOIDHolder(this);
                  e.addException(attribute.getName(), x);
               }
            }

            if (bFull)
            {
               if (attribute.isConstrained() &&
                  attribute.getEnumeration() != type &&
                  value != null && e == null)
               {
                  if (((InstanceList)attribute.getEnumeration().invoke("read",
                     new Object[]{null, (type.isPrimitive()) ?
                        Pair.attribute(Metaclass.ENUMERATION_VALUE).eq(value) :
                        new Pair(Symbol.AT).eq(value), null, Primitive.createInteger(-1),
                        Primitive.ZERO_INTEGER, Boolean.FALSE})).isEmpty())
                  {
                     e = createValidationException(e);

                     ValidationException x = new ValidationException("err.validation.enumerationValue",
                        new Object[]{new StringId(attribute.getEnumeration().getCaption()),
                           new StringId(attribute.getCaption()), new StringId(m_metaclass.getCaption())});

                     x.setClassName(m_metaclass.getName());
                     x.setOIDHolder(this);
                     e.addException(attribute.getName(), x);
                  }
               }

               if (attribute.getValidationFunction() != null && e == null)
               {
                  Object result = m_context.getMachine().invoke(attribute.getValidationFunction(), this, value, null);

                  if (result != null)
                  {
                     ValidationException x = null;

                     if (result instanceof Boolean)
                     {
                        if (!((Boolean)result).booleanValue())
                        {
                           x = new ValidationException("err.validation.valueRange",
                              new Object[]{new StringId(attribute.getCaption()), new StringId(m_metaclass.getCaption())});
                        }
                     }
                     else if (result instanceof String)
                     {
                        x = new ValidationException((String)result);
                     }
                     else if (result instanceof Pair)
                     {
                        x = new ValidationException((String)((Pair)result).getHead(),
                              Pair.toArray(((Pair)result).getNext()));
                     }

                     if (x != null)
                     {
                        x.setClassName(m_metaclass.getName());
                        x.setOIDHolder(this);
                        e = createValidationException(e);
                        e.addException(attribute.getName(), x);
                     }
                  }
               }
            }

            if (type.isPrimitive() && attribute.isPersistent() && adapter != null)
            {
               try
               {
                  adapter.validate(persistenceMapping.getAttributeMapping(attribute), value);
               }
               catch (ValidationException x)
               {
                  x.setClassName(m_metaclass.getName());
                  x.setOIDHolder(this);
                  e = createValidationException(e);
                  e.addException(attribute.getName(), x);
               }
            }
         }

         if (bFull && m_metaclass.getValidationFunction() != null && e == null)
         {
            Object result = m_context.getMachine().invoke(m_metaclass.getValidationFunction(), new Pair(this));

            if (result != null)
            {
               if (result instanceof Boolean)
               {
                  if (!((Boolean)result).booleanValue())
                  {
                     e = createValidationException(null);
                  }
               }
               else if (result instanceof String)
               {
                  e = new ValidationException((String)result);
               }
               else if (result instanceof Pair)
               {
                  e = new ValidationException((String)((Pair)result).getHead(),
                        Pair.toArray(((Pair)result).getNext()));
               }

               if (e != null)
               {
                  e.setClassName(m_metaclass.getName());
                  e.setOIDHolder(this);
               }
            }
         }

         if (e != null)
         {
            throw e;
         }
      }
   }

   /**
    * Helper method to create an invalid instance validation exception.
    * @param e The validation exception, can be null to create one.
    * @return The validation exception, or e if it was not null.
    */
   private ValidationException createValidationException(ValidationException e)
   {
      if (e == null)
      {
         e = new ValidationException("err.validation.invalidInstance",
            new Object[]{new StringId(m_metaclass.getCaption())});
         e.setClassName(m_metaclass.getName());
         e.setOIDHolder(this);
      }

      return e;
   }

   /**
    * Lazy-loads the initial instance state.
    */
   public void load()
   {
      assert m_oid == null ||
         m_nState != CLEAN && m_nState != DIRTY ||
         getPersistenceMapping() == null ||
         m_context.findInstance(m_metaclass, m_oid) == this :
         "Obsolete " + this;

      if (isLazy())
      {
         load((Pair)null, false, false);
      }
   }

   /**
    * Loads a lazy instance without throwing an optimistic locking
    * exception if it does not exist in the persistent store
    * (it can still throw the exception otherwise).
    * @return True if the instance has been loaded.
    * @see #load()
    */
   public boolean tryLoad()
   {
      assert m_oid == null ||
         m_nState != CLEAN && m_nState != DIRTY ||
         getPersistenceMapping() == null ||
         m_context.findInstance(m_metaclass, m_oid) == this :
         "Obsolete " + this;

      if (isLazy())
      {
         load((Pair)null, false, true);

         return !isLazy();
      }

      return true;
   }

   /**
    * Determines if an attribute value is loaded.
    * @param value The attribute value.
    * @param attribute The attribute object.
    * @param attributes The attributes to test, in read association format.
    * @return True if the attribute value has been loaded.
    */
   private boolean isLoaded(Object value, Attribute attribute, Pair attributes)
   {
      if (value instanceof Instance)
      {
         return ((Instance)value).isLoaded(attributes);
      }

      if (value instanceof InstanceList)
      {
         InstanceList list = (InstanceList)value;

         if (list.isLazy() || list.isWeak())
         {
            return false;
         }

         for (int i = 0, n = list.getCount(); i < n; ++i)
         {
            Instance instance = list.getInstance(i);

            if (instance == null || !instance.isLoaded(attributes))
            {
               return false;
            }
         }
      }

      return true;
   }

   /**
    * Determines if the persistent attributes on an instance are loaded.
    * @param attributes The attributes to test, in read association format.
    * @return True if all the attributes are loaded.
    */
   private boolean isLoaded(Pair attributes)
   {
      if (isLazy())
      {
         return (attributes == null);
      }

      for (Pair pair = attributes; pair != null; pair = pair.getNext())
      {
         Object head = pair.getHead();
         Pair assoc;
         Attribute attribute;

         if (head instanceof Pair)
         {
            assoc = (Pair)head;

            Symbol sym = (Symbol)assoc.getHead();

            assoc = assoc.getNext();

            if (Symbol.ATAT.equals(sym))
            {
               if (m_metaclass.getMetadata().getMetaclass(((Symbol)assoc.getHead()).getName())
                     .isUpcast(m_metaclass) && !isLoaded(assoc.getNext()))
               {
                  return false;
               }

               continue;
            }

            attribute = m_metaclass.getAttribute(sym);
         }
         else
         {
            if (head == null)
            {
               continue;
            }

            assoc = null;
            attribute = m_metaclass.getAttribute((Symbol)head);
         }

         if (!attribute.isStatic())
         {
            Object value = getValueDirect(attribute.getOrdinal());

            if (!(value instanceof Undefined))
            {
               if (!isLoaded(value, attribute, assoc))
               {
                  return false;
               }
            }
            else
            {
               if (value == Undefined.VALUE &&
                  (attribute.isPersistent() || !isPersistentDependencyLoaded(attribute)))
               {
                  return false;
               }
            }
         }
      }

      return true;
   }

   /**
    * Determines if the persistent dependency attributes have been loaded.
    * @param dependentAttribute The dependent attribute.
    * @see Attribute#getDependency()
    */
   private boolean isPersistentDependencyLoaded(Attribute dependentAttribute)
   {
      for (Pair dep = dependentAttribute.getDependency(); dep != null; dep = dep.getNext())
      {
         Object head = dep.getHead();
         Attribute attribute = (Attribute)((head instanceof Pair) ? ((Pair)head).getHead() : head);

         if (attribute.isCollection() && attribute.isPersistent() &&
            getValueDirect(attribute.getOrdinal()) == Undefined.VALUE)
         {
            return false;
         }
      }

      return true;
   }

   /**
    * Preprocesses and eliminates the load attributes and determines if a read operation is needed (m_bRead flag).
    * @param attributes The attributes to load, in read association format. This list is modified by the method.
    * @param tail The pair to which to append the attributes.
    * @param bPersistent True to compute the persistent read flag (m_bRead).
    * @return The first pair of the preprocessed attributes, or the new tail if tail is not null.
    */
   private Pair preload(Pair attributes, Pair tail, boolean bPersistent)
   {
      for (Pair pair = attributes, prev = null; pair != null; pair = pair.getNext())
      {
         Object head = pair.getHead();
         Pair assoc;
         Attribute attribute;

         if (head instanceof Pair)
         {
            assoc = (Pair)head;

            Symbol sym = (Symbol)assoc.getHead();

            assoc = assoc.getNext();

            if (Symbol.ATAT.equals(sym))
            {
               if (m_metaclass.getMetadata().getMetaclass(((Symbol)assoc.getHead()).getName()).isUpcast(m_metaclass))
               {
                  if (tail == null)
                  {
                     pair.setHead(null);
                     pair = preload(assoc.getNext(), pair, bPersistent);

                     if (pair.getHead() == null && tail == null)
                     {
                        if (prev == null)
                        {
                           attributes = pair.getNext();
                        }
                        else
                        {
                           prev.setTail(pair.getTail());
                        }
                     }
                  }
                  else
                  {
                     tail = preload(assoc.getNext(), tail, bPersistent);
                  }
               }
               else
               {
                  if (tail == null)
                  {
                     if (prev == null)
                     {
                        attributes = pair.getNext();
                     }
                     else
                     {
                        prev.setTail(pair.getTail());
                     }
                  }
               }

               continue;
            }

            if (!bPersistent)
            {
               continue;
            }

            attribute = m_metaclass.getAttribute(sym);
         }
         else
         {
            if (head == null)
            {
               if (tail == null)
               {
                  if (prev == null)
                  {
                     attributes = pair.getNext();
                  }
                  else
                  {
                     prev.setTail(pair.getTail());
                  }
               }

               continue;
            }

            if (!bPersistent)
            {
               continue;
            }

            assoc = null;
            attribute = m_metaclass.getAttribute((Symbol)head);
         }

         if (attribute.isStatic())
         {
            if (tail == null)
            {
               if (prev == null)
               {
                  attributes = pair.getNext();
               }
               else
               {
                  prev.setTail(pair.getTail());
               }
            }
         }
         else
         {
            Object value = getValueDirect(attribute.getOrdinal());

            if (!(value instanceof Undefined))
            {
               if (isLoaded(value, attribute, assoc))
               {
                  if (tail == null)
                  {
                     if (prev == null)
                     {
                        attributes = pair.getNext();
                     }
                     else
                     {
                        prev.setTail(pair.getTail());
                     }
                  }
               }
               else
               {
                  if (tail == null)
                  {
                     prev = pair;
                  }
                  else if (tail.getHead() == null)
                  {
                     tail.setHead(pair.getHead());
                  }
                  else
                  {
                     Pair copy = new Pair(pair.getHead(), tail.getTail());

                     tail.setTail(copy);
                     tail = copy;
                  }

                  if (attribute.isPersistent() || !isPersistentDependencyLoaded(attribute))
                  {
                     m_nFlags |= LOAD_READ;
                  }
               }
            }
            else
            {
               if (tail == null)
               {
                  prev = pair;
               }
               else if (tail.getHead() == null)
               {
                  tail.setHead(pair.getHead());
               }
               else
               {
                  Pair copy = new Pair(pair.getHead(), tail.getTail());

                  tail.setTail(copy);
                  tail = copy;
               }

               if ((m_nFlags & LOAD_READ) == 0 && value == Undefined.VALUE &&
                  (attribute.isPersistent() || !isPersistentDependencyLoaded(attribute)))
               {
                  m_nFlags |= LOAD_READ;
               }
            }
         }
      }

      if (tail != null)
      {
         return tail;
      }

      return attributes;
   }

   /**
    * Loads the specified attributes from a persistent store.
    * NOTE: This is for internal use only. Use invoke("load", ...) for general purpose coding.
    * @param attributes The attributes to load, in read association format. This list is *modified* by the method.
    * @param bSafe True to suppress exceptions is the instance does not exist.
    * @return The modified attribute list.
    */
   public Pair load(Pair attributes)
   {
      return load(attributes, false, false);
   }

   /**
    * Loads the specified attributes from a persistent store.
    * NOTE: This is for internal use only. Use invoke("load", ...) for general purpose coding.
    * @param attributes The attributes to load, in read association format. This list is *modified* by the method.
    * @param bRead True to skip the read optimization for already loaded attributes.
    * @param bSafe True to suppress exceptions if the instance does not exist.
    * @return The modified attribute list.
    */
   public Pair load(Pair attributes, boolean bRead, boolean bSafe)
   {
      PersistenceMapping mapping = getPersistenceMapping();

      // Optimization: Avoid load when invoking event on lazy instance whose metaclass is not polymorphic
      if (isLazy())
      {
         if (mapping != null && !mapping.isTypeCodeDispatched() && mapping.getLockingAttribute() == null)
         {
            setMetaclass(m_metaclass);
         }
         else
         {
            bRead = true;
         }
      }

      if (m_nState != NEW && m_nState != DELETED && mapping != null)
      {
         boolean bLoad = bRead;

         if (!bRead)
         {
            m_nFlags &= ~LOAD_READ;
            attributes = preload(attributes, null, true);
            bRead = (m_nFlags & LOAD_READ) != 0;
         }

         if (bRead)
         {
            Query query = Query.createRead(m_metaclass, attributes,
               new Pair(Symbol.AT).eq(m_oid), null, -1, 0, false,
               Query.SEC_NONE, m_context);

            for (int i = 0, nCount = m_metaclass.getInstanceAttributeCount(); i < nCount; ++i)
            {
               Attribute attribute = m_metaclass.getInstanceAttribute(i);

               if ((isLazy() || getValueDirect(attribute.getOrdinal()) == Undefined.VALUE) &&
                  !attribute.isLazy())
               {
                  query.addAttribute(Query.ASSOC_QUERY, attribute, null, false, Query.OUTPUT_EAGER);
               }
            }

            if (query.getFirstOutputField() != null &&
               (query.getFirstOutputField().getNext() != null ||
                  query.getFirstOutputField().getAttribute() !=
                  mapping.getLockingAttribute()) ||
               query.getAssocCount(Query.ASSOC_QUERY) > 0)
            {
               if (s_logger.isDebugEnabled())
               {
                  String sMsg = "Loading attributes " + attributes + " of " + this;

                  if (s_logger.isDumpEnabled())
                  {
                     StackTrace t = new StackTrace();

                     m_context.getMachine().updateStackTrace(t);
                     s_logger.dump(sMsg, t);
                  }
                  else
                  {
                     s_logger.debug(sMsg);
                  }
               }

               int nRPC = m_context.getRPCCount();

               try
               {
                  if (query.read().isEmpty())
                  {
                     if (bSafe)
                     {
                        return attributes;
                     }
   
                     // The instance could not be found
                     if (isLazy())
                     {
                        // Just the OID was available - check the referrer
                        for (Iterator itr = m_context.getInstanceRefIterator(); itr.hasNext();)
                        {
                           InstanceRef ref = (InstanceRef)itr.next();
   
                           if (ref != null)
                           {
                              Instance instance = ref.getInstance();
   
                              if (instance != null && !instance.isLazy() &&
                                 instance.getState() != NEW && instance.getOID() != null)
                              {
                                 Metaclass metaclass = instance.getMetaclass();
   
                                 if (metaclass.getPersistenceMapping() != null)
                                 {
                                    for (int i = 0, n = metaclass.getInstanceAttributeCount(); i < n; ++i)
                                    {
                                       Attribute attribute = metaclass.getInstanceAttribute(i);
   
                                       if (!attribute.isCollection() && attribute.isPersistent() &&
                                          instance.getOldValueDirect(i) == this)
                                       {
                                          if (!Query.createRead(metaclass, new Pair(attribute.getSymbol()),
                                             new Pair(Symbol.AT).eq(instance), null, -1, 0, false, Query.SEC_NONE,
                                             m_context).read().isEmpty() &&
                                             instance.getOldValueDirect(i) == this)
                                          {
                                             throw new AssociationIntegrityException(instance, attribute.getName());
                                          }
   
                                          throw new OptimisticLockException(this);
                                       }
                                    }
                                 }
                              }
                           }
                        }
                     }
                     else if (!Query.createRead(getLazyMetaclass(), null, new Pair(Symbol.AT).eq(m_oid),
                        null, -1, 0, false, Query.SEC_NONE, m_context).read().isEmpty())
                     {
                        throw new AssociationIntegrityException(this);
                     }
   
                     throw new OptimisticLockException(this);
                  }
               }
               finally
               {
                  if (m_context.getRPCCount() != nRPC)
                  {
                     m_context.addLoadCount(1);
                  }
               }
            }

            if (bLoad)
            {
               attributes = preload(attributes, null, false);
            }
         }
         else
         {
            bRead = true;
         }
      }
      else
      {
         attributes = preload(attributes, null, false);
      }

      for (; attributes != null; attributes = attributes.getNext())
      {
         Object head = attributes.getHead();
         Pair assoc = (head instanceof Pair) ? (Pair)head : null;
         Attribute attribute = m_metaclass.getAttribute(
            (Symbol)((assoc != null) ? assoc.getHead() : head));

         if (!attribute.isStatic())
         {
            int nOrdinal = attribute.getOrdinal();
            Object value = getValueDirect(nOrdinal);

            if (value instanceof Undefined)
            {
               switch (m_nState)
               {
               case INIT:
               case NEW:
                  if (attribute.getInitializerFunction() != null)
                  {
                     setCalcValue(attribute,
                        m_context.getMachine().invoke(attribute.getInitializerFunction(), this, (Object[])null));

                     continue;
                  }

                  break;

               case CLEAN:
               case DIRTY:
                  if (attribute.getValueFunction() != null)
                  {
                     if (attribute.isCached())
                     {
                        setCalcValue(attribute,
                           m_context.getMachine().invoke(attribute.getValueFunction(), this, (Object[])null));
                     }

                     continue;
                  }

                  break;
               }

               if (attribute.isCollection())
               {
                  InstanceList list = new InstanceArrayList(0);

                  setDefaultValue(nOrdinal, list);
                  list.setAssociation(this, attribute, true);
                  list.setLazy(false);
               }
               else
               {
                  setDefaultValue(nOrdinal, null);
               }

               if (s_logger.isDebugEnabled())
               {
                  s_logger.debug("Uninitialized " + attribute + ", set to " + getValueDirect(nOrdinal));
               }
            }
            else if (value != null && !bRead)
            {
               if (assoc != null && assoc.getTail() != null)
               {
                  if (value instanceof InstanceList)
                  {
                     InstanceList list = (InstanceList)value;

                     for (int i = 0, n = list.getCount(); i < n; ++i)
                     {
                        Instance instance = list.getInstance(i);

                        if (instance != null && !instance.isLoading())
                        {
                           instance.invoke("load", assoc.getNext());
                        }
                     }
                  }
                  else if (value instanceof Instance)
                  {
                     Instance instance = (Instance)value;

                     if (!instance.isLoading())
                     {
                        instance.invoke("load", assoc.getNext());
                     }
                  }
               }
            }
         }
      }

      return attributes;
   }

   /**
    * Locks the instance.
    */
   public void lock()
   {
      if (m_oid != null)
      {
         load();

         switch (m_nState)
         {
            case CLEAN:
            case DIRTY:
            case DELETED:
               Pair attributes = null;

               if (m_nState == CLEAN)
               {
                  for (int i = m_metaclass.getInstanceAttributeCount() - 1; i >= 0; --i)
                  {
                     Attribute attribute = m_metaclass.getInstanceAttribute(i);

                     if (!(getValueDirect(i) instanceof Undefined) && !attribute.isCollection())
                     {
                        attributes = new Pair(attribute.getSymbol(), attributes);
                     }
                  }
               }

               Query.createRead(m_metaclass, attributes, new Pair(Symbol.AT).eq(m_oid),
                  null, -1, 0, true, Query.SEC_NONE, m_context).read();

               break;
         }
      }
   }

   /**
    * Deletes the instance.
    */
   public void delete()
   {
      if (m_uow != null && m_uow != m_context.getUnitOfWork() && m_context.isUnitOfWorkGlobal())
      {
         throw new TransactionException("err.runtime.wrongTransaction",
            new Object[]{getLazyClassName()});
      }

      deleteRelated();

      if (m_nState != DELETED)
      {
         setDeleted();
      }

      uncache();
      setEventInvoked(true);
   }

   /**
    * Invokes cascade delete logic on related objects.
    */
   public void deleteRelated()
   {
      load();

      if (m_nState != DELETED)
      {
         byte nFlagsSaved = m_nFlags;

         m_nFlags |= UPDATEABLE;

         try
         {
            // check cancellation conditions first
            for (int i = m_metaclass.getInstanceAttributeCount() - 1; i >= 0; --i)
            {
               Attribute attribute = m_metaclass.getInstanceAttribute(i);

               if (attribute.getCascadeMode() == Attribute.CASCADE_CANCEL)
               {
                  boolean bCancel;

                  if (attribute.isCollection())
                  {
                     InstanceList list = (InstanceList)getValue(attribute.getOrdinal());

                     bCancel = (list != null && !list.isEmpty());
                  }
                  else
                  {
                     Instance instance = (Instance)getValue(attribute.getOrdinal());

                     bCancel = (instance != null && instance.tryLoad());
                  }

                  if (bCancel)
                  {
                     ConstraintViolationException e = new ConstraintViolationException(
                        "err.persistence.cascadeCancel",
                        new Object[]{new StringId(m_metaclass.getCaption()),
                           getName(), new StringId(attribute.getCaption())});

                     e.setClassName(getClassName());
                     e.setOIDHolder(this);

                     throw e;
                  }
               }
            }

            // then do the rest of the cascade operations
            for (int i = m_metaclass.getInstanceAttributeCount() - 1; i >= 0; --i)
            {
               Attribute attribute = m_metaclass.getInstanceAttribute(i);

               switch (attribute.getCascadeMode())
               {
                  case Attribute.CASCADE_NONE:
                     if (attribute.isSymmetric())
                     {
                        Object value = getValueDirect(attribute.getOrdinal());

                        if (value != null)
                        {
                           if (value == Undefined.VALUE &&
                              attribute.isReverseInverseDependencyPersistent())
                           {
                              value = getValue(attribute.getOrdinal());
                           }

                           if (!(value instanceof Undefined))
                           {
                              if (attribute.isCollection())
                              {
                                 if (value != null)
                                 {
                                    InstanceList list = (InstanceList)value;

                                    // Full load of the list is triggered by size()
                                    if (list.size() == 0)
                                    {
                                       continue;
                                    }

                                    do
                                    {
                                       int nOrdinal = list.size() - 1;
                                       Instance instance = list.getInstance(nOrdinal);
                                       byte nInstanceFlagsSaved = instance.m_nFlags;

                                       instance.m_nFlags |= UPDATEABLE;

                                       try
                                       {
                                          list.remove(nOrdinal);
                                       }
                                       finally
                                       {
                                          instance.m_nFlags &= ~UPDATEABLE;
                                          instance.m_nFlags |= nInstanceFlagsSaved & UPDATEABLE;
                                       }
                                    }
                                    while (!list.isEmpty());
                                 }
                              }
                              else
                              {
                                 if (value != null)
                                 {
                                    Instance instance = (Instance)value;

                                    if (instance.tryLoad())
                                    {
                                       byte nInstanceFlagsSaved = instance.m_nFlags;

                                       instance.m_nFlags |= UPDATEABLE;

                                       try
                                       {
                                          setValue(attribute.getOrdinal(), null);
                                       }
                                       finally
                                       {
                                          instance.m_nFlags &= ~UPDATEABLE;
                                          instance.m_nFlags |= nInstanceFlagsSaved & UPDATEABLE;
                                       }
                                    }
                                    else
                                    {
                                       setValueDirect(attribute.getOrdinal(), null);
                                    }
                                 }
                              }
                           }
                        }
                     }

                     continue;

                  case Attribute.CASCADE_DELETE:
                  case Attribute.CASCADE_CLEAR:
                     break;

                  default:
                     continue;
               }

               if (attribute.isCollection())
               {
                  InstanceList list = (InstanceList)getValue(attribute.getOrdinal());

                  if (list == null)
                  {
                     continue;
                  }

                  switch (attribute.getCascadeMode())
                  {
                     case Attribute.CASCADE_DELETE:
                        // Full load of the list is triggered by size()
                        if (list.size() == 0)
                        {
                           continue;
                        }

                        do
                        {
                           int nOrdinal = list.size() - 1;
                           Instance instance = list.getInstance(nOrdinal);
                           byte nInstanceFlagsSaved = instance.m_nFlags;

                           // Remove the association first to avoid circular deletions in business logic
                           instance.m_nFlags |= UPDATEABLE;

                           try
                           {
                              list.remove(nOrdinal);
                           }
                           finally
                           {
                              instance.m_nFlags &= ~UPDATEABLE;
                              instance.m_nFlags |= nInstanceFlagsSaved & UPDATEABLE;
                           }

                           instance.invoke("delete", (Object[])null);
                        }
                        while (!list.isEmpty());

                        break;

                     case Attribute.CASCADE_CLEAR:
                        // Full load of the list is triggered by size()
                        if (list.size() == 0)
                        {
                           continue;
                        }

                        list.clear();
                        break;
                  }
               }
               else
               {
                  Instance instance = (Instance)getValue(attribute.getOrdinal());

                  if (instance == null)
                  {
                     continue;
                  }

                  if (instance.tryLoad())
                  {
                     switch (attribute.getCascadeMode())
                     {
                        case Attribute.CASCADE_DELETE:
                           // Remove the association first to avoid circular deletions in business logic
                           byte nInstanceFlagsSaved = instance.m_nFlags;

                           instance.m_nFlags |= UPDATEABLE;

                           try
                           {
                              setValue(attribute.getOrdinal(), null);
                           }
                           finally
                           {
                              instance.m_nFlags &= ~UPDATEABLE;
                              instance.m_nFlags |= nInstanceFlagsSaved & UPDATEABLE;
                           }

                           instance.invoke("delete", (Object[])null);
                           break;

                        case Attribute.CASCADE_CLEAR:
                           setValue(attribute.getOrdinal(), null);
                           break;
                     }
                  }
                  else
                  {
                     setValueDirect(attribute.getOrdinal(), null);
                  }
               }
            }
         }
         finally
         {
            m_nFlags &= ~UPDATEABLE;
            m_nFlags |= nFlagsSaved & UPDATEABLE;
         }
      }
   }

   /**
    * Replicates the instance into the relevant fragments.
    */
   public void replicate()
   {
      PersistenceMapping mapping = getPersistenceMapping();

      if (mapping != null)
      {
         byte nReplication = mapping.getFragmentReplication();

         if (nReplication != PersistenceMapping.REPLICATION_NONE)
         {
            DataSource dataSource = mapping.getDataSource();

            if (dataSource.getFragmentCount() != 1)
            {
               UnitOfWork uow = m_uow;

               if (uow == null)
               {
                  uow = m_context.getUnitOfWork();
               }

               if (nReplication == PersistenceMapping.REPLICATION_UNICAST)
               {
                  if (getFragmentName() == null)
                  {
                     Attribute attribute = mapping.getFragmentAttribute();

                     if (attribute != null)
                     {
                        uow.addReplicationFragment(this, (String)((attribute.isStatic()) ?
                           m_metaclass : (Accessor)this).getValue(attribute.getOrdinal()));
                     }
                  }
                  else
                  {
                     uow.addReplicationFragment(this, null);
                  }
               }
               else if (nReplication == PersistenceMapping.REPLICATION_BROADCAST)
               {
                  if (getFragmentName() == null)
                  {
                     for (Iterator itr = dataSource.getFragmentIterator(); itr.hasNext();)
                     {
                        uow.addReplicationFragment(this, ((DataSourceFragment)itr.next()).getName());
                     }
                  }
                  else
                  {
                     throw new PersistenceException("err.persistence.broadcastFragment",
                        new Object[]{m_metaclass.getName(), m_context.getFragmentName()});
                  }
               }
            }
         }
      }
   }

   /**
    * Flushes the cache for this instance.
    */
   public void uncache()
   {
      if (m_uow != null && (m_oid != null || m_nState == NEW))
      {
         load();
         uncache(m_metaclass, m_oid, m_uow);

         Metaclass root = m_metaclass.getPersistenceRoot();

         for (int i = 0, n = root.getPersistenceAliasCount(); i < n; ++i)
         {
            Metaclass alias = root.getPersistenceAlias(i);
            PersistenceMapping mapping = alias.getPersistenceMapping();
            Attribute typeCodeAttr = mapping.getTypeCodeAttribute();

            if (typeCodeAttr != null)
            {
               Metaclass metaclass = mapping.findMetaclassByTypeCode(getValue(typeCodeAttr.getOrdinal()));

               if (metaclass != null)
               {
                  alias = metaclass;
               }
            }

            uncache(alias, m_oid, m_uow);
         }
      }
   }

   /**
    * Flushes the instance cache.
    * @param metaclass The class object.
    * @param oid The OID. Can be null.
    * @param uow The unit of work.
    */
   protected static void uncache(Metaclass metaclass, OID oid, UnitOfWork uow)
   {
      PersistenceMapping mapping = metaclass.getPersistenceMapping();

      if (mapping != null)
      {
         if (mapping.getCaching() != PersistenceMapping.CACHING_NONE)
         {
            if (oid != null)
            {
               uow.uncache(getInstanceKey(metaclass, oid), UnitOfWork.CACHE_UNPARTITIONED);
            }
         }

         Metaclass root = metaclass.getPersistenceRoot();

         do
         {
            if (mapping.getCaching() == PersistenceMapping.CACHING_CLASS)
            {
               uow.uncache(getClassKey(metaclass, null), UnitOfWork.CACHE_UNPARTITIONED);
            }

            metaclass = metaclass.getBase();

            if (metaclass == null)
            {
               break;
            }

            mapping = metaclass.getPersistenceMapping();

            if (metaclass.getPersistenceRoot() != root)
            {
               break;
            }
         }
         while (mapping != null);
      }
   }

   /**
    * Creates a class cache key.
    * @param metaclass The class object.
    * @param sKey Additional key part. Null for the main class cache.
    * @return The class cache key.
    */
   public static Object getClassKey(Metaclass metaclass, String sKey)
   {
      return new Pair(metaclass.getSymbol(), sKey);
   }

   /**
    * Creates an instance cache key.
    * @param metaclass The class object.
    * @param oid The instance OID.
    * @return The instance cache key.
    */
   public static Object getInstanceKey(Metaclass metaclass, OID oid)
   {
      return new Pair(metaclass.getSymbol(), oid);
   }

   /**
    * @return The instance name, which is the value
    * of the name attribute, if any is defined.
    */
   public String getName()
   {
      if (isLazy())
      {
         try
         {
            load();
         }
         catch (PersistenceException e)
         {
         }
      }

      Attribute attribute = m_metaclass.getNameAttribute();

      if (attribute != null)
      {
         try
         {
            return (String)getValue(attribute.getOrdinal());
         }
         catch (PersistenceException e)
         {
         }
      }

      if (m_oid != null)
      {
         return m_oid.toString();
      }

      return "";
   }

   /**
    * @see nexj.core.util.Printable#printOn(nexj.core.util.PrintWriter)
    */
   public void printOn(PrintWriter writer) throws IOException
   {
      writer.write("Instance<");
      writer.write((m_metaclass != null) ? m_metaclass.getName() : null);
      writer.write(", ");
      writer.write(String.valueOf(m_oid));
      writer.write(", ");
      writer.write(STATE_NAME_ARRAY[m_nState]);
      writer.write(">(");

      if (m_metaclass != null && !isLazy() && writer.addObject(this))
      {
         boolean bDirty = false;

         for (int i = 0; i < m_metaclass.getInstanceAttributeCount(); ++i)
         {
            Object value = getValueDirect(i);

            if (value instanceof Undefined)
            {
               continue;
            }

            if (bDirty)
            {
               writer.write(", ");
            }
            else
            {
               bDirty = true;
            }

            printOn(writer, m_metaclass.getInstanceAttribute(i).getName(), value);
         }

         if (m_annotationMap != null)
         {
            for (Lookup.Iterator itr = m_annotationMap.iterator(); itr.hasNext();)
            {
               if (bDirty)
               {
                  writer.write(", ");
               }
               else
               {
                  bDirty = true;
               }

               itr.next();
               printOn(writer, (String)itr.getKey(), itr.getValue());
            }
         }

         writer.removeObject(this);
      }

      writer.write(')');
   }

   /**
    * Prints an attribute value on an writer.
    * @param writer The writer.
    * @param sName The attribute name.
    * @param value The attribute value.
    */
   protected static void printOn(PrintWriter writer, String sName, Object value) throws IOException
   {
      writer.write(sName);
      writer.write('=');

      if (value instanceof Instance)
      {
         Instance instance = (Instance)value;

         writer.write("Instance<");
         writer.write((instance.m_metaclass != null) ? instance.m_metaclass.getName() : null);
         writer.write(", ");
         writer.write(String.valueOf(instance.m_oid));
         writer.write(", ");
         writer.write(STATE_NAME_ARRAY[instance.m_nState]);
         writer.write('>');
      }
      else if (value instanceof Collection)
      {
         writer.write("Collection(");
         writer.print((value instanceof InstanceList) ? ((InstanceList)value).getCount() : ((Collection)value).size());
         writer.write(')');
      }
      else
      {
         writer.print(value);
      }
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return PrintWriter.toString(this);
   }
}