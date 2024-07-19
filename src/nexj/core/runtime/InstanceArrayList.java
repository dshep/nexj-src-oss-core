// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.ConcurrentModificationException;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.NoSuchElementException;
import java.util.RandomAccess;
import java.util.Set;

import nexj.core.meta.Attribute;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Primitive;
import nexj.core.meta.TypeMismatchException;
import nexj.core.persistence.Query;
import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.Ditto;
import nexj.core.util.DuplicateItemException;
import nexj.core.util.GenericHashTab;
import nexj.core.util.HashHolder;
import nexj.core.util.HashTab;
import nexj.core.util.Invalid;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;
import nexj.core.util.StackTrace;
import nexj.core.util.Undefined;

/**
 * Instance list implementation using an array for storing the objects.
 */
public class InstanceArrayList implements InstanceList, RandomAccess, Cloneable, Ditto, Function
{
   // constants

   /**
    * Minimum instance array length.
    */
   protected final static int MIN_LENGTH = 4;

   /**
    * The array has not been loaded yet.
    */
   protected final static byte UNLOADED = 0;

   /**
    * The array is currently being loaded.
    */
   protected final static byte LOADING = 1;

   /**
    * The array has been loaded.
    */
   protected final static byte LOADED = 2;

   /**
    * The array has no references.
    */
   protected final static byte NO_REF = 0;

   /**
    * The array has weak references.
    */
   protected final static byte WEAK_REF = 1;

   /**
    * The array has no weak references.
    */
   protected final static byte STRONG_REF = 2;

   /**
    * Set of of reserved list attribute symbols: Symbol[].
    */
   protected final static Set RESERVED_SYMBOL_SET;

   static
   {
      String[] sNameArray =
      {
         "clear", "clone", "ditto", "attribute", "container", "count", "hashCode", "dirty",
         "empty", "lazy", "updated", "weak", "iterator", "list", "listIterator", "load",
         "reverse", "size", "toArray", "toString" 
      };
      int nCount = sNameArray.length;

      RESERVED_SYMBOL_SET = new HashHolder(nCount);

      for (int i = 0; i < nCount; ++i)
      {
         RESERVED_SYMBOL_SET.add(Symbol.define(sNameArray[i]));
      }
   }

   // attributes

   /**
    * The list element count.
    */
   protected int m_nCount;

   /**
    * The active iterator count.
    */
   protected short m_nIteratorCount;

   /**
    * The loading state: UNLOADED, LOADING, LOADED.
    */
   protected byte m_nLoadState;

   /**
    * The reference state: NO_REF, WEAK_REF, STRONG_REF.
    */
   protected byte m_nRefState;

   // associations

   /**
    * The collection storage.
    */
   protected InstanceHolder[] m_instanceArray;

   /**
    * Map of instance to its ordinal number: Integer[InstanceHolder].
    */
   protected Lookup m_instanceMap;

   /**
    * The association container instance.
    */
   protected Instance m_container;

   /**
    * The association attribute.
    */
   protected Attribute m_attribute;

   /**
    * Maps an instance to a change object: Change[Instance].
    */
   protected Lookup m_changeMap;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(InstanceArrayList.class);

   // constructors

   /**
    * Creates an instance array list preallocated for 8 elements.
    */
   public InstanceArrayList()
   {
      this(8);
   }

   /**
    * Creates an instance array list preallocated
    * for the specified number of elements.
    * @param nCount The number of elements for which to preallocate the list.
    */
   public InstanceArrayList(int nCount)
   {
      if (nCount < MIN_LENGTH)
      {
         nCount = MIN_LENGTH;
      }

      m_instanceArray = new InstanceHolder[nCount];
   }

   /**
    * Creates an instance array list containing
    * the specified collection elements.
    * @param collection The collection to add.
    */
   public InstanceArrayList(Collection collection)
   {
      this(collection.size());
      addAll(collection);
   }

   // operations

   /**
    * @see nexj.core.runtime.InstanceList#checkUpdateAccess(nexj.core.meta.Attribute, Instance)
    */
   public void checkUpdateAccess(Attribute attribute, Instance instance) throws SecurityViolationException
   {
      if (attribute != null)
      {
         int nOrdinal = attribute.getOrdinal();

         if (m_nLoadState == UNLOADED && m_container != null)
         {
            load();
         }

         for (int i = 0; i < m_nCount; ++i)
         {
            Instance inst = m_instanceArray[i].getInstance();

            if (inst != null)
            {
               inst.checkUpdateAccess(nOrdinal, instance);
            }
         }
      }
   }

   /**
    * @see nexj.core.runtime.InstanceList#setAssociation(nexj.core.runtime.Instance, nexj.core.meta.Attribute, boolean)
    */
   public void setAssociation(Instance container, Attribute attribute, boolean bDirect)
   {
      if (container != m_container || attribute != m_attribute)
      {
         if (!bDirect && m_attribute != null && m_attribute != attribute && m_attribute.isSymmetric())
         {
            int nReverseOrdinal = m_attribute.getReverse().getOrdinal();

            for (int i = 0; i < m_nCount; ++i)
            {
               Instance instance = m_instanceArray[i].getInstance();

               if (instance != null)
               {
                  instance.dissociate(nReverseOrdinal, m_container, bDirect);
               }
            }
         }

         m_container = container;
         m_attribute = attribute;

         if (container != null && attribute != null && attribute.getReverse() != null)
         {
            int nReverseOrdinal = attribute.getReverse().getOrdinal();

            for (int i = 0; i < m_nCount; ++i)
            {
               Instance instance = m_instanceArray[i].getInstance();

               if (instance != null)
               {
                  instance.associate(nReverseOrdinal, container, bDirect);
               }
            }
         }

         if (container == null || container.getOID() == null)
         {
            m_nLoadState = LOADED;
         }
      }
   }

   /**
    * @see nexj.core.runtime.InstanceList#getContainer()
    */
   public Instance getContainer()
   {
      return m_container;
   }

   /**
    * @see nexj.core.runtime.InstanceList#getAttribute()
    */
   public Attribute getAttribute()
   {
      return m_attribute;
   }

   /**
    * @see nexj.core.runtime.InstanceList#isWeak()
    */
   public boolean isWeak()
   {
      return m_nRefState == WEAK_REF;
   }

   /**
    * @see nexj.core.runtime.InstanceList#setWeak(boolean)
    */
   public void setWeak(boolean bWeak)
   {
      if (bWeak)
      {
         if (m_nRefState == STRONG_REF)
         {
            WeakKeyTab map = new WeakKeyTab(m_instanceArray.length);

            for (int i = 0; i != m_nCount; ++i)
            {
               Ref ref = new Ref((Instance)m_instanceArray[i], map.m_refq);

               map.put(ref, Primitive.createInteger(i));
               m_instanceArray[i] = ref;
            }

            m_instanceMap = map;
            m_nLoadState = UNLOADED;
         }

         m_nRefState = WEAK_REF;
      }
      else
      {
         if (m_nRefState == WEAK_REF)
         {
            cleanup();

            Lookup map = new HashTab(m_instanceArray.length);
            boolean bLazy = ((WeakKeyTab)m_instanceMap).m_bLazy;
            int i, k;

            for (i = k = 0; i < m_nCount; ++i)
            {
               Instance instance = m_instanceArray[i].getInstance();

               if (instance != null)
               {
                  map.put(instance, Primitive.createInteger(k));
                  m_instanceArray[k] = instance;
                  ++k;
               }
               else
               {
                  bLazy = true;
               }
            }

            m_nCount = k;

            while (k < i)
            {
               m_instanceArray[k++] = null;
            }

            m_instanceMap = map;

            if (bLazy && m_nLoadState == LOADED)
            {
               m_nLoadState = UNLOADED;
            }
         }

         m_nRefState = STRONG_REF;
      }
   }

   /**
    * Cleans up the instance array, if the weak reference hash tab is dirty.
    */
   protected void cleanup()
   {
      WeakKeyTab map = (WeakKeyTab)m_instanceMap;

      while (map.m_bDirty)
      {
         map.removeStaleReferences();
         map.m_bDirty = false;

         int i, k;

         for (i = k = 0; i < m_nCount; ++i)
         {
            InstanceHolder holder = m_instanceArray[i];
            Instance instance = holder.getInstance();

            if (instance != null)
            {
               m_instanceArray[k] = holder;

               if (k != i)
               {
                  map.put(holder, Primitive.createInteger(k));
               }

               k++;
            }
            else
            {
               map.remove(holder);
            }
         }

         m_nCount = k;

         if (k < (m_instanceArray.length >> 1) && m_instanceArray.length - k > MIN_LENGTH)
         {
            InstanceHolder[] instanceArray = new InstanceHolder[k + MIN_LENGTH];

            System.arraycopy(m_instanceArray, 0, instanceArray, 0, k);
            m_instanceArray = instanceArray;
         }
         else
         {
            while (k < i)
            {
               m_instanceArray[k++] = null;
            }
         }
      }
   }

   /**
    * @return The after-update copy in the UOW, or null if none.
    */
   protected InstanceList findNew()
   {
      if (!isDirty() &&
         m_container != null &&
         m_container.getState() == Instance.DIRTY &&
         m_container.getOldValueDirect(m_attribute.getOrdinal()) == this)
      {
         Object value = m_container.getNewValueDirect(m_attribute.getOrdinal());

         if (value != this && value instanceof InstanceList)
         {
            return (InstanceList)value;
         }
      }

      return null;
   }

   /**
    * @see nexj.core.runtime.InstanceList#setLazy(boolean)
    */
   public void setLazy(boolean bLazy)
   {
      if (bLazy != (m_nLoadState != LOADED))
      {
         InstanceList list = findNew();

         if (list != null)
         {
            list.setLazy(bLazy);
         }
      }

      m_nLoadState = (bLazy) ? UNLOADED : LOADED;
   }

   /**
    * @see nexj.core.runtime.InstanceList#isLazy()
    */
   public boolean isLazy()
   {
      return m_nLoadState != LOADED;
   }

   /**
    * @see nexj.core.runtime.InstanceList#setLoading()
    */
   public void setLoading()
   {
      if (m_nLoadState != UNLOADED)
      {
         if (m_nRefState != STRONG_REF)
         {
            setWeak(false);
         }

         m_nLoadState = LOADING;
      }
   }

   /**
    * Loads a given instance into the collection.
    * @param instance The instance to load. Can be null.
    */
   protected void load(Instance instance)
   {
      if (m_nLoadState == UNLOADED && m_container != null &&
         instance != null && ((Metaclass)m_attribute.getType()).isUpcast(instance.getMetaclass()))
      {
         if (m_attribute.getReverse() != null)
         {
            Object value = instance.getValueDirect(m_attribute.getReverse().getOrdinal());

            if (value == m_container)
            {
               add(instance, REPLACE | DIRECT | WEAK);
            }
            else if (value instanceof Undefined)
            {
               load(null, Pair.list(Symbol.AT).eq(instance), -1);
            }
         }
         else
         {
            load(null, Pair.list(Symbol.AT).eq(instance), -1);
         }
      }
   }

   /**
    * @see nexj.core.runtime.InstanceList#load(Pair, Object, int)
    */
   public void load(Pair attributes, Object where, int nCount)
   {
      if (m_nRefState != STRONG_REF)
      {
         setWeak(false);
      }

      if (m_nLoadState == UNLOADED && (nCount < 0 || nCount > m_nCount || where != null))
      {
         if (m_container == null)
         {
            m_nLoadState = LOADED;
         }
         else
         {
            try
            {
               m_nLoadState = LOADING;

               Metaclass metaclass = (Metaclass)m_attribute.getType();

               if (m_attribute.isPersistent() && metaclass.getPersistenceMapping() != null)
               {
                  if (m_container.getState() != Instance.NEW && m_container.getOID() != null)
                  {
                     if (s_logger.isDebugEnabled())
                     {
                        String sMsg = "Loading collection \"" + m_attribute.getName() + "\" of " + m_container;

                        if (s_logger.isDumpEnabled())
                        {
                           StackTrace t = new StackTrace();

                           m_container.getInvocationContext().getMachine().updateStackTrace(t);
                           s_logger.dump(sMsg, t);
                        }
                        else
                        {
                           s_logger.debug(sMsg);
                        }
                     }

                     Pair cond = Pair.list(Symbol.ATAT, m_container.getMetaclass().getSymbol(),
                        m_attribute.getSymbol()).eq(m_container.getOID());

                     if (where != null)
                     {
                        cond = cond.and(where);
                     }

                     if (nCount > 0 && m_changeMap != null)
                     {
                        for (Iterator itr = m_changeMap.valueIterator(); itr.hasNext();)
                        {
                           if (!((Change)itr.next()).added)
                           {
                              ++nCount;
                           }
                        }
                     }

                     InstanceList list;
                     InvocationContext context = m_container.getInvocationContext();
                     int nRPC = context.getRPCCount();

                     try
                     {
                        list = Query.createRead(metaclass, attributes, cond,
                           null, nCount, 0, false, Query.SEC_NONE, context).read();
                     }
                     finally
                     {
                        if (context.getRPCCount() != nRPC)
                        {
                           context.addLoadCount(1);
                        }
                     }

                     int nReverseOrdinal = -1;

                     if (m_attribute.getReverse() != null)
                     {
                        nReverseOrdinal = m_attribute.getReverse().getOrdinal();
                     }

                     int nListCount = list.getCount();

                     if (nCount > nListCount)
                     {
                        nCount = -1;
                     }

                     for (int i = 0; i < nListCount; ++i)
                     {
                        Instance instance = list.getInstance(i);

                        add(instance, REPLACE | DIRECT);

                        if (nReverseOrdinal >= 0)
                        {
                           instance.setOldValueDirect(nReverseOrdinal, m_container);
                        }
                     }
                  }
               }
               else
               {
                  m_container.load(m_attribute.getSymbol());
                  where = null;
                  nCount = -1;
               }

               if (nCount < 0 && where == null)
               {
                  m_nLoadState = LOADED;

                  InstanceList list = findNew();

                  if (list != null)
                  {
                     list.setLazy(false);
                  }
               }
            }
            finally
            {
               if (m_nLoadState == LOADING)
               {
                  m_nLoadState = UNLOADED;
               }
            }
         }
      }
   }

   /**
    * @see nexj.core.runtime.InstanceList#load()
    */
   public void load()
   {
      load(null, null, -1);
   }

   /**
    * @see nexj.core.runtime.InstanceList#add(int, nexj.core.runtime.Instance, int)
    */
   public boolean add(int nOrdinal, Instance instance, int nFlags)
   {
      if (nOrdinal < 0 || nOrdinal > m_nCount)
      {
         throw new IndexOutOfBoundsException("Invalid ordinal number " + nOrdinal);
      }

      Change oldChange = null;

      if (m_container != null)
      {
         if ((nFlags & DIRECT) == 0)
         {
            m_container.checkUpdateAccess(m_attribute);

            if (m_attribute.getReverse() != null)
            {
               instance.checkUpdateAccess(m_attribute.getReverse().getOrdinal(), m_container);
            }

            nFlags |= TRACK;
         }
         else
         {
            if (m_changeMap != null)
            {
               Change change = (Change)m_changeMap.get(instance);

               if (change != null && !change.added)
               {
                  return false;
               }
            }
         }

         if ((nFlags & TRACK) != 0 && !m_container.isLoading())
         {
            if (m_changeMap == null)
            {
               m_changeMap = new HashTab(4);
            }

            UnitOfWork uow = m_container.getInvocationContext().getUnitOfWork();
            Change change = new Change(true, uow);

            oldChange = (Change)m_changeMap.put(instance, change);

            if (oldChange != null)
            {
               if (oldChange.uow != uow)
               {
                  m_changeMap.put(instance, oldChange);

                  throw new WorkStateException("err.runtime.collectionChange",
                     new Object[]{m_attribute.getName(), m_container.getLazyClassName()});
               }

               if (!oldChange.added)
               {
                  m_changeMap.remove(instance);
               }
            }
            else
            {
               uow.addChange(this);
            }
         }
      }

      InstanceHolder holder = instance;

      if ((nFlags & WEAK) != 0 && m_nRefState != STRONG_REF)
      {
         if (m_instanceMap == null)
         {
            m_instanceMap = new WeakKeyTab(m_instanceArray.length);
            m_nRefState = WEAK_REF;
         }

         holder = new Ref(instance, ((WeakKeyTab)m_instanceMap).m_refq);
      }
      else
      {
         if (m_instanceMap == null)
         {
            m_instanceMap = new HashTab(m_instanceArray.length);
            m_nRefState = STRONG_REF;
         }
         else if (m_nRefState != STRONG_REF)
         {
            if (nOrdinal == m_nCount)
            {
               setWeak(false);
               nOrdinal = m_nCount;
            }
         }
      }

      Object oldOrdinal = m_instanceMap.put(holder, Primitive.createInteger(nOrdinal));

      if (oldOrdinal != null)
      {
         if ((nFlags & REPLACE) != 0)
         {
            m_instanceArray[((Integer)oldOrdinal).intValue()] = holder;
            m_instanceMap.put(holder, oldOrdinal);

            return false;
         }

         if ((nFlags & DIRECT) != 0 && m_changeMap != null)
         {
            if (oldChange == null)
            {
               m_changeMap.remove(instance);
            }
            else
            {
               m_changeMap.put(instance, oldChange);
            }
         }

         m_instanceMap.put(m_instanceArray[((Integer)oldOrdinal).intValue()], oldOrdinal);

         throw new DuplicateItemException("err.persistence.duplicateInstance",
            new Object[]{instance.getLazyMetaclass().getName(), instance.getOID()});
      }

      if (m_nIteratorCount != 0 || m_nCount == m_instanceArray.length)
      {
         InstanceHolder[] instanceArray = new InstanceHolder[(m_nCount == m_instanceArray.length) ?
            m_nCount << 1 : Math.max(Math.min((m_nCount + 1) << 1,
               m_instanceArray.length), MIN_LENGTH)];

         System.arraycopy(m_instanceArray, 0, instanceArray, 0, nOrdinal);

         if (nOrdinal < m_nCount)
         {
            System.arraycopy(m_instanceArray, nOrdinal, instanceArray, nOrdinal + 1, m_nCount - nOrdinal);
         }

         instanceArray[nOrdinal] = holder;
         m_instanceArray = instanceArray;
         m_nIteratorCount = 0;
      }
      else if (nOrdinal == m_nCount)
      {
         m_instanceArray[nOrdinal] = holder;
      }
      else
      {
         System.arraycopy(m_instanceArray, nOrdinal, m_instanceArray, nOrdinal + 1, m_nCount - nOrdinal);
         m_instanceArray[nOrdinal] = holder;
      }

      ++m_nCount;

      for (int i = nOrdinal + 1; i < m_nCount; ++i)
      {
         holder = m_instanceArray[i];

         if (holder.getInstance() != null)
         {
            m_instanceMap.put(holder, Primitive.createInteger(i));
         }
         else
         {
            m_instanceMap.remove(holder);
         }
      }

      if (m_container != null && m_attribute != null)
      {
         if ((nFlags & DIRECT) == 0)
         {
            m_attribute.invalidateDependency(m_container, Invalid.VALUE);
            m_container.updateAliases(m_attribute);

            if (m_attribute.getReverse() != null)
            {
               instance.associate(m_attribute.getReverse().getOrdinal(), m_container, false);
            }
         }
         else
         {
            InstanceList list = findNew();

            if (list != null)
            {
               Object obj;

               if (m_attribute.getReverse() == null ||
                  (obj = instance.getValueDirect(m_attribute.getReverse().getOrdinal())) == m_container ||
                  obj instanceof Undefined)
               {
                  list.add(instance, nFlags | REPLACE);
               }
            }
         }
      }

      if (m_nRefState != STRONG_REF)
      {
         if ((nFlags & WEAK) == 0)
         {
            setWeak(false);
         }
         else if (m_nRefState == WEAK_REF)
         {
            cleanup();
         }
      }

      return true;
   }

   /**
    * @see nexj.core.runtime.InstanceList#add(Instance, int)
    */
   public boolean add(Instance instance, int nFlags)
   {
      return add(m_nCount, instance, nFlags);
   }

   /**
    * @see nexj.core.runtime.InstanceList#getInstance(int)
    */
   public Instance getInstance(int nOrdinal)
   {
      if (nOrdinal < 0)
      {
         throw new IndexOutOfBoundsException("Invalid ordinal number " + nOrdinal);
      }
      else if (nOrdinal < 0 || nOrdinal >= m_nCount)
      {
         if (nOrdinal >= 0 && m_nLoadState == UNLOADED)
         {
            load(null, null, (nOrdinal >= (Integer.MAX_VALUE >> 1)) ? -1 : (nOrdinal + 1) << 1);
         }

         if (nOrdinal < 0 || nOrdinal >= m_nCount)
         {
            throw new IndexOutOfBoundsException("Invalid ordinal number " + nOrdinal);
         }
      }

      return m_instanceArray[nOrdinal].getInstance();
   }

   /**
    * @see nexj.core.runtime.InstanceList#remove(int, int)
    */
   public Instance remove(int nOrdinal, int nFlags)
   {
      if (nOrdinal < 0 || nOrdinal >= m_nCount)
      {
         throw new IndexOutOfBoundsException("Invalid ordinal number " + nOrdinal);
      }

      InstanceHolder holder = m_instanceArray[nOrdinal];
      Instance instance = holder.getInstance();

      if (m_container != null)
      {
         if ((nFlags & DIRECT) == 0)
         {
            m_container.checkUpdateAccess(m_attribute);

            if (instance != null)
            {
               if (m_attribute.isSymmetric())
               {
                  instance.checkUpdateAccess(m_attribute.getReverse().getOrdinal());
               }
            }

            nFlags |= TRACK;
         }

         if ((nFlags & TRACK) != 0 && !m_container.isLoading())
         {
            if (m_changeMap == null)
            {
               m_changeMap = new HashTab(4);
            }

            UnitOfWork uow = m_container.getInvocationContext().getUnitOfWork();
            Change change = new Change(false, uow);
            Change oldChange = (Change)m_changeMap.put(instance, change);

            if (oldChange != null)
            {
               if (oldChange.uow != uow)
               {
                  m_changeMap.put(instance, oldChange);

                  throw new WorkStateException("err.runtime.collectionChange",
                     new Object[]{m_attribute.getName(), m_container.getLazyClassName()});
               }

               if (oldChange.added)
               {
                  m_changeMap.remove(instance);
               }
            }
            else
            {
               uow.addChange(this);
            }
         }
      }

      m_instanceMap.remove(holder);
      --m_nCount;

      if (m_nIteratorCount != 0)
      {
         Instance[] instanceArray = new Instance[Math.max(m_nCount, MIN_LENGTH)];

         System.arraycopy(m_instanceArray, 0, instanceArray, 0, nOrdinal);

         if (nOrdinal != m_nCount)
         {
            System.arraycopy(m_instanceArray, nOrdinal + 1, instanceArray, nOrdinal, m_nCount - nOrdinal);
         }

         m_instanceArray = instanceArray;
         m_nIteratorCount = 0;
      }
      else
      {
         if (nOrdinal != m_nCount)
         {
            System.arraycopy(m_instanceArray, nOrdinal + 1, m_instanceArray, nOrdinal, m_nCount - nOrdinal);
         }

         m_instanceArray[m_nCount] = null;
      }

      if (nOrdinal != m_nCount)
      {
         for (int i = nOrdinal; i < m_nCount; ++i)
         {
            holder = m_instanceArray[i];

            Instance inst = holder.getInstance();

            if (inst != null)
            {
               m_instanceMap.put(holder, Primitive.createInteger(i));
            }
            else
            {
               m_instanceMap.remove(holder);
            }
         }
      }

      if (m_attribute != null && m_container != null)
      {
         if ((nFlags & DIRECT) == 0)
         {
            m_attribute.invalidateDependency(m_container, Invalid.VALUE);
            m_container.updateAliases(m_attribute);

            if (m_attribute.isSymmetric() && instance != null)
            {
               instance.dissociate(m_attribute.getReverse().getOrdinal(), m_container, false);
            }
         }
         else
         {
            InstanceList list = findNew();

            if (list != null)
            {
               Object obj;

               if (m_attribute.getReverse() == null ||
                  (obj = instance.getValueDirect(m_attribute.getReverse().getOrdinal())) == m_container ||
                  obj instanceof Undefined)
               {
                  list.remove(instance, nFlags);
               }
            }
         }
      }

      return instance;
   }

   /**
    * @see nexj.core.runtime.InstanceList#remove(nexj.core.runtime.Instance, int)
    */
   public boolean remove(Instance instance, int nFlags)
   {
      Object ordinal;

      if (m_instanceMap == null || (ordinal = m_instanceMap.get(instance)) == null)
      {
         if (m_nLoadState != UNLOADED || m_container == null ||
            (nFlags & DIRECT) != 0 && m_changeMap != null && m_changeMap.contains(instance))
         {
            return false;
         }

         load(instance);

         if (m_instanceMap == null)
         {
            return false;
         }

         ordinal = m_instanceMap.get(instance);

         if (ordinal == null)
         {
            return false;
         }
      }

      remove(((Integer)ordinal).intValue(), nFlags);

      return true;
   }

   /**
    * @see java.util.List#get(int)
    */
   public Object get(int nOrdinal)
   {
      return getInstance(nOrdinal);
   }

   /**
    * @see java.util.Collection#size()
    */
   public int size()
   {
      if (m_nLoadState == UNLOADED)
      {
         load();
      }
      else if (m_nRefState != STRONG_REF)
      {
         setWeak(false);

         if (m_nLoadState == UNLOADED)
         {
            load();
         }
      }

      return m_nCount;
   }

   /**
    * @see nexj.core.runtime.InstanceList#getCount()
    */
   public int getCount()
   {
      return m_nCount;
   }

   /**
    * @see java.util.List#add(int, java.lang.Object)
    */
   public void add(int nOrdinal, Object element)
   {
      add(nOrdinal, (Instance)element, DEFAULT);
   }

   /**
    * @see java.util.Collection#add(java.lang.Object)
    */
   public boolean add(Object element)
   {
      add(m_nCount, (Instance)element, DEFAULT);

      return true;
   }

   /**
    * @see java.util.List#remove(int)
    */
   public Object remove(int nOrdinal)
   {
      return remove(nOrdinal, DEFAULT);
   }

   /**
    * @see java.util.Collection#remove(java.lang.Object)
    */
   public boolean remove(Object o)
   {
      return remove((Instance)o, DEFAULT);
   }

   /**
    * @see java.util.Collection#contains(java.lang.Object)
    */
   public boolean contains(Object o)
   {
      if (o instanceof Instance)
      {
         if (m_instanceMap != null && m_instanceMap.contains(o))
         {
            return true;
         }

         if (m_nLoadState == UNLOADED && m_container != null)
         {
            load((Instance)o);

            return m_instanceMap != null && m_instanceMap.contains(o);
         }
      }

      return false;
   }

   /**
    * @see java.util.List#indexOf(java.lang.Object)
    */
   public int indexOf(Object o)
   {
      Object ordinal;

      if (m_instanceMap == null || (ordinal = m_instanceMap.get(o)) == null)
      {
         if (m_nLoadState != UNLOADED || m_container == null)
         {
            return -1;
         }

         load();

         if (m_instanceMap == null)
         {
            return -1;
         }

         ordinal = m_instanceMap.get(o);

         if (ordinal == null)
         {
            return -1;
         }
      }

      return ((Integer)ordinal).intValue();
   }

   /**
    * @see java.util.List#lastIndexOf(java.lang.Object)
    */
   public int lastIndexOf(Object o)
   {
      return indexOf(o);
   }

   /**
    * @see java.util.List#clear()
    */
   public void clear()
   {
      for (int i = size() - 1; i >= 0; --i)
      {
         remove(i);
      }
   }

   /**
    * @see java.util.List#isEmpty()
    */
   public boolean isEmpty()
   {
      if (m_nCount != 0)
      {
         return false;
      }

      if (m_nLoadState == UNLOADED)
      {
         load(null, null, 1);
      }

      return m_nCount == 0;
   }

   /**
    * @see java.util.List#toArray()
    */
   public Object[] toArray()
   {
      Instance[] instanceArray = new Instance[size()];

      for (int i = 0; i != instanceArray.length; ++i)
      {
         instanceArray[i] = getInstance(i);
      }

      return instanceArray;
   }

   /**
    * @see java.util.List#toArray(java.lang.Object[])
    */
   public Object[] toArray(Object[] a)
   {
      int nCount = size();

      if (nCount > a.length)
      {
         a = (Object[])Array.newInstance(a.getClass().getComponentType(), nCount);
      }

      for (int i = 0; i != nCount; ++i)
      {
         a[i] = getInstance(i);
      }

      if (nCount < a.length)
      {
         a[nCount] = null;
      }

      return a;
   }

   /**
    * @see java.util.List#addAll(int, java.util.Collection)
    */
   public boolean addAll(int i, Collection c)
   {
      boolean bModified = false;

      for (Iterator itr = c.iterator(); itr.hasNext();)
      {
         add(i++, itr.next());
         bModified = true;
      }

      return bModified;
   }

   /**
    * @see java.util.List#addAll(java.util.Collection)
    */
   public boolean addAll(Collection c)
   {
      return addAll(m_nCount, c);
   }

   /**
    * @see java.util.List#containsAll(java.util.Collection)
    */
   public boolean containsAll(Collection c)
   {
      load();

      for (Iterator itr = c.iterator(); itr.hasNext();)
      {
         if (!contains(itr.next()))
         {
            return false;
         }
      }

      return true;
   }

   /**
    * @see java.util.List#removeAll(java.util.Collection)
    */
   public boolean removeAll(Collection c)
   {
      boolean bModified = false;

      for (Iterator itr = c.iterator(); itr.hasNext();)
      {
         bModified |= remove(itr.next());
      }

      return bModified;
   }

   /**
    * @see java.util.List#retainAll(java.util.Collection)
    */
   public boolean retainAll(Collection c)
   {
      boolean bModified = false;

      for (int i = size() - 1; i >= 0; --i)
      {
         if (!c.contains(get(i)))
         {
            remove(i);
            bModified = true;
         }
      }

      return bModified;
   }

   /**
    * @see java.util.List#iterator()
    */
   public Iterator iterator()
   {
      load();

      return new InstanceIterator();
   }

   /**
    * @see java.util.List#listIterator()
    */
   public ListIterator listIterator()
   {
      return listIterator(0);
   }

   /**
    * @see java.util.List#listIterator(int)
    */
   public ListIterator listIterator(int nIndex)
   {
      load();

      if (nIndex < 0 || nIndex > m_nCount)
      {
         throw new IndexOutOfBoundsException("Index: " + nIndex);
      }

      return new InstanceListIterator(nIndex);
   }

   /**
    * @see java.util.List#subList(int, int)
    */
   public List subList(int nStart, int nEnd)
   {
      return new InstanceSublist(nStart, nEnd);
   }

   /**
    * @see java.util.List#set(int, java.lang.Object)
    */
   public Object set(int index, Object element)
   {
      throw new UnsupportedOperationException();
   }

   /**
    * @see nexj.core.runtime.InstanceList#isDirty()
    */
   public boolean isDirty()
   {
      return m_changeMap != null;
   }

   /**
    * @see nexj.core.runtime.InstanceList#isUpdated()
    */
   public boolean isUpdated()
   {
      if (m_changeMap != null)
      {
         for (Lookup.Iterator itr = m_changeMap.valueIterator(); itr.hasNext();)
         {
            if (!((Change)itr.next()).finished)
            {
               return true;
            }
         }
      }

      return false;
   }

   /**
    * @see nexj.core.runtime.InstanceList#complete(nexj.core.runtime.UnitOfWork, boolean, boolean)
    */
   public void complete(UnitOfWork uow, boolean bCommit, boolean bDelta)
   {
      if (m_changeMap != null)
      {
         for (Lookup.Iterator itr = m_changeMap.iterator(); itr.hasNext();)
         {
            Instance instance = (Instance)itr.next();
            Change change = (Change)itr.getValue();

            if ((change.uow == uow || uow == null))
            {
               if (!bDelta || change.finished == bCommit)
               {
                  if (bCommit)
                  {
                     itr.remove();
                  }
                  else
                  {
                     if (change.added)
                     {
                        remove(instance, DIRECT);
                        itr.remove();
                     }
                     else
                     {
                        itr.remove();
                        add(instance, (isWeak()) ? DIRECT | REPLACE | WEAK : DIRECT | REPLACE);
                     }
                  }
               }
               else
               {
                  itr.remove();
               }
            }
         }

         if (m_changeMap.size() == 0)
         {
            m_changeMap = null;
         }
      }
   }

   /**
    * @see nexj.core.runtime.InstanceList#finish()
    */
   public void finish()
   {
      if (m_changeMap != null)
      {
         for (Lookup.Iterator itr = m_changeMap.valueIterator(); itr.hasNext();)
         {
            Change change = (Change)itr.next();

            change.finished = true;
            change.uow.removePreCollection(this);
         }
      }
   }

   /**
    * @see nexj.core.meta.InstanceList#reverse()
    */
   public void reverse()
   {
      if (m_nCount != 0)
      {
         if (m_nIteratorCount != 0)
         {
            InstanceHolder[] instanceArray = new InstanceHolder[m_nCount];

            for (int i = 0; i != m_nCount; ++i)
            {
               instanceArray[m_nCount - 1 - i] = m_instanceArray[i];
            }

            m_instanceArray = instanceArray;
            m_nIteratorCount = 0;
         }
         else
         {
            for (int i = 0, k = m_nCount - 1; i < k; ++i, --k)
            {
               InstanceHolder holder = m_instanceArray[i];

               m_instanceArray[i] = m_instanceArray[k];
               m_instanceArray[k] = holder;
            }
         }

         for (Lookup.Iterator itr = m_instanceMap.iterator(); itr.hasNext();)
         {
            itr.next();
            itr.setValue(Primitive.createInteger(m_nCount - ((Integer)itr.getValue()).intValue() - 1));
         }
      }
   }

   /**
    * @see nexj.core.runtime.InstanceList#list()
    */
   public Pair list()
   {
      load();

      Pair pair = null;

      for (int i = m_nCount - 1; i >= 0; --i)
      {
         pair = new Pair(m_instanceArray[i], pair);
      }

      return pair;
   }

   /**
    * @see nexj.core.runtime.InstanceList#select(nexj.core.scripting.Pair)
    */
   public Collection select(Pair path)
   {
      Collection col = this;

      for (; path != null; path = path.getNext())
      {
         Object head = path.getHead();

         if (!(head instanceof Symbol))
         {
            throw new TypeMismatchException("#<instance-collection select/1>");
         }

         col = new InstanceAssociationCollection(col, head.toString());
      }

      return col;
   }

   /**
    * @see nexj.core.scripting.Function#invoke(int, nexj.core.scripting.Machine)
    */
   public boolean invoke(int nArgCount, Machine machine)
   {
      if (nArgCount == 1 &&
         (m_attribute == null || m_attribute.getMetaclass().getMetadata().isCollectionPathEnabled()))
      {
         Object obj = machine.getArg(0, nArgCount);

         if (obj instanceof Symbol)
         {
            String sName = obj.toString();

            if (sName.length() != 0 && sName.charAt(0) == ':')
            {
               if (Symbol._SIZE.equals(obj))
               {
                  machine.returnValue(Primitive.createInteger(size()), nArgCount);

                  return false;
               }

               if (Symbol._COUNT.equals(obj))
               {
                  machine.returnValue(Primitive.createInteger(getCount()), nArgCount);

                  return false;
               }

               if (Symbol._LIST.equals(obj))
               {
                  machine.returnValue(list(), nArgCount);

                  return false;
               }

               if (Symbol._ITERATOR.equals(obj))
               {
                  machine.returnValue(iterator(), nArgCount);

                  return false;
               }

               if (Symbol._EMPTY.equals(obj))
               {
                  machine.returnValue(Boolean.valueOf(isEmpty()), nArgCount);

                  return false;
               }

               if (Symbol._ASSOCIATION.equals(obj))
               {
                  machine.returnValue(new InstanceAssociationCollection(this, null), nArgCount);

                  return false;
               }
            }

            if (m_attribute != null && ((Metaclass)m_attribute.getType()).findAttribute(sName) != null ||
               !RESERVED_SYMBOL_SET.contains(obj))
            {
               machine.returnValue(new InstanceAssociationCollection(this, sName), nArgCount);

               return false;
            }
         }
      }

      return machine.invokeJavaMethod(this, nArgCount);
   }

   /**
    * @see nexj.core.util.Sortable#sort(java.util.Comparator)
    */
   public void sort(Comparator cmp)
   {
      load();

      if (m_nIteratorCount != 0)
      {
         Instance[] instanceArray = new Instance[Math.max(m_nCount, MIN_LENGTH)];

         System.arraycopy(m_instanceArray, 0, instanceArray, 0, m_nCount);
         m_instanceArray = instanceArray;
         m_nIteratorCount = 0;
      }

      Arrays.sort(m_instanceArray, 0, m_nCount, cmp);

      if (m_instanceMap != null)
      {
         for (int i = 0; i < m_nCount; ++i)
         {
            m_instanceMap.put(m_instanceArray[i], Primitive.createInteger(i));
         }
      }
   }

   /**
    * @see nexj.core.util.Ditto#ditto()
    */
   public Object ditto()
   {
      InstanceArrayList list = new InstanceArrayList(m_nCount);

      list.m_attribute = m_attribute;

      return list;
   }

   /**
    * @see java.lang.Object#clone()
    */
   public Object clone()
   {
      try
      {
         InstanceArrayList list = (InstanceArrayList)super.clone();
         InstanceHolder[] instanceArray = new InstanceHolder[m_instanceArray.length];

         System.arraycopy(m_instanceArray, 0, instanceArray, 0, m_nCount);
         list.m_instanceArray = instanceArray;
         list.m_nIteratorCount = 0;

         if (m_instanceMap != null)
         {
            list.m_instanceMap = (Lookup)((GenericHashTab)m_instanceMap).clone();
         }

         if (m_changeMap != null)
         {
            list.m_changeMap = (Lookup)((GenericHashTab)m_changeMap).clone();
         }

         return list;
      }
      catch (CloneNotSupportedException e)
      {
         return null;
      }
   }

   /**
    * @see java.lang.Object#equals(java.lang.Object)
    */
   public boolean equals(Object obj)
   {
      if (obj == this)
      {
         return true;
      }

      if (!(obj instanceof InstanceList))
      {
         return false;
      }

      InstanceList list = (InstanceList)obj;
      int nCount = m_nCount;

      if (nCount != list.getCount())
      {
         return false;
      }

      for (int i = 0; i != nCount; ++i)
      {
         if (!ObjUtil.equal(m_instanceArray[i], list.get(i)))
         {
            return false;
         }
      }

      return true;
   }

   /**
    * @see java.lang.Object#hashCode()
    */
   public int hashCode()
   {
      int nHash = 7;

      load();

      for (int i = 0; i != m_nCount; ++i)
      {
         nHash = (nHash << 1 | nHash >> 31 & 1) ^ m_instanceArray[i].hashCode() ;
      }

      return nHash;
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuffer buf = new StringBuffer(m_nCount << 5);

      buf.append('[');

      for (int i = 0; i != m_nCount; ++i)
      {
         if (i != 0)
         {
            buf.append(", ");
         }

         buf.append(m_instanceArray[i].getInstance());
      }

      buf.append(']');

      return buf.toString();
   }

   // inner classes

   /**
    * Represents a single-instance operation on the list.
    */
   protected final static class Change
   {
      // attributes

      /**
       * True if the instance has been added, false if removed.
       */
      public boolean added;

      /**
       * True if the change has been processed by a container instance life cycle event.
       */
      public boolean finished;

      // associations

      /**
       * The Unit-of-Work.
       */
      public UnitOfWork uow;

      // constructors

      /**
       * Constructs the change.
       * @param bAdd True if the instance has been added, false if removed.
       * @param uow The Unit-of-Work, where the change has occurred.
       */
      public Change(boolean bAdd, UnitOfWork uow)
      {
         this.added = bAdd;
         this.uow = uow;
      }

      // operations

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         StringBuffer buf = new StringBuffer(64);

         buf.append((this.added) ? "Added to " : "Removed from ");
         buf.append(this.uow);

         return buf.toString();
      }
   }

   /**
    * Weak instance reference.
    */
   protected final static class Ref extends WeakReference implements InstanceHolder
   {
      // attributes

      /**
       * The hash code.
       */
      protected int m_nHashCode;

      // constructors

      /**
       * Constructs the reference.
       * @param instance The instance.
       * @param q The reference queue.
       */
      public Ref(Instance instance, ReferenceQueue q)
      {
         super(instance, q);

         if (instance != null)
         {
            m_nHashCode = instance.hashCode();
         }
      }

      // operations

      /**
       * @see nexj.core.runtime.InstanceHolder#getInstance()
       */
      public Instance getInstance()
      {
         return (Instance)get();
      }

      /**
       * @see java.lang.Object#hashCode()
       */
      public int hashCode()
      {
         return m_nHashCode;
      }

      /**
       * @see java.lang.Object#equals(java.lang.Object)
       */
      public boolean equals(Object obj)
      {
         Object ref = get();

         if (obj instanceof Ref)
         {
            Object ref2 = ((Ref)obj).get();

            if (ref2 != null)
            {
               return ref2.equals(ref);
            }

            if (ref == null)
            {
               return obj == this;
            }

            return false;
         }

         if (obj != null)
         {
            return obj.equals(ref);
         }

         return false;
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         Object obj = get();

         return (obj != null) ? obj.toString() : super.toString();
      }
   }

   /**
    * Hash table with weak keys.
    */
   protected final static class WeakKeyTab extends HashTab
   {
      // constants

      /**
       * Serialization UID.
       */
      private static final long serialVersionUID = -7614792389621029657L;

      // attributes

      /**
       * True if the instance array needs cleanup.
       */
      protected boolean m_bDirty;

      /**
       * True if some of the instances have been garbage collected.
       */
      protected boolean m_bLazy;

      // associations

      /**
       * The reference queue with stale references, populated by the garbage collector.
       */
      protected transient ReferenceQueue m_refq = new ReferenceQueue();

      // constructors

      /**
       * Creates a hash table with an estimated key-value pair count.
       * @param nCount The estimated key-value pair count.
       */
      public WeakKeyTab(int nCount)
      {
         super(nCount);
      }

      // operations

      /**
       * Removes the stale references from the queue, the hash table and the instance array.
       */
      public void removeStaleReferences()
      {
         Ref ref;

         while ((ref = (Ref)m_refq.poll()) != null)
         {
            int nMask = m_table.length - 1;
            int i = (ref.hashCode() << 1) & nMask;

            for (;;)
            {
               Object key = m_table[i];

               if (key == ref)
               {
                  m_table[i + 1] = null;
                  --m_nCount;
                  ++m_nEmpty;

                  if (m_table[(i + 2) & nMask] == null)
                  {
                     do
                     {
                        m_table[i] = null;
                        i = (i - 2) & nMask;
                        --m_nEmpty;
                     }
                     while (m_table[i] == EMPTY);
                  }
                  else
                  {
                     m_table[i] = EMPTY;
                  }

                  int nMaxCount = (nMask + 1) >> 1;

                  // Load factor [1/4..3/4]
                  if ((m_nCount << 2) < nMaxCount ||
                     (nMaxCount - m_nCount - m_nEmpty) << 2 < nMaxCount)
                  {
                     rehash();
                  }

                  break;
               }

               if (key == null)
               {
                  break;
               }

               i = (i + 2) & nMask;
            }

            m_bLazy = true;
         }
      }

      /**
       * @see nexj.core.util.HashTab#rehash(int)
       */
      protected void rehash(int nSize)
      {
         if (nSize < m_table.length)
         {
            m_bDirty = true;
         }

         super.rehash(nSize);
      }

      /**
       * @see nexj.core.util.Lookup#contains(java.lang.Object)
       */
      public boolean contains(Object key)
      {
         removeStaleReferences();

         return super.contains(key);
      }

      /**
       * @see nexj.core.util.Lookup#get(java.lang.Object)
       */
      public Object get(Object key)
      {
         removeStaleReferences();

         return super.get(key);
      }

      /**
       * @see nexj.core.util.Lookup#put(java.lang.Object, java.lang.Object)
       */
      public Object put(Object key, Object value)
      {
         removeStaleReferences();

         return super.put(key, value);
      }

      /**
       * @see nexj.core.util.Lookup#remove(java.lang.Object)
       */
      public Object remove(Object key)
      {
         removeStaleReferences();

         return super.remove(key);
      }

      /**
       * @see nexj.core.util.Lookup#clear()
       */
      public void clear()
      {
         super.clear();

         while (m_refq.poll() != null);
      }

      /**
       * @see nexj.core.util.Lookup#size()
       */
      public int size()
      {
         removeStaleReferences();

         return super.size();
      }

      /**
       * @see nexj.core.util.GenericHashHolder#serialize(java.io.ObjectOutputStream)
       */
      protected void serialize(ObjectOutputStream out) throws IOException
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see nexj.core.util.GenericHashHolder#deserialize(java.io.ObjectInputStream)
       */
      protected void deserialize(ObjectInputStream in) throws IOException, ClassNotFoundException
      {
         throw new UnsupportedOperationException();
      }
   }

   /**
    * Iterator implementation.
    */
   protected class InstanceIterator implements Iterator
   {
      /**
       * The current instance.
       */
      protected int m_nCurrent;

      /**
       * The instance count.
       */
      protected int m_nCount = InstanceArrayList.this.m_nCount;

      /**
       * The instance array.
       */
      protected InstanceHolder[] m_instanceArray = InstanceArrayList.this.m_instanceArray;

      public InstanceIterator()
      {
         if (m_nCount != 0)
         {
            ++m_nIteratorCount;
         }
      }

      /**
       * @see java.util.Iterator#hasNext()
       */
      public boolean hasNext()
      {
         return m_nCurrent < m_nCount;
      }

      /**
       * @see java.util.Iterator#next()
       */
      public Object next()
      {
         if (m_nCurrent < m_nCount)
         {
            ++m_nCurrent;

            if (m_nCurrent == m_nCount && m_instanceArray == InstanceArrayList.this.m_instanceArray)
            {
               --m_nIteratorCount;
            }

            return m_instanceArray[m_nCurrent - 1];
         }

         throw new java.util.NoSuchElementException();
      }

      /**
       * @see java.util.Iterator#remove()
       */
      public void remove()
      {
         if (m_nCurrent > m_nCount || m_nCurrent == 0)
         {
            throw new IllegalStateException();
         }

         if (m_instanceArray == InstanceArrayList.this.m_instanceArray)
         {
            --m_nIteratorCount;

            try
            {
               InstanceArrayList.this.remove(m_nCurrent - 1, DEFAULT);
            }
            finally
            {
               if (m_instanceArray == InstanceArrayList.this.m_instanceArray)
               {
                  ++m_nIteratorCount;
                  m_nCount = InstanceArrayList.this.m_nCount;
               }
               else
               {
                  m_instanceArray[m_nCurrent - 1] = null;
               }
            }
         }
         else
         {
            if (m_instanceArray[m_nCurrent - 1] == null)
            {
               throw new IllegalStateException();
            }

            InstanceArrayList.this.remove(m_instanceArray[m_nCurrent - 1]);
            m_instanceArray[m_nCurrent - 1] = null;
         }

         --m_nCurrent;
      }
   }

   /**
    * ListIterator implementation.
    */
   protected class InstanceListIterator extends InstanceIterator implements ListIterator
   {
      /**
       * Constructs the iterator.
       * @param nCurrent The current instance index.
       */
      public InstanceListIterator(int nCurrent)
      {
         super();
         m_nCurrent = nCurrent;
      }

      /**
       * @see java.util.ListIterator#nextIndex()
       */
      public int nextIndex()
      {
         return m_nCurrent;
      }

      /**
       * @see java.util.ListIterator#previousIndex()
       */
      public int previousIndex()
      {
         return m_nCurrent - 1;
      }

      /**
       * @see java.util.ListIterator#hasPrevious()
       */
      public boolean hasPrevious()
      {
         return m_nCurrent > 0;
      }

      /**
       * @see java.util.Iterator#next()
       */
      public Object next()
      {
         if (m_nCurrent < m_nCount)
         {
            ++m_nCurrent;

            return m_instanceArray[m_nCurrent - 1];
         }

         throw new java.util.NoSuchElementException();
      }

      /**
       * @see java.util.ListIterator#previous()
       */
      public Object previous()
      {
         if (m_nCurrent > 0)
         {
            --m_nCurrent;

            return m_instanceArray[m_nCurrent];
         }

         throw new java.util.NoSuchElementException();
      }

      /**
       * @see java.util.ListIterator#add(java.lang.Object)
       */
      public void add(Object obj)
      {
         Instance instance = (Instance)obj;

         if (m_instanceArray == InstanceArrayList.this.m_instanceArray)
         {
            --m_nIteratorCount;

            try
            {
               InstanceArrayList.this.add(m_nCurrent, instance, DEFAULT);
            }
            finally
            {
               if (m_instanceArray == InstanceArrayList.this.m_instanceArray)
               {
                  ++m_nIteratorCount;
               }
            }

            if (m_instanceArray != InstanceArrayList.this.m_instanceArray)
            {
               if (m_nCount == m_instanceArray.length)
               {
                  Instance[] array = new Instance[m_nCount << 1];

                  System.arraycopy(m_instanceArray, 0, array, 0, m_nCurrent);
                  System.arraycopy(m_instanceArray, m_nCurrent, array, m_nCurrent + 1, m_nCount - m_nCurrent);
                  m_instanceArray = array;
               }
               else
               {
                  System.arraycopy(m_instanceArray, m_nCurrent, m_instanceArray, m_nCurrent + 1, m_nCount - m_nCurrent);
               }

               m_instanceArray[m_nCurrent] = instance;
            }

            ++m_nCurrent;
            ++m_nCount;
         }
         else
         {
            throw new ConcurrentModificationException();
         }
      }

      /**
       * @see java.util.ListIterator#set(java.lang.Object)
       */
      public void set(Object o)
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.util.Iterator#remove()
       */
      public void remove()
      {
         if (m_nCurrent > m_nCount || m_nCurrent == 0)
         {
            throw new IllegalStateException();
         }

         if (m_instanceArray == InstanceArrayList.this.m_instanceArray)
         {
            --m_nIteratorCount;

            try
            {
               InstanceArrayList.this.remove(m_nCurrent - 1, DEFAULT);
            }
            finally
            {
               if (m_instanceArray == InstanceArrayList.this.m_instanceArray)
               {
                  ++m_nIteratorCount;
                  m_nCount = InstanceArrayList.this.m_nCount;
               }
               else
               {
                  System.arraycopy(m_instanceArray, m_nCurrent, m_instanceArray, m_nCurrent - 1, m_nCount - m_nCurrent);
                  --m_nCount;
               }
            }
         }
         else
         {
            InstanceArrayList.this.remove(m_instanceArray[m_nCurrent - 1]);
            System.arraycopy(m_instanceArray, m_nCurrent, m_instanceArray, m_nCurrent - 1, m_nCount-- - m_nCurrent);
         }

         --m_nCurrent;
      }
   }

   /**
    * Sublist implementation.
    */
   protected class InstanceSublist implements List, RandomAccess
   {
      /**
       * The start offset.
       */
      protected int m_nStart;

      /**
       * The end offset.
       */
      protected int m_nEnd;

      /**
       * Constructs the sublist.
       * @param nStart The start offset in the master list.
       * @param nEnd The end offset in the master list.
       */
      public InstanceSublist(int nStart, int nEnd)
      {
         if (nStart < 0)
         {
            throw new IndexOutOfBoundsException("nStart: " + nStart);
         }

         if (nEnd > m_nCount)
         {
            throw new IndexOutOfBoundsException("nEnd: " + nEnd);
         }

         if (nStart > nEnd)
         {
            throw new IllegalArgumentException("nStart > nEnd");
         }

         m_nStart = nStart;
         m_nEnd = nEnd;
      }

      /**
       * @see java.util.List#size()
       */
      public int size()
      {
         return m_nEnd - m_nStart;
      }

      /**
       * @see java.util.List#clear()
       */
      public void clear()
      {
         for (int i = size() - 1; i >= 0; --i)
         {
            remove(i);
         }
      }

      /**
       * @see java.util.List#isEmpty()
       */
      public boolean isEmpty()
      {
         return m_nEnd == m_nStart;
      }

      /**
       * @see java.util.List#toArray()
       */
      public Object[] toArray()
      {
         Instance[] instanceArray = new Instance[size()];

         for (int i = 0; i != instanceArray.length; ++i)
         {
            instanceArray[i] = getInstance(i + m_nStart);
         }

         return instanceArray;
      }

      /**
       * @see java.util.List#toArray(java.lang.Object[])
       */
      public Object[] toArray(Object[] a)
      {
         int nCount = size();

         if (nCount > a.length)
         {
            a = (Object[])Array.newInstance(a.getClass().getComponentType(), nCount);
         }

         for (int i = 0; i != nCount; ++i)
         {
            a[i] = getInstance(i + m_nStart);
         }

         if (nCount < a.length)
         {
            a[nCount] = null;
         }

         return a;
      }

      /**
       * Verifies that the index is within the allowed range for the sublist.
       * @param nIndex The index to verify.
       * @throws IndexOutOfBoundsException if the index is invalid.
       */
      protected void verifyIndex(int nIndex) throws IndexOutOfBoundsException
      {
         if (nIndex < 0 || nIndex >= size())
         {
            throw new IndexOutOfBoundsException("Index: " + nIndex);
         }
      }

      /**
       * Verifies that the index is within the allowed range for the sublist,
       * including and one item past the current end.
       * @param nIndex The index to verify.
       * @throws IndexOutOfBoundsException if the index is invalid.
       */
      protected void verifySize(int nIndex) throws IndexOutOfBoundsException
      {
         if (nIndex < 0 || nIndex > size())
         {
            throw new IndexOutOfBoundsException("Index: " + nIndex);
         }
      }

      /**
       * @see java.util.List#get(int)
       */
      public Object get(int nIndex)
      {
         verifyIndex(nIndex);

         return InstanceArrayList.this.get(nIndex + m_nStart);
      }

      /**
       * @see java.util.List#remove(int)
       */
      public Object remove(int nIndex)
      {
         verifyIndex(nIndex);

         Object obj = InstanceArrayList.this.remove(nIndex + m_nStart);

         --m_nEnd;

         return obj;
      }

      /**
       * @see java.util.List#add(int, java.lang.Object)
       */
      public void add(int nIndex, Object obj)
      {
         verifySize(nIndex);
         InstanceArrayList.this.add(nIndex + m_nStart, obj);
         ++m_nEnd;
      }

      /**
       * @see java.util.List#indexOf(java.lang.Object)
       */
      public int indexOf(Object obj)
      {
         if (m_instanceMap == null)
         {
            return -1;
         }

         Object ordinal = m_instanceMap.get(obj);

         if (ordinal == null)
         {
            return -1;
         }

         int i = ((Integer)ordinal).intValue();

         if (i < m_nStart || i >= m_nEnd)
         {
            return -1;
         }

         return i - m_nStart;
      }

      /**
       * @see java.util.List#lastIndexOf(java.lang.Object)
       */
      public int lastIndexOf(Object obj)
      {
         return indexOf(obj);
      }

      /**
       * @see java.util.List#add(java.lang.Object)
       */
      public boolean add(Object obj)
      {
         InstanceArrayList.this.add(m_nEnd, obj);
         ++m_nEnd;

         return true;
      }

      /**
       * @see java.util.List#contains(java.lang.Object)
       */
      public boolean contains(Object obj)
      {
         return indexOf(obj) >= 0;
      }

      /**
       * @see java.util.List#remove(java.lang.Object)
       */
      public boolean remove(Object obj)
      {
         int i = indexOf(obj);

         if (i >= 0)
         {
            InstanceArrayList.this.remove(i + m_nStart);
            --m_nEnd;

            return true;
         }

         return false;
      }

      /**
       * @see java.util.List#addAll(int, java.util.Collection)
       */
      public boolean addAll(int nIndex, Collection c)
      {
         verifySize(nIndex);

         boolean bModified = InstanceArrayList.this.addAll(nIndex + m_nStart, c);

         m_nEnd += c.size();

         return bModified;
      }

      /**
       * @see java.util.List#addAll(java.util.Collection)
       */
      public boolean addAll(Collection c)
      {
         return addAll(size(), c);
      }

      /**
       * @see java.util.List#containsAll(java.util.Collection)
       */
      public boolean containsAll(Collection c)
      {
         for (Iterator itr = c.iterator(); itr.hasNext();)
         {
            if (!contains(itr.next()))
            {
               return false;
            }
         }

         return true;
      }

      /**
       * @see java.util.List#removeAll(java.util.Collection)
       */
      public boolean removeAll(Collection c)
      {
         boolean bModified = false;

         for (Iterator itr = c.iterator(); itr.hasNext();)
         {
            bModified |= remove(itr.next());
         }

         return bModified;
      }

      /**
       * @see java.util.List#retainAll(java.util.Collection)
       */
      public boolean retainAll(Collection c)
      {
         boolean bModified = false;

         for (int i = size() - 1; i >= 0; --i)
         {
            if (!c.contains(get(i)))
            {
               remove(i);
               bModified = true;
            }
         }

         return bModified;
      }

      /**
       * @see java.util.List#iterator()
       */
      public Iterator iterator()
      {
         return listIterator(0);
      }

      /**
       * @see java.util.List#subList(int, int)
       */
      public List subList(int nStart, int nEnd)
      {
         verifyIndex(nStart);
         verifySize(nEnd);

         return new InstanceSublist(nStart + m_nStart, nEnd + m_nStart);
      }

      /**
       * @see java.util.List#listIterator()
       */
      public ListIterator listIterator()
      {
         return listIterator(0);
      }

      /**
       * @see java.util.List#listIterator(int)
       */
      public ListIterator listIterator(final int nIndex)
      {
         verifySize(nIndex);

         return new ListIterator()
         {
            private ListIterator m_itr = InstanceArrayList.this.listIterator(nIndex + m_nStart);

            public int nextIndex()
            {
               return m_itr.nextIndex() - m_nStart;
            }

            public int previousIndex()
            {
               return m_itr.previousIndex() - m_nStart;
            }

            public void remove()
            {
               m_itr.remove();
               --m_nEnd;
            }

            public boolean hasNext()
            {
               return nextIndex() < size();
            }

            public boolean hasPrevious()
            {
               return previousIndex() >= 0;
            }

            public Object next()
            {
               if (hasNext())
               {
                  return m_itr.next();
               }

               throw new NoSuchElementException();
            }

            public Object previous()
            {
               if (hasPrevious())
               {
                  return m_itr.previous();
               }

               throw new NoSuchElementException();
            }

            public void add(Object o)
            {
               m_itr.add(o);
               ++m_nEnd;
            }

            public void set(Object o)
            {
               throw new UnsupportedOperationException();
            }
         };
      }

      /**
       * @see java.util.List#set(int, java.lang.Object)
       */
      public Object set(int index, Object element)
      {
         throw new UnsupportedOperationException();
      }

      /**
       * @see java.lang.Object#equals(java.lang.Object)
       */
      public boolean equals(Object obj)
      {
         if (!(obj instanceof List))
         {
            return false;
         }

         List list = (List)obj;
         int nSize = size();

         if (nSize != list.size())
         {
            return false;
         }

         for (int i = 0; i != nSize; ++i)
         {
            if (!get(i).equals(list.get(i)))
            {
               return false;
            }
         }

         return true;
      }

      /**
       * @see java.lang.Object#hashCode()
       */
      public int hashCode()
      {
         int nHash = 7;

         for (int i = 0, n = size(); i != n; ++i)
         {
            nHash = (nHash << 1) + get(i).hashCode();
         }

         return nHash;
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         StringBuffer buf = new StringBuffer(size() << 5);

         buf.append('[');

         for (int i = 0, n = size(); i != n; ++i)
         {
            if (i != 0)
            {
               buf.append(", ");
            }

            buf.append(get(i));
         }

         buf.append(']');

         return buf.toString();
      }
   }
}
