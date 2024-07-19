// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc;

import java.util.ArrayList;
import java.util.List;

import nexj.core.meta.Attribute;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Primitive;
import nexj.core.meta.persistence.Key;
import nexj.core.meta.persistence.PersistenceMapping;
import nexj.core.persistence.OID;
import nexj.core.persistence.OptimisticLockException;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InstanceArrayList;
import nexj.core.runtime.InstanceList;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.SecurityViolationException;
import nexj.core.util.HashTab;
import nexj.core.util.Invalid;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;
import nexj.core.util.PropertyIterator;
import nexj.core.util.Undefined;

/**
 * Transfer object graph to instance graph converter.
 */
public class InstanceFactory
{
   // constants

   /**
    * Do not instantiate object state.
    */
   public final static int IDENTITY = 0x00;

   /**
    * Instantiate the entire object state. 
    */
   public final static int STATE = 0x01;

   /**
    * Instantiate the pre-values. 
    */
   public final static int PRE = 0x02;

   /**
    * Skip cached instances. 
    */
   public final static int CACHE = 0x04;

   /**
    * Detect optimistic locking errors.
    */
   public final static int LOCK = 0x08;
   
   /**
    * The pre-object key name.
    */
   public final static String PRE_NAME = ":pre";

   // attributes
   
   /**
    * The instantiation mode - a combination of InstanceFactory.* flags.
    */
   protected int m_nMode;

   /**
    * The optimistic lock mismatch flag.
    */
   protected boolean m_bLockMismatch;

   // associations

   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   /**
    * Map for tracking instantiated object identity.
    */
   protected Lookup m_identityMap;

   /**
    * Fixup list for the second pass, updated by this class. Null to instantiate
    * from cache.
    */
   protected List m_fixupList;

   /**
    * Constructs the factory.
    * @param identityMap The object identity map, updated by this method.
    * @param fixupList The fixup list for the second pass, updated by this
    *           method. Null to instantiate from cache.
    * @param nMode Instantiation mode, one of the InstanceFactory.* constants.
    * @param context The invocation context.
    */
   public InstanceFactory(Lookup identityMap, List fixupList, int nMode, InvocationContext context)
   {
      m_identityMap = identityMap;
      m_fixupList = fixupList;
      m_nMode = nMode;
      m_context = context;
   }

   /**
    * Constructs the factory for a 2-phase instantiation with completion.
    * @param nMode Instantiation mode, one of the InstanceFactory.* constants.
    * @param context The invocation context.
    * @see #complete()
    */
   public InstanceFactory(int nMode, InvocationContext context)
   {
      m_identityMap = new HashTab();
      m_fixupList = new ArrayList();
      m_nMode = nMode;
      m_context = context;
   }

   /**
    * Constructs the factory for instantiation from cache.
    * @param context The invocation context.
    */
   public InstanceFactory(InvocationContext context)
   {
      m_identityMap = new HashTab();
      m_fixupList = null;
      m_nMode = STATE;
      m_context = context;
   }

   /**
    * @return The identity map.
    */
   public Lookup getIdentityMap()
   {
      return m_identityMap;
   }

   /**
    * @return The fixup list. Can be null.
    */
   public List getFixupList()
   {
      return m_fixupList;
   }

   /**
    * @return True if instantiating from cache.
    */
   public boolean isCached()
   {
      return m_fixupList == null;
   }

   /**
    * Sets the optimistic lock mismatch flag.
    * @param bLockMismatch The optimistic lock mismatch flag to set.
    */
   public void setLockMismatch(boolean bLockMismatch)
   {
      m_bLockMismatch = bLockMismatch;
   }

   /**
    * @return The optimistic lock mismatch flag.
    */
   public boolean isLockMismatch()
   {
      return m_bLockMismatch;
   }

   /**
    * Instantiates a primitive value.
    * @param value The value to instantiate.
    * @param attribute The corresponding attribute.
    * @return The resulting primitive value.
    */
   protected static Object instantiatePrimitive(Object value, Attribute attribute)
   {
      Primitive type = (Primitive)attribute.getType();

      if (attribute.isCollection())
      {
         if (value == null)
         {
            return new ArrayList(0);
         }

         if (!(value instanceof List))
         {
            throw new RequestException("err.rpc.collectionType", new Object[]
            {
               attribute.getName(),
               attribute.getMetaclass().getName()
            });
         }

         List list = (List)value;
         int nCount = list.size();
         List convertedList = new ArrayList(nCount);

         for (int i = 0; i < nCount; ++i)
         {
            convertedList.add(type.convert(list.get(i)));
         }

         return convertedList;
      }

      return type.convert(value);
   }

   /**
    * Instantiates a transfer object list.
    * @param value The value to instantiate.
    * @param The corresponding attribute.
    * @return The resulting instance list.
    */
   protected InstanceList instantiateList(Object value, Attribute attribute)
   {
      if (value == null)
      {
         return new InstanceArrayList(0);
      }

      if (!(value instanceof List))
      {
         throw new RequestException("err.rpc.collectionType", new Object[]
         {
            attribute.getName(),
            attribute.getMetaclass().getName()
         });
      }

      List list = (List)value;
      int nCount = list.size();
      InstanceList instanceList = new InstanceArrayList(nCount);
      Metaclass type = (Metaclass)attribute.getType();

      for (int i = 0; i < nCount; ++i)
      {
         Object item = list.get(i);

         if (!(item instanceof TransferObject))
         {
            throw new RequestException("err.rpc.collectionItemType", new Object[]
            {
               attribute.getName(),
               attribute.getMetaclass().getName()
            });
         }

         Instance obj = instantiate((TransferObject)item);

         if (obj != null)
         {
            if (!type.isUpcast(obj.getMetaclass()))
            {
               throw new RequestException("err.rpc.classCast", new Object[]
               {
                  obj.getMetaclass().getName(),
                  attribute.getName(),
                  attribute.getMetaclass().getName()
               });
            }

            instanceList.add(obj);
         }
      }

      return instanceList;
   }

   /**
    * Instantiates a transfer object.
    * @param value The object to instantiate.
    * @param attribute The corresponding attribute.
    * @return The resulting instance.
    */
   protected Instance instantiateObject(Object value, Attribute attribute)
   {
      if (value == null)
      {
         return null;
      }

      if (!(value instanceof TransferObject))
      {
         throw new RequestException("err.rpc.objectType", new Object[]
         {
            attribute.getName(),
            attribute.getMetaclass().getName()
         });
      }

      Instance obj = instantiate((TransferObject)value);

      if (!attribute.getType().isUpcast(obj.getLazyMetaclass()))
      {
         throw new RequestException("err.rpc.classCast", new Object[]
         {
            obj.getLazyClassName(),
            attribute.getName(),
            attribute.getMetaclass().getName()
         });
      }

      return obj;
   }

   /**
    * First instantiation pass: creates the instances, checks access rights and
    * populates the fixup list.
    * @param tobj The transfer object to instantiate.
    * @return The instance corresponding to the transfer object.
    */
   public Instance instantiate(TransferObject tobj)
   {
      Instance instance = (Instance)m_identityMap.get(tobj);

      if (instance != null)
      {
         return instance;
      }

      Metaclass metaclass = m_context.getMetadata().getMetaclass(tobj.getClassName());
      Attribute lockingAttribute = (metaclass.getPersistenceMapping() != null) ? metaclass.getPersistenceMapping()
         .getLockingAttribute() : null;
      OID oid = tobj.getOID();

      if (isCached())
      {
         if (oid == null)
         {
            instance = null;
         }
         else
         {
            instance = m_context.lockInstance(metaclass, oid, tobj.getEventName() != null);

            if (instance != null && instance.isLazy())
            {
               if (tobj.getVersion() >= 0)
               {
                  instance.setMetaclass(metaclass);
               }
               else if (instance.getLazyMetaclass().isUpcast(metaclass))
               {
                  instance.setLazyMetaclass(metaclass);
               }
            }
         }

         if ("create".equals(tobj.getEventName()))
         {
            if (instance == null)
            {
               instance = new Instance(metaclass, m_context);
               instance.setNew();
               instance.setOID(oid);
            }
         }
         else
         {
            if (instance == null)
            {
               instance = new Instance(metaclass, tobj.getVersion() < 0, m_context);
               m_context.getUnitOfWork().lock(instance.cache(oid),
                  tobj.getEventName() != null && lockingAttribute != null &&
                  tobj.hasValue(lockingAttribute.getName()));
            }
            else if ((m_nMode & CACHE) != 0 && instance.isCached())
            {
               return instance;
            }
         }

         if ((m_nMode & CACHE) != 0)
         {
            instance.setCached(true);
         }
      }
      else
      {
         if (oid == null)
         {
            instance = new Instance(metaclass, m_context);
            instance.setNew();
            instance.getUnitOfWork().keepChange(instance);
         }
         else
         {
            if (m_context.isProtected() && m_context.isSecure())
            {
               metaclass.checkReadAccess(m_context.getPrivilegeSet());
            }

            instance = m_context.lockInstance(metaclass, oid, tobj.getEventName() != null);

            if (instance == null)
            {
               PersistenceMapping mapping = metaclass.getPersistenceMapping();

               if (mapping != null)
               {
                  Object[] valueArray = oid.getValueArray();
                  Key key = mapping.getObjectKey();

                  if (key.getPartCount() != valueArray.length)
                  {
                     throw new RequestException("err.rpc.oidPartCount", new Object[]
                     {
                        metaclass.getName()
                     });
                  }

                  for (int i = 0; i < valueArray.length; ++i)
                  {
                     valueArray[i] = key.getPartType(i).convert(valueArray[i]);
                  }
               }

               instance = new Instance(metaclass, m_context);
               m_context.getUnitOfWork().lock(instance.cache(oid),
                  tobj.getEventName() != null && lockingAttribute != null &&
                  tobj.hasValue(lockingAttribute.getName()));
            }
         }
      }

      m_identityMap.put(tobj, instance);

      if ((m_nMode & STATE) != 0 && tobj.getVersion() >= 0)
      {
         instance.load();

         if (lockingAttribute != null && instance.getState() != Instance.NEW)
         {
            Object oldValue = instance.getOldValueDirect(lockingAttribute.getOrdinal());

            if (!(oldValue instanceof Undefined))
            {
               Object value = tobj.findValue(lockingAttribute.getName());

               if (value != null && !ObjUtil.equal(lockingAttribute.getType().convert(value), oldValue))
               {
                  m_bLockMismatch = true;

                  if ((m_nMode & LOCK) != 0)
                  {
                     throw new OptimisticLockException(instance);
                  }

                  if (isCached())
                  {
                     return instance;
                  }

                  if (m_context.isLocked(instance))
                  {
                     throw new OptimisticLockException(instance);
                  }
               }
            }
         }

         TransferObject preObj = null;

         if ((m_nMode & PRE) != 0)
         {
            Object value = tobj.findValue(PRE_NAME);

            if (value != null)
            {
               if (value instanceof TransferObject)
               {
                  preObj = (TransferObject)value;
               }
               else
               {
                  throw new RequestException("err.rpc.preObjectType");
               }
            }
         }

         for (PropertyIterator itr = tobj.getIterator(); itr.hasNext();)
         {
            String sName = (String)itr.next();

            if ((m_nMode & PRE) != 0 && sName.equals(PRE_NAME))
            {
               continue;
            }

            Attribute attribute = metaclass.getAttribute(sName);
            Object value = itr.getValue();

            if (attribute.isStatic())
            {
               throw new RequestException("err.rpc.staticAttribute", new Object[]
               {
                  attribute.getName(),
                  metaclass.getName()
               });
            }

            if (isCached())
            {
               if (!(instance.getOldValueDirect(attribute.getOrdinal()) instanceof Undefined))
               {
                  continue;
               }
            }
            else
            {
               if (!attribute.isCached())
               {
                  throw new RequestException("err.rpc.uncachedAttribute", new Object[]
                  {
                     attribute.getName(),
                     metaclass.getName()
                  });
               }

               if (m_context.isProtected() && m_context.isSecure())
               {
                  attribute.checkReadAccess(m_context.getPrivilegeSet());
               }

               instance.checkUpdateAccess(attribute);
            }

            Object pre = (preObj == null) ? Undefined.VALUE : preObj.findValue(sName, Undefined.VALUE);

            if (attribute.getType().isPrimitive())
            {
               value = instantiatePrimitive(value, attribute);

               if (pre != Undefined.VALUE)
               {
                  pre = instantiatePrimitive(pre, attribute);
               }
            }
            else
            {
               if (attribute.isCollection())
               {
                  InstanceList instanceList;

                  if (pre != Undefined.VALUE)
                  {
                     pre = instanceList = instantiateList(pre, attribute);

                     if (isCached())
                     {
                        instanceList.setLazy(false);
                     }
                  }

                  value = instanceList = instantiateList(value, attribute);

                  if (isCached())
                  {
                     instanceList.setLazy(false);
                  }
                  else
                  {
                     if (attribute.getReverse() != null)
                     {
                        instanceList.checkUpdateAccess(attribute.getReverse(), instance);
                     }
                  }

                  instanceList.setAssociation(instance, attribute, true);
               }
               else
               {
                  Instance inst;

                  if (pre != Undefined.VALUE)
                  {
                     pre = inst = instantiateObject(pre, attribute);
                  }

                  value = inst = instantiateObject(value, attribute);

                  if (isCached() && attribute.getReverse() != null && inst != null && !inst.isLazy())
                  {
                     inst.associate(attribute.getReverse().getOrdinal(), instance, true);
                  }
               }
            }

            if (pre != Undefined.VALUE && ObjUtil.equal(value, pre))
            {
               pre = Undefined.VALUE;
            }

            if (isCached())
            {
               if (pre != Undefined.VALUE)
               {
                  instance.setPreValueDirect(attribute.getOrdinal(), pre);
               }

               instance.setOldValueDirect(attribute.getOrdinal(), value);
            }
            else
            {
               m_fixupList.add(instance);
               m_fixupList.add(attribute);
               m_fixupList.add(value);
               m_fixupList.add(pre);
            }
         }
      }

      if (!isCached() && m_context.isProtected() && m_context.isSecure() && oid != null && !instance.isReadable())
      {
         throw new SecurityViolationException("err.rpc.instanceAccess", new Object[]
         {
            metaclass.getCaption(),
            metaclass.getName()
         });
      }

      return instance;
   }

   /**
    * Instantiates a value, for which the type is not known in advance.
    * @param value The argument value.
    * @return The instantiated value.
    */
   public Object instantiate(Object value)
   {
      if (value instanceof List)
      {
         List list = (List)value;
         int nCount = list.size();

         if (nCount == 0)
         {
            return new InstanceArrayList(0);
         }

         if (list.get(0) instanceof TransferObject)
         {
            InstanceList instanceList = new InstanceArrayList(nCount);

            for (int i = 0; i < nCount; ++i)
            {
               instanceList.add(instantiate((TransferObject)list.get(i)));
            }

            return instanceList;
         }

         return list;
      }

      if (value instanceof TransferObject)
      {
         return instantiate((TransferObject)value);
      }

      return value;
   }

   /**
    * Completes the second and the third instantiation passes.
    */
   public void complete()
   {
      if (!isCached())
      {
         complete(m_fixupList);
      }
   }

   /**
    * Second and third instantiation passes: invalidates the dependencies, then
    * assigns the values.
    * @param fixupList The fixup list populated by instantiate(Object).
    */
   public void complete(List fixupList)
   {
      boolean bUOWGlobalSaved = m_context.isUnitOfWorkGlobal();

      try
      {
         m_context.setUnitOfWorkGlobal(false);

         // Invalidate the dependencies
         for (int i = 0, n = fixupList.size(); i != n; i += 4)
         {
            Instance instance = (Instance)fixupList.get(i);
            Attribute attribute = (Attribute)fixupList.get(i + 1);

            if (attribute.getInverseDependency() != null)
            {
               attribute.invalidateDependency(instance, Invalid.VALUE);
            }

            Attribute reverse = attribute.getReverse();

            if (reverse != null && attribute.isReverseOf(reverse) && reverse.getInverseDependency() != null)
            {
               Object value = fixupList.get(i + 2);

               if (value != null)
               {
                  if (attribute.isCollection())
                  {
                     InstanceList list = (InstanceList)value;

                     for (int k = 0, m = list.getCount(); k != m; ++k)
                     {
                        reverse.invalidateDependency(list.getInstance(k), Invalid.VALUE);
                     }
                  }
                  else
                  {
                     reverse.invalidateDependency((Instance)value, Invalid.VALUE);
                  }
               }
            }
         }

         // Assign the values
         for (int i = 0, n = fixupList.size(); i != n; i += 4)
         {
            Instance instance = (Instance)fixupList.get(i);
            Attribute attribute = (Attribute)fixupList.get(i + 1);
            Object value = fixupList.get(i + 2);
            Object pre = fixupList.get(i + 3);

            instance.assign(attribute, value, true);

            if (pre != Undefined.VALUE)
            {
               InvocationContext context = instance.getInvocationContext();
               byte nGenerationSaved = context.getGeneration();

               try
               {
                  context.setGeneration(InvocationContext.GEN_PRE);
                  instance.assign(attribute, pre, true);
               }
               finally
               {
                  context.setGeneration(nGenerationSaved);
               }
            }
         }
      }
      finally
      {
         m_context.setUnitOfWorkGlobal(bUOWGlobalSaved);
      }
   }
}