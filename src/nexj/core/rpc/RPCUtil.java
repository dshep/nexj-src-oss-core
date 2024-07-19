// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc;

import java.io.Reader;
import java.io.StringReader;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.SimpleTimeZone;
import java.util.TimeZone;

import nexj.core.meta.Attribute;
import nexj.core.meta.Event;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Metadata;
import nexj.core.meta.Primitive;
import nexj.core.meta.PrimitivePrivilege;
import nexj.core.meta.Type;
import nexj.core.persistence.OID;
import nexj.core.rpc.text.TextMarshaller;
import nexj.core.runtime.Context;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InstanceList;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.SecurityViolationException;
import nexj.core.runtime.UnitOfWork;
import nexj.core.scripting.ConstPair;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Parser;
import nexj.core.scripting.ParserException;
import nexj.core.scripting.Symbol;
import nexj.core.scripting.object.BasicObject;
import nexj.core.scripting.object.TypedBasicObject;
import nexj.core.util.Binary;
import nexj.core.util.HashHolder;
import nexj.core.util.HashTab;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;
import nexj.core.util.PagedArrayList;
import nexj.core.util.PagedBinary;
import nexj.core.util.PropertyIterator;
import nexj.core.util.PropertyMap;
import nexj.core.util.StringId;
import nexj.core.util.StringUtil;
import nexj.core.util.TZ;
import nexj.core.util.Undefined;

/**
 * RPC utilities.
 */
public class RPCUtil
{
   /**
    * Let the top level objects in the graph to pass unrestricted, but restrict the associations.
    */
   public final static int TF_TOP = 0;

   /**
    * Let only the readable objects pass.
    */
   public final static int TF_READABLE = 0x01;

   /**
    * Let all the objects pass.
    */
   public final static int TF_ALL = 0x02;

   /**
    * Transfer the entire object state.
    */
   public final static int TF_STATE = 0x04;

   /**
    * Transfer the old values of the deleted objects.
    */
   public final static int TF_DELETED = 0x08;

   /**
    * Mark the transferred objects as cached.
    */
   public final static int TF_CACHE = 0x10;

   /**
    * The state for new objects will be transferred only if they do not have OIDs.
    */
   public final static int TF_IDENTITY = 0x20;
   
   /**
    * Transfer the old values.
    */
   public final static int TF_OLD = 0x40;

   /**
    * Do not load lazy instances.
    */
   public final static int TF_LAZY = 0x80;

   /**
    * Transfer all non-readable instances as empty objects with class name and null event and OID.
    * This does not apply to collections and top-level instances.
    * No effect if TF_ALL is used.
    */
   public final static int TF_HIDDEN = 0x100;

   /**
    * Do not transfer non-serializable values.
    */
   public final static int TF_SERIALIZABLE = 0x200; 

   /**
    * Clone the instance. Do not copy the OID.
    * All non-readonly attributes are copied.
    */
   public final static int TF_CLONE = 0x400;

   /**
    * Transfer all of the dirty attributes.
    */
   public final static int TF_DIRTY = 0x800;

   /**
    * Set during processing of a non-collection association.
    */
   protected final static int TF_REF = 0x80000000;

   /**
    * Do not instantiate object state.
    * @deprecated Use InstanceFactory.* constants instead.
    */
   public final static int INST_IDENTITY = InstanceFactory.IDENTITY;

   /**
    * Instantiate the entire object state. 
    * @deprecated Use InstanceFactory.* constants instead.
    */
   public final static int INST_STATE = InstanceFactory.STATE;

   /**
    * Instantiate the pre-values. 
    * @deprecated Use InstanceFactory.* constants instead.
    */
   public final static int INST_PRE = InstanceFactory.PRE;

   /**
    * Skip cached instances. 
    * @deprecated Use InstanceFactory.* constants instead.
    */
   public final static int INST_CACHE = InstanceFactory.CACHE;

   /**
    * Do not audit the state.
    */
   public final static byte AUDIT_NONE = 0;

   /**
    * Audit only the property map values.
    */
   public final static byte AUDIT_MAP = 1;

   /**
    * Audit only the attribute names.
    */
   public final static byte AUDIT_NAMES = 2;

   /**
    * Audit the current attribute values. 
    */
   public final static byte AUDIT_VALUES = 3;

   /**
    * Audit the current and the old (if available) attribute values.
    */
   public final static byte AUDIT_CHANGES = 4;

   // associations

   /**
    * Map of class object to validator.
    */
   protected final static Lookup s_validatorMap = new HashTab(32);

   static
   {
      s_validatorMap.put(Integer.class, Boolean.TRUE);
      s_validatorMap.put(Long.class, Boolean.TRUE);
      s_validatorMap.put(Float.class, Boolean.TRUE);
      s_validatorMap.put(Double.class, Boolean.TRUE);
      s_validatorMap.put(BigDecimal.class, Boolean.TRUE);
      s_validatorMap.put(java.sql.Timestamp.class, Boolean.TRUE);
      s_validatorMap.put(Boolean.class, Boolean.TRUE);
      s_validatorMap.put(String.class, Boolean.TRUE);
      s_validatorMap.put(StringId.class, Boolean.TRUE);
      s_validatorMap.put(Character.class, Boolean.TRUE);
      s_validatorMap.put(Binary.class, Boolean.TRUE);
      s_validatorMap.put(PagedBinary.class, Boolean.TRUE);
      s_validatorMap.put(Locale.class, Boolean.TRUE);
      s_validatorMap.put(TimeZone.class, Boolean.TRUE);
      s_validatorMap.put(TZ.UTC.getClass(), Boolean.TRUE);
      s_validatorMap.put(SimpleTimeZone.class, Boolean.TRUE);
      s_validatorMap.put(Symbol.class, Boolean.TRUE);
      s_validatorMap.put(char[].class, Boolean.TRUE);
      s_validatorMap.put(byte[].class, Boolean.TRUE);
      s_validatorMap.put(String[].class, Boolean.TRUE);
      s_validatorMap.put(Arrays.asList(new Object[0]).getClass(), Boolean.TRUE);

      s_validatorMap.put(ArrayList.class, new Validator()
      {
         public void validate(Object obj, Lookup identityMap, Context context)
         {
            List list = (List)obj;

            for (int i = 0, n = list.size(); i < n; ++i)
            {
               RPCUtil.validate(list.get(i), identityMap, context);
            }
         }
      });

      s_validatorMap.put(PagedArrayList.class, s_validatorMap.get(ArrayList.class));

      s_validatorMap.put(TransferObject.class, new Validator()
      {
         public void validate(Object obj, Lookup identityMap, Context context)
         {
            TransferObject tobj = (TransferObject)obj;

            RPCUtil.validate(tobj.getOID(), identityMap, context);

            for (PropertyIterator itr = tobj.getIterator(); itr.hasNext();)
            {
               itr.next();
               RPCUtil.validate(itr.getValue(), identityMap, context);
            }
         }
      });

      s_validatorMap.put(BasicObject.class, new Validator()
      {
         public void validate(Object obj, Lookup identityMap, Context context)
         {
            BasicObject bobj = (BasicObject)obj;
            int nCount = bobj.getClassObject().resolveAttributeCount();

            for (int i = 0; i < nCount; i++)
            {
               RPCUtil.validate(bobj.getValue(i, context.getMachine()), identityMap, context);
            }
         }
      });

      s_validatorMap.put(TypedBasicObject.class, s_validatorMap.get(BasicObject.class));

      s_validatorMap.put(OID.class, new Validator()
      {
         public void validate(Object obj, Lookup identityMap, Context context)
         {
            OID oid = (OID)obj;

            for (int i = 0, n = oid.getCount(); i < n; ++i)
            {
               RPCUtil.validate(oid.getValue(i), identityMap, context);
            }
         }
      });

      s_validatorMap.put(Pair.class, new Validator()
      {
         public void validate(Object obj, Lookup identityMap, Context context)
         {
            Pair pair = (Pair)obj;

            for (;;)
            {
               RPCUtil.validate(pair.getHead(), identityMap, context);
               obj = pair.getTail();

               if (obj instanceof Pair)
               {
                  pair = (Pair)obj;
               }
               else
               {
                  RPCUtil.validate(obj, identityMap, context);

                  break;
               }
            }
         }
      });

      s_validatorMap.put(ConstPair.class, s_validatorMap.get(Pair.class));

      s_validatorMap.put(Object[].class, new Validator()
      {
         public void validate(Object obj, Lookup identityMap, Context context)
         {
            Object[] array = (Object[])obj;

            for (int i = 0, n = array.length; i < n; ++i)
            {
               RPCUtil.validate(array[i], identityMap, context);
            }
         }
      });

      s_validatorMap.put(Request.class, new Validator()
      {
         public void validate(Object obj, Lookup identityMap, Context context)
         {
            Request request = (Request)obj;

            RPCUtil.validate(request.getCorrelator(), identityMap, context);

            for (int i = 0, n = request.getInvocationCount(); i < n; ++i)
            {
               Request.Invocation invocation = request.getInvocation(i);

               RPCUtil.validate(invocation.getObject(), identityMap, context);
               RPCUtil.validate(invocation.getArguments(), identityMap, context);
               RPCUtil.validate(invocation.getAttributes(), identityMap, context);
            }

            for (int i = 0, n = request.getFilterCount(); i < n; ++i)
            {
               RPCUtil.validate(request.getFilter(i), identityMap, context);
            }
         }
      });
   }

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(RPCUtil.class);

   // constructors

   /**
    * Prevents construction.
    */
   protected RPCUtil()
   {
   }

   // operations

   /**
    * Validates that a request object graph contains only allowed classes.
    * @param obj The object to validate.
    * @param identityMap Map for tracking object identity.
    * @throws RPCException if the graph is invalid.
    */
   public static void validate(Object obj, Lookup identityMap, Context context) throws RPCException
   {
      if (obj != null)
      {
         Object validator = s_validatorMap.get(obj.getClass());

         if (validator == null)
         {
            throw new RPCException("err.rpc.type", new Object[]{obj.getClass().getName()});
         }

         if (validator != Boolean.TRUE && !identityMap.contains(obj))
         {
            identityMap.put(obj, null);
            ((Validator)validator).validate(obj, identityMap, context);
         }
      }
   }

   /**
    * Instantiates an object from a cached transfer object graph.
    * @param obj The first object graph node.
    * @param identityMap Map for tracking object identity.
    * @param context The invocation context.
    * @return The instantiated value.
    */
   public static Object instantiateDirect(Object obj, Lookup identityMap, InvocationContext context)
   {
      return new InstanceFactory(identityMap, null, InstanceFactory.STATE, context).instantiate(obj);
   }

   /**
    * Instantiates a clean object from a cached transfer object graph (does not instantiate any attributes).
    * @param obj The first object graph node.
    * @param identityMap Map for tracking object identity.
    * @param context The invocation context.
    * @return The instantiated value.
    */
   public static Object instantiateClean(Object obj, Lookup identityMap, InvocationContext context)
   {
      return new InstanceFactory(identityMap, null, InstanceFactory.IDENTITY, context).instantiate(obj);
   }

   /**
    * Instantiates a value, for which the type is not known in advance.
    * @param value The argument value.
    * @param identityMap Map for tracking object identity.
    * @param context The invocation context.
    * @param fixupList The fixup list for the second pass, updated by this method.
    *    Null to instantiate from cache.
    * @param nInst Instantiation mode, one of the INST_* constants.
    * @return The instantiated value.
    * @deprecated Use InstanceFactory instead.
    */
   public static Object instantiate1(Object value, Lookup identityMap, InvocationContext context, List fixupList, int nInst)
   {
      return new InstanceFactory(identityMap, fixupList, nInst, context).instantiate(value);
   }
   
   /**
    * First instantiation pass: creates the instances,
    * checks access rights and populates the fixup list.
    * @param tobj The transfer object to instantiate.
    * @param identityMap The object identity map, updated by this method.
    * @param context The invocation context.
    * @param fixupList The fixup list for the second pass, updated by this method.
    *    Null to instantiate from cache.
    * @param nInst Instantiation mode, one of the INST_* constants.
    * @return The instance corresponding to the transfer object.
    * @deprecated Use InstanceFactory instead.
    */
   public static Instance instantiate1(TransferObject tobj, Lookup identityMap,
      InvocationContext context, List fixupList, int nInst)
   {
      return new InstanceFactory(identityMap, fixupList, nInst, context).instantiate(tobj);
   }

   /**
    * Instantiate an instance previously transferred.
    * 
    * @param tobj The transfer object to instantiate.
    * @param context The context in which to instantiate the instance.
    * @param bSecure True to perform the instantiation in a secure context.
    * @return The newly instantiated instance.
    */
   public static Instance instantiate(TransferObject tobj, InvocationContext context, boolean bSecure)
   {
      Instance inst = null;
      boolean bSecured = context.isSecure();

      try
      {
         context.setSecure(bSecure);

         InstanceFactory instanceFactory = new InstanceFactory(new HashTab(), new ArrayList(), InstanceFactory.STATE, context);

         inst = instanceFactory.instantiate(tobj);
         instanceFactory.complete();
      }
      finally
      {
         context.setSecure(bSecured);
      }

      return inst;
   }

   /**
    * Converts the object state into transfer objects.
    * @param obj The object to convert.
    * @param attributes The attribute list to transfer.
    * @param nTF Combination of TF_* flags.
    * @return The converted object.
    */
   public static Object transfer(Object obj, Pair attributes, int nTF)
   {
      return transfer(obj, attributes, null, new HashTab(), nTF);
   }

   /**
    * Converts the object state into transfer objects.
    * @param obj The object to convert.
    * @param attributes The attribute list to transfer.
    * @param identityMap Map for tracking object identity.
    * @param nTF Combination of TF_* flags.
    * @return The converted object.
    */
   public static Object transfer(Object obj, Pair attributes, Lookup identityMap, int nTF)
   {
      return transfer(obj, attributes, null, identityMap, nTF);
   }

   /**
    * Converts instance state into transfer objects.
    * @param obj The object to convert.
    * @param attributes The attribute list to transfer.
    * @param diffMap Map of instances to original transfer objects. If non-null, only the modified values are transferred.
    * @param identityMap Map for tracking object identity.
    * @param nTF Combination of TF_* flags.
    * @return The converted object.
    */
   public static Object transfer(Object obj, Pair attributes, Lookup diffMap, Lookup identityMap, int nTF)
   {
      if (obj instanceof Instance)
      {
         return transfer((Instance)obj, attributes, diffMap, identityMap, nTF);
      }

      if (obj instanceof InstanceList)
      {
         return transfer((InstanceList)obj, attributes, diffMap, identityMap, nTF);
      }

      return obj;
   }

   /**
    * Converts the object state into transfer objects.
    * @param instance The object to convert.
    * @param attributes The attribute list to transfer.
    * @param diffMap Map of instances to original transfer objects. If non-null, only the modified values are transferred.
    * @param identityMap Map for tracking object identity.
    * @param nTF Combination of TF_* flags.
    * @param bClone True to clone the instance and not copy the OID.
    * @return The converted object.
    */
   protected static TransferObject transfer(Instance instance, Pair attributes, Lookup diffMap, Lookup identityMap, int nTF)
   {
      Metaclass metaclass = (attributes != null || (nTF & TF_LAZY) == 0) ? instance.getMetaclass() : instance.getLazyMetaclass();
      Attribute lockingAttribute = (metaclass.getPersistenceMapping() == null) ? null : metaclass.getPersistenceMapping().getLockingAttribute();
      TransferObject tobj = (TransferObject)identityMap.get(instance);
      TransferObject orig = (diffMap == null) ? null : (TransferObject)diffMap.get(instance);
      boolean bMerge;

      if (tobj == null)
      {
         if ((nTF & (TF_READABLE | TF_ALL)) == TF_READABLE && !instance.isReadable())
         {
            return ((nTF & (TF_HIDDEN | TF_REF)) == (TF_HIDDEN | TF_REF)) ? new TransferObject(metaclass.getName(), 0) : null;
         }

         tobj = new TransferObject(metaclass.getName());

         if ((nTF & TF_CLONE) == 0)
         {
            tobj.setOID(instance.getOID());
         }

         if ((nTF & TF_CACHE) == 0)
         {
            if ((nTF & TF_CLONE) != 0)
            {
               tobj.setEventName("create");

               if ((nTF & TF_IDENTITY) == 0 || instance.getOID() == null)
               {
                  nTF |= TF_STATE;
               }
            }
            else
            {
               switch (instance.getState())
               {
                  case Instance.NEW:
                     tobj.setEventName("create");
   
                     if ((nTF & TF_IDENTITY) == 0 || instance.getOID() == null)
                     {
                        nTF |= TF_STATE;
                     }
   
                     break;
   
                  case Instance.DIRTY:
                     tobj.setEventName("update");
                     break;
   
                  case Instance.DELETED:
                     tobj.setEventName("delete");
                     break;
               }
            }
         }

         if (instance.isLazy())
         {
            tobj.setVersion((short)-1);
         }

         identityMap.put(instance, tobj);
         bMerge = false;
      }
      else
      {
         bMerge = true;
      }

      nTF |= TF_READABLE | TF_REF;

      boolean bOld = (nTF & TF_OLD) != 0 || ((nTF & TF_DELETED) != 0 && instance.getState() == Instance.DELETED);

      if ((nTF & TF_DIRTY) != 0)
      {
         for (Iterator iter = instance.getMetaclass().getInstanceAttributeIterator(); iter.hasNext();)
         {
            Attribute attribute = (Attribute)iter.next();

            if (instance.isUpdateable(attribute) && instance.isDirty(attribute.getOrdinal()))
            {
               attributes = new Pair(attribute.getSymbol(), attributes);
            }
         }
      }

      for (; attributes != null; attributes = attributes.getNext())
      {
         Object head = attributes.getHead();
         String sName;
         Pair pair;

         if (head instanceof Pair)
         {
            Pair field = (Pair)head;
            Symbol sym = (Symbol)field.getHead();

            if (Symbol.ATAT.equals(sym))
            {
               field = field.getNext();

               if (metaclass.getMetadata().getMetaclass(
                  ((Symbol)field.getHead()).getName()).isUpcast(metaclass))
               {
                  transfer(instance, field.getNext(), diffMap, identityMap, nTF & ~TF_CLONE);
               }

               continue;
            }

            if (Symbol.COLON.equals(sym))
            {
               sName = ((Symbol)field.getNext().getHead()).getName();
               tobj.setValue(sName, transfer(instance.findAnnotation(sName), null, diffMap, identityMap, nTF & ~TF_CLONE));

               continue;
            }

            sName = sym.getName();
            pair = field.getNext();
         }
         else
         {
            sName = ((Symbol)head).getName();

            if (bMerge && tobj.hasValue(sName))
            {
               continue;
            }

            pair = null;
         }

         Attribute attribute = metaclass.getAttribute(sName);
         Object value = (attribute.isStatic()) ? metaclass.getValue(attribute.getOrdinal()) :
            (bOld) ? instance.getOldValue(attribute.getOrdinal()) : instance.getValue(attribute.getOrdinal());

         if (value instanceof Instance)
         {
            tobj.setValue(sName, transfer((Instance)value, pair, diffMap, identityMap, nTF & ~TF_CLONE));
         }
         else if (value instanceof InstanceList)
         {
            tobj.setValue(sName, transfer((InstanceList)value, pair, diffMap, identityMap, nTF & ~TF_CLONE));
         }
         else if (orig == null || attribute == lockingAttribute ||
            !ObjUtil.equal(value, orig.findValue(sName, Undefined.VALUE)))
         {
            tobj.setValue(sName, value);
         }
      }

      // For non-persisted objects, add all the attributes with read/write access
      if (!bMerge && (nTF & TF_STATE) != 0 &&
         (tobj.getOID() == null || instance.getState() == Instance.NEW))
      {
         if (lockingAttribute != null)
         {
            instance.getValue(lockingAttribute.getOrdinal());
         }

         for (int i = 0, n = metaclass.getInstanceAttributeCount(); i < n; ++i)
         {
            Attribute attribute = metaclass.getInstanceAttribute(i);
            Object value = instance.getValueDirect(attribute.getOrdinal());

            if ((!(value instanceof Undefined) && !tobj.hasValue(attribute.getName()) &&
               ((instance.isReadable(attribute) && instance.isUpdateable(attribute)) || ((nTF & TF_CLONE) != 0 && !attribute.isReadOnly())) &&
               (!attribute.isCalculated() || instance.isOverridden(attribute.getOrdinal())))
               )
            {
               if (value instanceof Instance)
               {
                  tobj.setValue(attribute.getName(), transfer((Instance)value, null, diffMap, identityMap, nTF & ~TF_CLONE));
               }
               else if (value instanceof InstanceList)
               {
                  tobj.setValue(attribute.getName(), transfer((InstanceList)value, null, diffMap, identityMap, nTF & ~TF_CLONE));
               }
               else if ((orig == null ||
                  attribute == lockingAttribute ||
                  !ObjUtil.equal(value, orig.findValue(attribute.getName(), Undefined.VALUE))) &&
                  ((nTF & TF_SERIALIZABLE) == 0 ||
                   attribute.getType() != Primitive.ANY ||
                   TextMarshaller.isSerializable(value)))
               {
                  tobj.setValue(attribute.getName(), value);
               }
            }
         }
      }

      if ((nTF & TF_CACHE) != 0)
      {
         instance.setCached(true);
      }

      return tobj;
   }

   /**
    * Converts instance state into transfer objects.
    * @see RPCUtil#transfer(Object, Pair, Lookup, Lookup, int)
    */
   protected static List transfer(InstanceList ilist, Pair attributes, Lookup diffMap, Lookup identityMap, int nTF)
   {
      nTF &= ~TF_REF; 

      List list = new ArrayList(ilist.size());

      for (Iterator itr = ilist.iterator(); itr.hasNext();)
      {
         Object tobj = transfer(itr.next(), attributes, diffMap, identityMap, nTF);

         if (tobj != null)
         {
            list.add(tobj);
         }
      }

      return list;
   }

   /**
    * Converts the object state into transfer objects.
    * The attributes are not considered for the objects with OID, unless present in valueSet. 
    * @param obj The object to convert.
    * @param valueSet Set specifying the instances to be passed by value: [Instance].
    * @param identityMap Map for tracking object identity.
    * @param nTF Combination of TF_* constants.
    * @return The converted object.
    */
   public static Object transferState(Object obj, Set valueSet, Lookup identityMap, int nTF)
   {
      if (obj instanceof Instance)
      {
         Instance instance = (Instance)obj;
         Object result = identityMap.get(obj);

         if (result != null)
         {
            return result;
         }

         Metaclass metaclass = instance.getMetaclass();

         if ((nTF & (TF_READABLE | TF_ALL)) == TF_READABLE && !instance.isReadable())
         {
            return ((nTF & (TF_HIDDEN | TF_REF)) == (TF_HIDDEN | TF_REF)) ? new TransferObject(metaclass.getName(), 0) : null;
         }

         TransferObject tobj = new TransferObject(metaclass.getName());

         tobj.setOID(instance.getOID());
         identityMap.put(obj, tobj);

         if (instance.getOID() != null && metaclass.getPersistenceMapping() != null)
         {
            if (valueSet == null || !valueSet.contains(instance))
            {
               return tobj;
            }

            tobj.setEventName("create");
         }

         nTF |= TF_READABLE | TF_REF;

         for (int i = 0, nCount = metaclass.getInstanceAttributeCount(); i < nCount; ++i)
         {
            Attribute attribute = metaclass.getInstanceAttribute(i);
            Type type = attribute.getType();
            Object value;

            if (type.isPrimitive())
            {
               value = instance.getValue(attribute.getOrdinal());

               if ((nTF & TF_SERIALIZABLE) != 0 &&
                  type == Primitive.ANY &&
                  !TextMarshaller.isSerializable(value))
               {
                  continue;
               }
            }
            else
            {
               value = transferState(instance.getValue(attribute.getOrdinal()), valueSet, identityMap, nTF);
            }

            tobj.setValue(attribute.getName(), value);
         }

         if ((nTF & TF_CACHE) != 0)
         {
            instance.setCached(true);
         }

         return tobj;
      }

      if (obj instanceof InstanceList)
      {
         nTF &= ~TF_REF; 

         Collection col = (Collection)obj;
         List list = new ArrayList(col.size());

         for (Iterator itr = col.iterator(); itr.hasNext();)
         {
            Object tobj = transferState(itr.next(), valueSet, identityMap, nTF);

            if (tobj != null)
            {
               list.add(tobj);
            }
         }

         return list;
      }

      return obj;
   }

   /**
    * Converts the object state into transfer objects.
    * Only attributes which values have been updated are considered for the objects with OID. 
    * @param obj The object to convert.
    * @param identityMap Map for tracking object identity.
    * @param nTF Combination of TF_* flags.
    * @return The converted object.
    */
   public static Object transferUpdates(Object obj, Lookup identityMap, int nTF)
   {
      if (obj instanceof Instance)
      {
         Instance instance = (Instance)obj;
         Object result = identityMap.get(obj);

         if (result != null)
         {
            return result;
         }

         Metaclass metaclass = instance.getMetaclass();

         if ((nTF & (TF_READABLE | TF_ALL)) == TF_READABLE && !instance.isReadable())
         {
            return ((nTF & (TF_HIDDEN | TF_REF)) == (TF_HIDDEN | TF_REF)) ? new TransferObject(metaclass.getName(), 0) : null;
         }

         TransferObject tobj = new TransferObject(metaclass.getName());
         tobj.setOID(instance.getOID());

         switch (instance.getState())
         {
            case Instance.NEW:
               tobj.setEventName("create");
               nTF |= TF_STATE;
               break;

            case Instance.DIRTY:
               tobj.setEventName("update");
               break;

            case Instance.DELETED:
               tobj.setEventName("delete");
               break;
         }

         identityMap.put(obj, tobj);

         nTF |= TF_READABLE | TF_REF;

         for (int i = 0, nCount = metaclass.getInstanceAttributeCount(); i < nCount; ++i)
         {
            Attribute attribute = metaclass.getInstanceAttribute(i);

            if (attribute.isPersistent() && instance.isDirty(attribute.getOrdinal()))
            {
               Type type = attribute.getType();
               Object value;

               if (type.isPrimitive())
               {
                  value = instance.getValue(attribute.getOrdinal());

                  if ((nTF & TF_SERIALIZABLE) != 0 &&
                     type == Primitive.ANY &&
                     !TextMarshaller.isSerializable(value))
                  {
                     continue;
                  }
               }
               else
               {
                  value = transferUpdates(instance.getValue(attribute.getOrdinal()), identityMap, nTF);
               }

               tobj.setValue(attribute.getName(), value);
            }
         }

         // Set the read access attribute value
         Attribute attribute = metaclass.getReadAccessAttribute();

         if (attribute != null)
         {
            tobj.setValue(attribute.getName(), Boolean.valueOf(instance.isReadable()));
         }

         return tobj;
      }

      if (obj instanceof InstanceList)
      {
         nTF &= ~TF_REF; 

         Collection col = (Collection)obj;
         List list = new ArrayList(col.size());

         for (Iterator itr = col.iterator(); itr.hasNext();)
         {
            Object tobj = transferUpdates(itr.next(), identityMap, nTF);

            if (tobj != null)
            {
               list.add(tobj);
            }
         }

         return list;
      }

      return obj;
   }

   /**
    * Converts the object state into transfer objects.
    * The attributes are not considered for the objects with OID, unless specified in the attributes. 
    * @param obj The object to convert.
    * @param attributes Attribute list specifying the instances to be passed by value.
    * @param identityMap Map for tracking object identity.
    * @param nTF Combination of TF_* flags.
    * @return The converted object.
    */
   public static Object transferByRefAttributes(Object obj, Pair attributes, Lookup identityMap, int nTF)
   {
      Set instanceSet = new HashHolder();
   
      collectInstances(obj, attributes, instanceSet);
   
      return transferState(obj, instanceSet, identityMap, nTF);
   }

   /**
    * Collects in a map the instances according to the attribute list.
    * @param obj The starting object of the graph.
    * @param attributes The attribute list.
    * @param instanceSet The destination set: [Instance].
    */
   public static void collectInstances(Object obj, Pair attributes, Set instanceSet)
   {
      if (obj == null)
      {
         return;
      }

      if (obj instanceof Instance)
      {
         Instance instance = (Instance)obj;

         instanceSet.add(instance);

         for (; attributes != null; attributes = attributes.getNext())
         {
            Object head = attributes.getHead();

            if (head instanceof Pair)
            {
               Pair field = (Pair)head;
               Symbol sym = (Symbol)field.getHead();

               if (Symbol.ATAT.equals(sym))
               {
                  field = field.getNext();

                  if (instance.getMetaclass().getMetadata().getMetaclass(
                     ((Symbol)field.getHead()).getName()).isUpcast(instance.getMetaclass()))
                  {
                     collectInstances(instance, field.getNext(), instanceSet);
                  }
               }
               else
               {
                  collectInstances(instance.getValue(sym.getName()), field.getNext(), instanceSet);
               }
            }
            else
            {
               collectInstances(instance.getValue(((Symbol)head).getName()), null, instanceSet);
            }
         }
      }

      if (obj instanceof InstanceList)
      {
         for (Iterator itr = ((Collection)obj).iterator(); itr.hasNext();)
         {
            collectInstances(itr.next(), attributes, instanceSet);
         }
      }
   }

   /**
    * Parses a string using a specified parser.
    * @param parser The parser to use.
    * @param sText The string to parse. Can be null.
    * @param defaultValue The default value, if sText is empty.
    * @return The parsed value.
    */
   public static Object parse(Parser parser, String sText, Object defaultValue)
   {
      if (StringUtil.isEmpty(sText))
      {
         return defaultValue;
      }
   
      Reader reader = new StringReader(sText);
   
      try
      {
         Object value = parser.parse(reader, null);
   
         if (value == Parser.EOF)
         {
            return defaultValue;
         }
   
         if (parser.parse(reader, null) != Parser.EOF)
         {
            throw new RPCException("err.rpc.expression", new Object[]{sText});
         }
   
         return value;
      }
      catch (ParserException e)
      {
         throw new RPCException("err.rpc.expression", new Object[]{sText}, e);
      }
   }

   /**
    * Audits unconditionally a single instance or a static event.
    * @param instance The instance (or null for static object).
    * @param tobj The property map containing the audited attributes.
    * @param sClass The affected class.
    * @param sEvent The event name to audit.
    * @param nArgCount The event argument count.
    * @param sAuditClass The audit class name.
    * @param nMode The audit mode, one of the AUDIT_* constants.
    * @param context The invocation context.
    * @return The audit log instance.
    */
   public static Instance audit(Instance instance, PropertyMap tobj,
      String sClass, String sEvent, int nArgCount,
      String sAuditClass, byte nMode, InvocationContext context)
   {
      UnitOfWork uowSaved = context.setAuditUnitOfWork();
      boolean bSecureSaved = context.isSecure();

      try
      {
         context.setSecure(false);

         Instance audit = new Instance(context.getMetadata().getMetaclass(sAuditClass), context);

         audit.setNew();
         audit.setValue("object", instance);
         audit.setValue("class", sClass);
         audit.setValue("event", sEvent);
         audit.setValue("argCount", Primitive.createInteger(nArgCount));
         audit.setValue("user", context.getUser());

         if (nMode != AUDIT_NONE && (instance != null || nMode == AUDIT_MAP))
         {
            PropertyMap map = (PropertyMap)audit.getValue("values");

            for (PropertyIterator itr = tobj.getIterator(); itr.hasNext();)
            {
               itr.next();

               if (nMode <= AUDIT_MAP)
               {
                  Object value;

                  if (nMode == AUDIT_NAMES)
                  {
                     value = null;
                  }
                  else
                  {
                     value = itr.getValue();

                     if (!TextMarshaller.isSerializable(value))
                     {
                        value = String.valueOf(value);
                     }
                  }

                  map.setValue(itr.getName(), value);
               }
               else
               {
                  Attribute attr = instance.getMetaclass().findAttribute(itr.getName());

                  if (attr != null && !attr.isStatic() && attr.getVisibility() == Metaclass.PUBLIC)
                  {
                     Object value = instance.getValueDirect(attr.getOrdinal());

                     if (!(value instanceof Undefined))
                     {
                        if (!TextMarshaller.isSerializable(value))
                        {
                           value = String.valueOf(value);
                        }

                        if (nMode == AUDIT_VALUES)
                        {
                           if (value != null)
                           {
                              map.setValue(attr.getName(), value);
                           }
                        }
                        else
                        {
                           Object oldValue = instance.getOldValueDirect(attr.getOrdinal());

                           if (!ObjUtil.equal(value, oldValue))
                           {
                              map.setValue(attr.getName(), value);

                              if (!(oldValue instanceof Undefined))
                              {
                                 map.setValue('-' + attr.getName(), oldValue);
                              }
                           }
                        }
                     }
                  }
               }
            }
         }

         audit.invoke("create");

         return audit;
      }
      finally
      {
         context.setSecure(bSecureSaved);
         context.setUnitOfWork(uowSaved);
      }
   }

   /**
    * Audits a single instance or a static event.
    * @param instance The instance (or null for static object).
    * @param tob The property map containing the audited attributes.
    * @param event The event to audit.
    * @param bForce True to force the audit regardless of the metadata setting. 
    * @param context The invocation context.
    * @return The audit log instance, or null if none created.
    */
   public static Instance audit(Instance instance, PropertyMap tobj, Event event, boolean bForce, InvocationContext context)
   {
      if (bForce || event.isAudited())
      {
         Symbol symbol = event.getSymbol();
         int nArgCount = event.getArgumentCount(); 
         boolean bStatic = event.isStatic();

         if (instance == null ||
            instance.getUnitOfWork() == null ||
            !instance.getUnitOfWork().isTransient() ||
            nArgCount == 6 && Symbol.READ.equals(symbol) && bStatic && instance.getOID() != null)
         {
            byte nMode = (bStatic) ? AUDIT_MAP : AUDIT_NONE;

            switch (nArgCount)
            {
               case 0:
                  if (!bStatic)
                  {
                     if (Symbol.CREATE.equals(symbol) || Symbol.DELETE.equals(symbol))
                     {
                        nMode = AUDIT_VALUES;
                     }
                     else if (Symbol.UPDATE.equals(symbol))
                     {
                        nMode = AUDIT_CHANGES;
                     }
                  }

                  break;

               case 6:
                  if (bStatic && Symbol.READ.equals(symbol))
                  {
                     nMode = AUDIT_NAMES;
                  }

                  break;
            }

            return audit(instance, tobj, event.getMetaclass().getName(), event.getName(),
               event.getArgumentCount(), Metadata.AUDIT_LOG_CLASS_NAME, nMode, context);
         }
      }

      return null;
   }

   /**
    * Audits a single instance or a static event.
    * @param instance The instance (or null for static object).
    * @param tob The property map containing the audited attributes.
    * @param event The event to audit.
    * @param context The invocation context.
    * @return The audit log instance, or null if none created.
    */
   public static Instance audit(Instance instance, PropertyMap tobj, Event event, InvocationContext context)
   {
      return audit(instance, tobj, event, false, context);
   }

   /**
    * Logs an exception.
    * For system exceptions, a reference exception is created and returned.
    * @param t The exception to handle.
    * @param bSystem True if it is a system exception.
    * @param sType The request type name.
    * @param logger The logger to use.
    */
   public static Throwable handleException(Throwable t, boolean bSystem, String sType, Logger logger)
   {
      if (bSystem)
      {
         ReferenceException e = new ReferenceException();

         logger.error("Error processing the " + sType + " request (reference id " + e.getId() + ")", t);

         return e;
      }

      int nLevel = (ObjUtil.isError(t)) ? Logger.ERROR : Logger.DEBUG;

      if (logger.isLevelEnabled(nLevel))
      {
         logger.log(nLevel, "Error processing the " + sType + " request", t);
      }

      return t;
   }

   /**
    * Determines whether a given exception is a system error,
    * which should not be shown directly to end-users. 
    * @param t The exception to test.
    * @return True if the exception is an error.
    */
   public static boolean isSystem(Throwable t)
   {
      if (t instanceof RequestException)
      {
         return false;
      }

      return ObjUtil.isSystem(t);
   }

   /**
    * Checks if the invocation context user has the generic RPC privilege.
    * @param context The invocation context.
    * @throws SecurityViolationException if the user does not have the privilege.
    */
   public static void checkPrivilege(InvocationContext context) throws SecurityViolationException
   {
      PrimitivePrivilege privilege = context.getMetadata().getGenericRPCPrivilege();

      if (privilege != null && !context.getPrivilegeSet().contains(privilege))
      {
         throw new SecurityViolationException("err.rpc.privilege",
            new Object[]{context.getPrincipal().getName(), privilege.getName()}); 
      }
   }
   
   // inner classes

   /**
    * Interface implemented by RPC graph validators.
    */
   protected static interface Validator
   {
      /**
       * Validates an RPC object.
       * @param obj The object to validate.
       * @param identityMap The identity map for keeping track of visited objects.
       */
      void validate(Object obj, Lookup identityMap, Context context) throws RPCException;
   }
}
