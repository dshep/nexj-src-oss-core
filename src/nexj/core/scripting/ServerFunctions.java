// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import nexj.core.meta.Accessor;
import nexj.core.meta.Attribute;
import nexj.core.meta.Event;
import nexj.core.meta.Metaclass;
import nexj.core.meta.PrimitivePrivilege;
import nexj.core.meta.TypeMismatchException;
import nexj.core.persistence.OID;
import nexj.core.rpc.RPCUtil;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InstanceArrayList;
import nexj.core.runtime.InstanceList;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.SecurityViolationException;
import nexj.core.runtime.UnitOfWork;
import nexj.core.scripting.Intrinsic.JavaIntrinsicFunction;
import nexj.core.scripting.Intrinsic.UnaryJavaIntrinsicFunction;
import nexj.core.util.Undefined;

/**
 * Server-only functions.
 */
public class ServerFunctions extends ClientFunctions
{
   /**
    * @see nexj.core.scripting.Intrinsic.IsolatedFunctions#getInPrivilegePFunction()
    */
   public IntrinsicFunction getInPrivilegePFunction()
   {
      return new JavaIntrinsicFunction()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            verifyArgCount(nArgCount, 1);
            
            Object name = machine.getArg(0, nArgCount);
            
            if (!(name instanceof String))
            {
               throw new TypeMismatchException(getSymbol());
            }

            InvocationContext context = (InvocationContext)machine.getContext();
            PrimitivePrivilege privilege = context.getMetadata().getPrimitivePrivilege((String)name);

            machine.returnValue(Boolean.valueOf(context.getPrivilegeSet().contains(privilege)), nArgCount);

            return false;
         }

         public Symbol getSymbol()
         {
            return Symbol.IN_PRIVILEGE_P;
         }
      };
   }

   /**
    * @see nexj.core.scripting.Intrinsic.IsolatedFunctions#getInstanceFunction()
    */
   public IntrinsicFunction getInstanceFunction()
   {
      return new JavaIntrinsicFunction()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            verifyMinArgCount(nArgCount, 2);
            
            Object classObj = machine.getArg(0, nArgCount);
            Object oidArray = machine.getArg(1, nArgCount);
            
            if (!(classObj instanceof Metaclass))
            {
               throw new TypeMismatchException(getSymbol());
            }
            
            InvocationContext context = (InvocationContext)machine.getContext();
            Metaclass metaclass = (Metaclass)classObj;
            Instance instance;

            if (oidArray != null)
            {
               OID oid;
               
               if (oidArray instanceof Object[])
               {
                  oid = new OID((Object[])oidArray);
               }
               else if (oidArray instanceof OID)
               {
                  oid = (OID)oidArray;
               }
               else
               {
                  throw new TypeMismatchException(getSymbol());
               }

               instance = context.findInstance(metaclass, oid);

               if (instance == null)
               {
                  instance = new Instance(metaclass, context);
                  instance.cache(oid);
               }
            }
            else
            {
               instance = new Instance(metaclass, context);
               instance.setNew();
            }

            for (int i = 2; i < nArgCount; ++i)
            {
               Object obj = machine.getArg(i, nArgCount);

               if (!(obj instanceof Pair))
               {
                  throw new TypeMismatchException(getSymbol());
               }

               Pair pair = (Pair)obj;
               
               if (!(pair.getHead() instanceof Symbol))
               {
                  throw new TypeMismatchException(getSymbol());
               }

               Attribute attribute = metaclass.getAttribute((Symbol)pair.getHead());

               if (attribute.isStatic())
               {
                  throw new ScriptingException("err.scripting.staticAttribute",
                     new Object[]{attribute.getName(), metaclass.getName()});            
               }

               instance.setOldValueDirect(attribute.getOrdinal(), pair.getTail());
            }

            if (oidArray == null)
            {
               for (int i = 0, n = metaclass.getInstanceAttributeCount(); i != n; ++i)
               {
                  Attribute attribute = metaclass.getInstanceAttribute(i);
                  
                  if (instance.getValueDirect(attribute.getOrdinal()) instanceof Undefined &&
                     attribute.getInitializerFunction() != null)
                  {
                     instance.setValueDirect(attribute.getOrdinal(),
                        machine.invoke(attribute.getInitializerFunction(), instance, (Object[])null));
                  }
               }
            }

            machine.returnValue(instance, nArgCount);

            return false;
         }

         public Symbol getSymbol()
         {
            return Symbol.INSTANCE;
         }
      };
   }

   /**
    * @see nexj.core.scripting.Intrinsic.IsolatedFunctions#getInstanceCollectionFunction()
    */
   public IntrinsicFunction getInstanceCollectionFunction()
   {
      return new JavaIntrinsicFunction()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            InstanceList list = new InstanceArrayList(Math.max(8, nArgCount));
            
            for (int i = 0; i < nArgCount; ++i)
            {
               Object instance = machine.getArg(i, nArgCount);

               if (!(instance instanceof Instance))
               {
                  throw new TypeMismatchException(getSymbol());
               }

               list.add(instance);
            }

            machine.returnValue(list, nArgCount);

            return false;
         }

         public Symbol getSymbol()
         {
            return Symbol.INSTANCE_COLLECTION;
         }
      };
   }

   /**
    * @see nexj.core.scripting.Intrinsic.IsolatedFunctions#getInstanceCollectionPFunction()
    */
   public IntrinsicFunction getInstanceCollectionPFunction()
   {
      return new UnaryJavaIntrinsicFunction()
      {
         protected Object invoke(Object obj)
         {
            return Boolean.valueOf(obj instanceof InstanceList);
         }

         public Symbol getSymbol()
         {
            return Symbol.INSTANCE_COLLECTION_P;
         }
      };
   }

   /**
    * @see nexj.core.scripting.Intrinsic.IsolatedFunctions#getSysAuditFunction()
    */
   public IntrinsicFunction getSysAuditFunction()
   {
      return new JavaIntrinsicFunction()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            if (machine.isStackEmpty(nArgCount))
            {
               InvocationContext context = (InvocationContext)machine.getContext();

               if (context.isAudited())
               {
                  verifyArgCount(nArgCount, 2);

                  PCodeFunction fun = (PCodeFunction)machine.getArg(0, nArgCount);
                  Object[] frame = fun.frame;
                  Object obj;

                  while ((obj = frame[0]) != null)
                  {
                     frame = (Object[])obj;
                  }

                  obj = frame[1];

                  Event event = (Event)machine.getArg(1, nArgCount);
                  Instance instance = (obj instanceof Instance) ? (Instance)obj : null;

                  RPCUtil.audit(instance, instance, event, context);
               }
            }

            machine.returnValue(null, nArgCount);

            return false;
         }

         public Symbol getSymbol()
         {
            return Symbol.SYS_AUDIT;
         }
      };
   }

   /**
    * @see nexj.core.scripting.Intrinsic.IsolatedFunctions#getSysCheckAccessFunction()
    */
   public IntrinsicFunction getSysCheckAccessFunction()
   {
      return new JavaIntrinsicFunction()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            verifyArgCount(nArgCount, 2);

            if (machine.getContext().isSecure())
            {
               Object obj = machine.getArg(0, nArgCount);
               Object ordinal = machine.getArg(1, nArgCount);

               if (!(obj instanceof Accessor) || !(ordinal instanceof Number))
               {
                  throw new TypeMismatchException(getSymbol());
               }

               int nOrdinal = ((Number)ordinal).intValue();
               Accessor accessor = (Accessor)obj;

               if (!Boolean.TRUE.equals(accessor.getValue(nOrdinal)) &&
                  (!(accessor instanceof Instance) ||
                     !Boolean.TRUE.equals(((Instance)accessor).getOldValue(nOrdinal))))
               {
                  throw new SecurityViolationException("err.runtime.access",
                     new Object[]{accessor.getMetaclass().getName()});
               }
            }

            machine.returnValue(null, nArgCount);

            return false;
         }

         public Symbol getSymbol()
         {
            return Symbol.SYS_CHECK_ACCESS;
         }
      };
   }

   /**
    * @see nexj.core.scripting.Intrinsic.IsolatedFunctions#getSysSetDirtyFunction()
    */
   public IntrinsicFunction getSysSetDirtyFunction()
   {
      return new JavaIntrinsicFunction()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            verifyArgCount(nArgCount, 1, 2);
            
            Object obj = machine.getArg(0, nArgCount);
            
            if (!(obj instanceof Instance))
            {
               throw new TypeMismatchException(getSymbol());
            }
            
            Instance instance = (Instance)obj;
            boolean bReset = false;
            
            if (nArgCount > 1)
            {
               Object reset = machine.getArg(1, nArgCount);
               
               if (!(reset instanceof Boolean))
               {
                  throw new TypeMismatchException(getSymbol());
               }
               
               bReset = ((Boolean)reset).booleanValue();
            }
            
            instance.setDirty();
            
            if (bReset)
            {
               for (int i = 0, n = instance.getMetaclass().getInstanceAttributeCount(); i < n; ++i)
               {
                  instance.setOldValueDirect(i, Undefined.VALUE);
               }
            }

            instance.setEventPending(true);
            machine.returnValue(null, nArgCount);

            return false;
         }

         public Symbol getSymbol()
         {
            return Symbol.SYS_SET_DIRTY;
         }
      };
   }

   /**
    * @see nexj.core.scripting.Intrinsic.IsolatedFunctions#getSysSetNewFunction()
    */
   public IntrinsicFunction getSysSetNewFunction()
   {
      return new JavaIntrinsicFunction()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            verifyArgCount(nArgCount, 1);
            
            Object obj = machine.getArg(0, nArgCount);
            
            if (!(obj instanceof Instance))
            {
               throw new TypeMismatchException(getSymbol());
            }
            
            Instance instance = (Instance)obj;

            instance.setNew();
            instance.setEventPending(true);
            machine.returnValue(null, nArgCount);

            return false;
         }

         public Symbol getSymbol()
         {
            return Symbol.SYS_SET_NEW;
         }
      };
   }

   /**
    * @see nexj.core.scripting.Intrinsic.IsolatedFunctions#getSysTxBeginFunction()
    */
   public IntrinsicFunction getSysTxBeginFunction()
   {
      return new JavaIntrinsicFunction()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            verifyArgCount(nArgCount, 0, 1);

            InvocationContext context = (InvocationContext)machine.getContext();
            int nTimeout = -1;

            if (nArgCount > 0)
            {
               Number number = (Number)machine.getArg(0, nArgCount);

               if (number != null)
               {
                  nTimeout = number.intValue();
               }
            }

            machine.returnValue(context.beginTransaction(true, nTimeout), nArgCount);

            return false;
         }

         public Symbol getSymbol()
         {
            return Symbol.SYS_TX_BEGIN;
         }
      };
   }

   /**
    * @see nexj.core.scripting.Intrinsic.IsolatedFunctions#getSysTxCommitFunction()
    */
   public IntrinsicFunction getSysTxCommitFunction()
   {
      return new JavaIntrinsicFunction()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            verifyArgCount(nArgCount, 1);
            ((InvocationContext)machine.getContext()).commitAndResume((UnitOfWork)machine.getArg(0, nArgCount));
            machine.returnValue(null, nArgCount);
            
            return false;
         }

         public Symbol getSymbol()
         {
            return Symbol.SYS_TX_COMMIT;
         }
      };
   }

   /**
    * @see nexj.core.scripting.Intrinsic.IsolatedFunctions#getSysTxMandateFunction()
    */
   public IntrinsicFunction getSysTxMandateFunction()
   {
      return new JavaIntrinsicFunction()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            ((InvocationContext)machine.getContext()).mandateTransaction();
            machine.returnValue(null, nArgCount);
            
            return false;
         }

         public Symbol getSymbol()
         {
            return Symbol.SYS_TX_MANDATE;
         }
      };
   }

   /**
    * @see nexj.core.scripting.Intrinsic.IsolatedFunctions#getSysTxMandateNoneFunction()
    */
   public IntrinsicFunction getSysTxMandateNoneFunction()
   {
      return new JavaIntrinsicFunction()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            ((InvocationContext)machine.getContext()).mandateNoTransaction();
            machine.returnValue(null, nArgCount);
            
            return false;
         }

         public Symbol getSymbol()
         {
            return Symbol.SYS_TX_MANDATE_NONE;
         }
      };
   }

   /**
    * @see nexj.core.scripting.Intrinsic.IsolatedFunctions#getSysTxRequireFunction()
    */
   public IntrinsicFunction getSysTxRequireFunction()
   {
      return new JavaIntrinsicFunction()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            machine.returnValue(((InvocationContext)machine.getContext()).requireTransaction(), nArgCount);
            
            return false;
         }

         public Symbol getSymbol()
         {
            return Symbol.SYS_TX_REQUIRE;
         }
      };
   }

   /**
    * @see nexj.core.scripting.Intrinsic.IsolatedFunctions#getSysTxRollbackFunction()
    */
   public IntrinsicFunction getSysTxRollbackFunction()
   {
      return new JavaIntrinsicFunction()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            verifyArgCount(nArgCount, 1);
            ((InvocationContext)machine.getContext()).rollbackAndResume((UnitOfWork)machine.getArg(0, nArgCount));
            machine.returnValue(null, nArgCount);
            
            return false;
         }

         public Symbol getSymbol()
         {
            return Symbol.SYS_TX_ROLLBACK;
         }
      };
   }

   /**
    * @see nexj.core.scripting.Intrinsic.IsolatedFunctions#getSysTxSuspendFunction()
    */
   public IntrinsicFunction getSysTxSuspendFunction()
   {
      return new JavaIntrinsicFunction()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            machine.returnValue(((InvocationContext)machine.getContext()).suspendTransaction(), nArgCount);

            return false;
         }

         public Symbol getSymbol()
         {
            return Symbol.SYS_TX_SUSPEND;
         }
      };
   }
}
