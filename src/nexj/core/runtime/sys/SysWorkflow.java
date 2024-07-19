// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime.sys;


import java.util.Iterator;
import java.util.concurrent.Semaphore;

import nexj.core.meta.Metaclass;
import nexj.core.meta.MetadataException;
import nexj.core.meta.Primitive;
import nexj.core.meta.workflow.Flow;
import nexj.core.meta.workflow.State;
import nexj.core.meta.workflow.Step;
import nexj.core.meta.workflow.State.ThreadInfo;
import nexj.core.persistence.OID;
import nexj.core.persistence.Query;
import nexj.core.rpc.RPCUtil;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.ActionContext;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InstanceArrayList;
import nexj.core.runtime.InstanceList;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.runtime.UnitOfWork;
import nexj.core.runtime.WorkflowException;
import nexj.core.scripting.PCodeFunction;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.Binary;
import nexj.core.util.GUIDUtil;
import nexj.core.util.Logger;
import nexj.core.util.ObjUtil;

/**
 * Workflow implementation.
 */
public class SysWorkflow implements InvocationContextAware
{
   // constants

   /**
    * Empty binary value.
    */
   protected final static Binary EMPTY_BINARY = new Binary(new byte[]{0x00});

   // associations

   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   /**
    * The cached flow key.
    */
   protected FlowKey m_key;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(SysWorkflow.class);

   // operations

   /**
    * @see nexj.core.runtime.InvocationContextAware#setInvocationContext(nexj.core.runtime.InvocationContext)
    */
   public void setInvocationContext(InvocationContext context)
   {
      m_context = context;
   }

   /**
    * Gets the current version of the flow metadata by name.
    * @param sName The flow name.
    * @return The flow metadata object.
    */
   protected Flow getFlow(String sName)
   {
      return m_context.getMetadata().getWorkflow(sName);
   }

   /**
    * Gets flow metadata by name and version.
    * @param sName The flow name.
    * @param nVersion The flow version.
    * @return The flow metadata object.
    */
   protected Flow getFlow(String sName, int nVersion)
   {
      return m_context.getMetadata().getWorkflow(sName, nVersion);
   }

   /**
    * Retrieves a flow list from the cache.
    * @param metaclass The flow implementation class.
    * @param instance The instance for which to retrieve the flow list.
    * @return The flow instance list.
    */
   protected InstanceList getCachedFlowList(Metaclass metaclass, Instance instance)
   {
      assert instance != null;

      if (m_key == null)
      {
         m_key = new FlowKey();
      }

      m_key.instance = instance;

      return (InstanceList)m_context.getUnitOfWork().getCachedTransient(m_key);
   }

   /**
    * Finds flow metadata by name and version.
    * @param sName The flow name.
    * @param version The flow version.
    * @return The found flow object, or null if not found.
    */
   public Flow findFlow(Metaclass metaclass, String sName, Number version, ActionContext actx)
   {
      return m_context.getMetadata().findWorkflow(sName, version.intValue());
   }

   /**
    * Retrieves all the running flows for a given instance.
    * @param primary The instance for which to retrieve the flows.
    * @return The list of retrieved flows.
    */
   public InstanceList forInstance(Metaclass metaclass, Instance primary, ActionContext actx)
   {
      InstanceList list = getCachedFlowList(metaclass, primary);

      if (list != null && !list.isLazy())
      {
         return list;
      }

      OID oid = primary.getOID();

      if (oid == null)
      {
         if (list != null)
         {
            return list;
         }

         list = new InstanceArrayList(4);
      }
      else
      {
         Pair attributes =
            new Pair(Symbol.define("name"),
            new Pair(Symbol.define("version"),
            new Pair(Symbol.define("serializedState"),
            new Pair(Symbol.define("serializedVariables")))));
         Object where = Pair.attribute("oid").eq(oid.toBinary()).and(
            Pair.attribute("class").eq(primary.getMetaclass().getName()));

         InstanceList ilist = Query.createRead(metaclass, attributes, where, null, -1, 0,
            false, Query.SEC_NONE, m_context).read();

         if (list == null)
         {
            list = ilist;
         }
         else
         {
            for (int i = 0, nCount = ilist.size(); i < nCount; ++i)
            {
               list.add(ilist.getInstance(i), InstanceList.REPLACE | InstanceList.DIRECT);
            }
         }
      }

      list.setLazy(false);
      m_context.getUnitOfWork().cacheTransient(m_key, list);
      m_key = null;

      return list;
   }

   /**
    * Starts a named flow for a given instance.
    * @param primary The instance for which to start the flow.
    * @param sName The flow name.
    * @param bGlobal True to instantiate a unique flow for the instance.
    * @return The flow instance.
    */
   public Instance start(Metaclass metaclass, Instance primary, String sName, boolean bGlobal, ActionContext actx)
   {
      if (primary == null && bGlobal)
      {
         return null;
      }

      InstanceList list = (primary == null) ? null :
         (bGlobal) ? forInstance(metaclass, primary, actx) : getCachedFlowList(metaclass, primary);

      if (list == null && primary != null)
      {
         list = new InstanceArrayList(4);
         list.setLazy(false);
      }
      else if (bGlobal)
      {
         for (int i = 0, n = list.getCount(); i != n; ++i)
         {
            if (list.getInstance(i).getValue("name").equals(sName))
            {
               return null;
            }
         }
      }

      Flow flow = getFlow(sName);

      if (primary != null)
      {
         if (flow.getMetaclass() == null || !flow.getMetaclass().isUpcast(primary.getMetaclass()))
         {
            throw new WorkflowException("err.workflow.invalidClass",
               new Object[]{flow.getName(), primary.getMetaclass().getName()});
         }

         if (primary.getMetaclass().getPersistenceMapping() == null)
         {
            throw new MetadataException("err.meta.workflow.nonPersistentClass",
               new Object[]{primary.getMetaclass().getName(), flow.getFullName()});
         }
      }

      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Starting " + flow + " for " + primary);
      }

      boolean bSecure = m_context.isSecure();
      Instance instance = null;

      try
      {
         m_context.setSecure(false);
         instance = new Instance(metaclass, m_context);

         if (list != null)
         {
            list.add(instance);

            if (m_key == null)
            {
               m_key = new FlowKey();
            }

            m_key.instance = primary;
            m_context.getUnitOfWork().cacheTransient(m_key, list);
            m_key = null;
         }

         State state = new State(flow, true);

         state.setReservedValue(0, instance);
         state.setReservedValue(1, state);

         instance.setNew();
         instance.setValue("name", flow.getName());
         instance.setValue("version", Primitive.createInteger(flow.getVersion()));
         instance.setValue("oid", EMPTY_BINARY);
         instance.setValue("class", (primary == null) ? "" : primary.getMetaclass().getName());
         instance.setValue("local", (bGlobal) ? EMPTY_BINARY : GUIDUtil.generateGUID());
         instance.setValue("object", primary);
         instance.setValue("state", state);
         instance.setValue("serializedState", null);
         instance.setValue("serializedVariables", null);
         instance.invoke("create");
         m_context.setSecure(bSecure);
         instance.invoke("run");
      }
      catch (Throwable e)
      {
         if (instance != null)
         {
            m_context.setSecure(false);
            instance.invoke("delete");

            if (list != null)
            {
               list.remove(instance);
            }
         }

         ObjUtil.rethrow(e);
      }
      finally
      {
         m_context.setSecure(bSecure);
      }

      return instance;
   }

   /**
    * Cleans up the resources.
    */
   public void delete(Instance instance, ActionContext actx)
   {
      Instance object = (Instance)instance.getValue("object");

      if (s_logger.isDebugEnabled())
      {
         State state = (State)instance.getValue("state");

         if (state == null)
         {
            s_logger.debug("Completed " + instance + ((object == null) ? "" : " for " + object));
         }
         else
         {
            s_logger.debug("Completed " + state.getFlow() + ((object == null) ? "" : " for " + object));
         }
      }

      if (object != null)
      {
         InstanceList list = getCachedFlowList(instance.getMetaclass(), object);

         if (list != null)
         {
            list.remove(instance);
            m_context.getUnitOfWork().cacheTransient(m_key, list);
            m_key = null;
         }
      }
   }

   /**
    * Loads the object, state and variables attributes.
    */
   public void load(Instance instance, Pair attributes, ActionContext actx)
   {
      for (; attributes != null; attributes = attributes.getNext())
      {
         if (!(attributes.getHead() instanceof Symbol))
         {
            continue;
         }

         String sName = attributes.getHead().toString();

         if (sName.equals("object"))
         {
            Flow flow = getFlow((String)instance.getValue("name"), ((Number)instance.getValue("version")).intValue());
            String sClass = (String)instance.getValue("class");

            if (flow.getMetaclass() == null || sClass == null || sClass.length() == 0)
            {
               instance.setValue("object", null);
            }
            else
            {
               InstanceList list = (InstanceList)m_context.getMetadata().getMetaclass(sClass).invoke("read",
                  new Object[]{flow.getAttributes(), new Pair(Symbol.AT).eq(OID.fromBinary((Binary)instance.getValue("oid"))),
                     null, null, null, null});

               instance.setValue("object", (list.isEmpty()) ? null : list.get(0));
            }
         }
         else if (sName.equals("state"))
         {
            Flow flow = getFlow((String)instance.getValue("name"), ((Number)instance.getValue("version")).intValue());
            State state = new State(flow, (String)instance.getValue("serializedState"),
               (String)instance.getValue("serializedVariables"), m_context);

            state.setReservedValue(0, instance);
            state.setReservedValue(1, state);

            instance.setValue("state", state);
         }
      }
   }

   /**
    * Used for parallel execution of Fork/Join branches.
    * @param state The current Flow's state object.
    * @param sEntry The name of the Fork/Join step to start execution.
    * @param sFlow The name of the flow.
    * @param nVersion The version of the flow.
    * @param parallelArgs List of lists of arguments to pass, one for each new thread.
    */
   public void invokeParallel(Metaclass metaclass, State state, String sEntry, String sFlow,
      int nVersion, Pair parallelArgs, ActionContext actx)
   {
      Instance flow = state.getFlowInstance();
      int nPermits = 0;
      Iterator itr = Pair.getIterator(parallelArgs);

      while (itr.hasNext())
      {
         Pair args = (Pair)itr.next();

         nPermits += (args == null ? 1 : Pair.length(args));
      }

      Semaphore semaphore = new Semaphore(nPermits, true);

      flow.setValue("serializedVariables", state.serializeValues(m_context));
      flow.setValue("serializedState", state.toString());

      TransferObject cloned = (TransferObject)RPCUtil.transfer(flow, null, RPCUtil.TF_CLONE);

      try
      {
         semaphore.acquire(nPermits);
      }
      catch (InterruptedException e)
      {
         s_logger.error("Semaphore interrupted.");

         return;
      }

      int nBranchIndex = 0;

      itr = Pair.getIterator(parallelArgs);

      while (itr.hasNext())
      {
         Iterator itr2 = Pair.getIterator((Pair)itr.next());

         do
         {
            UnitOfWork oldUOW = null;
            Object arg = (itr2.hasNext() ? itr2.next() : null);

            try
            {
               oldUOW = m_context.beginTransaction();

               m_context.getMetadata().getMetaclass("SysQueue").invoke("invokeLocal", new Object[]{
                  metaclass, Symbol.define("startParallel"),
                  Pair.list(
                     semaphore, cloned, sEntry, Primitive.createInteger(nBranchIndex), sFlow, Primitive.createInteger(nVersion), arg
                  ), Primitive.createInteger(-1), Primitive.createInteger(-1), "SysAsyncActivity"
               });
            }
            catch (Throwable t)
            {
               s_logger.warn("Sending to object queue failed for branch \"" + sEntry + "\".", t);
               --nPermits;
            }
            finally
            {
               m_context.commitAndResume(oldUOW);
            }
         }
         while (itr2.hasNext());

         ++nBranchIndex;
      }

      try
      {
         semaphore.acquire(nPermits);
      }
      catch (InterruptedException e)
      {
         s_logger.error("Semaphore interrupted.");
      }
   }

   /**
    * Used for parallel execution of Fork/Join branches.
    * Start the cloned SysService instance at the specified Fork/Join, running
    * only the branch with the specified index, and release the semaphore when finished.
    * @param semaphore The Semaphore to release when execution is finished.
    * @param inst The cloned SysService instance.
    * @param sEntry The name of the Fork/Join step to start execution.
    * @param nIndex The zero-based index of the Fork/Join branch to execute.
    * @param sFlow The name of the Flow.
    * @param nVersion The version of the Flow.
    * @param arg The argument to pass to the branch that will be executed.
    */
   public void startParallel(Metaclass metaclass, Semaphore semaphore, Instance inst, String sEntry,
      int nIndex, String sFlow, int nVersion, Object arg, ActionContext actx)
   {
      try
      {
         inst.setValue("local", GUIDUtil.generateGUID());

         Flow flow = getFlow(sFlow, nVersion);
         PCodeFunction fun = flow.getRunFunction();
         Step entryStep = flow.getFlowStep(sEntry);
         State state = new State(flow, (String)inst.getValue("serializedState"),
            (String)inst.getValue("serializedVariables"), m_context);

         inst.setValue("state", state);

         state.clear();

         state.setThreadInfo(sEntry, new ThreadInfo(nIndex, arg));
         state.add(entryStep);
         state.setReservedValue(0, inst);
         state.setReservedValue(1, state);
         m_context.getMachine().invoke(state.bind(fun), new Object[]{null});
         m_context.getUnitOfWork().rollback();

         if (inst.getOID() != null)
         {
            inst.delete();
         }

         semaphore.release();
      }
      catch (Throwable t)
      {
         m_context.getUnitOfWork().rollback();
         semaphore.release();
         s_logger.error("Released semaphore \"" + sEntry + "\" due to uncaught exception", t);
         ObjUtil.rethrow(t);
      }
   }

   // inner classes

   /**
    * The flow lookup key for the transient cache.
    */
   public static class FlowKey
   {
      public Instance instance;

      /**
       * @see java.lang.Object#equals(java.lang.Object)
       */
      public boolean equals(Object obj)
      {
         return obj instanceof FlowKey && ((FlowKey)obj).instance.equals(instance);
      }

      /**
       * @see java.lang.Object#hashCode()
       */
      public int hashCode()
      {
         return instance.hashCode() ^ 0xAAA;
      }
   }
}
