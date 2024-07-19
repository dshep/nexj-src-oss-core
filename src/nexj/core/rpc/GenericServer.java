// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nexj.core.meta.Argument;
import nexj.core.meta.Attribute;
import nexj.core.meta.Event;
import nexj.core.meta.Member;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Metadata;
import nexj.core.meta.Selector;
import nexj.core.meta.integration.Channel;
import nexj.core.persistence.OIDHolder;
import nexj.core.rpc.jms.JMSSender;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InstanceList;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.runtime.InvocationContextHolder;
import nexj.core.runtime.SecurityViolationException;
import nexj.core.runtime.UnitOfWork;
import nexj.core.scripting.ConstPair;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.ExceptionHolder;
import nexj.core.util.HashTab;
import nexj.core.util.IdentityHashTab;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;
import nexj.core.util.PropertyIterator;
import nexj.core.util.Undefined;

/**
 * The generic request server implementation.
 */
public class GenericServer implements Server, InvocationContextAware, InvocationContextHolder
{
   // constants

   /**
    * Empty pair.
    */
   protected final static Pair EMPTY_PAIR = new ConstPair(null);

   // attributes

   /**
    * The master server flag. True to complete the invocation context.
    */
   protected boolean m_bMaster = true;

   // associations

   /**
    * The invocation context.
    */
   protected InvocationContext m_context;
   
   /**
    * The server interceptor.
    */
   protected Interceptor m_interceptor;
   
   /**
    * The class logger.
    */
   private final static Logger s_logger = Logger.getLogger(GenericServer.class);

   // operations

   /**
    * Sets the master server flag.
    * @param bMaster The master server flag to set.
    */
   public void setMaster(boolean bMaster)
   {
      m_bMaster = bMaster;
   }

   /**
    * @return The master server flag.
    */
   public boolean isMaster()
   {
      return m_bMaster;
   }
   
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
    * Sets the server interceptor.
    * @param interceptor The server interceptor to set.
    */
   public void setInterceptor(Interceptor interceptor)
   {
      m_interceptor = interceptor;
   }

   /**
    * @return The server interceptor.
    */
   public Interceptor getInterceptor()
   {
      return m_interceptor;
   }

   /**
    * @see nexj.core.rpc.Server#invoke(nexj.core.rpc.Request)
    */
   public Response invoke(Request request)
   {
      boolean bLogEnabledSaved = Logger.isEnabled();
      boolean bAuditedSaved = m_context.isAudited();
      boolean bTransientSaved = m_context.isTransient();
      boolean bStealth = request.isStealth() || m_context.isStealth();
      Object[][] argArray = null;

      try
      {
         if (bStealth)
         {
            Logger.setEnabled(false);
         }

         s_logger.dump(request);

         if (m_context.isProtected())
         {
            RPCUtil.validate(request, new IdentityHashTab(), m_context);
         }

         if (request.getLocale() != null)
         {
            m_context.setLocale(request.getLocale());
         }

         if (request.getTimeZone() != null)
         {
            m_context.setTimeZone(request.getTimeZone());
         }

         Metadata metadata = m_context.getMetadata();

         if (request.getNamespace() != null && metadata.getNamespace() != null)
         {
            if (!metadata.getNamespace().equals(request.getNamespace()))
            {
               throw new RequestVersionException("err.rpc.requestNamespace",
                  new Object[]{metadata.getNamespace(), request.getNamespace()});
            }

            if (!metadata.getVersion().equals(request.getVersion()))
            {
               throw new RequestVersionException("err.rpc.requestVersion",
                  new Object[]{metadata.getVersion(), request.getVersion()});
            }
         }

         UnitOfWork uow = m_context.initUnitOfWork();

         uow.checkLicense();

         if (m_interceptor != null && m_interceptor.isEnabled(m_context))
         {
            m_interceptor.interceptRequest(request);
         }

         if (request.isAsync())
         {
            if (request.isCommit())
            {
               TransferObject properties = new TransferObject(1);

               properties.setValue(JMSSender.USER, m_context.getPrincipal().getName());

               Channel channel = uow.addMessage((String)null, request, properties, -1, -1, false);

               if (s_logger.isDebugEnabled())
               {
                  s_logger.debug("Asynchronous request - forwarding to " + channel.getName());
               }

               m_context.complete(true);
            }

            Response response = new Response();

            if (s_logger.isDumpEnabled())
            {
               s_logger.dump(response);
            }

            return response;
         }

         m_context.setAudited(false);

         boolean bTransient = !request.isCommit();

         uow.setTransient(bTransient);

         // Add to the unit of work the classes to track
         int nFilterCount = request.getFilterCount();
         boolean bInstanceFilter = false;

         for (int i = 0; i < nFilterCount; ++i)
         {
            TransferObject tobj = request.getFilter(i);

            if (tobj == null || tobj.getClassName() == null)
            {
               throw new RequestException("err.rpc.filterTO");
            }

            m_context.track(metadata.getMetaclass(tobj.getClassName()));

            if (!bInstanceFilter)
            {
               bInstanceFilter = tobj.hasValue("instances");
            }
         }

         int nInvocationCount = request.getInvocationCount();
         Lookup identityMap = new HashTab(nInvocationCount << 1);
         Lookup eventMap = new HashTab(nInvocationCount);
         Event[] eventArray = new Event[nInvocationCount];

         argArray = new Object[nInvocationCount][];
         InstanceFactory instanceFactory = new InstanceFactory(identityMap,
            new ArrayList(nInvocationCount << 3), InstanceFactory.STATE | InstanceFactory.PRE,
            m_context);

         // Instantiate the request objects
         for (int i = 0; i < nInvocationCount; ++i)
         {
            Request.Invocation invocation = request.getInvocation(i);
            TransferObject tobj = invocation.getObject();
            String sEventName = invocation.getEventName();

            if (sEventName == null)
            {
               sEventName = tobj.getEventName();
            }

            if (tobj == null || tobj.getClassName() == null || sEventName == null)
            {
               throw new RequestException("err.rpc.requestTO");
            }

            m_context.setTransient(bTransient);

            Object[] arguments = invocation.getArguments();
            Metaclass metaclass = metadata.getMetaclass(tobj.getClassName());
            Selector selector = metaclass.getSelector(sEventName);
            Member member;
            int nArgCount;

            if (arguments != null)
            {
               nArgCount = arguments.length;
               member = selector.getMember(nArgCount);

               if (member.isStatic() && tobj.getValueCount() != 0)
               {
                  throw new RequestException("err.rpc.requestTO");
               }
            }
            else
            {
               nArgCount = tobj.getValueCount();
               member = selector.findMember(0);

               if (member == null || member.isStatic())
               {
                  member = selector.getMember(nArgCount);

                  if (!member.isStatic())
                  {
                     throw new RequestException("err.rpc.argCount",
                        new Object[]{member.getName(), metaclass.getName()});
                  }
               }
               else
               {
                  nArgCount = 0;
               }
            }

            if (member.isAttribute())
            {
               throw new RequestException("err.rpc.attributeInvocation",
                  new Object[]{member.getName(), metaclass.getName()});
            }

            if (m_context.isProtected() && member.getVisibility() != Metaclass.PUBLIC)
            {
               throw new SecurityViolationException("err.rpc.eventVisibility",
                  new Object[]{member.getName(), metaclass.getName()});
            }

            Event event = (Event)member;
            Object[] args = new Object[nArgCount + 1];

            if (arguments != null)
            {
               for (int k = 0; k < nArgCount; ++k)
               {
                  args[k + 1] = instanceFactory.instantiate(arguments[k]); 
               }
            }

            if (event.isStatic())
            {
               args[0] = metaclass;

               if (arguments == null)
               {
                  int nEventArgCount = event.getArgumentCount();

                  if (event.isVarArg())
                  {
                     --nEventArgCount;
                  }

                  for (int k = 0; k < nEventArgCount; ++k)
                  {
                     args[k + 1] = instanceFactory.instantiate(tobj.getValue(event.getArgument(k).getName())); 
                  }

                  if (event.isVarArg())
                  {
                     int nArg = nEventArgCount;

                     for (PropertyIterator itr = tobj.getIterator(); itr.hasNext();)
                     {
                        itr.next();

                        Argument arg = event.findArgument(itr.getName());

                        if (arg == null || arg == event.getArgument(nEventArgCount))
                        {
                           args[++nArg] = new Pair(Symbol.define(itr.getName()), instanceFactory.instantiate(itr.getValue()));
                        }
                     }
                  }
               }

               // Allow auditing of static events
               identityMap.put(tobj, null);
            }
            else
            {
               args[0] = instanceFactory.instantiate(tobj);
            }

            eventArray[i] = event;
            argArray[i] = args;
            eventMap.put(tobj, event);
         }

         Lookup diffMap = null;

         // Instantiate the transfer objects from the instance filters
         if (bInstanceFilter)
         {
            diffMap = new HashTab();

            for (int i = 0; i < nFilterCount; ++i)
            {
               m_context.setTransient(bTransient);

               TransferObject tobj = request.getFilter(i);
               Object instances = tobj.findValue("instances");

               if (instances != null)
               {
                  List instanceList = (List)instances;
                  int nCount = instanceList.size();

                  for (int k = 0; k < nCount; ++k)
                  {
                     tobj = (TransferObject)instanceList.get(k);

                     Instance inst = instanceFactory.instantiate(tobj);

                     instanceList.set(k, inst);

                     if (inst != null)
                     {
                        diffMap.put(inst, tobj);
                     }
                  }
               }
            }
         }

         instanceFactory.complete();
         instanceFactory = null;
         GenericServer.auditRequest(identityMap, eventMap, m_context);
         identityMap = null;

         Object[] resultArray = new Object[nInvocationCount << 1];

         // Unset the pending event flag on the request argument instances
         for (int i = 0; i < nInvocationCount; ++i)
         {
            Object[] args = argArray[i];
            Object obj = args[0];

            if (obj instanceof Instance)
            {
               Instance instance = (Instance)obj;

               if (args.length == 1 && eventArray[i].getName().equals(instance.getPendingEventName()))
               {
                  resultArray[i << 1] = instance;
               }

               instance.suspendEvent();
            }
         }

         m_context.setTransient(false);
         uow.invokePendingEvents(false, false);

         // Invoke the events and accumulate the results
         for (int i = 0; i < nInvocationCount; ++i)
         {
            TransferObject tobj = request.getObject(i);
            Event event = eventArray[i];
            Pair attributes = EMPTY_PAIR;
            Object[] args = argArray[i];
            Object obj = args[0];
            int nAttrIndex = -1;

            m_context.setTransient(event.isTransient(bTransient));

            if (event.getArgumentCount() != 0)
            {
               Argument arg = event.findArgument("attributes");

               if (arg != null)
               {
                  nAttrIndex = arg.getOrdinal() + 1;

                  Object value = args[nAttrIndex];

                  attributes = (value instanceof Pair) ? (Pair)value : null;
               }
            }

            if (nAttrIndex < 0)
            {
               Object value = tobj.findValue("attributes", EMPTY_PAIR);

               if (value != EMPTY_PAIR)
               {
                  nAttrIndex = 0;
                  attributes =  (Pair)value;
               }
            }

            if (event.isStatic() && m_context.isProtected() && m_context.isSecure())
            {
               Metaclass metaclass = event.getMetaclass();
               Argument result = event.getResult();

               if (result != null && !result.getType().isPrimitive())
               {
                  metaclass = (Metaclass)result.getType();
               }

               if (nAttrIndex > 0)
               {
                  Pair security = metaclass.checkReadAccess(attributes, m_context.getPrivilegeSet());
   
                  if (security != null)
                  {
                     args[nAttrIndex] = Pair.nconc(security, attributes);
                  }
               }

               metaclass.checkExpressionAccess(findArg(event, "where", args, tobj),  m_context.getPrivilegeSet());
               metaclass.checkOrderByAccess(findArg(event, "orderBy", args, tobj), m_context.getPrivilegeSet());
            }

            boolean bInvoked = false;
            boolean bDeleted = false;

            m_context.setUnitOfWork(uow);
            m_context.setTransient(event.isTransient(bTransient));

            if (obj instanceof Instance)
            {
               Instance instance = (Instance)obj;
               UnitOfWork instanceUOW = instance.getUnitOfWork();

               // Use the instance UoW
               if (!event.isStatic() && instanceUOW != null && instanceUOW != uow)
               {
                  m_context.setUnitOfWork(instanceUOW);
               }

               bInvoked = instance.invokeSuspendedEvent();
               bDeleted = (instance.getState() == Instance.DELETED);

               if (nAttrIndex <= 0)
               {
                  Attribute attribute = instance.getMetaclass().findAttribute("attributes");

                  if (attribute != null && !attribute.isStatic())
                  {
                     attributes = (Pair)instance.getValue(attribute.getOrdinal());
                  }
               }
            }

            if (resultArray[i << 1] == null)
            {
               if (!bDeleted)
               {
                  resultArray[i << 1] = event.invoke(args, m_context.getMachine());
               }
            }
            else
            {
               if (!bInvoked && !bDeleted)
               {
                  event.invoke(args, m_context.getMachine());
               }

               eventArray[i] = null;
            }

            resultArray[(i << 1) + 1] = attributes;
         }

         // Pre-commit all units of work in the context
         for (int i = m_context.getUnitOfWorkCount() - 1; i >= 0; i--)
         {
            UnitOfWork work = m_context.getUnitOfWork(i);

            m_context.setUnitOfWork(work);

            if (work.isTransient())
            {
               work.invokePendingEvents(true, false);
               work.computeHiddenness();
               work.accumulateChanges();
            }
            else
            {
               work.commit(false);
            }
         }

         m_context.setUnitOfWork(uow);

         // Transfer the results into the response object.
         // This is done as a separate step after the commit
         // so that the objects are in the correct state (e.g. with OIDs).
         Response response = new Response();

         identityMap = new HashTab();

         for (int i = 0; i < nInvocationCount; ++i)
         {
            Object result = resultArray[i << 1];
            Pair attributes = request.getInvocation(i).getAttributes();
            int nTF = RPCUtil.TF_HIDDEN;
            
            if (attributes == null)
            {
               attributes = (Pair)resultArray[(i << 1) + 1];

               if (attributes == EMPTY_PAIR)
               {
                  attributes = null;
                  nTF |= RPCUtil.TF_READABLE;
               }
            }
            else if (m_context.isProtected() && m_context.isSecure())
            {
               if (result instanceof Instance)
               {
                  Instance instance = (Instance)result;

                  instance.invoke("load", Pair.nconc(instance.getMetaclass()
                     .checkReadAccess(attributes, m_context.getPrivilegeSet()), attributes));
               }
               else if (result instanceof InstanceList)
               {
                  InstanceList list = (InstanceList)result;
                  Lookup classMap = new HashTab(4);

                  for (int k = 0, n = list.size(); k < n; ++k)
                  {
                     Instance instance = list.getInstance(k);
                     Metaclass metaclass = instance.getMetaclass();
                     Pair all = (Pair)classMap.get(metaclass);

                     if (all == null)
                     {
                        all = Pair.nconc(metaclass.checkReadAccess(
                           attributes, m_context.getPrivilegeSet()), attributes);

                        classMap.put(metaclass, all);
                     }

                     instance.invoke("load", all);
                  }
               }

               nTF |= RPCUtil.TF_READABLE;
            }

            response.addResult(RPCUtil.transfer(result, attributes,
               (eventArray[i] == null) ? diffMap : null, identityMap, nTF));
         }

         // Apply the change filters
         for (int i = 0; i < nFilterCount; ++i)
         {
            TransferObject tobj = request.getFilter(i);
            Metaclass metaclass = metadata.getMetaclass(tobj.getClassName());
            Lookup map = m_context.getClassChangeMap(metaclass);
            Object instances = tobj.findValue("instances");
            Pair attributes = (Pair)tobj.findValue("attributes");
            Pair all = attributes;
            List eventList = new ArrayList();

            if (m_context.isProtected() && m_context.isSecure())
            {
               all = Pair.nconc(metaclass.checkReadAccess(attributes, m_context.getPrivilegeSet()), attributes);
            }

            if (instances != null)
            {
               List instanceList = (List)instances;
               int nCount = instanceList.size();

               for (int k = 0; k < nCount; ++k)
               {
                  Instance instance = (Instance)instanceList.get(k);
                  Object state = map.get(instance);

                  if (state != null)
                  {
                     instance.invoke("load", all);
                     eventList.add(transferEvent(instance, ((Integer)state).byteValue(),
                        attributes, diffMap, identityMap));
                  }
                  else
                  {
                     eventList.add(null);
                  }
               }
            }
            else
            {
               for (Lookup.Iterator itr = map.iterator(); itr.hasNext();)
               {
                  Instance instance = (Instance)itr.next();

                  instance.invoke("load", all);

                  TransferObject obj = transferEvent(instance, ((Integer)itr.getValue()).byteValue(),
                     attributes, diffMap, identityMap);

                  if (obj != null)
                  {
                     eventList.add(obj);
                  }
               }
            }

            response.addEvent(eventList);
         }

         GenericServer.auditResponse(identityMap, m_context);

         if (m_bMaster)
         {
            m_context.complete(true);
         }

         if (s_logger.isDumpEnabled())
         {
            s_logger.dump(response);
         }

         TransferObject corellator = request.getCorrelator();

         if (corellator != null)
         {
            s_logger.debug("Invoking the request correlator");

            Request coreq = new Request();

            if (corellator.findValue("response", Undefined.VALUE) == null)
            {
               corellator.setValue("response", response);
            }

            if (corellator.findValue("results", Undefined.VALUE) == null)
            {
               int nCount = response.getResultCount();
               Object[] results = new Object[nCount];

               for (int i = 0; i < nCount; ++i)
               {
                  results[i] = response.getResult(i);
               }

               corellator.setValue("results", results);
            }

            coreq.setLocale(request.getLocale());
            coreq.setTimeZone(request.getTimeZone());
            coreq.setCommit(request.isCommit());
            coreq.addInvocation(corellator);

            invoke(coreq);

            s_logger.debug("Completed the correlator invocation");
         }

         if (m_interceptor != null && m_interceptor.isEnabled(m_context))
         {
            m_interceptor.interceptResponse(response);
         }

         return response;
      }
      catch (Throwable t)
      {
         if (bStealth)
         {
            Logger.setEnabled(bLogEnabledSaved);
         }

         int nLevel = Logger.DEBUG;

         if (RPCUtil.isSystem(t) && !s_logger.isDumpEnabled())
         {
            s_logger.error(request);
            nLevel = Logger.ERROR;
         }

         s_logger.log(nLevel, "Server invocation exception", t);

         if (t instanceof ErrorLocationHolder)
         {
            completeErrorLocation((ErrorLocationHolder)t, argArray);
         }

         if (m_bMaster)
         {
            m_context.complete(false);
         }

         throw ObjUtil.rethrow(t);
      }
      finally
      {
         m_context.setTransient(bTransientSaved);
         m_context.setAudited(bAuditedSaved);
         Logger.setEnabled(bLogEnabledSaved);
      }
   }

   /**
    * Finds an argument value.
    * @param event The event object.
    * @param sName The argument name.
    * @param args The argument array. The instance is at index 0.
    * @param tobj The transfer object.
    */
   protected static Object findArg(Event event, String sName, Object[] args, TransferObject tobj)
   {
      Argument arg = (event.getActionCount() == 0) ? null : event.findArgument(sName);

      if (arg != null)
      {
         return args[arg.getOrdinal() + 1];
      }

      return tobj.findValue(sName);
   }

   /**
    * Audits request objects.
    * @param identityMap Map of transfer object to instance.
    * @param eventMap Map of transfer object to event. Can be null.
    * @param context The invocation context.
    */
   public static void auditRequest(Lookup identityMap, Lookup eventMap, InvocationContext context)
   {
      for (Lookup.Iterator itr = identityMap.iterator(); itr.hasNext();)
      {
         if (itr.next() instanceof TransferObject)
         {
            TransferObject tobj = (TransferObject)itr.getKey();
            Instance instance = (Instance)itr.getValue();
            Event pendingEvent = null;
            Event event = null;

            if (instance != null && instance.isEventPending())
            {
               pendingEvent = (Event)instance.getMetaclass().getSelector(instance.getPendingEventName()).getMember(0);
            }

            if (eventMap != null)
            {
               event = (Event)eventMap.get(tobj);
            }
            else if (instance != null && tobj.getEventName() != null)
            {
               event = instance.getMetaclass().findEvent(tobj.getEventName(), 0);
            }

            if (pendingEvent != null && pendingEvent != event)
            {
               RPCUtil.audit(instance, tobj, pendingEvent, context);
            }

            if (event != null)
            {
               RPCUtil.audit(instance, tobj, event, context);
            }
         }
      }
   }

   /**
    * Audits response objects.
    * @param identityMap Map of instance to transfer object.
    * @param context The invocation context.
    */
   public static void auditResponse(Lookup identityMap, InvocationContext context)
   {
      for (Lookup.Iterator itr = identityMap.iterator(); itr.hasNext();)
      {
         if (itr.next() instanceof Instance)
         {
            Instance instance = (Instance)itr.getKey();
            TransferObject tobj = (TransferObject)itr.getValue();
   
            RPCUtil.audit(instance, tobj, (Event)instance.getMetaclass().getSelector("read").getMember(6), context);
         }
      }
   }

   /**
    * Creates a transfer object event for a given instance.
    * @param instance The instance for which to create the event.
    * @param nState The instance state, one of the Instance.* constants.
    * @param attributes The attribute list to transfer.
    * @param diffMap Map of instances to original transfer objects. If non-null, only the modified values are transferred.
    * @param identityMap Map for tracking object identity.
    * @return The converted object. 
    */
   private static TransferObject transferEvent(Instance instance, byte nState,
      Pair attributes, Lookup diffMap, Lookup identityMap)
   {
      TransferObject tobj = RPCUtil.transfer(instance,
         (nState == Instance.DELETED) ? null : attributes, diffMap, identityMap,
            RPCUtil.TF_READABLE | RPCUtil.TF_HIDDEN);

      if (tobj != null)
      {
         if (tobj.getEventName() == null)
         {
            switch (nState)
            {
               case Instance.CLEAN:
                  break;

               case Instance.NEW:
                  tobj.setEventName("create");
                  break;

               case Instance.DIRTY:
                  tobj.setEventName("update");
                  break;

               case Instance.DELETED:
                  tobj.setEventName("delete");
                  break;

               default:
                  throw new IllegalStateException();
            }
         }
      }
      else if (instance != null && (instance.isHidden() || diffMap != null && diffMap.contains(instance)))
      {
         // Visibility change
         tobj = new TransferObject(instance.getMetaclass().getName(), 0);
         tobj.setOID(instance.getOID());
         tobj.setEventName("delete");
         identityMap.put(instance, tobj);
      }

      return tobj;
   }

   /**
    * Completes the ordinal numbers in error location holders.
    */
   private static void completeErrorLocation(ErrorLocationHolder e, Object[][] argArray)
   {
      OIDHolder holder = e.getOIDHolder();
      
      if (holder != null)
      {
         for (int i = 0, n = argArray.length; i != n; ++i)
         {
            Object[] invArray = argArray[i]; 
            
            if (invArray != null && invArray[0] == holder)
            {
               e.setOrdinal(i);

               break;
            }
         }
      }
      
      for (Iterator itr = e.getAttributeIterator(); itr.hasNext();)
      {
         Throwable t = e.findException((String)itr.next());
         
         if (t instanceof ErrorLocationHolder && t != e)
         {
            completeErrorLocation(((ErrorLocationHolder)t), argArray);
         }
      }
      
      if (e instanceof ExceptionHolder)
      {
         for (Iterator itr = ((ExceptionHolder)e).getExceptionIterator(); itr.hasNext();)
         {
            Throwable t = (Throwable)itr.next();
            
            if (t instanceof ErrorLocationHolder)
            {
               completeErrorLocation(((ErrorLocationHolder)t), argArray);
            }
         }
      }
   }
}
