// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.object;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import nexj.core.integration.CompoundIntegrationException;
import nexj.core.integration.IntegrationException;
import nexj.core.integration.Output;
import nexj.core.integration.ObjectMismatchException;
import nexj.core.integration.sync.DefaultSyncEngine;
import nexj.core.meta.Attribute;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Metadata;
import nexj.core.meta.Primitive;
import nexj.core.meta.integration.CompositeMessagePart;
import nexj.core.meta.integration.CompositeMessagePartRef;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessagePart;
import nexj.core.meta.integration.PrimitiveMessagePart;
import nexj.core.meta.integration.format.object.ObjectMessagePartMapping;
import nexj.core.meta.integration.service.Sync;
import nexj.core.persistence.OID;
import nexj.core.persistence.Query;
import nexj.core.rpc.GenericServer;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InstanceArrayList;
import nexj.core.runtime.InstanceList;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.SecurityViolationException;
import nexj.core.runtime.UnitOfWork;
import nexj.core.scripting.Intrinsic;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.Binary;
import nexj.core.util.HashTab;
import nexj.core.util.HashTab2D;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.Lookup2D;
import nexj.core.util.ObjUtil;
import nexj.core.util.StringUtil;

/**
 * Object message formatter strategy.
 */
public abstract class ObjectMessageFormatterStrategy
{
   // constants

   /**
    * Special constant to indicate an empty where clause.
    */
   protected final static Pair EMPTY = new Pair(null);

   /**
    * Maximum number of retry attempts in case of an Optimistic Lock
    */
   private final static int MAX_RETRIES = 5;

   // associations

   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   /**
    * Instance map: Instance[TransferObject].
    */
   protected Lookup m_instanceByTobjMap;

   /**
    * Invocation list: TransferObject[].
    */
   protected List m_invocationList;

   /**
    * Query result map: InstanceList[composite][where].
    */
   protected Lookup2D m_queryMap;

   /**
    * Map of info object, indexed by transfer object.
    */
   protected Lookup m_infoObjectByTobjMap;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(ObjectMessageFormatter.class);

   // operations

   /**
    * @see nexj.core.runtime.InvocationContextAware#setInvocationContext(nexj.core.runtime.InvocationContext)
    */
   public void setInvocationContext(InvocationContext context)
   {
      m_context = context;
   }

   /**
    * Sets the OIDHolder of the synchronization link - source of data used for this persist step.
    * @param syncLink The OIDHolder of the synchronization link - source of data used for this persist step to set.
    */
   public void setSyncLink(Sync.Link syncLink)
   {
   }

   /**
    * @return The Logger instance if it exists. Otherwise, return null.
    */
   public Logger getLinkLogger()
   {
      return null;
   }

   /**
    * Prepare state for incoming changes.
    * @param tobjRoot The transfer object or collection - root of the graph.
    * @param composite The message part metadata.
    */
   protected void prepareFind(Object tobjRoot, CompositeMessagePart composite)
   {
   }

   /**
    * Prepares instance state.
    */
   protected void prepareInstantiate()
   {
   }

   /**
    * @return Map of instances to audit, indexed by transfer object.
    */
   protected Lookup prepareAudit()
   {
      Lookup auditMap = new HashTab(m_instanceByTobjMap.size());

      for (Lookup.Iterator tobjItr = m_instanceByTobjMap.iterator(); tobjItr.hasNext();)
      {
         TransferObject tobj = (TransferObject)tobjItr.next();
         ObjectMessageInfo infoObject = getInfoObject(tobj);

         if (infoObject.isAudited())
         {
            auditMap.put(tobj, tobjItr.getValue());
         }
      }

      return auditMap;
   }

   /**
    * Finalizes instance state.
    */
   protected void finalizeInstantiate()
   {
   }

   /**
    * Adjusts the transfer object's event name.
    * @param tobj The transfer object.
    * @param messageClass The message metaclass.
    */
   protected void adjustEventName(TransferObject tobj, Metaclass messageClass)
   {
   }

   /**
    * Sets event name on the transfer object.
    * @param tobj The root transfer object.
    * @param sEventName The event name to set.
    */
   protected void setEventName(TransferObject tobj, String sEventName)
   {
      tobj.setEventName(sEventName);
   }

   /**
    * Log inbound synchronization events if nLevel is enabled.
    * @param nLevel The logging level.
    * @param sCode The string identifier of the message.
    */
   protected void logEvent(int nLevel, String sCode)
   {
   }

   /**
    * @return True, if the instance read by sync key was already done in batch (if we did not find them, we won't find
    * them now either). Otherwise, false.
    * @param tobj The transfer object.
    * @param mapping The object mapping.
    */
   protected boolean isInstanceRead(TransferObject tobj, ObjectMessagePartMapping mapping)
   {
      return false;
   }

   /**
    * Deletes instance state.
    * @param tobj The transfer object.
    * @param messageClass The message metaclass.
    * @param bDelete True, if the change is a delete.
    */
   protected void deleteInstanceState(TransferObject tobj, Metaclass messageClass, boolean bDelete)
   {
   }

   /**
    * Find all instances dependent on the instances that needs to be deleted
    * @param mapping The object message part mapping
    * @param allInstanceList List containing all instances.
    * @param instanceToKeepList List containing instance to keep.
    */
   protected void findDependentInstances(ObjectMessagePartMapping mapping, InstanceList allInstanceList,
      List instanceToKeepList)
   {
   }

   /**
    * Adds the instance to delete to the list.
    * @param instance The instance to delete.
    * @param composite The message part metadata corresponding to the instance to delete.
    * @param parentTobj The parent transfer object.
    */
   protected void addInstanceToDelete(Instance instance, CompositeMessagePart composite, TransferObject parentTobj)
   {
      ObjectMessageInfo infoObject = (ObjectMessageInfo)m_infoObjectByTobjMap.get(parentTobj);

      if (infoObject == null)
      {
         infoObject = new ObjectMessageInfo();
         m_infoObjectByTobjMap.put(parentTobj, infoObject);
      }

      infoObject.addInstanceToDelete(composite, instance);
   }

   /**
    * @return The message information object corresponding to the transfer object.
    * @param tobj The transfer object.
    */
   protected ObjectMessageInfo getInfoObject(TransferObject tobj)
   {
      ObjectMessageInfo infoObject = (ObjectMessageInfo)m_infoObjectByTobjMap.get(tobj);

      if (infoObject == null)
      {
         infoObject = ObjectMessageInfo.EMPTY_INFO;
      }

      return infoObject;
   }

   /**
    * @return True, if the root instance has already been synchronize.
    * @param tobj The transfer object.
    */
   protected boolean isRootSynchronized(TransferObject tobj)
   {
      return true;
   }

   /**
    * @see nexj.core.integration.MessageFormatter#format(TransferObject, Message, Output)
    */
   public void format(TransferObject tobj, Message message, Output out) throws IntegrationException
   {
      out.setObject(format(tobj, message.getRoot()));
   }

   /**
    * Formats an object message.
    * @param tobj The message to format.
    * @param composite The message part metadata.
    * @return The formatted instance.
    */
   private Object format(TransferObject tobj, CompositeMessagePart composite) throws IntegrationException
   {
      boolean bAuditedSaved = m_context.isAudited();

      try
      {
         m_context.setAudited(false);

         ObjectMessagePartMapping mapping = (ObjectMessagePartMapping)composite.getMapping();

         if (mapping != null)
         {
            String sDerivedMessageName = tobj.getClassName();
            Message message = mapping.getMessage();

            if (!StringUtil.isEmpty(sDerivedMessageName) && !sDerivedMessageName.equals(message.getName()))
            {
               Message derivedMessage = m_context.getMetadata().getMessage(sDerivedMessageName);
               Message.validatePolymorphism(message, derivedMessage, derivedMessage.getRoot());

               composite = derivedMessage.getRoot();
            }
         }

         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Formatting object message \"" + composite.getName() + "\"");
            s_logger.dump(tobj);
         }

         m_instanceByTobjMap = new HashTab();
         m_invocationList = new ArrayList();
         m_queryMap = new HashTab2D();
         m_infoObjectByTobjMap = new HashTab();

         Object obj = (composite.isCollection()) ? Collections.singletonList(tobj) : (Object)tobj;

         prepareFind(obj, composite);
         logEvent(Logger.DUMP, "ids.sync.formattingStarted");
         findInstance1(tobj, composite, null, null);
         prepareInstantiate();

         Object instance = instantiate(obj, composite);
         Lookup auditMap = prepareAudit();

         GenericServer.auditRequest(auditMap, null, m_context);

         for (int i = 0, n = m_invocationList.size(); i != n; ++i)
         {
            TransferObject nextTobj = (TransferObject)m_invocationList.get(i);
            Instance inst = (Instance)m_instanceByTobjMap.get(nextTobj);
            boolean bInvoke = true;

            if (inst.isEventPending())
            {
               // For a change from an external system, the event could be set to create. Additionally, the internal
               // instance may already exist. In such a case, the instance should simply be updated and not create
               // any duplicates in the internal system.
               bInvoke = (("create".equals(nextTobj.getEventName()) && inst.getState() == Instance.DIRTY) &&
                  "update".equals(inst.getPendingEventName()))
                  ? false
                  : !inst.getPendingEventName().equals(nextTobj.getEventName());

               inst.invokePendingEvent();
            }

            if (bInvoke && nextTobj.getEventName() != null
               // Ignore pending "update" events - they should only run if there has been an actual update.
               && (!nextTobj.getEventName().equals("update") || inst.getState() == Instance.DIRTY)
               // Instance update or insert was rejected (e.g. by sync engine)
               && inst.getState() != Instance.DELETED)
            {
               inst.invoke(nextTobj.getEventName());
            }
         }

         finalizeInstantiate();
         s_logger.debug("Completed formatting the object message");
         logEvent(Logger.DEBUG, "ids.sync.formattingCompleted");

         if (composite.isCollection() && instance instanceof List)
         {
            List instanceList = (List)instance;

            return (instanceList.isEmpty()) ? null : instanceList.get(0);
         }

         return instance;
      }
      catch (Exception e)
      {
         s_logger.debug("Object formatting error", e);

         throw new IntegrationException("err.integration.format",
            new Object[]{composite.getName()}, e);
      }
      finally
      {
         for (Lookup.Iterator itr = m_infoObjectByTobjMap.iterator(); itr.hasNext();)
         {
            TransferObject itemTobj = (TransferObject)itr.next();
            ObjectMessageInfo infoObject = getInfoObject(itemTobj);

            itemTobj.setEventName(infoObject.getOriginalEvent());
         }

         m_context.setAudited(bAuditedSaved);
      }
   }

   /**
    * Formats the object message to the specified output. 
    * Attempts to retry the entire message several times before splitting it into chunks.
    *  
    * @param tobj The object message to format.
    * @param message The message metadata.
    * @param out The message output. It will be populated even if this method throws an exception.
    * @param bCommit flag whether results of successful formatting should be committed
    * in case when some parts of the message raise an exception
    * @param nRetry number of a retry attempt
    * @throws a collection of all the exceptions that occur during formatting.
    */
   public void formatFailSafe(TransferObject tobj, Message message, Output out, boolean bCommit, int nRetry)
      throws CompoundIntegrationException
   {
      UnitOfWork oldUOW = m_context.beginTransaction(); // We use unit of work to recover from database failures
      String sOriginalEvent = tobj.getEventName();

      try
      {
         format(tobj, message, out);
         m_context.commitAndResume(oldUOW);
      }
      catch (Throwable e)
      {
         m_context.rollbackAndResume(oldUOW);

         if ((nRetry < MAX_RETRIES) && DefaultSyncEngine.isTransient(e))
         {
            if (s_logger.isDebugEnabled())
            {
               s_logger.debug("Error processing the composite message on attempt " + nRetry + "; trying again.", e);
            }

            Logger logger = getLinkLogger();

            if (logger != null && logger.isWarnEnabled())
            {
               logger.log(Logger.WARN, "err.sync.recoverableFormatting", new Object[] {tobj.getClassName()}, e);
            }

            tobj.setEventName(sOriginalEvent);
            formatFailSafe(tobj, message, out, bCommit, nRetry + 1);
         }
         else
         {
            s_logger.debug("Error processing the composite message; trying to process it in chunks", e);

            Lookup objectParts = getObjectParts(tobj, message.getRoot(), new HashTab());
            CompoundIntegrationException ex = new CompoundIntegrationException(e.getMessage());

            out.setObject(new FailSafeFormatter(ex, bCommit).divideAndFormat(objectParts));

            if (ex.getExceptionCount() > 0)
            {
               throw ex;
            }
         }
      }
   }

   /**
    * Find all top-level transfer objects, contained within a given transfer object or a collection thereof.
    * @param tobj The transfer object.
    * @param composite The message part metadata.
    * @param objectParts object part messages mapped by corresponding transfer objects: Message[TransferObject]
    * @return objectParts map
    */
   protected Lookup getObjectParts(Object obj, CompositeMessagePart composite, Lookup objectParts)
   {
      if (composite.isCollection())
      {
         List list = getList(obj, composite);

         for (int i = 0, n = list.size(); i != n; ++i)
         {
            getObjectParts1((TransferObject)list.get(i), composite, objectParts);
         }
      }
      else
      {
         getObjectParts1((TransferObject)obj, composite, objectParts);
      }

      return objectParts;
   }

   /**
    * Finds all top-level transfer objects, contained within a given transfer object .
    * @param tobj The transfer object.
    * @param composite The message part metadata.
    * @param objectParts object part messages mapped by corresponding transfer objects: Message[TransferObject]
    * @return objectParts map
    */
   protected void getObjectParts1(TransferObject tobj, CompositeMessagePart composite, Lookup objectParts)
   {
      if (tobj == null)
      {
         return;
      }

      composite = getPolymorphic(composite, tobj);

      if (composite.getMapping() != null)
      {
         if (composite instanceof CompositeMessagePartRef)
         {
            composite = ((CompositeMessagePartRef)composite).getRefPart();
         }

         objectParts.put(tobj, composite);
      }
      else
      {
         for (int i = 0, n = composite.getPartCount(); i != n; ++i)
         {
            MessagePart part = composite.getPart(i);
   
            if (part instanceof CompositeMessagePart)
            {
               getObjectParts(tobj.findValue(part.getName()), (CompositeMessagePart)part, objectParts);
            }
         }
      }
   }

   /**
    * Finds an instance for a given transfer object or a collection thereof.
    * @param tobj The transfer object.
    * @param composite The message part metadata.
    * @param parentTobj The parent transfer object, if any.
    * @param parentComposite composite's parent metadata, if any.
    */
   protected void findInstance(Object obj, CompositeMessagePart composite, TransferObject parentTobj,
      CompositeMessagePart parentComposite)
   {
      if (composite.isCollection())
      {
         List list = getList(obj, composite);
         InstanceList foundInstances = new InstanceArrayList(list.size());
         ObjectMessagePartMapping mapping = (ObjectMessagePartMapping)composite.getMapping();

         for (int i = 0, n = list.size(); i != n; ++i)
         {
            Instance instance = findInstance1((TransferObject)list.get(i), composite, parentTobj, parentComposite);

            if (instance != null)
            {
               foundInstances.add(instance, InstanceList.REPLACE | InstanceList.DIRECT);
            }
         }

         if (mapping.isDelete() && obj != null) // Delete all unmatched instances
         {
            Object where = mapping.getWhere();

            if (mapping.isLocal() && parentComposite.getMapping() != null)
            {
               Pair eq = Pair.list(Symbol.ATAT, ((ObjectMessagePartMapping)
                  parentComposite.getMapping()).getMetaclass().getSymbol(),
                  mapping.getAttribute().getSymbol()).eq(m_instanceByTobjMap.get(parentTobj));

               where = eq.and(where);
            }

            InstanceList allInstances = Query.createRead(mapping.getMetaclass(), getAttributes(composite),
               where, null, -1, 0, false, Query.SEC_NONE, m_context).read();

            if (!allInstances.isEmpty())
            {
               List instToKeepList = new ArrayList();

               for (Iterator itr = allInstances.iterator(); itr.hasNext();)
               {
                  Instance instance = (Instance)itr.next();

                  if (!foundInstances.contains(instance))
                  {
                     addInstanceToDelete(instance, composite, parentTobj);
                  }
                  else
                  {
                     instToKeepList.add(instance);
                  }
               }

               // Find all instances dependent on the instances that need to be deleted
               findDependentInstances(mapping, allInstances, instToKeepList);
            }
         }
      }
      else
      {
         findInstance1((TransferObject)obj, composite, parentTobj, parentComposite);
      }
   }

   /**
    * Finds an instance for a given transfer object.
    * @param tobj The transfer object.
    * @param composite The message part metadata.
    * @param parentTobj The parent transfer object, if any.
    * @param parentComposite composite's parent metadata, if any.
    */
   protected Instance findInstance1(TransferObject tobj, CompositeMessagePart composite, TransferObject parentTobj,
      CompositeMessagePart parentComposite)
   {
      if (tobj == null)
      {
         return null;
      }

      ObjectMessagePartMapping mapping = (ObjectMessagePartMapping)composite.getMapping();
      CompositeMessagePart derivedComposite = getPolymorphic(composite, tobj);
      ObjectMessagePartMapping derivedMapping = (ObjectMessagePartMapping)derivedComposite.getMapping();
      Metaclass derivedClass = (derivedMapping == null) ? null : derivedMapping.getMetaclass();

      // If any message part is mapped to special attributes ATTR_EVENT and ATTR_OID,
      // it overrides the event or oid set on message. 
      String sEventName = (String)getSystemAttributeValue(derivedComposite, tobj, ObjectMessagePartMapping.ATTR_EVENT);
      OID oid = (OID)getSystemAttributeValue(derivedComposite, tobj, ObjectMessagePartMapping.ATTR_OID);

      if (sEventName != null)
      {
         setEventName(tobj, sEventName);
      }

      if (oid != null)
      {
         tobj.setOID(oid);
      }

      boolean bDelete = "delete".equals(tobj.getEventName());

      adjustEventName(tobj, derivedClass);

      Instance instance = (Instance)m_instanceByTobjMap.get(tobj);

      if (instance == null)
      {
         deleteInstanceState(tobj, derivedClass, bDelete);

         if ("create".equals(tobj.getEventName())
            && (!derivedMapping.hasAlternativeSyncKey() || !isRootSynchronized(tobj)))
         {
            // The internal instance corresponding to the transfer object will be created during instantiation.
            m_instanceByTobjMap.put(tobj, null);
         }
         else
         {
            if (derivedClass != null)
            {
               if (derivedClass.getPersistenceMapping() == null)
               {
                  if (tobj.getEventName() == null)
                  {
                     if (parentTobj == null)
                     {
                        throw new IntegrationException("err.integration.object.missingEvent",
                           new Object[]{derivedComposite.getFullPath()});
                     }

                     setEventName(tobj, "create");
                  }

                  m_instanceByTobjMap.put(tobj, null);
               }
               else if (!mapping.isLocal() || m_instanceByTobjMap.contains(parentTobj))
               {
                  Pair where = getWhere1(tobj, composite, parentTobj, parentComposite, null, true);
                  InstanceList list = null;

                  if (where != EMPTY && !(mapping.isLocal() && m_instanceByTobjMap.get(parentTobj) == null))
                  {
                     if (mapping.isLocal())
                     {
                        Pair eq = Pair.list(Symbol.ATAT, ((ObjectMessagePartMapping)
                           parentComposite.getMapping()).getMetaclass().getSymbol(),
                           mapping.getAttribute().getSymbol()).eq(m_instanceByTobjMap.get(parentTobj));

                        if (where != null)
                        {
                           where = where.and(eq);
                        }
                        else
                        {
                           where = eq;
                        }
                     }

                     if (where != null)
                     {
                        list = (InstanceList)m_queryMap.get(derivedComposite, where);

                        if (list == null)
                        {
                           list = Query.createRead(derivedClass, getAttributes(derivedComposite), where, null,
                              2, 0, false, Query.SEC_NONE, m_context).read();

                           m_queryMap.put(derivedComposite, where, list);
                        }
                     }
                     else
                     {
                        where = EMPTY;
                     }
                  }

                  if (where != EMPTY && list != null && list.size() != 0)
                  {
                     if (list.size() > 1)
                     {
                        throw new IntegrationException("err.integration.object.ambiguousMatch",
                           new Object[]{derivedComposite.getFullPath()});
                     }

                     instance = list.getInstance(0);
                     m_instanceByTobjMap.put(tobj, instance);
                  }
                  else if (!bDelete)
                  {
                     if (tobj.getEventName() != null && (!mapping.isLocal() || m_instanceByTobjMap.get(parentTobj) == null) ||
                        !mapping.isCreate() && !mapping.isKey())
                     {
                        throw new ObjectMismatchException("err.integration.object.noMatch",
                           new Object[]{derivedComposite.getFullPath()});
                     }

                     setEventName(tobj, (!mapping.isCreate() ||
                        where == EMPTY && !isKeyAvailable(tobj, derivedComposite)) ? "delete" : "create");

                     m_instanceByTobjMap.put(tobj, null);
                  }
               }
            }
         }
      }

      for (int i = 0, n = derivedComposite.getPartCount(); i != n; ++i)
      {
         MessagePart part = derivedComposite.getPart(i);

         if (part instanceof CompositeMessagePart)
         {
            findInstance(tobj.findValue(part.getName()), (CompositeMessagePart)part, tobj, derivedComposite);
         }
      }

      return instance;
   }

   /**
    * @return True, if the child key is available to the parent.
    */
   private boolean isKeyAvailable(TransferObject tobj, CompositeMessagePart derivedComposite)
   {
      for (int i = 0, n = derivedComposite.getPartCount(); i < n; ++i)
      {
         MessagePart part = derivedComposite.getPart(i);

         if (part instanceof CompositeMessagePart && !part.isCollection())
         {
            ObjectMessagePartMapping mapping = (ObjectMessagePartMapping)part.getMapping();

            if (mapping != null && mapping.isKey())
            {
               Object childObj = tobj.findValue(part.getName());

               if (!(childObj instanceof TransferObject))
               {
                  return false;
               }

               TransferObject childTobj = (TransferObject)childObj;

               if (m_instanceByTobjMap.get(childTobj) == null && !"create".equals(childTobj.getEventName()))
               {
                  return false;
               }
            }
         }
      }

      return true;
   }

   /**
    * Returns object attributes included in the given object message part, excluding collection attributes. 
    * @param composite The message part metadata.
    * @return a list of attribute to be read from the datasource
    */
   private Pair getAttributes(CompositeMessagePart composite)
   {
      ObjectMessagePartMapping mapping = (ObjectMessagePartMapping)composite.getMapping();
      Pair attributes = null;

      for (int i = composite.getPartCount() - 1; i >= 0; --i)
      {
         ObjectMessagePartMapping partMapping = ((ObjectMessagePartMapping)composite.getPart(i).getMapping());
         Attribute attr = partMapping.getAttribute();
         
         if (attr != null && !attr.isCollection())
         {
            attributes = new Pair(attr.getSymbol(), attributes);

            if (partMapping.getAccessAttribute() != null)
            {
               attributes = new Pair(partMapping.getAccessAttribute().getSymbol(), attributes);
            }
         }
      }

      if (composite.getParent() == null && mapping.getAccessAttribute() != null)
      {
         attributes = new Pair(mapping.getAccessAttribute().getSymbol(), attributes);
      }

      if (m_context.isSecure())
      {
         Metaclass metaclass = mapping.getMetaclass();

         if (metaclass.getReadAccessAttribute() != null)
         {
            attributes = new Pair(metaclass.getReadAccessAttribute().getSymbol(), attributes);
         }

         if (mapping.isUpdate() && metaclass.getUpdateAccessAttribute() != null)
         {
            attributes = new Pair(metaclass.getUpdateAccessAttribute().getSymbol(), attributes);
         }
      }

      return attributes;
   }

   /**
    * Generates a where clause for looking up one object.
    * @param obj The object for which to generate the where clause. Can be a list.
    * @param composite The message part metadata.
    * @param parent The parent transfer object, if any.
    * @param parentComposite composite's parent metadata, if any.
    * @param assoc The reverse association symbol list.
    * @param bRoot True if this is the object to which the where clause applies.
    */
   protected Pair getWhere(Object obj, CompositeMessagePart composite, TransferObject parent,
      CompositeMessagePart parentComposite, Pair assoc, boolean bRoot)
   {
      if (composite.isCollection())
      {
         Pair where = null;
         List list = getList(obj, composite);

         for (int i = 0, n = list.size(); i != n; ++i)
         {
            Pair expr = getWhere1((TransferObject)list.get(i), composite, parent, parentComposite, assoc, bRoot);

            if (expr != EMPTY && expr != null)
            {
               where = new Pair(expr, where);
            }
         }

         if (where != null)
         {
            if (where.getTail() == null)
            {
               return (Pair)where.getHead();
            }

            return new Pair(Symbol.OR, where);
         }

         return EMPTY;
      }

      return getWhere1((TransferObject)obj, composite, parent, parentComposite, assoc, bRoot);
   }

   /**
    * Generates a where clause for looking up one object.
    * @param tobj The object for which to generate the where clause.
    * @param composite The message part metadata.
    * @param parent The parent transfer object, if any.
    * @param parentComposite composite's parent metadata, if any.
    * @param assoc The reverse association symbol list.
    * @param bRoot True if this is the object to which the where clause applies.
    */
   protected Pair getWhere1(TransferObject tobj, CompositeMessagePart composite, TransferObject parent,
      CompositeMessagePart parentComposite, Pair assoc, boolean bRoot)
   {
      ObjectMessagePartMapping mapping = (ObjectMessagePartMapping)composite.getMapping();
      CompositeMessagePart derivedComposite = getPolymorphic(composite, tobj);
      ObjectMessagePartMapping derivedMapping = ((ObjectMessagePartMapping)derivedComposite.getMapping());
      Metaclass derivedMetaclass = derivedMapping.getMetaclass();
      Pair where = null;

      if (!bRoot)
      {
         assoc = new Pair(mapping.getAttribute().getSymbol(), assoc);

         if (!mapping.isLocal())
         {
            Instance instance = (Instance)m_instanceByTobjMap.get(tobj);

            if (instance == null)
            {
               if (!m_instanceByTobjMap.contains(tobj) && !mapping.isSubKeyParent())
               {
                  instance = findInstance1(tobj, composite, parent, parentComposite);
               }

               if (instance == null && !mapping.isSubKeyParent())
               {
                  return EMPTY;
               }
            }

            if (instance != null && !mapping.isSubKeyParent())
            {
               return getComparison(assoc, instance);
            }
         }
      }

      if (tobj == null || isInstanceRead(tobj, derivedMapping))
      {
         return EMPTY;
      }

      if (tobj.getOID() != null)
      {
         where = new Pair(getComparison(assoc, tobj.getOID()), where);
      }
      else
      {
         for (int i = 0, n = derivedComposite.getPartCount(); i != n; ++i)
         {
            MessagePart part = derivedComposite.getPart(i);
            ObjectMessagePartMapping partMapping = (ObjectMessagePartMapping)part.getMapping();

            if (bRoot && partMapping.isKey() || !bRoot && partMapping.isSubKey())
            {
               if (!tobj.hasValue(part.getName()))
               {
                  return EMPTY;
               }

               Object obj = tobj.getValue(part.getName());
               Pair expr = null;

               if (part instanceof CompositeMessagePart)
               {
                  expr = getWhere(obj, (CompositeMessagePart)part, tobj, composite, assoc, false);

                  if (expr == EMPTY)
                  {
                     return EMPTY;
                  }
               }
               else if (partMapping.getSystemAttribute() == ObjectMessagePartMapping.ATTR_OID)
               {
                  expr = getComparison(assoc, obj);
               }
               else if (partMapping.getAttribute() != null)
               {
                  expr = getComparison(new Pair(partMapping.getAttribute().getSymbol(), assoc),
                     partMapping.getAttribute().getType().convert(obj));
               }

               if (expr != null)
               {
                  where = new Pair(expr, where);
               }
            }
         }

         if (!bRoot && derivedMetaclass != mapping.getAttribute().getType())
         {
            where = new Pair(Pair.binary(Symbol.INSTANCE_P, getAssociation(assoc),
               derivedMetaclass.getSymbol()), where);
         }

         if (mapping.getWhere() != Boolean.TRUE)
         {
            where = new Pair(mapping.getWhere(), where);
         }
      }

      if (where != null)
      {
         if (where.getTail() == null)
         {
            return (Pair)where.getHead();
         }

         where = new Pair(Symbol.AND, where);
      }

      return where;
   }

   /**
    * Creates an association operator based on an association list.
    * @param assoc The reversed association symbol list.
    */
   protected static Pair getAssociation(Pair assoc)
   {
      Pair pair = null;

      while (assoc != null)
      {
         pair = new Pair(assoc.getHead(), pair);
         assoc = assoc.getNext();
      }

      return new Pair(Symbol.AT, pair);
   }

   /**
    * Creates a comparison operator based on an association list and a constant.
    * @param assoc The reversed association symbol list.
    * @param obj The constant value.
    */
   protected static Pair getComparison(Pair assoc, Object obj)
   {
      return getAssociation(assoc).eq(obj);
   }

   /**
    * Instantiates an object or collection thereof.
    * @param obj The object to instantiate.
    * @param msg The message part metadata.
    */
   protected Object instantiate(Object obj, MessagePart msg)
   {
      if (msg.isCollection())
      {
         if (obj == null)
         {
            if (msg.isRequired())
            {
               throw new IntegrationException("err.integration.minPartCount", new Object[]{msg.getFullPath()});
            }

            return new InstanceArrayList(0);
         }

         List list = (List)obj;
         int nCount = list.size();

         if (nCount < msg.getMinCount())
         {
            throw new IntegrationException("err.integration.minPartCount", new Object[]{msg.getFullPath()});
         }

         if (nCount > msg.getMaxCount())
         {
            throw new IntegrationException("err.integration.maxPartCount", new Object[]{msg.getFullPath()});
         }

         InstanceList instanceList = new InstanceArrayList(list.size());

         for (int i = 0; i < nCount; ++i)
         {
            Object instance = instantiate1(list.get(i), msg);

            if (instance != null && ! instanceList.contains(instance))
            {
               instanceList.add(instance);
            }
         }

         return instanceList;
      }

      return instantiate1(obj, msg);
   }

   /**
    * Given a composite part and the TransferObject for it, return the composite part to use
    * taking into account inheritance on a reference.
    * @param composite The composite part.
    * @param tobj The TransferObject.
    * @return The derived message part, if composite part is a reference. Otherwise, returns
    * composite.
    */
   protected CompositeMessagePart getPolymorphic(CompositeMessagePart composite, TransferObject tobj)
   {
      return getPolymorphic(composite, tobj, m_context);
   }

   /**
    * Given a composite part and the TransferObject for it, return the composite part to use
    * taking into account inheritance on a reference.
    * @param composite The composite part.
    * @param tobj The TransferObject.
    * @param context The invocation context.
    * @return The derived message part, if composite part is a reference. Otherwise, returns
    * composite.
    */
   protected static CompositeMessagePart getPolymorphic(CompositeMessagePart composite, TransferObject tobj,
      InvocationContext context)
   {
      if (composite instanceof CompositeMessagePartRef)
      {
         String sDerivedMessageName = tobj.getClassName();
         CompositeMessagePart referencedPart = ((CompositeMessagePartRef)composite).getRefPart();

         if (!StringUtil.isEmpty(sDerivedMessageName))
         {
            Metadata metadata = context.getMetadata();
            Message derivedMessage = metadata.getMessage(sDerivedMessageName);
            Message baseMessage = metadata.getMessage(referencedPart.getRoot().getName());

            Message.validatePolymorphism(baseMessage, derivedMessage, composite);

            return derivedMessage.getRoot();
         }
      }

      return composite;
   }

   /**
    * Instantiates one object.
    * @param obj The object to instantiate.
    * @param msg The message part metadata.
    */
   protected Object instantiate1(Object obj, MessagePart part)
   {
      ObjectMessagePartMapping mapping = (ObjectMessagePartMapping)part.getMapping();
      Metaclass derivedClass = (mapping == null) ? null : mapping.getMetaclass();
      MessagePart derivedPart = part;

      if (part instanceof CompositeMessagePart)
      {
         derivedPart = getPolymorphic((CompositeMessagePart)part, (TransferObject)obj);

         if (derivedPart != part)
         {
            ObjectMessagePartMapping derivedMapping = (ObjectMessagePartMapping)derivedPart.getMapping();

            if (mapping == null)
            {
               mapping = derivedMapping;
            }

            derivedClass = derivedMapping.getMetaclass();
         }
      }

      Metaclass metaclass = null;

      if (mapping != null)
      {
         Attribute attribute = mapping.getAttribute();

         if (obj == null)
         {
            if (part.isRequired())
            {
               throw new IntegrationException("err.integration.minPartCount", new Object[]{part.getFullPath()});
            }

            return null;
         }

         if (derivedPart instanceof CompositeMessagePart)
         {
            metaclass = (Metaclass)getSystemAttributeValue((CompositeMessagePart)derivedPart,
               (TransferObject)obj, ObjectMessagePartMapping.ATTR_CLASS);
         }

         if (m_context.isSecure())
         {
            if (attribute != null)
            {
               attribute.checkReadAccess(m_context.getPrivilegeSet());
            }

            if (metaclass != null)
            {
               metaclass.checkReadAccess(m_context.getPrivilegeSet());
            }
         }

         if (derivedPart instanceof PrimitiveMessagePart)
         {
            Object validatedValue = ((PrimitiveMessagePart)derivedPart).validateValue(obj);
            
            if (attribute == null)
            {
               return validatedValue;
            }

            validatedValue = attribute.getType().convert(validatedValue);

            int nMaxLength = attribute.getMaxLength();

            if (nMaxLength > 0 && mapping.isTruncated())
            {
               if (validatedValue instanceof String)
               {
                  String sValue = (String) validatedValue;

                  if (sValue.length() > nMaxLength)
                  {
                     validatedValue = sValue.substring(0, nMaxLength);

                     if (s_logger.isDebugEnabled())
                     {
                        s_logger.debug("Value of attribute " + attribute.getName()
                           + " has been truncated to maximum allowed length " + nMaxLength
                           + ". The original value is \"" + sValue + "\".");
                     }
                  }
               }
               else if (validatedValue instanceof Binary)
               {
                  byte[] binaryValue = ((Binary) validatedValue).getData();

                  if (binaryValue.length > nMaxLength)
                  {
                     byte[] truncatedValue = new byte[nMaxLength];

                     System.arraycopy(binaryValue, 0, truncatedValue, 0, nMaxLength);
                     validatedValue = new Binary(truncatedValue);

                     if (s_logger.isDebugEnabled())
                     {
                        s_logger.debug("Value of attribute " + attribute.getName()
                           + " has been truncated to maximum allowed length " + nMaxLength
                           + ". The original value is \"" + binaryValue + "\".");
                     }
                  }
               }
            }

            return validatedValue;
         }
      }

      CompositeMessagePart composite = (CompositeMessagePart)derivedPart;
      TransferObject tobj = (TransferObject)obj;
      Instance instance = (Instance)m_instanceByTobjMap.get(tobj);

      if (instance == null && "delete".equals(tobj.getEventName())) // Obsolete delete
      {
         if (part.isRequired())
         {
            throw new IntegrationException("err.integration.minPartCount", new Object[]{part.getFullPath()});
         }

         return null;
      }

      boolean bNew = (instance == null);

      if (bNew && derivedClass != null)
      {
         instance = new Instance(metaclass, m_context);
         instance.setNew();
         m_instanceByTobjMap.put(tobj, instance);
      }
      else
      {
         if (m_context.isSecure() && instance != null && !instance.isReadable())
         {
            throw new SecurityViolationException("err.rpc.instanceAccess",
               new Object[]{instance.getLazyCaption(), instance.getLazyClassName()});
         }
      }

      if (instance != null && ! m_invocationList.contains(tobj))
      {
         m_invocationList.add(tobj);
      }

      if ((bNew ||
         part.getParent() != null ||
         mapping.isUpdate() &&
         (mapping.getAccessAttribute() == null ||
          Intrinsic.isTrue(instance.getValue(mapping.getAccessAttribute().getOrdinal())))))
      {
         updateInstance(bNew, tobj, instance, composite, mapping);
      }

      return instance;
   }

   /**
    * Gets a system attribute value from a message.
    * @param composite The message part metadata.
    * @param tobj The transfer object containing the data.
    * @param nAttr One of the ObjectMessagePartMapping.ATTR_* constants.
    * @return The system attribute value, or null if not found.
    */
   protected Object getSystemAttributeValue(CompositeMessagePart composite, TransferObject tobj, int nAttr)
   {
      ObjectMessagePartMapping mapping = (ObjectMessagePartMapping)composite.getMapping();
      Object value = null;

      if (mapping != null)
      {
         MessagePart part = mapping.getSystemPart(nAttr);

         if (part != null)
         {
            value =  tobj.findValue(part.getName());
         }

         switch (nAttr)
         {
            case ObjectMessagePartMapping.ATTR_CLASS:
               String sClass = (String)value;

               if (StringUtil.isEmpty(sClass))
               {
                  return mapping.getMetaclass();
               }

               return m_context.getMetadata().getMetaclass(sClass);

            case ObjectMessagePartMapping.ATTR_OID:
               if (value != null && !(value instanceof OID))
               {
                  value = OID.fromBinary(Primitive.toBinary(value));
               }

               return value;
         }
      }

      return value;
   }

   /**
    * Updates attributes on the instance
    * @param composite The message part metadata.
    * @param tobj The object containing update data.
    * @param instance The instance to be updated
    * @param bNew flag whether this is a new instance
    * @param conflictResolution conflict resolution strategy
    */
   protected void updateInstance(CompositeMessagePart composite, TransferObject tobj, Instance instance,
      boolean bNew, ConflictResolutionStrategy conflictResolution)
   {
      for (int i = 0, n = composite.getPartCount(); i != n; ++i)
      {
         MessagePart part = composite.getPart(i);

         if (tobj.hasValue(part.getName()))
         {
            ObjectMessagePartMapping partMapping = (ObjectMessagePartMapping)part.getMapping();

            if (isUpdateRequired(partMapping, instance, bNew))
            {
               Object value = instantiate(tobj.getValue(part.getName()), part);
               Attribute attribute = partMapping.getAttribute();

               if (attribute == null || instance == null)
               {
                  if (partMapping.getSystemAttribute() == ObjectMessagePartMapping.ATTR_OID)
                  {
                     if (value != null && !(value instanceof OID))
                     {
                        instance.setOID(OID.fromBinary(Primitive.toBinary(value)));
                     }
                     else
                     {
                        instance.setOID((OID)value);
                     }
                  }

                  continue;
               }

               if (bNew)
               {
                  instance.setValue(attribute.getOrdinal(), value);
               }
               else
               {
                  if (conflictResolution != null &&
                     !conflictResolution.shouldUpdate(instance, attribute, value))
                  {
                     continue;
                  }

                  if (attribute.isCollection())
                  {
                     InstanceList list = (InstanceList)instance.getValue(attribute.getOrdinal());
                     InstanceList valueList = (InstanceList)value;

                     for (int l = 0; l < valueList.size(); ++l)
                     {
                        Instance newInst = valueList.getInstance(l);

                        if (!list.contains(newInst))
                        {
                           list.add(newInst);
                        }
                     }
                  }
                  else if (!partMapping.isKey() && !ObjUtil.equal(instance.getValue(attribute.getOrdinal()), value))
                  {
                     instance.setValue(attribute.getOrdinal(), value);
                  }
               }
            }
         }
      }
   }

   /**
    * Determine whether an update to this instance is required
    * @param partMapping object message mapping used for the instance
    * @param instance instance being updated
    * @param bNew flag indicating whether the instance is new
    * @return whether an update to this instance is required
    */
   protected boolean isUpdateRequired(ObjectMessagePartMapping partMapping, Instance instance, boolean bNew)
   {
      return bNew ||
          partMapping.isUpdate() &&
          (partMapping.getAccessAttribute() == null ||
           Intrinsic.isTrue(instance.getValue(partMapping.getAccessAttribute().getOrdinal())));
   }

   /**
    * Gets a list from an object and validates it against message part metadata.
    * @param obj The source object.
    * @param msg The message part metadata.
    */
   public static List getList(Object obj, MessagePart msg)
   {
      if (obj == null)
      {
         if (msg.isRequired())
         {
            throw new IntegrationException("err.integration.minPartCount", new Object[]{msg.getFullPath()});
         }

         return Collections.EMPTY_LIST;
      }

      List list = (List)obj;
      int nCount = list.size();

      if (nCount < msg.getMinCount())
      {
         throw new IntegrationException("err.integration.minPartCount", new Object[]{msg.getFullPath()});
      }

      if (nCount > msg.getMaxCount())
      {
         throw new IntegrationException("err.integration.maxPartCount", new Object[]{msg.getFullPath()});
      }

      return list;
   }

   /**
    * @param oldTobj Old transfer object
    * @param newTobj New transfer object
    * @param composite a message part that determines structure of both oldTobj and newTobj
    * @return whether the two given transfer objects have the same key,
    * as defined in the object mapping of the message part
    */
   public static boolean isSameKey(TransferObject oldTobj, TransferObject newTobj,
      CompositeMessagePart composite, InvocationContext context)
   {
      if (oldTobj == null)
      {
         return (newTobj == null);
      }

      if (oldTobj.getOID() != null && newTobj.getOID() != null) 
      {
         // An image of a freshly created instance could lack OID
         return oldTobj.getOID().equals(newTobj.getOID());
      }

      boolean bKeyIsPresent = false;
      boolean bSameKey = true;
      boolean bSameValues = true;

      for (int nPartIndex = 0, nPartCount = composite.getPartCount(); nPartIndex != nPartCount; ++nPartIndex)
      {
         MessagePart part = composite.getPart(nPartIndex);
         ObjectMessagePartMapping mapping = (ObjectMessagePartMapping)part.getMapping();
         String sName = part.getName();

         if (newTobj.hasValue(sName) || oldTobj.hasValue(sName))
         {
            boolean bDifferent = newTobj.hasValue(sName) != oldTobj.hasValue(sName);

            if (!bDifferent)
            {
               if (part instanceof CompositeMessagePart && !part.isCollection())
               {
                  CompositeMessagePart derivedMessagePart = getPolymorphic((CompositeMessagePart)part, newTobj,
                     context);

                  bDifferent = !isSameKey((TransferObject)oldTobj.getValue(sName),
                     (TransferObject)newTobj.getValue(sName), derivedMessagePart, context);
               }
               else
               {
                  bDifferent = !ObjUtil.equal(oldTobj.getValue(sName), newTobj.getValue(sName));
               }
            }

            // OID attribute might not be populated;
            bSameValues = bSameValues && (!bDifferent
               || mapping.getSystemAttribute() == ObjectMessagePartMapping.ATTR_OID);

            if (mapping.isKey())
            {
               bKeyIsPresent = true;
               bSameKey = bSameKey && !bDifferent;
            }
         }
      }

      if (bKeyIsPresent)
      {
         return bSameKey;
      }

      return bSameValues && "create".equals(oldTobj.getEventName())
         && "create".equals(newTobj.getEventName());
   }

   // abstract methods

   /**
    * @param bNew True, if it is a new instance.
    * @param tobj The transfer object.
    * @param instance The instance.
    * @param composite The message part metadata.
    * @param mapping The message mapping.
    */
   protected abstract Object updateInstance(boolean bNew, TransferObject tobj, Instance instance,
      CompositeMessagePart composite, ObjectMessagePartMapping mapping);

   // inner classes

   /**
    * Formats a collection of messages in a fail safe way by formatting collection chunks
    * on separate units of work and collecting all exceptions
    */
   private class FailSafeFormatter
   {
      /**
       * Defines into how many chunks a collection is split on each recursion.
       * Formatting of each chunk is attempted on a single unit of work.
       */
      // Our chunking uses few queries for each instance anyway - better to process objects one-by-one
      private final static int CHUNK_COUNT = 10000;
      // TODO: either optimize chunking SQL, 
      // or remove chunking code and always process instances one-by-one in a case of failure.

      /**
       * Holder that accumulates all the exceptions raised during formatter execution
       */
      private final CompoundIntegrationException m_ex;

      /**
       * Flag whether transaction should be committed in case when some parts of the message raise an exception
       */
      private final boolean m_bCommit;

      /**
       * Constructor
       */
      private FailSafeFormatter(CompoundIntegrationException ex, boolean bCommit)
      {
         m_ex = ex;
         m_bCommit = bCommit;
      }

      /**
       * Format the given map of object messages by dividing it into chunks
       * and formatting each chunk on a separate unit of work 
       * @param objectParts object part messages mapped by corresponding transfer objects:
       * Message[TransferObject]
       * @return collection of results of successful formatting
       */
      public Collection divideAndFormat(Lookup objectParts)
      {
         int nMapSize = objectParts.size();
         Collection result = new ArrayList(nMapSize);
         int nChunkSize = (nMapSize + CHUNK_COUNT - 1) / CHUNK_COUNT;
         Lookup chunk = new HashTab(nChunkSize);

         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Dividing message of size " + objectParts.size() + " into chunks of size " + nChunkSize);
         }

         for (Lookup.Iterator itr = objectParts.iterator(); itr.hasNext(); )
         {
            chunk.put(itr.next(), itr.getValue());
            
            if (chunk.size() == nChunkSize)
            {
               result.addAll(formatChunk(chunk, 0));
               chunk.clear();
            }
         }

         result.addAll(formatChunk(chunk, 0));

         return result;
      }

      /**
       * Format the given map of object messages on a separate unit of work
       * @param chunk object part messages mapped by corresponding transfer objects: Message[TransferObject]
       * @return collection of results of successful formatting
       */
      public Collection formatChunk(Lookup chunk, int nRetry)
      {
         if (chunk.size() == 0)
         {
            return Collections.EMPTY_LIST;
         }

         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Formatting chunk of size " + chunk.size() + (nRetry == 0 ? "" : ", retry #" + nRetry));
         }

         // We use unit of work to recover from database failures
         UnitOfWork oldUOW = m_context.beginTransaction();

         try
         {
            Collection result = new ArrayList(chunk.size());

            for (Lookup.Iterator itr = chunk.iterator(); itr.hasNext();)
            {
               result.add(format((TransferObject)itr.next(), (CompositeMessagePart)itr.getValue()));
            }

            if (m_bCommit)
            {
               m_context.commitAndResume(oldUOW);
            }
            else
            {
               m_context.rollbackAndResume(oldUOW);
            }

            if (s_logger.isDebugEnabled())
            {
               s_logger.debug("Successfully " + (m_bCommit ? "committed" : "rolled back")
                  + " chunk of size " + chunk.size());

               s_logger.dump(chunk);
            }

            return result;
         }
         catch (Throwable e)
         {
            m_context.rollbackAndResume(oldUOW);

            if ((nRetry < MAX_RETRIES) && DefaultSyncEngine.isTransient(e))
            {
               return formatChunk(chunk, nRetry + 1);
            }

            if (chunk.size() == 1)
            {
               TransferObject tobj = (TransferObject) chunk.iterator().next();

               m_ex.addException(e, tobj);

               if (s_logger.isDebugEnabled())
               {
                  s_logger.debug("Caught single exception ", e);
                  s_logger.dump(tobj);
               }

               return Collections.EMPTY_LIST;
            }
            else
            {
               return divideAndFormat(chunk);
            }
         }
      }
   }

   /**
    * Interface implemented by conflict resolution strategies.
    */
   protected interface ConflictResolutionStrategy
   {
      /**
       * Advises whether NexJ hub should update a particular attribute to a value received from a spoke,
       * in case when there is a conflict with the current value of this attribute on the hub.
       * @param attribute The attribute in conflict.
       * @return true if this conflict resolution algorithm decides that spoke should win this conflict.
       */
      boolean isUpdateRequired(Attribute attribute);

      /**
       * @return whether there was a conflict win for the hub on the given attribute value.
       * @param attribute The attribute in conflict.
       */
      boolean isConflictWon(Attribute attribute);

      /**
       * @return True, if this conflict resolution algorithm decides that external system wins this conflict.
       * Otherwise, false.
       * @param instance The internal instance.
       * @param attribute The attribute.
       * @param value The attribute value.
       */
      boolean shouldUpdate(Instance instance, Attribute attribute, Object value);
   }
}