// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.object;

import nexj.core.integration.CompoundIntegrationException;
import nexj.core.integration.IntegrationException;
import nexj.core.integration.MessageFormatter;
import nexj.core.integration.Output;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.service.Sync;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.util.Logger;

/**
 * Object message formatter.
 */
public class ObjectMessageFormatter implements MessageFormatter, InvocationContextAware
{
   // associations

   /**
    * The object message formatter strategy object.
    */
   private ObjectMessageFormatterStrategy m_formatterStrategy;

   /**
    * The sync link for which the formatter strategy is configured.
    */
   private Instance m_link;

   /**
    * The invocation context.
    */
   private InvocationContext m_context;

   // operations

   /**
    * If object message formatter has already been initialized, does nothing.  Otherwise initializes with null link.
    */
   public void initFormatterStrategy()
   {
      if (m_formatterStrategy == null)
      {
         initFormatterStrategy(null);
      }
   }

   /**
    * Initializes the appropriate object message formatter strategy based one of the following conditions:
    * Synchronization link is null, synchronization link is not null and the use of sync objects is allowed, or
    * synchronization link is not null and the use of sync objects is not allowed.
    * @param syncLink The OIDHolder of the synchronization link - source of data used for this persist step to set.
    */
   public void initFormatterStrategy(Sync.Link syncLink)
   {
      Instance link = (syncLink == null || syncLink.getLink() == null) ? null : syncLink.getLink();

      if (m_formatterStrategy == null || link != m_link)
      {
         m_link = link;

         if (syncLink == null)
         {
            m_formatterStrategy = (ObjectMessageFormatterStrategy)new BasicFormatter();
         }
         else
         {
            Instance target = (link != null) ? (Instance)link.getValue("target") : null;

            boolean bUseSyncObjects = (target == null) ? false :
               ((Boolean)target.getValue("SYNC_OBJECTS_USED", "USE_SYNC_OBJECTS")).booleanValue();

            String sFormatterClassName = (bUseSyncObjects) ?
               "nexj.core.integration.format.object.StatefulSyncObjectFormatter" :
                  "nexj.core.integration.format.object.StatelessSyncObjectFormatter";

            try
            {
               m_formatterStrategy = (ObjectMessageFormatterStrategy)Class.forName(sFormatterClassName).newInstance();
            }
            catch (Throwable t)
            {
               throw new IntegrationException("err.integration.format.classLoad", new Object[]{sFormatterClassName}, t);
            }
         }

         m_formatterStrategy.setInvocationContext(m_context);
         m_formatterStrategy.setSyncLink(syncLink);
      }
   }

   /**
    * @see nexj.core.runtime.InvocationContextAware#setInvocationContext(nexj.core.runtime.InvocationContext)
    */
   public void setInvocationContext(InvocationContext context)
   {
      m_context = context;
   }

   /**
    * @see nexj.core.integration.MessageFormatter#format(nexj.core.rpc.TransferObject,nexj.core.meta.integration.Message, nexj.core.integration.Output)
    */
   public void format(TransferObject tobj, Message message, Output out) throws IntegrationException
   {
      initFormatterStrategy();
      m_formatterStrategy.format(tobj, message, out);
   }

   /**
    * Formats the object message to the specified output.
    * When transfer object is mapped to several independent instances using a composite object message
    * (the message root has no object mapping, its descendants use "ref" mapping), this method collects
    * exceptions thrown when formatting different instances,
    * and formats all those instances that do not raise any exceptions.
    *
    *  Note that this method starts new units of work in order to be able to detect
    *  and recover from database layer exceptions. Thus, the caller must make sure that any database operations
    *  performed on the current unit of work do not interfere with the  formatting logic.
    *  For example, if format will attempt to read a database row updated on the current unit of work,
    *  the database read will lock indefinitely.
    *
    * @param tobj The object message to format.
    * @param message The message metadata.
    * @param out The message output. It will be populated even if this method throws an exception.
    * @param bCommit flag whether results of successful formatting should be committed
    * in case when some parts of the message raise an exception
    * @throws a collection of all the exceptions that occur during formatting.
    */
   public void formatFailSafe(TransferObject tobj, Message message, Output out, boolean bCommit)
      throws CompoundIntegrationException
   {
      initFormatterStrategy();
      m_formatterStrategy.formatFailSafe(tobj, message, out, bCommit, 0);
   }

   /**
    * @return The Logger instance if it exists. Otherwise, return null.
    */
   public Logger getLinkLogger()
   {
      initFormatterStrategy();

      return m_formatterStrategy.getLinkLogger();
   }
}