package nexj.core.rpc.queueing;

import java.util.ArrayList;
import java.util.List;

import nexj.core.integration.Receiver;
import nexj.core.meta.Component;
import nexj.core.meta.integration.channel.queueing.ObjectQueue;
import nexj.core.monitoring.jmx.StatManager;
import nexj.core.monitoring.jmx.StatManagerAware;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.GenericSerializablePropertyMap;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.UnitOfWork;
import nexj.core.util.Binary;
import nexj.core.util.ObjUtil;
import nexj.core.util.PropertyIterator;

/**
 * The ObjectReceiver delegates to ObjectQueueDispatcher
 */
public class ObjectReceiver extends Receiver implements ObjectDispatchListener, StatManagerAware
{
   // associations

   /**
    * The ObjectQueueDispatcher.
    */
   protected Component m_dispatcherComponent;

   /**
    * The object queue channel.
    */
   protected ObjectQueue m_channel;

   /**
    * The server list.
    */
   protected List m_serverList;

   // operations

   /**
    * Sets the message queue channel.
    * @param channel The message queue channel to set.
    */
   public void setChannel(ObjectQueue channel)
   {
      m_channel = channel;
   }

   /**
    * @return The message queue channel.
    */
   public ObjectQueue getChannel()
   {
      return m_channel;
   }

   /**
    * Adds a server to the list of servers to consider.
    * @param server The server to add.
    */
   public void addServer(ObjectServer server)
   {
      server.setReceiver(this);

      if (m_serverList == null)
      {
         m_serverList = new ArrayList(4);
      }

      m_serverList.add(server);
   }

   /**
    * Sets the ObjectQueueDispatcher.
    * @param dispatcherComponent The ObjectQueueDispatcher to set.
    */
   public void setDispatcherComponent(Component dispatcherComponent)
   {
      m_dispatcherComponent = dispatcherComponent;
   }

   /**
    * @return The ObjectQueueDispatcher.
    */
   public Component getDispatcherComponent()
   {
      return m_dispatcherComponent;
   }

   /**
    * @see nexj.core.rpc.queueing.ObjectDispatchListener#onMessage(int, java.lang.Object[])
    */
   public void onMessage(DispatcherMessage message)
   {
      if (m_dispatcherComponent != null)
      {
         if (message.getType() == DispatcherMessage.WAKE)
         {
            message.respond(null);

            return;
         }

         ((ObjectDispatchListener)m_dispatcherComponent.getInstance(null)).onMessage(message);
      }
   }

   /**
    * Receive a message by a server.
    * @param context the context.
    * @param instance the persisted message, may be null.
    * @param message the message.
    * @return true if the message has been received.
    */
   protected boolean receiveServerMessage(InvocationContext context, Instance instance, TransferObject message)
   {
      if (message.hasValue(ObjectSender.PROTECTED))
      {
         context.setProtected(((Boolean)message.getValue(ObjectSender.PROTECTED)).booleanValue());
      }

      if (message.hasValue(ObjectSender.STATE))
      {
         GenericSerializablePropertyMap map = new GenericSerializablePropertyMap();

         map.deserializeValues(message.getValue(ObjectSender.STATE), context);

         UnitOfWork uow = context.getUnitOfWork();

         for (PropertyIterator itr = map.getIterator(); itr.hasNext();)
         {
            itr.next();
            uow.setValue(itr.getName(), itr.getValue());
         }
      }

      boolean bProcessed = false;

      if (m_serverList != null)
      {
         Object body;

         if (message.hasValue(ObjectSender.OBJECT))
         {
            try
            {
               body = ((Binary)message.getValue(ObjectSender.OBJECT)).toObject();
            }
            catch (Exception e)
            {
               throw ObjUtil.rethrow(e);
            }
         }
         else
         {
            body = message.findValue(ObjectSender.BODY);
         }

         if (body != null)
         {
            for (int i = 0, nCount = m_serverList.size(); i < nCount; ++i)
            {
               bProcessed = ((ObjectServer)m_serverList.get(i)).receive(body, instance, context);
   
               if (bProcessed)
               {
                  break;
               }
            }
         }
      }

      if (!bProcessed)
      {
         if (isBound(m_channel, context))
         {
            receive(message, m_channel, context);
         }
         else
         {
            return false;
         }
      }

      return true;
   }

   /**
    * @see nexj.core.monitoring.jmx.StatManagerAware#setStatManager(nexj.core.monitoring.jmx.StatManager)
    */
   public void setStatManager(StatManager statManager)
   {
   }
}
