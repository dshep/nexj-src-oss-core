// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.resource.NotSupportedException;
import javax.resource.ResourceException;

import nexj.core.meta.Component;
import nexj.core.meta.Metadata;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataFinder;
import nexj.core.meta.MetadataLookupException;
import nexj.core.meta.MetadataObject;
import nexj.core.meta.NamedMetadataObject;
import nexj.core.meta.integration.service.Binding;
import nexj.core.meta.integration.service.Interface;
import nexj.core.meta.integration.service.Service;
import nexj.core.util.HashTab;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.pool.consumer.ConsumerConfig;
import nexj.core.util.pool.consumer.ConsumerPool;
import nexj.core.util.pool.resource.ResourcePool;

/**
 * Communication channel metadata.
 * A communication channel has a client adapter for sending
 * outgoing messages, implementing the Sender interface,
 * and a server adapter receiving incoming messages and
 * optionally invoking a service implementation.
 */
public abstract class Channel extends NamedMetadataObject implements MetadataFinder, ConsumerConfig
{
   // constants

   /**
    * Multiple services declaring the same message in the interface are not supported. 
    */
   public final static byte COMBINE_NONE = 0;
   
   /**
    * Only the first one among the services with a duplicate message is invoked.
    */
   public final static byte COMBINE_FIRST = 1;
   
   /**
    * All the services with a duplicate message are invoked.
    */
   public final static byte COMBINE_ALL = 2;

   /**
    * Channel persistent statistic class.
    */
   public final static String STAT_PERSIST_CLASS = "SysChannelStat";

   /**
    * Total count name.
    */
   public final static String STAT_TOTAL_COUNT = "Total Message Count";

   /**
    * Average send time statistics name.
    */
   public final static String STAT_AVERAGE_SEND_TIME = "Average Send Time (ms)";

   /**
    * Average receive time statistics name.
    */
   public final static String STAT_AVERAGE_RECEIVE_TIME = "Average Receive Time (ms)";

   // attributes

   /**
    * The sendable flag.
    */
   protected boolean m_bSendable = true;

   /**
    * The receivable flag.
    */
   protected boolean m_bReceivable = true;

   /**
    * The stealth flag.
    * True to suppress all the logging for the channel at
    * levels above ALL, unless the system category is enabled.
    */
   protected boolean m_bStealth;

   /**
    * The duplicate message combination mode, one of the COMBINE_* constants.
    */
   protected byte m_nCombinationMode;

   /**
    * Sender's statistic path.
    */
   protected String m_sSenderStatPath;

   /**
    * Receiver's statistic path.
    */
   protected String m_sReceiverStatPath;

   /**
    * The category of the channel.
    */
   protected String m_sCategory;

   // associations

   /**
    * The channel type.
    */
   protected ChannelType m_type;

   /**
    * The sender adapter component.
    */
   protected Component m_sender;

   /**
    * The receiver adapter component.
    */
   protected Component m_receiver;

   /**
    * The message table.
    */
   protected MessageTable m_messageTable;

   /**
    * The message to service list map: List[Message].
    */
   protected Lookup m_messageMap;

   /**
    * The service binding collection.
    */
   protected List m_bindingList = new ArrayList(2); // of type Binding

   /**
    * The resource pool.
    */
   protected ResourcePool m_resourcePool;

   /**
    * The consumer pool.
    */
   protected ConsumerPool m_consumerPool;

   // constructors

   /**
    * Constructs the channel.
    * @param sName The channel name.
    */
   public Channel(String sName)
   {
      super(sName);
   }

   // operations
   
   /**
    * Sets the channel type.
    * @param type The channel type to set.
    */
   public void setType(ChannelType type)
   {
      verifyNotReadOnly();
      m_type = type;
   }

   /**
    * Get sender statistic path.
    */
   public String getSenderStatPath()
   {
      return m_sSenderStatPath;
   }

   /**
    * Get receiver statistic path.
    */
   public String getReceiverStatPath()
   {
      return m_sReceiverStatPath;
   }

   /**
    * Initialize statistics path of sender.
    */
   protected void initSenderStatPath()
   {
      m_sSenderStatPath = "Channel/" + m_type.getName() + "/Sender/" + m_sName;
   }

   /**
    * Initialize statistics path of receiver.
    */
   protected void initReceiverStatPath()
   {
      m_sReceiverStatPath = "Channel/" + m_type.getName() + "/Receiver/" + m_sName;
   }

   /**
    * @return The channel type.
    */
   public ChannelType getType()
   {
      return m_type;
   }

   /**
    * @return True if the channel can participate in distributed transactions.
    */
   public abstract boolean isTransactional();
   
   /**
    * @return True if the channel is synchronous,
    * i.e. if send-receive can be done within the same transaction.
    */
   public abstract boolean isSynchronous();
   
   /**
    * Sets the sendable flag.
    * @param bSendable The sendable flag to set.
    */
   public void setSendable(boolean bSendable)
   {
      verifyNotReadOnly();
      m_bSendable = bSendable;

      if (m_bSendable)
      {
         initSenderStatPath();
      }
   }

   /**
    * @return The sendable flag.
    */
   public boolean isSendable()
   {
      return m_bSendable;
   }
   
   /**
    * Sets the receivable flag.
    * @param bReceivable The receivable flag to set.
    */
   public void setReceivable(boolean bReceivable)
   {
      verifyNotReadOnly();
      m_bReceivable = bReceivable;

      if (m_bReceivable)
      {
         initReceiverStatPath();
      }
   }

   /**
    * @return The receivable flag.
    */
   public boolean isReceivable()
   {
      return m_bReceivable;
   }

   /**
    * @return True if the channel is configured as a sender, a receiver or both.
    */
   public boolean isEnabled()
   {
      return m_bSendable || m_bReceivable;
   }

   /**
    * Sets the stealth flag.
    * @param bStealth The stealth flag to set.
    */
   public void setStealth(boolean bStealth)
   {
      verifyNotReadOnly();
      m_bStealth = bStealth;
   }

   /**
    * @return The stealth flag.
    */
   public boolean isStealth()
   {
      return m_bStealth;
   }

   /**
    * Sets the category.
    * @param sCategory The category.
    */
   public void setCategory(String sCategory)
   {
      verifyNotReadOnly();
      m_sCategory = sCategory;
   }

   /**
    * Sets the sender adapter component.
    * @param sender The sender adapter component to set.
    */
   public void setSender(Component sender)
   {
      verifyNotReadOnly();
      m_sender = sender;
   }

   /**
    * @return The sender adapter component.
    */
   public Component getSender()
   {
      return m_sender;
   }

   /**
    * Sets the receiver adapter component.
    * @param receiver The receiver adapter component to set.
    */
   public void setReceiver(Component receiver)
   {
      verifyNotReadOnly();
      m_receiver = receiver;
   }

   /**
    * @return The receiver adapter component.
    */
   public Component getReceiver()
   {
      return m_receiver;
   }

   /**
    * @return The maximum receiver thread pool size. -1 means unlimited.
    */
   public int getMaxReceivers()
   {
      return 0;
   }

   /**
    * Sets the duplicate message combination mode.
    * @param nCombinationMode The duplicate message combination mode to set,
    * one of the COMBINE_* constants.
    */
   public void setCombinationMode(byte nCombinationMode)
   {
      verifyNotReadOnly();
      m_nCombinationMode = nCombinationMode;
   }

   /**
    * @return The duplicate message combination mode, one of the COMBINE_* constants.
    */
   public byte getCombinationMode()
   {
      return m_nCombinationMode;
   }
   
   /**
    * @return The message table.
    */
   public MessageTable getMessageTable()
   {
      return m_messageTable;
   }
   
   /**
    * Adds a new service binding to the channel.
    * @param binding The service binding to add.
    */
   public void addBinding(Binding binding) throws MetadataException
   {
      verifyNotReadOnly();
      
      Service service = binding.getService();
      Interface iface = service.getInterface();
      
      for (int i = m_bindingList.size() - 1; i >= 0; --i)
      {
         Binding prevbnd = (Binding)m_bindingList.get(i);
         Service prevsvc = prevbnd.getService();
         
         if (prevsvc == service && m_nCombinationMode != COMBINE_ALL)
         {
            throw new MetadataException("err.meta.integration.bindingDup",
               new Object[]{service.getFullName(), getName()});
         }

         if (i == 0)
         {
            if (prevsvc.getInterface() != null)
            {
               if (iface != null)
               {
                  if (iface.getFormat() != prevsvc.getInterface().getFormat())
                  {
                     throw new MetadataException("err.meta.integration.binding.formatMismatch",
                        new Object[]{service.getFullName(), getName()});
                  }
               }
               else if (m_nCombinationMode != COMBINE_ALL)
               {
                  throw new MetadataException("err.meta.integration.binding.genericService",
                     new Object[]{service.getFullName(), getName()});
               }
            }
            else if (iface != null || m_nCombinationMode != COMBINE_ALL)
            {
               throw new MetadataException("err.meta.integration.binding.genericService",
                  new Object[]{service.getFullName(), getName()});
            }
         }
      }

      if (iface != null)
      {
         MessageTable table = iface.getRequestTable();

         if (m_messageTable == null)
         {
            m_messageTable = new MessageTable();
            m_messageTable.setFormat(iface.getFormat());
            m_messageMap = new HashTab(table.getMessageCount());
         }

         for (int i = 0, n = table.getMessageCount(); i != n; ++i)
         {
            Message message = table.getMessage(i);
            List list = (List)m_messageMap.get(message);

            if (list == null)
            {
               list = new ArrayList(1);
               m_messageMap.put(message, list);
               list.add(binding);
            }
            else
            {
               switch (m_nCombinationMode)
               {
                  case COMBINE_NONE:
                     throw new MetadataException("err.meta.integration.binding.messageDup",
                        new Object[]{message.getName(), service.getFullName(),
                           ((Binding)list.get(0)).getService().getFullName(), getName()});
                     
                  case COMBINE_ALL:
                     list.add(binding);
                     break;
               }
            }

            if (m_messageTable.findMessage(message.getName()) == null)
            {
               m_messageTable.addMessage(message);
            }
         }
      }

      m_bindingList.add(binding);
      binding.setChannel(this);
   }

   /**
    * Gets a service binding by ordinal number.
    * @param nOrdinal The service binding ordinal number (0-based).
    * @return The service binding object.
    */
   public Binding getBinding(int nOrdinal)
   {
      return (Binding)m_bindingList.get(nOrdinal);
   }

   /**
    * @return The service binding count.
    */
   public int getBindingCount()
   {
      return m_bindingList.size();
   }

   /**
    * @return An iterator for the contained service binding objects.
    */
   public Iterator getBindingIterator()
   {
      return m_bindingList.iterator();
   }
   
   /**
    * Gets a service binding iterator for a given message.
    * @param message The message.
    * @return The service binding iterator.
    * @throws MetadataLookupException if no service bindings are available for this message.
    */
   public Iterator getBindingIterator(Message message) throws MetadataLookupException
   {
      if (m_messageMap != null)
      {
         List list = (List)m_messageMap.get(message);
         
         if (list != null)
         {
            return list.iterator();
         }
      }

      throw new MetadataLookupException("err.meta.integration.bindingLookup",
         message.getName(), getName());
   }

   /**
    * @param channel A channel whose attributes will be copied over to this channel.
    * @throws ResourceException if the associated consumer pool fails to startup again
    *    or the operation is not supported.
    */
   public void update(Channel channel) throws ResourceException
   {
      throw new NotSupportedException();
   }

   /**
    * @see nexj.core.meta.MetadataFinder#find(nexj.core.meta.Metadata)
    */
   public MetadataObject find(Metadata metadata)
   {
      return metadata.findChannel(m_sName);
   }

   /**
    * @see nexj.core.util.pool.resource.ResourcePoolProvider#setResourcePool(nexj.core.util.pool.resource.ResourcePool)
    */
   public synchronized void setResourcePool(ResourcePool pool)
   {
      m_resourcePool = pool;
   }

   /**
    * @see nexj.core.util.pool.resource.ResourcePoolProvider#getResourcePool()
    */
   public synchronized ResourcePool getResourcePool()
   {
      return m_resourcePool;
   }

   /**
    * @see nexj.core.util.pool.consumer.ConsumerPoolProvider#setConsumerPool(ConsumerPool)
    */
   public synchronized void setConsumerPool(ConsumerPool pool)
   {
      m_consumerPool = pool;
   }

   /**
    * @see nexj.core.util.pool.consumer.ConsumerPoolProvider#getConsumerPool()
    */
   public synchronized ConsumerPool getConsumerPool()
   {
      return m_consumerPool;
   }

   /**
    * @see nexj.core.util.pool.consumer.ConsumerConfig#getMaxPoolSize()
    */
   public int getMaxPoolSize()
   {
      return getMaxReceivers();
   }

   /**
    * @see nexj.core.util.pool.consumer.ConsumerConfig#getRetryDelay()
    */
   public long getRetryDelay()
   {
      return 8000;
   }

   /**
    * @see nexj.core.util.pool.consumer.ConsumerConfig#getMaxRetryDelay()
    */
   public long getMaxRetryDelay()
   {
      return 120000;
   }

   /**
    * @see nexj.core.util.pool.consumer.ConsumerConfig#getTransactionTimeout()
    */
   public int getTransactionTimeout()
   {
      return 0;
   }

   /**
    * @see nexj.core.util.pool.consumer.ConsumerConfig#isCompatible(nexj.core.util.pool.consumer.ConsumerConfig)
    */
   public boolean isCompatible(ConsumerConfig config)
   {
      return false;
   }

   /**
    * @return A logger for the channel sender and receiver.
    */
   public Logger getLogger()
   {
      return Logger.getLogger(Channel.class.getName() + '.' + m_sName);
   }

   /**
    * @return A system log enabler for stealth mode. Null if stealth mode is disabled.
    */
   public Logger getEnabler()
   {
      return (m_bStealth) ? Logger.getSystemLogger(Channel.class.getName() + '.' + m_sName) : null;
   }

   /**
    * @see nexj.core.meta.MetadataObject#makeReadOnly()
    */
   public void makeReadOnly()
   {
      super.makeReadOnly();

      if (m_messageTable != null)
      {
         m_messageTable.makeReadOnly();
      }

      ((ArrayList)m_bindingList).trimToSize();

      for (int i = m_bindingList.size() - 1; i >= 0; i--)
      {
         ((Binding)m_bindingList.get(i)).makeReadOnly();
      }
   }
}
