// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.channel.jms;

import java.util.Iterator;

import org.w3c.dom.Element;

import nexj.core.meta.Component;
import nexj.core.meta.ComponentPropertyInitializer;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataValidationException;
import nexj.core.meta.PrimitivePropertyInitializer;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.ChannelType;
import nexj.core.meta.integration.XMLIntegrationMetadataLoader;
import nexj.core.meta.xml.XMLMetadataHelper;
import nexj.core.meta.xml.XMLMetadataLoader;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;
import nexj.core.util.XMLUtil;

/**
 * XML Message queue metadata loader.
 */
public class XMLMessageQueueMetadataLoader implements XMLIntegrationMetadataLoader
{
   /**
    * @see nexj.core.meta.integration.XMLIntegrationMetadataLoader#loadChannel(org.w3c.dom.Element, java.lang.String, nexj.core.meta.integration.ChannelType, nexj.core.meta.xml.XMLMetadataLoader)
    */
   public Channel loadChannel(Element element, String sName, ChannelType type, final XMLMetadataLoader loader)
   {
      final MessageQueue mq = new MessageQueue(sName);

      mq.setType(type);
      loader.loadChannel(element, mq);
      
      mq.setTransactionTimeout(XMLUtil.getIntAttr(element, "transactionTimeout"));

      mq.setAlias(XMLUtil.getStringAttr(element, "alias"));

      if (mq.getAlias() == null)
      {
         mq.setAlias(XMLMetadataHelper.makeAlias(mq.getName()));
      }

      String sAck = XMLUtil.getStringAttr(element, "acknowledgement");

      if (sAck != null)
      {
         if (sAck.equals("transacted"))
         {
            mq.setAckMode(MessageQueue.ACK_TX);
         }
         else if (sAck.equals("auto"))
         {
            mq.setAckMode(MessageQueue.ACK_AUTO);
         }
         else if (sAck.equals("lazy"))
         {
            mq.setAckMode(MessageQueue.ACK_LAZY);
         }
         else
         {
            throw new MetadataException("err.meta.integration.mq.ack", new Object[]{sAck, sName});
         }
      }

      mq.setBroadcast(XMLUtil.getBooleanAttr(element, "broadcast", mq.isBroadcast()));
      mq.setPersistent(XMLUtil.getBooleanAttr(element, "persistent", mq.isPersistent()));
      mq.setPriority(XMLUtil.getIntAttr(element, "priority", mq.getPriority()));
      mq.setTimeToLive(XMLUtil.getLongAttr(element, "ttl", mq.getTimeToLive()));
      mq.setMaxSenders(XMLUtil.getIntAttr(element, "maxSenders", mq.getMaxSenders()));
      mq.setMaxReceivers(XMLUtil.getIntAttr(element, "maxReceivers", mq.getMaxReceivers()));
      mq.setTrusted(XMLUtil.getBooleanAttr(element, "trusted", mq.isTrusted()));
      mq.setLoopback(XMLUtil.getBooleanAttr(element, "loopback", mq.isLoopback()));
      mq.setDefaultUser(XMLUtil.getStringAttr(element, "defaultUser"));
      mq.setSelector(XMLUtil.getStringAttr(element, "selector", mq.getSelector()));
      mq.setSubscription(XMLUtil.getStringAttr(element, "subscription", mq.getSubscription()));
      mq.setClientId(XMLUtil.getStringAttr(element, "clientId", mq.getClientId()));
      mq.setErrorCount(XMLUtil.getIntAttr(element, "errorCount", mq.getErrorCount()));
      
      final String sErrorQueue = XMLUtil.getStringAttr(element, "errorQueue");
      
      if (sErrorQueue != null)
      {
         loader.addIOFixup(new XMLMetadataHelper.ContextFixup(loader.getHelper())
         {
            public void fixup()
            {
               mq.setErrorQueue(loader.getMetadata().getChannel(sErrorQueue));
            }
         });
      }
      
      final String sReplyQueue = XMLUtil.getStringAttr(element, "replyQueue");
      
      if (sReplyQueue != null)
      {
         loader.addIOFixup(new XMLMetadataHelper.ContextFixup(loader.getHelper())
         {
            public void fixup()
            {
               mq.setReplyQueue(loader.getMetadata().getChannel(sReplyQueue));
            }
         });
      }

      boolean bFirstMessageQueue = true;

      for (Iterator itr = loader.getMetadata().getChannelIterator(); itr.hasNext() && bFirstMessageQueue; )
      {
         Channel channel = (Channel)itr.next();

         if (channel instanceof MessageQueue)
         {
            bFirstMessageQueue = false;
         }
      }

      if (bFirstMessageQueue)
      {
         loader.addIOFixup(new XMLMetadataHelper.ContextFixup(loader.getHelper())
         {
            public void fixup()
            {
               Lookup aliasQueueMap = new HashTab();

               for (Iterator itr = loader.getMetadata().getChannelIterator(); itr.hasNext(); )
               {
                  Channel channel = (Channel)itr.next();

                  if (channel instanceof MessageQueue)
                  {
                     MessageQueue queue = (MessageQueue)channel;
                     MessageQueue oldQueue = (MessageQueue)aliasQueueMap.put(queue.getAlias(), queue);

                     if (oldQueue == null)
                     {
                        queue.setFirst(true);
                     }
                     else
                     {
                        queue.setFirst(false);
                        aliasQueueMap.put(queue.getAlias(), oldQueue);

                        if (!ObjUtil.equal(queue.getDestination(), oldQueue.getDestination()))
                        {
                           throw new MetadataValidationException("err.meta.integration.mq.destinationMismatch",
                              new Object[]{queue.getName(), oldQueue.getName(), queue.getAlias()});
                        }

                        if (queue.isBroadcast() != oldQueue.isBroadcast())
                        {
                           throw new MetadataValidationException("err.meta.integration.mq.broadcastMismatch",
                              new Object[]{queue.getName(), oldQueue.getName(), queue.getAlias()});
                        }
                     }
                  }
               }
            }
         });
      }

      return mq;
   }

   /**
    * @see nexj.core.meta.integration.XMLIntegrationMetadataLoader#loadConnection(org.w3c.dom.Element, nexj.core.meta.integration.Channel, nexj.core.meta.xml.XMLMetadataLoader)
    */
   public void loadConnection(Element element, Channel channel, final XMLMetadataLoader loader)
   {
      final MessageQueue mq = (MessageQueue)channel;

      if (element != null)
      {
         mq.setAlias(XMLUtil.getStringAttr(element, "alias", mq.getAlias()));
         mq.setUser(XMLUtil.getStringAttr(element, "user"));
         mq.setPassword(loader.decryptPassword(XMLUtil.getStringAttr(element, "password")));
         mq.setErrorCount(XMLUtil.getIntAttr(element, "errorCount", mq.getErrorCount()));
         mq.setMaxSenders(XMLUtil.getIntAttr(element, "maxSenders", mq.getMaxSenders()));
         mq.setMaxReceivers(XMLUtil.getIntAttr(element, "maxReceivers", mq.getMaxReceivers()));
         mq.setSubscription(XMLUtil.getStringAttr(element, "subscription", mq.getSubscription()));
         mq.setClientId(XMLUtil.getStringAttr(element, "clientId", mq.getClientId()));
         mq.setConnectionFactory(XMLUtil.getStringAttr(element, "factory", mq.getConnectionFactory()));
         mq.setDestination(XMLUtil.getStringAttr(element, "destination", mq.getDestination()));
         mq.setTransactionTimeout(XMLUtil.getIntAttr(element,
            "transactionTimeout", mq.getTransactionTimeout()));
         loader.loadProperties(element, "FactoryProperties", mq.getConnectionFactoryPropertyHolder());
         loader.loadProperties(element, "DestinationProperties", mq.getDestinationPropertyHolder());
      }

      if (mq.isClient())
      {
         Component component = new Component("JMSSender." + channel.getName(), channel.getType().getSender(), Component.SINGLETON);

         loader.getMetadata().addComponent(component);
         loader.addSingletonFixup(component);
         mq.setSender(component);
         component.addPrimitivePropertyInitializer("channel", channel);

         final ComponentPropertyInitializer initializer = component.addComponentPropertyInitializer("transactionManager", null);

         loader.addComponentFixup(new XMLMetadataHelper.ContextFixup(loader.getHelper())
         {
            public void fixup()
            {
               initializer.setInstanceComponent(loader.getMetadata().getComponent("System.TransactionManager"));
            }
         });
      }

      if (mq.isReceivable())
      {
         Component component = new Component("JMSReceiver." + channel.getName(), channel.getType().getReceiver(), Component.SINGLETON);

         loader.addSingletonFixup(component);
         mq.setReceiver(component);
         component.setMetadata(loader.getMetadata());
         component.addPrimitivePropertyInitializer("channel", channel);

         final PrimitivePropertyInitializer initializer = component.addPrimitivePropertyInitializer("contextComponent", null);

         loader.addComponentFixup(new XMLMetadataHelper.ContextFixup(loader.getHelper())
         {
            public void fixup()
            {
               initializer.setValue(loader.getMetadata().getComponent("System.InvocationContext"));
            }
         });
      }
   }
}
