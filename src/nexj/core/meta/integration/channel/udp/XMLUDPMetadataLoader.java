// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.channel.udp;

import org.w3c.dom.Element;

import nexj.core.meta.Component;
import nexj.core.meta.PrimitivePropertyInitializer;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.ChannelType;
import nexj.core.meta.integration.XMLIntegrationMetadataLoader;
import nexj.core.meta.xml.XMLMetadataHelper;
import nexj.core.meta.xml.XMLMetadataLoader;
import nexj.core.util.XMLUtil;

/**
 * XML metadata loader for UDP channel
 */
public class XMLUDPMetadataLoader implements XMLIntegrationMetadataLoader
{
   /**
    * @see nexj.core.meta.integration.XMLIntegrationMetadataLoader#loadChannel(org.w3c.dom.Element, java.lang.String, nexj.core.meta.integration.ChannelType, nexj.core.meta.xml.XMLMetadataLoader)
    */
   public Channel loadChannel(Element element, String sName, ChannelType type, final XMLMetadataLoader loader)
   {
      final UDPChannel udp = new UDPChannel(sName);

      udp.setType(type);
      loader.loadChannel(element, udp);

      udp.setEncoding(XMLUtil.getStringAttr(element, "encoding", udp.getEncoding()));
      udp.setPort(XMLUtil.getIntAttr(element, "port", udp.getPort()));
      udp.setLocalPort(XMLUtil.getIntAttr(element, "localPort", udp.getLocalPort()));
      udp.setGroup(XMLUtil.getStringAttr(element, "group", udp.getGroup()));
      udp.setTTL(XMLUtil.getIntAttr(element, "ttl", udp.getTTL()));
      udp.setTOS(XMLUtil.getIntAttr(element, "tos", udp.getTOS()));
      udp.setMaxPacketSize(XMLUtil.getIntAttr(element, "maxPacketSize", udp.getMaxPacketSize()));
      udp.setMaxSenders(XMLUtil.getIntAttr(element, "maxSenders", udp.getMaxSenders()));
      udp.setMaxReceivers(XMLUtil.getIntAttr(element, "maxReceivers", udp.getMaxReceivers()));
      udp.setDefaultUser(XMLUtil.getStringAttr(element, "defaultUser", udp.getDefaultUser()));

      final String sQueue = XMLUtil.getStringAttr(element, "queue");

      if (sQueue != null)
      {
         loader.addIOFixup(new XMLMetadataHelper.ContextFixup(loader.getHelper())
         {
            public void fixup()
            {
               udp.setQueue(loader.getMetadata().getChannel(sQueue));
            }
         });
      }

      return udp;
   }

   /**
    * @see nexj.core.meta.integration.XMLIntegrationMetadataLoader#loadConnection(org.w3c.dom.Element, nexj.core.meta.integration.Channel, nexj.core.meta.xml.XMLMetadataLoader)
    */
   public void loadConnection(Element element, Channel channel, final XMLMetadataLoader loader)
   {
      final UDPChannel udp = (UDPChannel)channel;

      if (element != null)
      {
         udp.setEncoding(XMLUtil.getStringAttr(element, "encoding", udp.getEncoding()));
         udp.setHost(XMLUtil.getStringAttr(element, "host", null));
         udp.setPort(XMLUtil.getIntAttr(element, "port", udp.getPort()));
         udp.setLocalHost(XMLUtil.getStringAttr(element, "localHost", udp.getLocalHost()));
         udp.setLocalPort(XMLUtil.getIntAttr(element, "localPort", udp.getLocalPort()));
         udp.setGroup(XMLUtil.getStringAttr(element, "group", udp.getGroup()));
         udp.setTTL(XMLUtil.getIntAttr(element, "ttl", udp.getTTL()));
         udp.setMaxSenders(XMLUtil.getIntAttr(element, "maxSenders", udp.getMaxSenders()));
         udp.setMaxReceivers(XMLUtil.getIntAttr(element, "maxReceivers", udp.getMaxReceivers()));
         udp.setTOS(XMLUtil.getIntAttr(element, "tos", udp.getTOS()));
      }

      if (udp.isSendable())
      {
         Component component = new Component("UDPSender." + channel.getName(), channel.getType().getSender(), Component.SINGLETON);
         loader.getMetadata().addComponent(component);
         loader.addSingletonFixup(component);
         udp.setSender(component);
         component.addPrimitivePropertyInitializer("channel", channel);
      }

      if (udp.isReceivable())
      {
         Component component = new Component("UDPReceiver." + channel.getName(), channel.getType().getReceiver(), Component.SINGLETON);

         loader.addSingletonFixup(component);
         udp.setReceiver(component);
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
