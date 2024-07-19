package nexj.core.meta.integration.channel.queueing;

import org.w3c.dom.Element;

import nexj.core.meta.Component;
import nexj.core.meta.PrimitivePropertyInitializer;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.ChannelType;
import nexj.core.meta.integration.XMLIntegrationMetadataLoader;
import nexj.core.meta.xml.XMLMetadataHelper;
import nexj.core.meta.xml.XMLMetadataLoader;

/**
 * The loader for ObjectQueueChannel and connections
 */
public class XMLObjectQueueMetadataLoader implements XMLIntegrationMetadataLoader
{

   /**
    * @see nexj.core.meta.integration.XMLIntegrationMetadataLoader#loadChannel(org.w3c.dom.Element, java.lang.String, nexj.core.meta.integration.ChannelType, nexj.core.meta.xml.XMLMetadataLoader)
    */
   public Channel loadChannel(Element element, String sName, ChannelType type, XMLMetadataLoader loader)
   {
      final ObjectQueue channel = new ObjectQueue(sName);

      channel.setType(type);
      loader.loadChannel(element, channel);

      return channel;
   }

   /**
    * @see nexj.core.meta.integration.XMLIntegrationMetadataLoader#loadConnection(org.w3c.dom.Element, nexj.core.meta.integration.Channel, nexj.core.meta.xml.XMLMetadataLoader)
    */
   public void loadConnection(Element element, Channel channel, final XMLMetadataLoader loader)
   {
      final ObjectQueue msg = (ObjectQueue)channel;

      if (msg.isSendable())
      {
         Component component = new Component("ObjectSender." + channel.getName(), channel.getType().getSender(), Component.CONTEXT);
         loader.getMetadata().addComponent(component);
         msg.setSender(component);
         component.addPrimitivePropertyInitializer("channel", channel);
      }

      if (msg.isReceivable())
      {
         Component component = new Component("ObjectReceiver." + channel.getName(), channel.getType().getReceiver(), Component.SINGLETON);

         loader.addSingletonFixup(component);
         msg.setReceiver(component);
         component.setMetadata(loader.getMetadata());
         component.addPrimitivePropertyInitializer("channel", channel);

         final PrimitivePropertyInitializer dispatcherInitializer = component.addPrimitivePropertyInitializer("dispatcherComponent", null);

         loader.addComponentFixup(new XMLMetadataHelper.ContextFixup(loader.getHelper())
         {
            public void fixup()
            {
               // Use findComponent, as "System.ObjectQueueDispatcher" does not exist if core repository is not used.
               // The component instance can't be recovered at this point, because the metadata may not be sufficiently loaded
               // for the initializer to succeed.
               dispatcherInitializer.setValue(loader.getMetadata().findComponent("System.ObjectQueueDispatcher"));
            }
         });
      }
   }

}
