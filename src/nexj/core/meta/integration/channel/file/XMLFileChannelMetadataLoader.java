// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.channel.file;

import java.io.File;

import org.w3c.dom.Element;

import nexj.core.meta.Component;
import nexj.core.meta.PrimitivePropertyInitializer;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.ChannelType;
import nexj.core.meta.integration.XMLIntegrationMetadataLoader;
import nexj.core.meta.xml.XMLMetadataHelper;
import nexj.core.meta.xml.XMLMetadataLoader;
import nexj.core.rpc.file.FileSenderConnectionFactoryFactory;
import nexj.core.rpc.file.ra.FileConnectionFactory;
import nexj.core.util.XMLUtil;

/**
 * Loads metadata describing a FileChannel instance.
 */
public class XMLFileChannelMetadataLoader implements XMLIntegrationMetadataLoader
{

   /**
    * @see nexj.core.meta.integration.XMLIntegrationMetadataLoader#loadChannel(org.w3c.dom.Element, java.lang.String, nexj.core.meta.integration.ChannelType, nexj.core.meta.xml.XMLMetadataLoader)
    */
   public Channel loadChannel(Element element, String sName, ChannelType type, XMLMetadataLoader loader)
   {
      FileChannel fchan = new FileChannel(sName);
      
      fchan.setType(type);
      
      //Load properties common to all channels
      loader.loadChannel(element, fchan);
      
      //Load FileChannel-specific properties
      fchan.setOutgoingName(XMLUtil.getStringAttr(element, "outgoingName", fchan.getOutgoingName()));
      fchan.setEncoding(XMLUtil.getStringAttr(element, "encoding"));
      fchan.setProcessedName(XMLUtil.getStringAttr(element, "processedName", fchan.getProcessedName()));
      fchan.setDefaultUser(XMLUtil.getStringAttr(element, "defaultUser"));
      fchan.setTransactionTimeout(XMLUtil.getIntAttr(element, "transactionTimeout"));
      fchan.setMaxReceivers(XMLUtil.getIntAttr(element, "maxReceivers", fchan.getMaxReceivers()));
      fchan.setMaxSenders(XMLUtil.getIntAttr(element, "maxSenders", fchan.getMaxSenders()));
      
      return fchan;
   }

   /**
    * @see nexj.core.meta.integration.XMLIntegrationMetadataLoader#loadConnection(org.w3c.dom.Element, nexj.core.meta.integration.Channel, nexj.core.meta.xml.XMLMetadataLoader)
    */
   public void loadConnection(Element element, Channel channel, final XMLMetadataLoader loader)
   {
      FileChannel fchan = (FileChannel)channel;

      //Connection properties
      if (element != null)
      {
         fchan.setIncomingDirectory(XMLUtil.getStringAttr(element, "incomingDirectory"));
         fchan.setSubdirectoryLevels(XMLUtil.getIntAttr(element, "levels", fchan.getSubdirectoryLevels()));
         fchan.setProcessedDirectory(XMLUtil.getStringAttr(element, "processedDirectory"));
         fchan.setOutgoingDirectory(XMLUtil.getStringAttr(element, "outgoingDirectory"));
         
         if (fchan.getOutgoingDirectory() != null)
         {
            File defaultTempDirectory = new File(fchan.getOutgoingDirectory(), "tmp");
            
            fchan.setTemporaryDirectory(XMLUtil.getStringAttr(element, "temporaryDirectory", defaultTempDirectory.getAbsolutePath()));
         }
         else
         {
            fchan.setTemporaryDirectory(null);
         }
         
         fchan.setJournalPath(XMLUtil.getStringAttr(element, "journalDirectory"));
         fchan.setInterval(XMLUtil.getLongAttr(element, "interval", fchan.getInterval()));
         fchan.setPattern(XMLUtil.getStringAttr(element, "pattern", fchan.getPattern()));
         fchan.setAge(XMLUtil.getLongAttr(element, "age", fchan.getAge()));
         fchan.setMaxReceivers(XMLUtil.getIntAttr(element, "maxReceivers", fchan.getMaxReceivers()));
         fchan.setMaxSenders(XMLUtil.getIntAttr(element, "maxSenders", fchan.getMaxSenders()));
         fchan.setOutgoingName(XMLUtil.getStringAttr(element, "outgoingName", fchan.getOutgoingName()));
         fchan.setProcessedName(XMLUtil.getStringAttr(element, "processedName", fchan.getProcessedName()));
         fchan.setTransactionTimeout(XMLUtil.getIntAttr(element, "transactionTimeout", fchan.getTransactionTimeout()));
      }
      
      fchan.resolve();
      
      if (fchan.isSendable())
      {
         Component component = new Component("FileSender." + channel.getName(), channel.getType().getSender(), Component.CONTEXT);
         Component connectionFactory = new Component("FileSenderConnectionFactory." + channel.getName(), FileConnectionFactory.class, Component.SINGLETON);
         Component factoryFactory = new Component("FileSenderConnectionFactoryFactory." + channel.getName(), FileSenderConnectionFactoryFactory.class, Component.NEW);
         
         factoryFactory.addPrimitivePropertyInitializer("channel", channel);
         connectionFactory.setFactory(factoryFactory, "create");
         component.addComponentPropertyInitializer("connectionFactory", connectionFactory);
         component.addPrimitivePropertyInitializer("channel", channel);
         
         fchan.setSender(component);
      }
      
      if (fchan.isReceivable())
      {
         Component component = new Component("FileReceiver." + channel.getName(), channel.getType().getReceiver(), Component.SINGLETON);

         loader.addSingletonFixup(component);
         fchan.setReceiver(component);
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
