// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.channel.tcp;

import org.w3c.dom.Element;

import nexj.core.meta.Component;
import nexj.core.meta.MetadataException;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.ChannelType;
import nexj.core.meta.integration.XMLIntegrationMetadataLoader;
import nexj.core.meta.xml.XMLMetadataHelper;
import nexj.core.meta.xml.XMLMetadataLoader;
import nexj.core.rpc.tcp.ra.TCPResourceAdapter;
import nexj.core.util.CertificateUtil;
import nexj.core.util.EncryptedKeyStore;
import nexj.core.util.StringUtil;
import nexj.core.util.XMLUtil;

/**
 * XML metadata loader for TCP channel
 */
public class XMLTCPMetadataLoader implements XMLIntegrationMetadataLoader
{
   /**
    * @see nexj.core.meta.integration.XMLIntegrationMetadataLoader#loadChannel(org.w3c.dom.Element, java.lang.String, nexj.core.meta.integration.ChannelType, nexj.core.meta.xml.XMLMetadataLoader)
    */
   public Channel loadChannel(Element element, String sName, ChannelType type, final XMLMetadataLoader loader)
   {
      final TCPChannel tcp = new TCPChannel(sName);

      tcp.setType(type);
      loader.loadChannel(element, tcp);

      tcp.setEncoding(XMLUtil.getStringAttr(element, "encoding", tcp.getEncoding()));
      tcp.setRemotePort(XMLUtil.getIntAttr(element, "remotePort", tcp.getRemotePort()));
      tcp.setLocalPort(XMLUtil.getIntAttr(element, "localPort", tcp.getLocalPort()));
      tcp.setLocalHost(XMLUtil.getStringAttr(element, "localHost", tcp.getLocalHost()));
      tcp.setDefaultUser(XMLUtil.getStringAttr(element, "defaultUser", tcp.getDefaultUser()));
      tcp.setResolvingEnabled(XMLUtil.getBooleanAttr(element, "resolve", tcp.isResolvingEnabled()));
      tcp.setBacklog(XMLUtil.getIntAttr(element, "backlog", tcp.getBacklog()));
      tcp.setReadTimeout(XMLUtil.getIntAttr(element, "readTimeout", tcp.getReadTimeout()));
      tcp.setSecure(XMLUtil.getBooleanAttr(element, "secure", tcp.isSecure()));
      tcp.setIdleTimeout(XMLUtil.getIntAttr(element, "idleTimeout", tcp.getIdleTimeout()));
      tcp.setConnectionTimeout(XMLUtil.getIntAttr(element, "connectionTimeout", tcp.getConnectionTimeout()));
      tcp.setKeepAlive(XMLUtil.getBooleanAttr(element, "keepAlive", tcp.isKeepAlive()));
      tcp.setNoDelay(XMLUtil.getBooleanAttr(element, "noDelay", tcp.isNoDelay()));
      tcp.setMaxSenders(XMLUtil.getIntAttr(element, "maxSenders", tcp.getMaxSenders()));
      tcp.setMaxReceivers(XMLUtil.getIntAttr(element, "maxReceivers", tcp.getMaxReceivers()));
      tcp.setSenderBufferSize(XMLUtil.getIntAttr(element, "senderBufferSize", tcp.getSenderBufferSize()));
      tcp.setReceiverBufferSize(XMLUtil.getIntAttr(element, "receiverBufferSize", tcp.getReceiverBufferSize()));
      tcp.setTOS(XMLUtil.getIntAttr(element, "tos", tcp.getTOS()));

      // Verify port ranges
      if (tcp.getLocalPort() < TCPResourceAdapter.MIN_PORT || tcp.getLocalPort() > TCPResourceAdapter.MAX_PORT)
      {
         throw new MetadataException("err.rpc.tcp.invalidLocalPort",
            new Object[] { "" + TCPResourceAdapter.MIN_PORT, "" + TCPResourceAdapter.MAX_PORT });
      }

      if (tcp.getRemotePort() < TCPResourceAdapter.MIN_PORT || tcp.getRemotePort() > TCPResourceAdapter.MAX_PORT)
      {
         throw new MetadataException("err.rpc.tcp.invalidRemotePort",
            new Object[] { "" + TCPResourceAdapter.MIN_PORT, "" + TCPResourceAdapter.MAX_PORT });
      }

      // Get splitter component
      final String sSplitterComp = XMLUtil.getStringAttr(element, "splitter", null);

      loader.addSingletonFixup(new XMLMetadataHelper.ContextFixup(loader.getHelper())
      {
         public void fixup()
         {
            if (sSplitterComp == null)
            {
               tcp.setSplitter(null);
            }
            else
            {
               tcp.setSplitter(loader.getMetadata().getComponent(sSplitterComp));
            }
         }
      });

      // Get JMS queue
      final String sQueue = XMLUtil.getStringAttr(element, "queue");

      if (sQueue != null)
      {
         loader.addIOFixup(new XMLMetadataHelper.ContextFixup(loader.getHelper())
         {
            public void fixup()
            {
               tcp.setQueue(loader.getMetadata().getChannel(sQueue));
            }
         });
      }

      return tcp;
   }

   /**
    * @see nexj.core.meta.integration.XMLIntegrationMetadataLoader#loadConnection(org.w3c.dom.Element, nexj.core.meta.integration.Channel, nexj.core.meta.xml.XMLMetadataLoader)
    */
   public void loadConnection(Element element, Channel channel, final XMLMetadataLoader loader)
   {
      final TCPChannel tcp = (TCPChannel)channel;

      if (element != null)
      {
         tcp.setEncoding(XMLUtil.getStringAttr(element, "encoding", tcp.getEncoding()));
         tcp.setRemoteHost(XMLUtil.getStringAttr(element, "host", tcp.getRemoteHost()));
         tcp.setRemotePort(XMLUtil.getIntAttr(element, "port", tcp.getRemotePort()));
         tcp.setLocalHost(XMLUtil.getStringAttr(element, "localHost", tcp.getLocalHost()));
         tcp.setLocalPort(XMLUtil.getIntAttr(element, "localPort", tcp.getLocalPort()));
         tcp.setBacklog(XMLUtil.getIntAttr(element, "backlog", tcp.getBacklog()));
         tcp.setReadTimeout(XMLUtil.getIntAttr(element, "readTimeout", tcp.getReadTimeout()));
         tcp.setSecure(XMLUtil.getBooleanAttr(element, "secure", tcp.isSecure()));
         tcp.setIdleTimeout(XMLUtil.getIntAttr(element, "idleTimeout", tcp.getIdleTimeout()));
         tcp.setConnectionTimeout(XMLUtil.getIntAttr(element, "connectionTimeout", tcp.getConnectionTimeout()));
         tcp.setKeepAlive(XMLUtil.getBooleanAttr(element, "keepAlive", tcp.isKeepAlive()));
         tcp.setNoDelay(XMLUtil.getBooleanAttr(element, "noDelay", tcp.isNoDelay()));
         tcp.setMaxSenders(XMLUtil.getIntAttr(element, "maxSenders", tcp.getMaxSenders()));
         tcp.setMaxReceivers(XMLUtil.getIntAttr(element, "maxReceivers", tcp.getMaxReceivers()));
         tcp.setSenderBufferSize(XMLUtil.getIntAttr(element, "senderBufferSize", tcp.getSenderBufferSize()));
         tcp.setReceiverBufferSize(XMLUtil.getIntAttr(element, "receiverBufferSize", tcp.getReceiverBufferSize()));
         tcp.setTOS(XMLUtil.getIntAttr(element, "tos", tcp.getTOS()));

         // Get client authentication mode
         String sClientAuth = XMLUtil.getStringAttr(element, "authentication", "none");

         if (sClientAuth.equals("none"))
         {
            tcp.setClientAuthMode(TCPChannel.CLIENT_AUTH_NONE);
         }
         else if (sClientAuth.equals("supported"))
         {
            tcp.setClientAuthMode(TCPChannel.CLIENT_AUTH_SUPPORTED);
         }
         else if (sClientAuth.equals("required"))
         {
            tcp.setClientAuthMode(TCPChannel.CLIENT_AUTH_REQUIRED);
         }
         else
         {
            throw new MetadataException("err.meta.integration.tcp.clientAuthMode",
               new Object[] { sClientAuth, channel.getName() });
         }

         if (tcp.isSecure())
         {
            String sPassword = loader.decryptPassword(XMLUtil.getStringAttr(element, "password"));

            tcp.setPassword(sPassword);
            
            try
            {
               String sCertificate = XMLUtil.getStringAttr(element, "certificate");

               if (loader.getMetadata().isEncrypted())
               {
                  // create special sub-classed KeyStore object
                  tcp.setCertificateStore(new EncryptedKeyStore(sCertificate));
               }
               else
               {
                  tcp.setCertificateStore(CertificateUtil.parseKeyStore(sCertificate, sPassword));
               }
            }
            catch (Exception e)
            {
               throw new MetadataException("err.meta.integration.tcp.invalidCertificate",
                  new Object[]{tcp.getName()}, e);
            }

            try
            {
               tcp.setTrustedCertificate(CertificateUtil.parseCertificate(XMLUtil.getStringAttr(element, "trust")));
            }
            catch (Exception e)
            {
               throw new MetadataException("err.meta.integration.tcp.invalidTrustCertificate",
                  new Object[]{tcp.getName()}, e);
            }
         }

         // Get the mapping component
         final String sMapperComp = XMLUtil.getStringAttr(element, "mapper");

         loader.addSingletonFixup(new XMLMetadataHelper.ContextFixup(loader.getHelper())
         {
            public void fixup()
            {
               if (!StringUtil.isEmpty(sMapperComp))
               {
                  tcp.setMapper(loader.getMetadata().getComponent(sMapperComp));
               }
            }
         });
      }

      if (tcp.isSendable())
      {
         Component component = new Component("TCPSender." + channel.getName(), channel.getType().getSender(), Component.SINGLETON);

         loader.getMetadata().addComponent(component);
         loader.addSingletonFixup(component);
         tcp.setSender(component);
         component.addPrimitivePropertyInitializer("channel", channel);
      }

      if (tcp.isReceivable())
      {
         Component component = new Component("TCPReceiver." + channel.getName(), channel.getType().getReceiver(), Component.SINGLETON);

         tcp.setReceiver(component);
         component.setMetadata(loader.getMetadata());
         component.addPrimitivePropertyInitializer("channel", channel);
      }
   }
}
