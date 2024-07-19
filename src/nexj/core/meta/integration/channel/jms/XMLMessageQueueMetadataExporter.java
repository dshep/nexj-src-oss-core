// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.channel.jms;

import java.io.IOException;
import java.util.List;
import java.util.Properties;
import java.util.regex.Pattern;

import nexj.core.meta.Metadata;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter;
import nexj.core.meta.j2ee.J2EEProperty;
import nexj.core.meta.j2ee.J2EEResourceRef;
import nexj.core.meta.xml.XMLMetadata;
import nexj.core.meta.xml.XMLMetadataExporter;
import nexj.core.rpc.jms.JMSUtil;
import nexj.core.util.GUIDUtil;
import nexj.core.util.J2EEUtil;
import nexj.core.util.PropertyUtil;
import nexj.core.util.SysUtil;

/**
 * XML message queue metadata exporter.
 */
public class XMLMessageQueueMetadataExporter extends XMLMDBIntegrationMetadataExporter
{
   // constants

   protected final static Pattern DATA_DIR_PATTERN = Pattern.compile("[^a-zA-Z0-9\\\\-]+");

   /**
    * Creates the exporter with an XML print stream.
    * @param exporter The main metadata exporter.
    */
   public XMLMessageQueueMetadataExporter(XMLMetadataExporter exporter)
   {
      super(exporter);
   }

   // operations

   // TODO: Singleton MDBs for durable subscriptions etc

   /**
    * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#addResourceRefs(nexj.core.meta.integration.Channel, int, java.util.List)
    */
   protected void addResourceRefs(Channel channel, int nContainer, List list)
   {
      if (((MessageQueue)channel).isClient())
      {
         list.add(getConnectionFactoryResourceRef((MessageQueue)channel, nContainer));
      }
   }

   /**
    * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#addResourceEnvRefs(nexj.core.meta.integration.Channel, int, java.util.List)
    */
   protected void addResourceEnvRefs(Channel channel, int nContainer, List list)
   {
      if (((MessageQueue)channel).isClient())
      {
         list.add(getDestinationResourceEnvRef((MessageQueue)channel, nContainer));
      }
   }

   /**
    * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#addConnectionRefs(nexj.core.meta.integration.Channel, int, java.util.List)
    */
   protected void addConnectionRefs(Channel channel, int nContainer, List list)
   {
      MessageQueue mq = (MessageQueue)channel;
      J2EEResourceRef ref = getConnectionFactoryResourceRef(mq, nContainer);

      ref.setResourceAdapterName(getResourceAdapterName(mq));
      ref.setTxMode((channel.isTransactional()) ? J2EEResourceRef.TX_XA : J2EEResourceRef.TX_NONE);
      ref.setShareable(false);
      ref.setAuthAlias(SysUtil.NAMESPACE + '-' + getEnvironmentName(channel) + "-channel-" + channel.getName());
      ref.setMaxConnections(mq.getMaxSenders());

      String sConnFactory = mq.getConnectionFactory();

      if (sConnFactory == null)
      {
         if (nContainer == J2EEUtil.TEEE)
         {
            sConnFactory = "class:" + SysUtil.PACKAGE + ".core.rpc.jms.ra.engine.activemq.ActiveMQAdapter";
         }
         else
         {
            // no need to add anything as other servers' default JMS engine's connection factories
            // will be configured and written out by their respective installers.
            return;
         }
      }
      else if (JMSUtil.isPlatformAdapter(sConnFactory))
      {
      	return;
      }

      ref.addProperty(new J2EEProperty("JMSConnectionFactoryName", sConnFactory));

      String sConnFactoryProps = getConnectionFactoryProperties(mq, nContainer);

      if (sConnFactoryProps != null)
      {
         ref.addProperty(new J2EEProperty("JMSConnectionFactoryProperties", sConnFactoryProps));
      }

      ref.addProperty(new J2EEProperty("transacted", mq.isTransactional()));

      list.add(ref);
   }

   /**
    * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#addAdminObjectRefs(nexj.core.meta.integration.Channel, int, java.util.List)
    */
   protected void addAdminObjectRefs(Channel channel, int nContainer, List list)
   {
      MessageQueue mq = (MessageQueue)channel;
      J2EEResourceRef ref = getDestinationResourceEnvRef(mq, nContainer, false);

      ref.setResourceAdapterName(getResourceAdapterName(mq));
      ref.setClassName("javax.jms.Destination");

      String sDestination = mq.getDestination();

      if (sDestination == null)
      {
         if (nContainer == J2EEUtil.TEEE)
         {
            sDestination = "class:org.apache.activemq.command.ActiveMQ" + ((mq.isBroadcast()) ? "Topic" : "Queue");
         }
         else
         {
            // no need to add anything as other servers' default JMS engine's destinations
            // will be configured and written out by their respective installers.
            return;
         }
      }
      else if (JMSUtil.isPlatformAdapter(mq.getConnectionFactory()))
      {
      	return;
      }

      ref.addProperty(new J2EEProperty("JMSDestinationName", sDestination));

      String sDestinationProps = getDestinationProperties(mq, nContainer);

      if (sDestinationProps != null)
      {
         ref.addProperty(new J2EEProperty("JMSDestinationProperties", sDestinationProps));
      }

      list.add(ref);

   }

   /**
    * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#addActivationProperties(nexj.core.meta.integration.Channel, java.util.List)
    */
   protected void addActivationProperties(Channel channel, List list)
   {
      super.addActivationProperties(channel, list);

      MessageQueue mq = (MessageQueue)channel;

      switch (mq.getAckMode())
      {
         case MessageQueue.ACK_AUTO:
            list.add(new J2EEProperty("acknowledgeMode", "Auto-acknowledge"));
            break;

         case MessageQueue.ACK_LAZY:
            list.add(new J2EEProperty("acknowledgeMode", "Dups-ok-acknowledge"));
            break;
      }

      list.add(new J2EEProperty("destinationType", getDestinationResourceEnvRef(mq, J2EEUtil.NONE).getClassName()));

      if (mq.isBroadcast())
      {
         if (mq.getSubscription() != null)
         {
            list.add(new J2EEProperty("subscriptionDurability", "Durable"));
            list.add(new J2EEProperty("subscriptionName", mq.getSubscription()));
         }
         else
         {
            list.add(new J2EEProperty("subscriptionDurability", "NonDurable"));
         }

         list.add(new J2EEProperty("clientId", mq.getClientId()));
      }

      list.add(new J2EEProperty("messageSelector", mq.getSelector()));

      if (mq.isTransactional())
      {
         list.add(new J2EEProperty("transactionTimeout", mq.getTransactionTimeout()));
      }
   }

   /**
    * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#addPlatformActivationProperties(nexj.core.meta.integration.Channel, int, java.util.List)
    */
   protected void addPlatformActivationProperties(Channel channel, int nContainer, List list)
   {
      MessageQueue mq = (MessageQueue)channel;

      list.add(new J2EEProperty("connectionFactory", getConnectionFactoryResourceRefMDB(mq, nContainer).getJNDIName()));

      String sConnFactoryProps = getConnectionFactoryProperties(mq, nContainer);

      if (sConnFactoryProps != null)
      {
         list.add(new J2EEProperty("connectionFactoryProperties", sConnFactoryProps));
      }

      list.add(new J2EEProperty("destination", getDestinationResourceEnvRefMDB(mq, nContainer).getJNDIName()));

      String sDestinationProps = getDestinationProperties(mq, nContainer);

      if (sDestinationProps != null)
      {
         list.add(new J2EEProperty("destinationProperties", sDestinationProps));
      }

      list.add(new J2EEProperty("user", mq.getUser()));
      list.add(new J2EEProperty("password", mq.getPassword()));

      Metadata metadata = channel.getType().getMetadata();

      list.add(new J2EEProperty("maxPoolSize", (SysUtil.ENTERPRISE
         && metadata.findComponent("System.ClusterManager") != null && metadata.isDynamicReceiverEnabled()) ? 0
            : mq.getMaxReceivers()));
   }

   /**
    * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#getDestinationResourceEnvRef(nexj.core.meta.integration.Channel, int)
    */
   protected J2EEResourceRef getDestinationResourceEnvRef(Channel channel, int nContainer)
   {
      return getDestinationResourceEnvRef(channel, nContainer, true);
   }

   /**
    * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#getDestinationResourceEnvRef(nexj.core.meta.integration.Channel, int)
    * @param bHAJNDI True to use HAJNDI.
    */
   protected J2EEResourceRef getDestinationResourceEnvRef(Channel channel, int nContainer, boolean bHAJNDI)
   {
      MessageQueue mq = (MessageQueue)channel;
      String sJNDIName = mq.getDestination();
      String sEnvName = getEnvironmentName(channel);

      if (sJNDIName == null && nContainer == J2EEUtil.JBOSS)
      {
         sJNDIName = ((mq.isBroadcast()) ? "topic/" : "queue/") + SysUtil.NAMESPACE + '/' + sEnvName + '/' + mq.getAlias();
      }
      else if (sJNDIName == null || sJNDIName.startsWith("class:"))
      {
         sJNDIName = SysUtil.NAMESPACE + '/' + sEnvName + "/jms/" + ((mq.isBroadcast()) ? "topic/" : "queue/") + mq.getAlias();
      }

      return new J2EEResourceRef("jms/" + ((mq.isBroadcast()) ? "topic/" : "queue/") + mq.getName(),
         (bHAJNDI) ? getHAJNDIName(mq, sJNDIName, nContainer) : sJNDIName,
         (mq.isBroadcast()) ? "javax.jms.Topic" : "javax.jms.Queue");
   }

   /**
    * Gets the MDB destination resource env ref.
    * @param mq The message queue.
    * @param nContainer The container type, one of the J2EEUtil.* constants.
    */
   protected J2EEResourceRef getDestinationResourceEnvRefMDB(MessageQueue mq, int nContainer)
   {
      String sDestination = mq.getDestination();
      String sEnvName = getEnvironmentName(mq);

      if (sDestination == null)
      {
         switch (nContainer)
         {
            case J2EEUtil.JBOSS:
               sDestination = ((mq.isBroadcast()) ? "topic/" : "queue/") + SysUtil.NAMESPACE + '/' + sEnvName + '/' + mq.getAlias();
               break;

            case J2EEUtil.TEEE:
               sDestination = "class:org.apache.activemq.command.ActiveMQ" + ((mq.isBroadcast()) ? "Topic" : "Queue");
               break;

            default:
               sDestination = SysUtil.NAMESPACE + '/' + sEnvName + "/jms/" + ((mq.isBroadcast()) ? "topic/" : "queue/") + mq.getAlias();
               break;
         }
      }

      return new J2EEResourceRef("jms/mdb/" + ((mq.isBroadcast()) ? "topic/" : "queue/") + mq.getName(),
         getHAJNDIName(mq, sDestination, nContainer), (mq.isBroadcast()) ? "javax.jms.Topic" : "javax.jms.Queue");
   }

   /**
    * Gets the destination properties.
    * @param mq The message queue.
    * @param nContainer The container type, one of the J2EEUtil.* constants.
    */
   protected String getDestinationProperties(MessageQueue mq, int nContainer)
   {
      if (!mq.getDestinationPropertyHolder().isEmpty())
      {
         return PropertyUtil.toString(mq.getDestinationPropertyHolder().getProperties());
      }
      else
      {
         if (mq.getConnectionFactory() == null)
         {
            if (nContainer == J2EEUtil.TEEE)
            {
               Properties jmsDestinationProps = new Properties();

               jmsDestinationProps.setProperty("PhysicalName", (mq.isBroadcast()) ? "topic." : "queue." + mq.getAlias());

               return PropertyUtil.toString(jmsDestinationProps);
            }
         }
      }

      return null;
   }
  
   /**
    * Gets the client connection factory resource ref.
    * @param mq The message queue.
    * @param nContainer The container type, one of the J2EEUtil.* constants.
    */
   protected static J2EEResourceRef getConnectionFactoryResourceRef(MessageQueue mq, int nContainer)
   {
      String sJNDIName = mq.getConnectionFactory();

      if (sJNDIName == null || sJNDIName.startsWith("class:"))
      {
         sJNDIName = SysUtil.NAMESPACE + '/' + getEnvironmentName(mq) + "/jms/cf/" + mq.getName();
      }

      return new J2EEResourceRef("jms/cf/" + mq.getName(), sJNDIName, "javax.jms.ConnectionFactory", mq.getUser());
   }

   /**
    * Gets the MDB connection factory resource ref.
    * @param mq The message queue.
    * @param nContainer The container type, one of the J2EEUtil.* constants.
    */
   protected static J2EEResourceRef getConnectionFactoryResourceRefMDB(MessageQueue mq, int nContainer)
   {
      String sFactory = mq.getConnectionFactory();

      if (sFactory == null)
      {
         switch (nContainer)
         {
            case J2EEUtil.JBOSS:
               sFactory = "XAConnectionFactory";
               break;

            case J2EEUtil.WEBSPHERE:
               sFactory = "class:" + SysUtil.PACKAGE + ".core.rpc.jms.ra.platform.websphere.WebSphereSIBAdapter";
               break;

            case J2EEUtil.TEEE:
               sFactory = "class:" + SysUtil.PACKAGE + ".core.rpc.jms.ra.engine.activemq.ActiveMQAdapter";
               break;

            default:
               sFactory = SysUtil.NAMESPACE + '/' + getEnvironmentName(mq) + "/jms/cf/" + mq.getName();
               break;
         }
      }

      return new J2EEResourceRef("jms/mdb/cf/" + mq.getName(),
         getHAJNDIName(mq, sFactory, nContainer), "javax.jms.ConnectionFactory", mq.getUser());
   }

   /**
    * Gets the connection factory properties.
    * @param mq The message queue.
    * @param nContainer The container type, one of the J2EEUtil.* constants.
    */
   protected String getConnectionFactoryProperties(MessageQueue mq, int nContainer)
   {
      if (!mq.getConnectionFactoryPropertyHolder().isEmpty())
      {
         return PropertyUtil.toString(mq.getConnectionFactoryPropertyHolder().getProperties());
      }
      else
      {
         if (mq.getConnectionFactory() == null)
         {
            if (nContainer == J2EEUtil.TEEE)
            {
               Metadata metadata = mq.getType().getMetadata();
               String sDataDir = ((metadata.isTestEnvironment()) ? "test/" : "") + metadata.getEnvironment();
               String sBrokerName = SysUtil.NAMESPACE + "-amq-broker";
               String sBrokerURL = null;
               
               if (metadata.isDistributed())
               {
                  sBrokerURL = "peer://" + SysUtil.NAMESPACE + '-' + metadata.getEnvironment() + "-peerGroup-" + 
                     GUIDUtil.generateGUID().toString() + "/" + sBrokerName;
               }
               else
               {
                  sBrokerURL = "vm://" + sBrokerName;
               }

               Properties jmsConnFactoryProps = new Properties();
               
               jmsConnFactoryProps.setProperty("brokerURL", sBrokerURL);
               jmsConnFactoryProps.setProperty("dataDirectory", sDataDir);

               if (metadata.isTestEnvironment())
               {
                  jmsConnFactoryProps.setProperty("test", "true");
               }
               
               return PropertyUtil.toString(jmsConnFactoryProps);               
            }
         }
      }
      
      return null;
   }

   /**
    * @return The HA JNDI name of a JMS object.
    * @param mq The message queue.
    * @param sName The object name.
    * @param nContainer The container type, one of the J2EEUtil.* constants.
    * @return The JNDI name of the JMS object
    */
   protected static String getHAJNDIName(MessageQueue mq, String sName, int nContainer)
   {
      if (nContainer == J2EEUtil.JBOSS && mq.getType().getMetadata().isDistributed() && sName.indexOf(':') < 0)
      {
         return "jnp://${jboss.bind.address:localhost}:${jboss.hajndi.port:1100}/" + sName;
      }

      return sName;
   }

   /**
    * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#getMDBClass(nexj.core.meta.integration.Channel)
    */
   protected String getMDBClass(Channel channel)
   {
      return SysUtil.PACKAGE + ".core.rpc.jms.JMSServerMDB";
   }

   /**
    * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#getListenerClass(nexj.core.meta.integration.Channel)
    */
   protected String getListenerClass(Channel channel)
   {
      return SysUtil.PACKAGE + ".core.rpc.jms.JMSListener";
   }

   /**
    * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#getResourceAdapterName(nexj.core.meta.integration.Channel)
    */
   protected String getResourceAdapterName(Channel channel)
   {
      return SysUtil.NAMESPACE + "-jms.rar";
   }

   /**
    * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#getTransactionType(nexj.core.meta.integration.Channel)
    */
   protected String getTransactionType(Channel channel)
   {
      return (((MessageQueue)channel).getAckMode() == MessageQueue.ACK_TX) ? "Container" : "Bean";
   }

   /**
    * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#exportChannel(nexj.core.meta.integration.Channel)
    */
   public void exportChannel(Channel channel) throws IOException
   {
      MessageQueue mq = (MessageQueue)channel;

      String sRootNodeName = ((XMLMetadata)mq.getType().getMetadata()).getChannelTypeElement(mq.getType());

      m_writer.openElement(sRootNodeName);

      m_exporter.exportChannelAttributes(channel);

      if (mq.getTransactionTimeout() != 0)
      {
         m_writer.writeAttribute("transactionTimeout", mq.getTransactionTimeout());
      }

      if (mq.getAlias() != null)
      {
         m_writer.writeAttribute("alias", mq.getAlias());
      }

      switch (mq.getAckMode())
      {
         case MessageQueue.ACK_AUTO:
            m_writer.writeAttribute("acknowledgement", "auto");
            break;

         case MessageQueue.ACK_LAZY:
            m_writer.writeAttribute("acknowledgement", "lazy");
            break;
      }

      m_exporter.writeAttribute("broadcast", mq.isBroadcast(), false);
      m_exporter.writeAttribute("persistent", mq.isPersistent(), true);
      m_exporter.writeAttribute("priority", mq.getPriority(), 4);
      m_exporter.writeAttribute("ttl", mq.getTimeToLive(), 0);
      m_exporter.writeAttribute("maxSenders", mq.getMaxSenders(), 16);
      m_exporter.writeAttribute("maxReceivers", mq.getMaxReceivers(), 4);
      m_exporter.writeAttribute("trusted", mq.isTrusted(), false);
      m_exporter.writeAttribute("loopback", mq.isLoopback(), true);
      m_exporter.writeAttribute("defaultUser", mq.getDefaultUser(), null);
      m_exporter.writeAttribute("selector", mq.getSelector(), null);
      m_exporter.writeAttribute("subscription", mq.getSubscription(), null);
      m_exporter.writeAttribute("clientId", mq.getClientId(), null);
      m_exporter.writeAttribute("errorCount", mq.getErrorCount(), 0);

      if (mq.getErrorQueue() != null)
      {
         m_writer.writeAttribute("errorQueue", mq.getErrorQueue().getName());
      }

      if (mq.getReplyQueue() != null)
      {
         m_writer.writeAttribute("replyQueue", mq.getReplyQueue().getName());
      }

      m_writer.closeElement();

      m_exporter.exportChannelElements(channel);

      m_writer.endElement(sRootNodeName);
   }
}