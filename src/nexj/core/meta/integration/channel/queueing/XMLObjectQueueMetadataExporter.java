package nexj.core.meta.integration.channel.queueing;

import java.io.IOException;
import java.util.List;

import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter;
import nexj.core.meta.j2ee.J2EEProperty;
import nexj.core.meta.j2ee.J2EEResourceRef;
import nexj.core.meta.xml.XMLMetadataExporter;
import nexj.core.util.SysUtil;

/**
 * Exports J2EE descriptors for the ObjectQueue dispatcher.
 */
public class XMLObjectQueueMetadataExporter extends XMLMDBIntegrationMetadataExporter
{
   /**
    * @param exporter
    */
   public XMLObjectQueueMetadataExporter(XMLMetadataExporter exporter)
   {
      super(exporter);
   }

   /**
    * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#addActivationProperties(nexj.core.meta.integration.Channel, java.util.List)
    */
   protected void addActivationProperties(Channel channel, List list)
   {
      super.addActivationProperties(channel, list);

      ObjectDispatcherQueue mc = (ObjectDispatcherQueue)channel;

      list.add(new J2EEProperty("port", mc.getPort()));
      list.add(new J2EEProperty("maxPoolSize", mc.getMaxReceivers()));
   }

   /**
    * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#addAdminObjectRefs(nexj.core.meta.integration.Channel, int, java.util.List)
    */
   protected void addAdminObjectRefs(Channel channel, int nContainer, List list)
   {
   }

   /**
    * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#addConnectionRefs(nexj.core.meta.integration.Channel, int, java.util.List)
    */
   protected void addConnectionRefs(Channel channel, int nContainer, List list)
   {
      addResourceRefs(channel, nContainer, list);
   }

   /**
    * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#addPlatformActivationProperties(nexj.core.meta.integration.Channel, int, java.util.List)
    */
   protected void addPlatformActivationProperties(Channel channel, int nContainer, List list)
   {
   }

   /**
    * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#addResourceEnvRefs(nexj.core.meta.integration.Channel, int, java.util.List)
    */
   protected void addResourceEnvRefs(Channel channel, int nContainer, List list)
   {
   }

   /**
    * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#addResourceRefs(nexj.core.meta.integration.Channel, int, java.util.List)
    */
   protected void addResourceRefs(Channel channel, int nContainer, List list)
   {
      if (channel.isSendable())
      {
         ObjectDispatcherQueue mc = (ObjectDispatcherQueue)channel;
         J2EEResourceRef ref = new J2EEResourceRef("queueing/ObjectQueue",
            SysUtil.NAMESPACE + '/' + getEnvironmentName(channel) + "/queueing/ObjectQueue",
            SysUtil.PACKAGE + ".core.rpc.queueing.ObjectQueueConnectionFactory");

         ref.setResourceAdapterName(getResourceAdapterName(mc));
         ref.setTxMode(J2EEResourceRef.TX_NONE);
         ref.setShareable(false);
         ref.addProperty(new J2EEProperty("port", mc.getPort()));
         ref.setMaxConnections(mc.getMaxSenders());

         list.add(ref);
      }
   }

   /**
    * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#getListenerClass(nexj.core.meta.integration.Channel)
    */
   protected String getListenerClass(Channel channel)
   {
      return SysUtil.PACKAGE + ".core.rpc.queueing.ObjectDispatchListener";
   }

   /**
    * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#getMDBClass(nexj.core.meta.integration.Channel)
    */
   protected String getMDBClass(Channel channel)
   {
      return SysUtil.PACKAGE + ".core.rpc.queueing.ObjectQueueServerMDB";
   }

   /**
    * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#getResourceAdapterName(nexj.core.meta.integration.Channel)
    */
   protected String getResourceAdapterName(Channel channel)
   {
      return SysUtil.NAMESPACE + "-queueing.rar";
   }

   /**
    * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#getTransactionType(nexj.core.meta.integration.Channel)
    */
   protected String getTransactionType(Channel channel)
   {
      return "Bean";
   }

   /**
    * @see nexj.core.meta.integration.XMLIntegrationMetadataExporter#exportJ2EEDescriptor(nexj.core.meta.integration.Channel, int, java.lang.String, int, int)
    */
   public void exportJ2EEDescriptor(Channel channel, int nPart, String sNamespace, int nContainer, int nContext)
      throws IOException
   {
      // Only export the dispatcher channel, since all channels share the same connection factory and consumer pool.
      if (channel instanceof ObjectDispatcherQueue)
      {
         super.exportJ2EEDescriptor(channel, nPart, sNamespace, nContainer, nContext);
      }
   }
}
