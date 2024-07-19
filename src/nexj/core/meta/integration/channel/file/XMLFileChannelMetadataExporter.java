// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.channel.file;

import java.util.List;

import nexj.core.meta.Primitive;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter;
import nexj.core.meta.j2ee.J2EEProperty;
import nexj.core.meta.j2ee.J2EEResourceRef;
import nexj.core.meta.xml.XMLMetadataExporter;
import nexj.core.util.SysUtil;

/**
 * Exports appserver configuration data for FileChannel instances.
 */
public class XMLFileChannelMetadataExporter extends XMLMDBIntegrationMetadataExporter
{
   // constructors

   /**
    * Creates the exporter with an XML print stream.
    * 
    * @param exporter The main metadata exporter.
    */
   public XMLFileChannelMetadataExporter(XMLMetadataExporter exporter)
   {
      super(exporter);
   }

   // operations

   /**
    * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#addResourceRefs(nexj.core.meta.integration.Channel, int, java.util.List)
    */
   protected void addResourceRefs(Channel channel, int nContainer, List list)
   {
      if (channel.isSendable())
      {
         FileChannel fc = (FileChannel)channel;
         J2EEResourceRef ref = new J2EEResourceRef("file/" + channel.getName(),
            SysUtil.NAMESPACE + '/' + getEnvironmentName(channel) + "/file/" + channel.getName(),
            SysUtil.PACKAGE + ".core.rpc.file.FileConnectionFactory");

         ref.setResourceAdapterName(getResourceAdapterName(fc));
         ref.setTxMode(J2EEResourceRef.TX_XA);
         
         ref.setShareable(true);

         ref.addProperty(new J2EEProperty("inputConnection", false));
         ref.addProperty(new J2EEProperty("outgoingDirectory", fc.getOutgoingDirectory()));
         ref.addProperty(new J2EEProperty("outgoingTempDirectory", fc.getTemporaryDirectory()));
         ref.addProperty(new J2EEProperty("journalDirectory", fc.getOutgoingJournalPath()));
         ref.setMaxConnections(((FileChannel)channel).getMaxSenders());

         list.add(ref);
      }
   }

   /**
    * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#addResourceEnvRefs(nexj.core.meta.integration.Channel, int, java.util.List)
    */
   protected void addResourceEnvRefs(Channel channel, int nContainer, List list)
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
    * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#addAdminObjectRefs(nexj.core.meta.integration.Channel, int, java.util.List)
    */
   protected void addAdminObjectRefs(Channel channel, int nContainer, List list)
   {
   }
   
   /**
    * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#addActivationProperties(nexj.core.meta.integration.Channel, java.util.List)
    */
   protected void addActivationProperties(Channel channel, List list)
   {
      super.addActivationProperties(channel, list);

      FileChannel fc = (FileChannel)channel;

      list.add(new J2EEProperty("incomingDirectory", fc.getIncomingDirectory()));
      list.add(new J2EEProperty("subdirectoryLevels", fc.getSubdirectoryLevels()));
      list.add(new J2EEProperty("processedDirectory", fc.getProcessedDirectory()));
      list.add(new J2EEProperty("interval", fc.getInterval()));
      list.add(new J2EEProperty("age", fc.getAge()));
      list.add(new J2EEProperty("pattern", Primitive.likePattern(fc.getPattern(), 0).toString()));
      list.add(new J2EEProperty("journalDirectory", fc.getIncomingJournalPath()));
      list.add(new J2EEProperty("transactionTimeout", fc.getTransactionTimeout()));
      list.add(new J2EEProperty("maxPoolSize", fc.getMaxReceivers()));
   }

   /**
    * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#addPlatformActivationProperties(nexj.core.meta.integration.Channel, int, java.util.List)
    */
   protected void addPlatformActivationProperties(Channel channel, int nContainer, List list)
   {
   }

   /**
    * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#getListenerClass(nexj.core.meta.integration.Channel)
    */
   protected String getListenerClass(Channel channel)
   {
      return SysUtil.PACKAGE + ".core.rpc.file.FileListener";
   }

   /**
    * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#getMDBClass(nexj.core.meta.integration.Channel)
    */
   protected String getMDBClass(Channel channel)
   {
      return SysUtil.PACKAGE + ".core.rpc.file.FileServerMDB";
   }

   /**
    * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#getResourceAdapterName(nexj.core.meta.integration.Channel)
    */
   protected String getResourceAdapterName(Channel channel)
   {
      return SysUtil.NAMESPACE + "-file.rar";
   }

   /**
    * @see nexj.core.meta.integration.channel.XMLMDBIntegrationMetadataExporter#getTransactionType(nexj.core.meta.integration.Channel)
    */
   protected String getTransactionType(Channel channel)
   {
      return "Container";
   }
}
