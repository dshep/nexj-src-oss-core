// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.channel.mail;

import java.io.IOException;

import nexj.core.meta.Metadata;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.XMLIntegrationMetadataExporter;
import nexj.core.meta.j2ee.J2EEResourceRef;
import nexj.core.meta.xml.XMLMetadataExporter;
import nexj.core.util.SysUtil;
import nexj.core.util.XMLWriter;

/**
 * XML mail metadata exporter.
 */
public class XMLMailMetadataExporter implements XMLIntegrationMetadataExporter
{
   // associations
   
   /**
    * The main metadata exporter.
    */
   protected XMLMetadataExporter m_exporter;

   /**
    * The output stream.
    */
   protected XMLWriter m_writer;
   
   // constructors
   
   /**
    * Creates the exporter with an XML print stream.
    * @param exporter The main metadata exporter.
    */
   public XMLMailMetadataExporter(XMLMetadataExporter exporter)
   {
      m_exporter = exporter;
      m_writer = exporter.getWriter();
   }

   /**
    * @see nexj.core.meta.integration.XMLIntegrationMetadataExporter#exportJ2EEDescriptor(nexj.core.meta.integration.Channel, int, java.lang.String, int, int)
    */
   public void exportJ2EEDescriptor(Channel channel, int nPart, String sNamespace, int nContainer, int nContext) throws IOException
   {
      Mail mail = (Mail)channel;

      if (!mail.isEnabled())
      {
         return;
      }

      switch (nPart)
      {
         case XMLMetadataExporter.J2EE_PLATFORM_CONNECTION_FACTORY:
            // The namespace test value ("Mail") must match the name of the data source type.
            if (!"Mail".equals(sNamespace) || !mail.isFirst()) //only export one Mail per JNDI alias
            {
               break;
            }
            // else fall through

         case XMLMetadataExporter.J2EE_RESOURCE_REF:
         case XMLMetadataExporter.J2EE_PLATFORM_RESOURCE_REF:
            Metadata metadata = channel.getType().getMetadata();
            J2EEResourceRef ref = new J2EEResourceRef(
               "mail/" + mail.getName(),
               SysUtil.NAMESPACE + '/' + metadata.getEnvironment() + "/mail/" + mail.getAlias(),
               SysUtil.PACKAGE + ".core.rpc.mail.MailConnectionFactory");

            // all volatile properties set in CRI, non volatile properties set on CF
            //    for ease of administrator/user inspection of configuration files
            // Mail does not have non-volatile properties since connections dynamically configurable
            // need separate CF per channel to have separate pool per channel, always 1 pool per CF
            // partition pool based by CRI -> ref.setConnectionPoolPartitioned(true)
            ref.setResourceAdapterName(getResourceAdapterName(channel));
            ref.setConnectionPoolPartitioned(true); // partition pool by CRI
            m_exporter.exportJ2EEResourceRef(metadata, ref, nPart, sNamespace, nContainer, nContext);

            break;
      }
   }
   
   /**
    * @see nexj.core.meta.integration.XMLIntegrationMetadataExporter#exportChannel(nexj.core.meta.integration.Channel)
    */
   public void exportChannel(Channel channel) throws IOException
   {
      throw new UnsupportedOperationException();
   }

   /**
    * Gets the resource adapter name.
    * @param channel The channel.
    * @return The resource adapter name.
    */
   protected String getResourceAdapterName(Channel channel)
   {
      return SysUtil.NAMESPACE + "-mail.rar";
   }
}