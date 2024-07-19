// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.channel.udp;

import java.io.IOException;

import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.XMLIntegrationMetadataExporter;
import nexj.core.meta.xml.XMLMetadataExporter;

/**
 * XML UDP metadata exporter.
 */
public class XMLUDPMetadataExporter implements XMLIntegrationMetadataExporter
{
   // constructors

   /**
    * Creates the exporter with an XML print stream.
    * @param exporter The main metadata exporter.
    */
   public XMLUDPMetadataExporter(XMLMetadataExporter exporter)
   {
   }

   // operations

   /**
    * @see nexj.core.meta.integration.XMLIntegrationMetadataExporter#exportJ2EEDescriptor(nexj.core.meta.integration.Channel, int, java.lang.String, int, int)
    */
   public void exportJ2EEDescriptor(Channel channel, int nPart, String sNamespace, int nContainer, int nContext)
      throws IOException
   {
   }

   /**
    * @see nexj.core.meta.integration.XMLIntegrationMetadataExporter#exportChannel(nexj.core.meta.integration.Channel)
    */
   public void exportChannel(Channel channel) throws IOException
   {
      throw new UnsupportedOperationException();
   }
}
