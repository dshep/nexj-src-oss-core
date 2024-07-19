// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.channel.http;

import java.io.IOException;

import nexj.core.meta.Primitive;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.XMLIntegrationMetadataExporter;
import nexj.core.meta.xml.XMLMetadata;
import nexj.core.meta.xml.XMLMetadataExporter;
import nexj.core.util.XMLWriter;

/**
 * XML HTTP channel metadata exporter. 
 */
public class XMLHTTPMetadataExporter implements XMLIntegrationMetadataExporter
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
   public XMLHTTPMetadataExporter(XMLMetadataExporter exporter)
   {
      m_exporter = exporter;
      m_writer = exporter.getWriter();
   }
   
   /**
    * @see nexj.core.meta.integration.XMLIntegrationMetadataExporter#exportChannel(nexj.core.meta.integration.Channel)
    */
   public void exportChannel(Channel channel) throws IOException
   {
      HTTPChannel httpChannel = (HTTPChannel)channel;

      String sRootNodeName = ((XMLMetadata)httpChannel.getType().getMetadata()).getChannelTypeElement(httpChannel.getType());
      
      m_writer.openElement(sRootNodeName);

      m_exporter.exportChannelAttributes(httpChannel);

      m_exporter.writeAttribute("url", httpChannel.getURL(), null);
      m_exporter.writeAttribute("agent", httpChannel.getAgent(), null);
      m_exporter.writeAttribute("contentType", httpChannel.getContentType(), null);

      if (httpChannel.getDataType() != null)
      {
         m_writer.writeAttribute("binary", httpChannel.getDataType() == Primitive.BINARY);
      }

      m_exporter.writeAttribute("delete", httpChannel.isDeleteImplemented(), false);
      m_exporter.writeAttribute("get", httpChannel.isGetImplemented(), false);
      m_exporter.writeAttribute("head", httpChannel.isHeadImplemented(), false);
      m_exporter.writeAttribute("options", httpChannel.isOptionsImplemented(), false);
      m_exporter.writeAttribute("post", httpChannel.isPostImplemented(), true);
      m_exporter.writeAttribute("put", httpChannel.isPutImplemented(), false);
      m_exporter.writeAttribute("trace", httpChannel.isTraceImplemented(), false);

      if (httpChannel.getPrivilege() != null)
      {
         m_writer.writeAttribute("privilege", httpChannel.getPrivilege().getName());
      }

      m_exporter.exportSExpression(httpChannel.getErrorExpression(), false, false, null, "error");

      m_writer.closeElement();

      m_exporter.exportChannelElements(httpChannel);
      
      m_writer.endElement(sRootNodeName);
   }

   /**
    * @see nexj.core.meta.integration.XMLIntegrationMetadataExporter#exportJ2EEDescriptor(nexj.core.meta.integration.Channel, int, java.lang.String, int, int)
    */
   public void exportJ2EEDescriptor(Channel channel, int nPart, String sNamespace, int nContainer, int nContext)
      throws IOException
   {
   }
}
