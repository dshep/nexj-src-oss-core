// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.object;

import java.io.IOException;

import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessagePartMapping;
import nexj.core.meta.integration.XMLMessageMappingExporter;
import nexj.core.meta.xml.XMLMetadataExporter;
import nexj.core.util.XMLWriter;

/**
 * Exports object message mapping metadata to an output stream.
 */
public class XMLObjectMessageMappingExporter implements XMLMessageMappingExporter
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
   public XMLObjectMessageMappingExporter(XMLMetadataExporter exporter)
   {
      m_exporter = exporter;
      m_writer = exporter.getWriter();
   }

   /**
    * @see nexj.core.meta.integration.XMLMessageMappingExporter#exportMapping(nexj.core.meta.integration.Message, nexj.core.meta.integration.MessagePartMapping)
    */
   public void exportMapping(Message message, MessagePartMapping mapping) throws IOException
   {
      if (!(mapping instanceof ObjectMessagePartMapping))
      {
         return;
      }
      
      ObjectMessagePartMapping objMapping = (ObjectMessagePartMapping)mapping;

      m_writer.openElement("ObjectMapping");

      m_exporter.writeAttribute("key", objMapping.isKey(), false);
      m_exporter.writeAttribute("subkey", objMapping.isSubKey(), false);
      m_exporter.writeAttribute("local", objMapping.isLocal(), !objMapping.isKey());
      m_exporter.writeAttribute("create", objMapping.isCreate(), true);
      m_exporter.writeAttribute("update", objMapping.isUpdate(), true);
      m_exporter.writeAttribute("delete", objMapping.isDelete(), false);
      m_exporter.writeAttribute("truncate", objMapping.isTruncated(), false);
      m_exporter.exportSExpression(objMapping.getWhere(), false, false, Boolean.TRUE, "where");

      if (objMapping.getAttribute() != null)
      {
         m_writer.writeAttribute("attribute", objMapping.getAttribute().getName());
      }
      else
      {
         String sSpecialAttribute = objMapping.getSystemAttributeName();

         if (sSpecialAttribute != null)
         {
            m_writer.writeAttribute("attribute", sSpecialAttribute);
         }
      }

      if (objMapping.getMetaclass() != null)
      {
         m_writer.writeAttribute("class", objMapping.getMetaclass().getName());
      }

      if (objMapping.getAccessAttribute() != null)
      {
         m_writer.writeAttribute("access", objMapping.getAccessAttribute().getName());
      }

      m_writer.closeEmptyElement();
   }
}
