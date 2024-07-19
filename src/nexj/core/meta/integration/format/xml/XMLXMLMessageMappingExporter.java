// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.xml;

import java.io.IOException;
import java.io.Writer;
import java.net.URL;

import nexj.core.meta.MetadataObject;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessagePartMapping;
import nexj.core.meta.integration.SchemaExporter;
import nexj.core.meta.integration.XMLMessageMappingExporter;
import nexj.core.meta.integration.service.Interface;
import nexj.core.meta.xml.XMLMetadata;
import nexj.core.meta.xml.XMLMetadataExporter;
import nexj.core.meta.xml.XMLMetadataHelper;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.runtime.InvocationContextHolder;
import nexj.core.util.HTTP;
import nexj.core.util.HashHolder;
import nexj.core.util.Holder;
import nexj.core.util.Lookup;
import nexj.core.util.LookupDeque;
import nexj.core.util.URIUtil;
import nexj.core.util.XMLWriter;

/**
 * Exports message mapping metadata to an output stream.
 */
public class XMLXMLMessageMappingExporter implements XMLMessageMappingExporter, SchemaExporter, InvocationContextAware, InvocationContextHolder
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

   /**
    * Set of encountered namespaces.
    */
   protected Holder m_namespaceSet;

   /**
    * The invocation context;
    */
   protected InvocationContext m_context;

   // constructors

   /**
    * Creates the exporter with no initialization (for use as a SchemaExporter).
    */
   public XMLXMLMessageMappingExporter()
   {
   }

   /**
    * Creates the exporter with an XML print stream.
    * @param exporter The main metadata exporter.
    */
   public XMLXMLMessageMappingExporter(XMLMetadataExporter exporter)
   {
      m_exporter = exporter;
      m_writer = exporter.getWriter();
      m_namespaceSet = new HashHolder();
   }

   // operations

   /**
    * @see nexj.core.runtime.InvocationContextAware#setInvocationContext(nexj.core.runtime.InvocationContext)
    */
   public void setInvocationContext(InvocationContext context)
   {
      m_context = context;
   }

   /**
    * @see nexj.core.runtime.InvocationContextHolder#getInvocationContext()
    */
   public InvocationContext getInvocationContext()
   {
      return m_context;
   }

   /**
    * @see nexj.core.meta.integration.XMLMessageMappingExporter#exportMapping(nexj.core.meta.integration.Message, nexj.core.meta.integration.MessagePartMapping)
    */
   public void exportMapping(Message message, MessagePartMapping mapping) throws IOException
   {
      if (!(mapping instanceof XMLMessagePartMapping))
      {
         return;
      }

      XMLMessagePartMapping xmlMapping = (XMLMessagePartMapping)mapping;

      m_writer.openElement("XMLMapping");

      String sNodeType = "element";

      if (xmlMapping.getNodeType() == XMLMessagePartMapping.ATTRIBUTE)
      {
         sNodeType = "attribute";
      }
      else if (xmlMapping.getNodeType() == XMLMessagePartMapping.VALUE)
      {
         sNodeType = "value";
      }

      m_writer.writeAttribute("type", sNodeType);

      if (xmlMapping.getSubtype() != XMLMessagePartMapping.SUBTYPE_DEFAULT)
      {
         m_writer.writeAttribute("subtype", XMLMessagePartMapping.SUBTYPE_NAMES[xmlMapping.getSubtype()]);
      }

      if (xmlMapping.isNillable())
      {
         m_writer.writeAttribute("nillable", true);
      }

      XMLNamespace namespace = xmlMapping.getNamespace();

      if (namespace != null)
      {
         m_writer.writeAttribute("namespace", namespace.getName());

         if (!m_namespaceSet.contains(namespace))
         {
            if (namespace.getURI() != null)
            {
               m_writer.writeAttribute("uri", namespace.getURI());
            }

            if (namespace.getSchema() != null)
            {
               m_writer.writeAttribute("schema", namespace.getSchema());
            }

            m_namespaceSet.add(namespace);
         }
      }

      if (xmlMapping.getNodeName() != null )
      {
         if (!xmlMapping.getNodeName().equals(xmlMapping.getMessagePart().getName()) ||
            xmlMapping instanceof RootXMLMessagePartMapping)
         {
            m_writer.writeAttribute("node", xmlMapping.getNodeName());
         }
      }

      if (xmlMapping instanceof RootXMLMessagePartMapping)
      {
         RootXMLMessagePartMapping rootMapping = (RootXMLMessagePartMapping)xmlMapping;
         LookupDeque schemaResourceMap = rootMapping.getSchemaResourceMap();

         if (schemaResourceMap != null && message.getMetadata() instanceof XMLMetadata)
         {
            XMLMetadata xmlMetadata = (XMLMetadata)message.getMetadata();
            XMLMetadataHelper helper = xmlMetadata.getHelper();
            StringBuilder buf = new StringBuilder();
            boolean bAppended = false;

            for (Lookup.Iterator itr = schemaResourceMap.iterator(); itr.hasNext(); )
            {
               String sSource = (String)itr.next();
               String sSchemaExternalForm = ((URL)itr.getValue()).toExternalForm();
               String sResourceName = parseResource(helper.getBaseURL(), sSchemaExternalForm);

               if (sResourceName == null)
               {
                  sResourceName = parseResource(helper.getRootURL(), sSchemaExternalForm);

                  if (sResourceName == null)
                  {
                     sResourceName = sSchemaExternalForm;
                  }
               }

               if (bAppended)
               {
                  buf.append(' ');
               }

               if (sSource.equals(sSchemaExternalForm))
               {
                  buf.append(sResourceName);
                  bAppended = true;
               }
               else
               {
                  buf.append('(');
                  buf.append(sSource);
                  buf.append(' ');
                  buf.append(sResourceName);
                  buf.append(')');
                  bAppended = true;
               }
            }

            if (bAppended)
            {
               m_writer.writeAttribute("resource", buf.toString());
            }
         }

         switch (rootMapping.getEnvelope())
         {
            case RootXMLMessagePartMapping.ENVELOPE_SOAP:
               m_writer.writeAttribute("envelope", "soap");
               break;

            case RootXMLMessagePartMapping.ENVELOPE_SOAP12:
               m_writer.writeAttribute("envelope", "soap12");
               break;
         }

         if (rootMapping.getAction() != null)
         {
            m_writer.writeAttribute("action", rootMapping.getAction());
         }
         
         if (rootMapping.getXSDType() != null)
         {
            m_writer.writeAttribute("xsdtype", rootMapping.getXSDType());
         }
      }

      m_writer.closeEmptyElement();
   }

   /**
    * Parses for a relative resource path from a candidate repositoryPath URL and an absolute resource URL path.
    * @param repositoryPath The candidate metadata folder URL
    * @param resourceExternalForm The absolute resource URL path.
    * @return A resource name if successful, null otherwise.
    */
   protected String parseResource(URL repositoryPath, String sResourceExternalForm)
   {
      if (repositoryPath == null)
      {
         return null;
      }

      String sExternalForm = repositoryPath.toExternalForm();

      if (sResourceExternalForm.startsWith(sExternalForm))
      {
         return sResourceExternalForm.substring(sExternalForm.length());
      }

      return null;
   }

   /**
    * @see nexj.core.meta.integration.SchemaExporter#exportSchema(nexj.core.meta.MetadataObject, java.lang.String, java.io.Writer)
    */
   public void exportSchema(MetadataObject metaObj, String sLocation, Writer writer) throws IOException
   {
      XSDMessageExporter exporter = new XSDMessageExporter(m_context);

      exporter.setHostName(URIUtil.getHostPort(sLocation));
      exporter.setContextRoot(HTTP.getContextPath(sLocation));
      exporter.setOutputMode(XSDMessageExporter.OUTMODE_WSDL);

      if (metaObj instanceof Channel)
      {
         exporter.export((Channel)metaObj, writer);
      }
      else if (metaObj instanceof Interface)
      {
         exporter.export((Interface)metaObj, writer);
      }
      else if (metaObj instanceof Message)
      {
         exporter.export((Message)metaObj, writer);
      }
      else
      {
         throw new IllegalArgumentException(metaObj.toString() + " not of valid type for schema export.");
      }
   }
}
