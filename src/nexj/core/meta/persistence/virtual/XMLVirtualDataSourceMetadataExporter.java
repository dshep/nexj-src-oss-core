// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.virtual;

import java.io.IOException;
import java.util.Iterator;

import nexj.core.meta.Attribute;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Primitive;
import nexj.core.meta.persistence.AttributeMapping;
import nexj.core.meta.persistence.DataSource;
import nexj.core.meta.persistence.Key;
import nexj.core.meta.persistence.PersistenceMapping;
import nexj.core.meta.persistence.XMLPersistenceMetadataExporter;
import nexj.core.meta.xml.XMLMetadataExporter;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.Named;
import nexj.core.util.XMLWriter;

/**
 * Metadata exporter for the virtual persistence adapter.
 */
public class XMLVirtualDataSourceMetadataExporter implements XMLPersistenceMetadataExporter
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

   public XMLVirtualDataSourceMetadataExporter(XMLMetadataExporter exporter)
   {
      m_exporter = exporter;
      m_writer = exporter.getWriter();
   }

   // operations

   /**
    * @see nexj.core.meta.persistence.XMLPersistenceMetadataExporter#exportDataSource(nexj.core.meta.persistence.DataSource)
    */
   public void exportDataSource(DataSource dataSource) throws IOException
   {
      m_writer.startElement("ServiceDataSource");
      m_writer.endElement("ServiceDataSource");
   }

   /**
    * @see nexj.core.meta.persistence.XMLPersistenceMetadataExporter#exportJ2EEDescriptor(nexj.core.meta.persistence.DataSource, int, java.lang.String, int, int)
    */
   public void exportJ2EEDescriptor(DataSource dataSource, int nPart, String sNamespace, int nContainer, int nContext)
      throws IOException
   {
      // Nothing to add to J2EE descriptors.
   }

   /**
    * @see nexj.core.meta.persistence.XMLPersistenceMetadataExporter#exportMapping(nexj.core.meta.persistence.PersistenceMapping)
    */
   public void exportMapping(PersistenceMapping mapping) throws IOException
   {
      exportVirtualMapping((VirtualMapping)mapping);
   }

   /**
    * Exports a virtual persistence mapping.
    * @param mapping The virtual persistence mapping to export.
    * @throws IOException If an I/O error occurs.
    */
   protected void exportVirtualMapping(VirtualMapping mapping) throws IOException
   {
      m_writer.openElement("ServiceMapping");

      if (mapping.getKeyGenerator() != null)
      {
         m_writer.writeAttribute("keyGenerator", mapping.getKeyGenerator().getName());
      }

      m_writer.writeAttribute("derived", mapping.isDerived());
      m_writer.closeElement();

      exportObjectKey(mapping);
      exportAttributeMappings(mapping);
      exportCreateMapping(mapping.getCreateMapping());
      exportReadMapping(mapping.getReadMapping());
      exportUpdateMapping(mapping.getUpdateMapping());
      exportDeleteMapping(mapping.getDeleteMapping());

      m_writer.endElement("ServiceMapping");
   }

   /**
    * Exports the object key definition for a virtual persistence mapping.
    * @param mapping The virtual persistence mapping.
    * @throws IOException If an I/O error occurs.
    */
   protected void exportObjectKey(VirtualMapping mapping) throws IOException
   {
      Metaclass metaclass = mapping.getMetaclass();
      Key key = mapping.getObjectKey();
      int nCount = (key == null) ? 0 : key.getPartCount();

      if (nCount > 0)
      {
         m_writer.startElement("KeyParts");

         for (int i = 0; i < nCount; i++)
         {
            Primitive type = key.getPartType(i);

            m_writer.openElement("KeyPart");
            m_writer.writeAttribute("type", type.getName());

            for (int k = 0, nAttrCount = metaclass.getInstanceAttributeCount(); k < nAttrCount; k++)
            {
               Attribute attribute = metaclass.getInstanceAttribute(k);
               AttributeMapping attributeMapping = mapping.getAttributeMapping(attribute);

               if (attributeMapping instanceof VirtualPrimitiveMapping)
               {
                  VirtualPrimitiveMapping primitiveMapping = (VirtualPrimitiveMapping)attributeMapping;

                  if (primitiveMapping.getObjectKeyPart() == i)
                  {
                     m_writer.writeAttribute("attribute", attribute.getName());

                     break;
                  }
               }
            }

            m_writer.closeEmptyElement();
         }

         m_writer.endElement("KeyParts");
      }
   }

   /**
    * Exports the attribute mappings for a virtual persistence mapping.
    * @param mapping The virtual persistence mapping.
    * @throws IOException If an I/O error occurs.
    */
   protected void exportAttributeMappings(VirtualMapping mapping) throws IOException
   {
      Metaclass metaclass = mapping.getMetaclass();
      int nCount = metaclass.getInstanceAttributeCount();
      boolean bWroteMapping = false;

      for (int i = 0; i < nCount; i++)
      {
         Attribute attribute = metaclass.getInstanceAttribute(i);
         AttributeMapping attrMapping = mapping.getAttributeMapping(attribute);

         if (attrMapping != null)
         {
            if (!bWroteMapping)
            {
               m_writer.startElement("AttributeMappings");
               bWroteMapping = true;
            }

            m_writer.openElement("AttributeMapping");
            m_writer.writeAttribute("name", attribute.getName());

            if (attrMapping instanceof VirtualClassMapping)
            {
               VirtualClassMapping classMapping = (VirtualClassMapping)attrMapping;

               if (classMapping.getKey(false).isObjectKey())
               {
                  m_writer.writeAttribute("objectSourceKey", true);
               }

               if (classMapping.getKey(true) instanceof Named)
               {
                  m_writer.writeAttribute("destinationKey", ((Named)classMapping.getKey(true)).getName());
               }

               Pair compositionAttributes = classMapping.getCompositionAttributes();

               if (compositionAttributes != null)
               {
                  m_exporter.exportSExpression(compositionAttributes, true, false, null, "attributes");
               }
            }
            else if (!(attrMapping instanceof VirtualPrimitiveMapping))
            {
               throw new IllegalStateException();
            }

            m_writer.closeEmptyElement();
         }
      }

      if (bWroteMapping)
      {
         m_writer.endElement("AttributeMappings");
      }
   }

   /**
    * Exports a create mapping.
    * @param mapping The create mapping.
    * @throws IOException If an I/O error occurs.
    */
   protected void exportCreateMapping(WorkMapping mapping) throws IOException
   {
      exportWorkMapping(mapping, "CreateMapping");
   }

   /**
    * Exports a read mapping.
    * @param mapping The read mapping.
    * @throws IOException If an I/O error occurs.
    */
   protected void exportReadMapping(ReadMapping mapping) throws IOException
   {
      if (mapping != null)
      {
         boolean bWroteMapping = false;

         for (Iterator itr = mapping.getCaseIterator(); itr.hasNext(); )
         {
            ReadMappingCase read = (ReadMappingCase)itr.next();

            if (!bWroteMapping)
            {
               m_writer.startElement("ReadMapping");
               bWroteMapping = true;
            }

            m_writer.openElement("Case");

            if (read.getWhere() == null)
            {
               m_writer.writeAttribute("where", "()");
            }
            else if (!Symbol.ELSE.equals(read.getWhere()))
            {
               m_exporter.exportSExpression(read.getWhere(), false, false, null, "where");
            }

            if (read.getVariables() != null)
            {
               m_exporter.exportSExpression(read.getVariables(), true, false, null, "variables");
            }

            m_writer.closeElement();

            if (read.getReadScript() != null)
            {
               m_writer.startElement("Read");
               exportScript(read.getReadScript());
               m_writer.endElement("Read");
            }

            if (read.getCloseScript() != null)
            {
               m_writer.startElement("Close");
               exportScript(read.getCloseScript());
               m_writer.endElement("Close");
            }

            m_writer.endElement("Case");
         }

         if (bWroteMapping)
         {
            m_writer.endElement("ReadMapping");
         }
      }
   }

   /**
    * Exports an update mapping.
    * @param mapping The update mapping.
    * @throws IOException If an I/O error occurs.
    */
   protected void exportUpdateMapping(UpdateMapping mapping) throws IOException
   {
      if (mapping != null)
      {
         boolean bWroteMapping = false;

         for (Iterator itr = mapping.getCaseIterator(); itr.hasNext(); )
         {
            UpdateMappingCase update = (UpdateMappingCase)itr.next();

            if (!bWroteMapping)
            {
               m_writer.startElement("UpdateMapping");
               bWroteMapping = true;
            }

            m_writer.openElement("Case");
            m_exporter.exportSExpression(mapping.getAttributes(update.getAttributeSet()),
               true, false, null, "attributes");
            m_writer.writeAttribute("batch", update.isBatch());
            m_writer.writeAttribute("dirty", update.isDirty());
            m_writer.writeAttribute("full", update.isFull());

            m_writer.closeElement();
            exportScript(update.getScript());
            m_writer.endElement("Case");
         }

         if (bWroteMapping)
         {
            m_writer.endElement("UpdateMapping");
         }
      }
   }

   /**
    * Exports a delete mapping.
    * @param mapping The delete mapping.
    * @throws IOException If an I/O error occurs.
    */
   protected void exportDeleteMapping(WorkMapping mapping) throws IOException
   {
      exportWorkMapping(mapping, "DeleteMapping");
   }

   /**
    * Exports a work mapping, which has a "batch" attribute and a "Script".
    * @param mapping The mapping to export.
    * @param sElementName The mapping element name.
    * @throws IOException If an I/O error occurs.
    */
   protected void exportWorkMapping(WorkMapping mapping, String sElementName) throws IOException
   {
      if (mapping != null)
      {
         m_writer.openElement(sElementName);
         m_writer.writeAttribute("batch", mapping.isBatch());
         m_writer.closeElement();
         exportScript(mapping.getScript());
         m_writer.endElement(sElementName);
      }
   }

   /**
    * Exports a script tag with the script in a CDATA section.
    * @param script The code to export.
    * @throws IOException If an I/O error occurs.
    */
   protected void exportScript(Pair script) throws IOException
   {
      m_writer.startElement("Script");
      m_exporter.exportSExpression(script, true, true, null, null);
      m_writer.endElement("Script");
   }
}
