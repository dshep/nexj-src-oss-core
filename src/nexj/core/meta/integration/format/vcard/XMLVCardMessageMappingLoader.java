// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.vcard;

import org.w3c.dom.Element;

import nexj.core.meta.MetadataValidationException;
import nexj.core.meta.Primitive;
import nexj.core.meta.integration.CompositeMessagePart;
import nexj.core.meta.integration.Format;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessagePart;
import nexj.core.meta.integration.MessagePartMapping;
import nexj.core.meta.integration.PrimitiveMessagePart;
import nexj.core.meta.integration.XMLMessageMappingLoader;
import nexj.core.meta.xml.XMLMetadataLoader;
import nexj.core.util.XMLUtil;

/**
 * Loads the mappings for vCard-mapped message parts from XML metadata.
 */
public class XMLVCardMessageMappingLoader implements XMLMessageMappingLoader
{
   /**
    * @see nexj.core.meta.integration.XMLMessageMappingLoader#loadMapping(org.w3c.dom.Element, nexj.core.meta.integration.Message, nexj.core.meta.integration.MessagePart, nexj.core.meta.integration.Format, nexj.core.meta.xml.XMLMetadataLoader)
    */
   public MessagePartMapping loadMapping(Element element, Message msg, final MessagePart part, Format format, XMLMetadataLoader loader)
   {
      Element mappingElement = XMLUtil.findChildElement(element, "VCardMapping");
      final VCardMessagePartMapping mapping = new VCardMessagePartMapping();

      mapping.setName(part.getName());

      if (mappingElement != null)
      {
         mapping.setName(XMLUtil.getStringAttr(mappingElement, "name", mapping.getName()));
         mapping.setDefault(XMLUtil.getStringAttr(mappingElement, "default"));
         mapping.setEncoding(XMLUtil.getStringAttr(mappingElement, "encoding", mapping.getEncoding()));

         String sDelimiter = XMLUtil.getStringAttr(mappingElement, "delimiter");

         if (sDelimiter != null)
         {
            mapping.setDelimiter(sDelimiter.charAt(0));
         }

         String sType = XMLUtil.getStringAttr(mappingElement, "type");

         if ("value".equals(sType))
         {
            mapping.setType(VCardMessagePartMapping.TYPE_VALUE);
         }
         else if ("parameter".equals(sType))
         {
            mapping.setType(VCardMessagePartMapping.TYPE_PARAMETER);
         }
         else if ("group".equals(sType))
         {
            mapping.setType(VCardMessagePartMapping.TYPE_GROUP);

            if (XMLUtil.findAttribute(mappingElement, "name", ""))
            {
               mapping.setName("");
            }
         }
         else if (sType != null)
         {
            throw new MetadataValidationException("err.meta.integration.vcard.invalidType",
               new Object[]{sType, part.getFullPath()});
         }

         String sQuoting = XMLUtil.getStringAttr(mappingElement, "quoting");

         if ("none".equals(sQuoting))
         {
            mapping.setQuoting(VCardMessagePartMapping.QUOTING_NONE);
         }
         else if ("base64".equals(sQuoting))
         {
            mapping.setQuoting(VCardMessagePartMapping.QUOTING_BASE64);
         }
         else if ("qp".equals(sQuoting))
         {
            mapping.setQuoting(VCardMessagePartMapping.QUOTING_QP);
         }
         else if ("vcard".equals(sQuoting))
         {
            mapping.setQuoting(VCardMessagePartMapping.QUOTING_VCARD);
         }
         else if (sQuoting != null)
         {
            throw new MetadataValidationException("err.meta.integration.vcard.invalidQuoting",
               new Object[]{sQuoting, part.getFullPath()});
         }

         CompositeMessagePart parentPart = part.getParent();
         String sWrapping = XMLUtil.getStringAttr(mappingElement, "wrapping");

         if (parentPart != null && sWrapping == null)
         {
            mapping.setWrapping(((VCardMessagePartMapping)parentPart.getMapping()).getWrapping());
         }
         else if ("whitespace".equals(sWrapping))
         {
            mapping.setWrapping(VCardMessagePartMapping.WRAPPING_WHITESPACE);
         }
         else if ("anywhere".equals(sWrapping))
         {
            mapping.setWrapping(VCardMessagePartMapping.WRAPPING_ANYWHERE);
         }
         else if (sWrapping != null)
         {
            throw new MetadataValidationException("err.meta.integration.vcard.invalidWrapping",
               new Object[]{sWrapping, part.getFullPath()});
         }

         String sSubtype = XMLUtil.getStringAttr(mappingElement, "subtype");

         if (sSubtype == null)
         {
            if ((part instanceof PrimitiveMessagePart) &&
               (((PrimitiveMessagePart)part).getType() == Primitive.TIMESTAMP))
            {
               mapping.setSubtype(VCardMessagePartMapping.SUBTYPE_DATETIME);
            }
         }
         else if ("date".equals(sSubtype))
         {
            mapping.setSubtype(VCardMessagePartMapping.SUBTYPE_DATE);
         }
         else if ("dateTime".equals(sSubtype))
         {
            mapping.setSubtype(VCardMessagePartMapping.SUBTYPE_DATETIME);
         }
         else
         {
            throw new MetadataValidationException("err.meta.integration.vcard.invalidSubtype",
               new Object[]{sSubtype, part.getFullPath()});
         }
      }

      return mapping;
   }
}
