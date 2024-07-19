// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.json;

import nexj.core.meta.MetadataException;
import nexj.core.meta.integration.Format;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessagePart;
import nexj.core.meta.integration.MessagePartMapping;
import nexj.core.meta.integration.PrimitiveMessagePart;
import nexj.core.meta.integration.XMLMessageMappingLoader;
import nexj.core.meta.xml.XMLMetadataLoader;
import nexj.core.util.XMLUtil;

import org.w3c.dom.Element;

/**
 * Loads the mappings for JSON message parts.
 */
public class XMLJSONMessageMappingLoader implements XMLMessageMappingLoader
{
   /**
    * @see nexj.core.meta.integration.XMLMessageMappingLoader#loadMapping(org.w3c.dom.Element, nexj.core.meta.integration.Message,
    * nexj.core.meta.integration.MessagePart, nexj.core.meta.integration.Format, nexj.core.meta.xml.XMLMetadataLoader)
    */
   public MessagePartMapping loadMapping(Element element, Message msg, MessagePart part, Format format, XMLMetadataLoader loader)
   {
      JSONMessagePartMapping mapping = (part instanceof PrimitiveMessagePart) ? new JSONMessagePartMapping() : new CompositeJSONMessagePartMapping();

      element = XMLUtil.findChildElement(element, "JSONMapping");

      if (element == null)
      {
         mapping.setKeyName(part.getName());

         return mapping;
      }

      boolean bPrimitivePart = part instanceof PrimitiveMessagePart;
      String sSubtype = XMLUtil.getStringAttr(element, "subtype");

      if (sSubtype != null)
      {
         byte nSubtype = (bPrimitivePart) ? JSONMessagePartMapping.parseSubtype(sSubtype) : CompositeJSONMessagePartMapping.parseSubtype(sSubtype);

         if (nSubtype < 0)
         {
            throw new MetadataException("err.meta.integration.json.invalidSubtype", new Object[]{sSubtype, part.getFullPath()});
         }

         mapping.setSubtype(nSubtype);
      }

      mapping.setFormat(XMLUtil.getStringAttr(element, "format"));
      mapping.setKeyName(XMLUtil.getStringAttr(element, "name", part.getName()));

      String sKey = XMLUtil.getStringAttr(element, "key");

      if (sKey == null)
      {
         return mapping;
      }

      if (bPrimitivePart)
      {
         throw new MetadataException("err.meta.integration.json.messageKeyNotSupported", new Object[]{part.getFullPath()});
      }

      ((CompositeJSONMessagePartMapping)mapping).setMessageKeyValue(sKey);

      return mapping;
   }
}