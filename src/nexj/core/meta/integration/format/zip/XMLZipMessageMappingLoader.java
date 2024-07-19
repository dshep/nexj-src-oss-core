// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.zip;

import org.w3c.dom.Element;

import nexj.core.meta.MetadataException;
import nexj.core.meta.integration.CompositeMessagePart;
import nexj.core.meta.integration.Format;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessagePart;
import nexj.core.meta.integration.MessagePartMapping;
import nexj.core.meta.integration.XMLMessageMappingLoader;
import nexj.core.meta.xml.XMLMetadataLoader;
import nexj.core.util.XMLUtil;

/**
 * @see nexj.core.meta.integration.XMLMessageMappingLoader
 */
public class XMLZipMessageMappingLoader implements XMLMessageMappingLoader
{
   /**
    * @see nexj.core.meta.integration.XMLMessageMappingLoader#loadMapping(org.w3c.dom.Element, nexj.core.meta.integration.Message, nexj.core.meta.integration.MessagePart, nexj.core.meta.integration.Format, nexj.core.meta.xml.XMLMetadataLoader)
    */
   public MessagePartMapping loadMapping(Element element, Message msg, MessagePart part, Format format, final XMLMetadataLoader loader)
   {
      ZipMessagePartMapping parentMapping = null;
      Element mappingElement = XMLUtil.findChildElement(element, "ZipMapping");

      // Compute current level
      int nThisLevel;
      CompositeMessagePart parentPart = part.getParent();

      if (parentPart == null)
      {
         nThisLevel = 0;
      }
      else 
      {
         parentMapping = (ZipMessagePartMapping)parentPart.getMapping();
         nThisLevel = parentMapping.getLevel() + 1;
      }

      final ZipMessagePartMapping mapping = new ZipMessagePartMapping();

      mapping.setLevel(nThisLevel);

      if (mappingElement != null)
      {
         if (nThisLevel == 0)
         {
            mapping.setComment(XMLUtil.getStringAttr(mappingElement, "comment"));
            mapping.setCompression((byte)XMLUtil.getIntAttr(mappingElement, "compression", mapping.getCompression()));
         }
         else if (nThisLevel == 2)
         {
            String sValue = XMLUtil.getStringAttr(mappingElement, "value");

            if (sValue.equals("comment"))
            {
               mapping.setValue(ZipMessagePartMapping.VALUE_COMMENT);
            }
            else if (sValue.equals("contents"))
            {
               mapping.setValue(ZipMessagePartMapping.VALUE_CONTENTS);
            }
            else if (sValue.equals("directory"))
            {
               mapping.setValue(ZipMessagePartMapping.VALUE_DIRECTORY);
            }
            else if (sValue.equals("extra"))
            {
               mapping.setValue(ZipMessagePartMapping.VALUE_EXTRA);
            }
            else if (sValue.equals("name"))
            {
               mapping.setValue(ZipMessagePartMapping.VALUE_NAME);
            }
            else if (sValue.equals("size"))
            {
               mapping.setValue(ZipMessagePartMapping.VALUE_SIZE);
            }
            else if (sValue.equals("time"))
            {
               mapping.setValue(ZipMessagePartMapping.VALUE_TIME);
            }
            else
            {
               throw new MetadataException("err.meta.integration.zip.unknownValueMapping",
                  new Object[]{sValue, part.getFullPath()});
            }
         }
      }

      return mapping;
   }
}
