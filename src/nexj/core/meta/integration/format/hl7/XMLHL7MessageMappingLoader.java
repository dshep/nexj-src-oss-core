// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.hl7;

import org.w3c.dom.Element;

import nexj.core.meta.MetadataException;
import nexj.core.meta.integration.CompositeMessagePartRef;
import nexj.core.meta.integration.Format;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessagePart;
import nexj.core.meta.integration.MessagePartMapping;
import nexj.core.meta.integration.XMLMessageMappingLoader;
import nexj.core.meta.xml.XMLMetadataLoader;
import nexj.core.util.XMLUtil;

/**
 * XML HL7 representation mapping loader.
 */
public class XMLHL7MessageMappingLoader implements XMLMessageMappingLoader
{
   // operations
   
   /**
    * @see nexj.core.meta.integration.XMLMessageMappingLoader#loadMapping(org.w3c.dom.Element, nexj.core.meta.integration.Message, nexj.core.meta.integration.MessagePart, nexj.core.meta.integration.Format, nexj.core.meta.xml.XMLMetadataLoader)
    */
   public MessagePartMapping loadMapping(Element element, Message msg, final MessagePart part, Format format, XMLMetadataLoader loader)
   {
      HL7MessagePartMapping mapping = null; 

      element = XMLUtil.findChildElement(element, "HL7Mapping");

      if (element == null)
      {
         if (!(part instanceof CompositeMessagePartRef))
         {
            mapping = new HL7MessagePartMapping();
            mapping.setLevel(HL7MessagePartMapping.LEVEL_GROUP);
         }
      }
      else
      {
         mapping = new HL7MessagePartMapping();

         String sName = XMLUtil.getStringAttr(element, "name");

         if (sName != null)
         {
            int i = sName.indexOf(';');

            if (i >= 0)
            {
               mapping.setVersion(sName.substring(i + 1));
               sName = sName.substring(0, i);
            }
         }

         mapping.setName(sName);
         mapping.setSeq(XMLUtil.getIntAttr(element, "seq"));

         String sSubtype = XMLUtil.getStringAttr(element, "subtype");

         if (sSubtype == null)
         {
            mapping.setSubtype(HL7MessagePartMapping.SUBTYPE_DEFAULT);
         }
         else if (sSubtype.equals("DTM"))
         {
            mapping.setSubtype(HL7MessagePartMapping.SUBTYPE_DTM);
         }
         else if (sSubtype.equals("DT"))
         {
            mapping.setSubtype(HL7MessagePartMapping.SUBTYPE_DT);
         }
         else if (sSubtype.equals("TM"))
         {
            mapping.setSubtype(HL7MessagePartMapping.SUBTYPE_TM);
         }
         else
         {
            throw new MetadataException("err.meta.integration.hl7.invalidSubtype",
               new Object[]{sSubtype, part.getFullPath()});
         }
      }

      return mapping;
   }
}
