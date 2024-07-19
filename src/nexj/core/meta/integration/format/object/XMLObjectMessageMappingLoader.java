// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.object;

import org.w3c.dom.Element;

import nexj.core.meta.integration.Format;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessagePart;
import nexj.core.meta.integration.MessagePartMapping;
import nexj.core.meta.integration.XMLMessageMappingLoader;
import nexj.core.meta.xml.XMLMetadataHelper;
import nexj.core.meta.xml.XMLMetadataLoader;
import nexj.core.util.XMLUtil;

/**
 * XML object representation message mapping loader.
 */
public class XMLObjectMessageMappingLoader implements XMLMessageMappingLoader
{
   // operations

   /**
    * @see nexj.core.meta.integration.XMLMessageMappingLoader#loadMapping(org.w3c.dom.Element, nexj.core.meta.integration.Message, nexj.core.meta.integration.MessagePart, nexj.core.meta.integration.Format, nexj.core.meta.xml.XMLMetadataLoader)
    */
   public MessagePartMapping loadMapping(Element element, final Message msg, final MessagePart part, Format format, XMLMetadataLoader loader)
   {
      element = XMLUtil.findChildElement(element, "ObjectMapping");

      if (element == null &&
            ((part.getParent() == null && msg.getDerivation() != Message.DERIVATION_ABSTRACT)
            || (part.getParent() != null && part.getParent().getMapping() == null)))
      {
         return null;
      }

      final ObjectMessagePartMapping mapping = new ObjectMessagePartMapping();
      String sClass = null;

      if (element != null)
      {
         sClass = XMLUtil.getStringAttr(element, "class");
         mapping.setKey(XMLUtil.getBooleanAttr(element, "key", mapping.isKey()));
         mapping.setSubKey(XMLUtil.getBooleanAttr(element, "subkey", mapping.isSubKey()));
         mapping.setLocal(XMLUtil.getBooleanAttr(element, "local", mapping.isLocal()));
         mapping.setCreate(XMLUtil.getBooleanAttr(element, "create", mapping.isCreate()));
         mapping.setUpdate(XMLUtil.getBooleanAttr(element, "update", mapping.isUpdate()));
         mapping.setDelete(XMLUtil.getBooleanAttr(element, "delete", mapping.isDelete()));
         mapping.setPrimary(XMLUtil.getBooleanAttr(element, "primary", mapping.isPrimary()));
         mapping.setTruncated(XMLUtil.getBooleanAttr(element, "truncate", mapping.isTruncated()));
         mapping.setWhere(loader.getHelper().parse(XMLUtil.getStringAttr(element, "where"),
            false, null, Boolean.TRUE, loader.getMetadata().getGlobalEnvironment()));
         mapping.setAssociationCode(XMLUtil.getStringAttr(element, "associationCode"));

         final String sAttribute = XMLUtil.getStringAttr(element, "attribute");
         final String sAccessAttribute = XMLUtil.getStringAttr(element, "access");

         loader.addPostInheritanceMessageFixup(new XMLMetadataHelper.ContextFixup(loader.getHelper())
         {
            public void fixup()
            {
               mapping.setAttributes(sAttribute, sAccessAttribute, part, msg);
            }
         });
      }
      else
      {
         loader.addPostInheritanceMessageFixup(new XMLMetadataHelper.ContextFixup(loader.getHelper())
         {
            public void fixup()
            {
               mapping.setAttributes(null, null, part, msg);
            }
         });
      }

      if (sClass != null)
      {
         mapping.setMetaclass(loader.getMetadata().getMetaclass(sClass));
      }

      if (part.getRoot() == null)
      {
         loader.addComponentFixup(new XMLMetadataHelper.ContextFixup(loader.getHelper())
         {
            public void fixup()
            {
               mapping.finish2(part);
            }
         });
      }

      return mapping;
   }
}
