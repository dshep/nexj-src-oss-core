// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.xml;

import java.net.URL;

import org.w3c.dom.Element;

import nexj.core.meta.MetadataException;
import nexj.core.meta.integration.CompositeMessagePartRef;
import nexj.core.meta.integration.Format;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessagePart;
import nexj.core.meta.integration.MessagePartMapping;
import nexj.core.meta.integration.XMLMessageMappingLoader;
import nexj.core.meta.xml.XMLMetadataHelper;
import nexj.core.meta.xml.XMLMetadataLoader;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.XMLUtil;

/**
 * XML message mapping loader for XML.
 */
public class XMLXMLMessageMappingLoader implements XMLMessageMappingLoader
{
   /**
    * @see nexj.core.meta.integration.XMLMessageMappingLoader#loadMapping(org.w3c.dom.Element, nexj.core.meta.integration.Message, nexj.core.meta.integration.MessagePart, nexj.core.meta.integration.Format, nexj.core.meta.xml.XMLMetadataLoader)
    */
   public MessagePartMapping loadMapping(Element element, Message msg, final MessagePart part, Format format, final XMLMetadataLoader loader)
   {
      final XMLMessagePartMapping mapping;
      RootXMLMessagePartMapping rootMapping = null;

      element = XMLUtil.findChildElement(element, "XMLMapping");

      if (msg.getRoot() == part)
      {
         rootMapping = new RootXMLMessagePartMapping(msg);
         mapping = rootMapping;
      }
      else if (element != null || !(part instanceof CompositeMessagePartRef))
      {
         mapping = new XMLMessagePartMapping();
         rootMapping = (RootXMLMessagePartMapping)part.getRoot().getMapping(); 
      }
      else
      {
         mapping = null;
      }

      if (element == null)
      {
         if (mapping != null)
         {
            mapping.setNodeName(part.getName());
         }
      }
      else
      {
         mapping.setNillable(XMLUtil.getBooleanAttr(element, "nillable", mapping.isNillable()));

         String sType = XMLUtil.getStringAttr(element, "type", "element");
         
         if (sType.equals("element"))
         {
            mapping.setNodeType(XMLMessagePartMapping.ELEMENT);
         }
         else if (sType.equals("attribute"))
         {
            mapping.setNodeType(XMLMessagePartMapping.ATTRIBUTE);
         }
         else if (sType.equals("value"))
         {
            mapping.setNodeType(XMLMessagePartMapping.VALUE);
         }
         else
         {
            throw new MetadataException("err.meta.integration.xml.invalidNodeType",
               new Object[]{sType, part.getFullPath()});
         }
         
         String sSubtype = XMLUtil.getStringAttr(element, "subtype");

         if (sSubtype != null)
         {
            byte nSubtype = XMLMessagePartMapping.parseSubtype(sSubtype);

            if (nSubtype >= 0)
            {
               mapping.setSubtype(nSubtype);
            }
            else
            {
               throw new MetadataException("err.meta.integration.xml.invalidSubtype",
                  new Object[]{sSubtype, part.getFullPath()});
            }
         }

         mapping.setFormat(XMLUtil.getStringAttr(element, "format", mapping.getFormat()));

         // set the envelope before resolving the element namespace, since the namespace
         // may be implicitly declared through the presence of an envelope
         String sEnvelope = XMLUtil.getStringAttr(element, "envelope");

         if (sEnvelope != null)
         {
            if (mapping != rootMapping)
            {
               throw new MetadataException("err.meta.integration.xml.misplacedEnvelope",
                  new Object[]{part.getFullPath()});
            }

            if (sEnvelope.equals("soap"))
            {
               rootMapping.setEnvelope(RootXMLMessagePartMapping.ENVELOPE_SOAP);
            }
            else if (sEnvelope.equals("soap12"))
            {
               rootMapping.setEnvelope(RootXMLMessagePartMapping.ENVELOPE_SOAP12);
            }
            else
            {
               throw new MetadataException("err.meta.integration.xml.invalidEnvelope",
                  new Object[]{sEnvelope, part.getFullPath()});
            }
         }

         final String sNamespace = XMLUtil.getStringAttr(element, "namespace");
         String sURI = XMLUtil.getStringAttr(element, "uri");
         String sSchema = XMLUtil.getStringAttr(element, "schema");

         if (sNamespace != null)
         {
            if (sURI == null && sSchema != null)
            {
               throw new MetadataException("err.meta.integration.inapplicableSchema",
                  new Object[]{sSchema, part.getFullPath()});
            }

            mapping.setNamespace(rootMapping.addNamespace(sNamespace, sURI, sSchema, false));
         }
         else if (sURI != null)
         {
            throw new MetadataException("err.meta.integration.xml.missingNS",
               new Object[]{part.getFullPath()});
         }
         else if (sSchema != null)
         {
            throw new MetadataException("err.meta.integration.inapplicableSchema",
               new Object[]{sSchema, part.getFullPath()});
         }

         String sResource = XMLUtil.getStringAttr(element, "resource");

         if (sResource != null)
         {
            if (mapping != rootMapping)
            {
               throw new MetadataException("err.meta.integration.xml.misplacedResource",
                  new Object[]{part.getFullPath()});
            }

            try
            {
               XMLMetadataHelper helper = loader.getHelper();
               Pair list = (Pair)helper.parse(sResource, true, null, null, msg.getMetadata().getGlobalEnvironment());

               while (list != null)
               {
                  Object resource = list.getHead();
                  String sSourceURL;
                  URL dstURL;

                  if (resource instanceof Pair)
                  {
                     Pair resMap = (Pair)resource;

                     sSourceURL = resMap.getOperator().getName();
                     dstURL = helper.getResource(resMap.getNext().getOperator().getName()).getURL();
                  }
                  else
                  {
                     sSourceURL = ((Symbol)resource).getName();
                     dstURL = helper.getResource(sSourceURL).getURL();
                  }

                  rootMapping.addSchemaResourceMapping(sSourceURL, dstURL);
                  list = list.getNext();
               }
            }
            catch (Exception ex)
            {
               throw new MetadataException("err.meta.integration.xml.invalidResource", new Object[]{part.getFullPath()}, ex);
            }
         }

         String sAction = XMLUtil.getStringAttr(element, "action");

         if (sAction != null)
         {
            if (mapping != rootMapping)
            {
               throw new MetadataException("err.meta.integration.xml.misplacedAction",
                  new Object[]{part.getFullPath()});
            }

            rootMapping.setAction(sAction);
         }

         String sOperation = XMLUtil.getStringAttr(element, "operation");

         if (sOperation != null)
         {
            if (mapping != rootMapping)
            {
               throw new MetadataException("err.meta.integation.xml.misplacedOperation",
                  new Object[]{part.getFullPath()});
            }

            rootMapping.setOperation(sOperation);
         }

         String sXSDType = XMLUtil.getStringAttr(element, "xsdtype");

         if (sXSDType != null)
         {
            if (mapping != rootMapping)
            {
               throw new MetadataException("err.meta.integration.xml.misplacedXSDType",
                  new Object[]{part.getFullPath()});
            }

            rootMapping.setXSDType(sXSDType);
         }

         String sNodeName = XMLUtil.getStringAttr(element, "node");

         if (sNodeName != null)
         {
            mapping.setNodeName(sNodeName);
         }
         else
         {
            mapping.setNodeName(part.getName());
         }

         String sInterfaceName = XMLUtil.getStringAttr(element, "interface");

         if (sInterfaceName != null)
         {
            mapping.setInterface(loader.getMetadata().defineInterface(sInterfaceName, msg));
         }
      }

      return mapping;
   }
}
