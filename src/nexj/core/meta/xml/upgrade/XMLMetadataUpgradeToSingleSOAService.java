// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.xml.upgrade;

import java.util.List;

import org.w3c.dom.Element;

import nexj.core.meta.xml.XMLMetadataHelper;
import nexj.core.util.HashHolder;
import nexj.core.util.Holder;
import nexj.core.util.XMLUtil;

/**
 * Modifies SOA definition metadata for one service per definition.
 */
public class XMLMetadataUpgradeToSingleSOAService
{
   // constants

   /**
    * The order of elements in a SOA definition.
    */
   public final static String[] SOADEF_ORDER = new String[] {"Service",
      "Interfaces",
      "Enumerations",
      "Types",
      "Diagrams",
      "Bindings",
      "Channels",
      "IntegrationInterfaces",
      "Libraries",
      "Messages",
      "IntegrationServices",
      "Transformations"
   };

   /**
    * This upgrade applicable to all Metadata prior to this version.
    */
   public final static String VERSION = "8.0.70.0";

   // associations

   /**
    * Set of service implementations that have been added to prevent duplication.
    * BE CAREFUL COPYING THIS PATTERN: IT WILL NOT WORK FOR LAZY-UPGRADED METADATA.
    */
   protected Holder m_serviceImplSet = new HashHolder();

   // operations

   /**
    * Update SOA definitions.
    */
   public void upgradeSOADefinition(Element root, String sName, XMLMetadataHelper helper)
   {
      // Skip if already upgraded.
      if (XMLUtil.findChildElement(root, "Service") != null)
      {
         return;
      }

      Element servicesElement = XMLUtil.findChildElement(root, "Services");

      if (servicesElement != null)
      {
         XMLUtil.removeNode(servicesElement);
      }

      Element srvElement = XMLUtil.addChildElement(root, SOADEF_ORDER, "Service");

      if (servicesElement == null)
      {
         return;
      }

      List servicesList = XMLUtil.findChildElements(servicesElement, "Service");

      if (servicesList.isEmpty())
      {
         return;
      }

      Element firstService = (Element)servicesList.get(0);
      Element firstInterfaces = XMLUtil.findChildElement(firstService, "Interfaces");

      if (firstInterfaces != null)
      {
         XMLUtil.addChildElement(srvElement, null, firstInterfaces);
      }
   }

   /**
    * Update SOA implementations.
    *    - Changes the service name from "SOADefName:service:ServiceName" to just "SOADefName"
    *    - Ensures that there is only one active implementation of a given service.
    * Doesn't have to ensure that the interfaces match the service's interfaces as that isn't validated by
    * the loader.
    */
   public void upgradeSOAImplementation(Element root, String sName, XMLMetadataHelper helper)
   {
      String sService = XMLUtil.getStringAttr(root, "service");
      int nPos;

      if (sService != null && (nPos = sService.indexOf(":service:")) > 0)
      {
         sService = sService.substring(0, nPos);

         if (!m_serviceImplSet.add(sService))
         {
            sService = "";
         }

         XMLUtil.setAttribute(root, "service", sService, true);
      }
   }
}
