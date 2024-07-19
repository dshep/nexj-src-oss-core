// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.xml.upgrade;

import nexj.core.meta.xml.XMLMetadataHelper;
import nexj.core.util.XMLUtil;
import nexj.core.util.XMLUtil.ElementHandler;

import org.w3c.dom.Element;

/**
 * Class to upgrade individual XML DOM elements from some version prior to VERSION into VERSION.
 * Remove inSecure/outSecure from XSD
 * Modify outAuthentication enum from none|credential|inFirst to none|basic|receive
 * Add inEncryption/outEncryption enum with none|SSL|TLS
 */
public class XMLMetadataUpgradeToMailPropertyRenaming2
{
   // attributes

   /**
    * This upgrade applicable to all Metadata prior to this version.
    */
   public final static String VERSION = "7.0.0.62";

   // operations

   /**
    * Modify the Connections/Environment element.
    * @param element The element to modify.
    */
   protected void modify(Element element)
   {
      Element collection = XMLUtil.findChildElement(element, "ChannelConnections");

      if (collection == null)
      {
         return;
      }

      XMLUtil.forEachChildElement(collection, "MailConnection", new ElementHandler()
      {
         public void handleElement(Element element)
         {
            element.removeAttribute("inSecure");
            element.removeAttribute("outSecure");

            String sOutAuthentication = XMLUtil.getStringAttr(element, "outAuthentication");

            if ("credential".equalsIgnoreCase(sOutAuthentication))
            {
               element.setAttribute("outAuthentication", "basic");
            }
            else if ("inFirst".equalsIgnoreCase(sOutAuthentication))
            {
               element.setAttribute("outAuthentication", "receive");
            }

            String sInProtocol = XMLUtil.getStringAttr(element, "inProtocol");

            if (sInProtocol != null && sInProtocol.endsWith("s"))
            {
               element.setAttribute("inEncryption", "SSL");
            }

            String sOutProtocol = XMLUtil.getStringAttr(element, "outProtocol");

            if (sOutProtocol != null && sOutProtocol.endsWith("s"))
            {
               element.setAttribute("outEncryption", "SSL");
            }
         }
      });
   }

   public void upgradeConnections(Element element, String sName, XMLMetadataHelper helper)
   {
      modify(element);
   }

   public void upgradeEnvironment(Element element, String sName, XMLMetadataHelper helper)
   {
      modify(element);
   }
}
