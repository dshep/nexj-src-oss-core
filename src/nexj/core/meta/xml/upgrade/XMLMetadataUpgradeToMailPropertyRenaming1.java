// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.xml.upgrade;

import nexj.core.meta.xml.XMLMetadataHelper;
import nexj.core.util.XMLUtil;
import nexj.core.util.XMLUtil.ElementHandler;

import org.w3c.dom.Element;

/**
 * Class to upgrade individual XML DOM elements from some version prior to VERSION into VERSION.
 * Remove inSecure/outSecure from MailConnection and add outAuthentication.
 */
public class XMLMetadataUpgradeToMailPropertyRenaming1
{
   // attributes

   /**
    * This upgrade applicable to all Metadata prior to this version.
    */
   public final static String VERSION = "7.0.0.40";

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
            Boolean inSecure = XMLUtil.getBooleanObjAttr(element, "inSecure");

            if (inSecure != null)
            {
               // default from @see nexj.core.meta.integration.channel.mail.Mail#m_nInProtocol
               String sInProtocol = XMLUtil.getStringAttr(element, "inProtocol", "pop3");
               boolean bSecure = sInProtocol.endsWith("s");

               if (inSecure == Boolean.TRUE && !bSecure)
               {
                  element.setAttribute("inProtocol", sInProtocol + "s");
               }
               else if (inSecure == Boolean.FALSE && bSecure)
               {
                  element.setAttribute(
                     "inProtocol", sInProtocol.substring(0, sInProtocol.length() - 1));
               }
            }

            Boolean outSecure = XMLUtil.getBooleanObjAttr(element, "outSecure");

            if (outSecure != null)
            {
               // default from @see nexj.core.meta.integration.channel.mail.Mail#m_nOutProtocol
               String sOutProtocol = XMLUtil.getStringAttr(element, "outProtocol", "smtp");
               boolean bSecure = sOutProtocol.endsWith("s");

               if (outSecure == Boolean.TRUE && !bSecure)
               {
                  element.setAttribute("OutProtocol", sOutProtocol + "s");
               }
               else if (outSecure == Boolean.FALSE && bSecure)
               {
                  element.setAttribute(
                     "outProtocol", sOutProtocol.substring(0, sOutProtocol.length() - 1));
               }
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