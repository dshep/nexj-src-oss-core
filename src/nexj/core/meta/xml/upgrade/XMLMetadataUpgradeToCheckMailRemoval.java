// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.xml.upgrade;

import nexj.core.meta.xml.XMLMetadataHelper;
import nexj.core.util.XMLUtil;
import nexj.core.util.XMLUtil.ElementHandler;

import org.w3c.dom.Element;

/**
 * Class to upgrade individual XML DOM elements from some version prior to VERSION into VERSION.
 * Remove ReceiverProperties from Mail channel since nexj.core.rpc.mail.MailQueueServer is no longer
 * available.
 * Remove references to "nexj.core.rpc.mail.CheckMailCommand" in MessageQueue services.
 */
public class XMLMetadataUpgradeToCheckMailRemoval
{
   // attributes

   /**
    * This upgrade applicable to all Metadata prior to this version.
    */
   public final static String VERSION = "7.0.0.38";

   public void upgradeMail(Element element, String sName, XMLMetadataHelper helper)
   {
      XMLUtil.removeNode(XMLUtil.findChildElement(element, "ReceiverProperties"));
   }

   // operations

   public void upgradeMessageQueue(Element element, String sName, XMLMetadataHelper helper)
   {
      Element component = XMLUtil.findElementRecur(
                             element, "Component", "type", "nexj.core.rpc.jms.JMSCommandServer");

      if (component == null)
      {
         return;
      }

      Element collection =
         XMLUtil.findElementRecur(component, "Collection", "name", "commandClass");

      if (collection == null)
      {
         return;
      }

      XMLUtil.forEachChildElement(collection, "Item", new ElementHandler()
      {
         public void handleElement(Element element)
         {
            if ("nexj.core.rpc.mail.CheckMailCommand".equals(XMLUtil.getElementValue(element)))
            {
               XMLUtil.removeNode(element);
            }
         }
      });
   }
}
