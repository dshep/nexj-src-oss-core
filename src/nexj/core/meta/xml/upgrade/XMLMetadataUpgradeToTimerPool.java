package nexj.core.meta.xml.upgrade;

import org.w3c.dom.Element;

import nexj.core.meta.integration.channel.timer.Timeout;
import nexj.core.meta.xml.XMLMetadataHelper;
import nexj.core.util.XMLUtil;

/**
 * Adds System.Timer to System.PoolManager component as a pool provider and sets up system timeouts.
 */
public class XMLMetadataUpgradeToTimerPool
{
   // constants

   /**
    * The upgrade applies to all metadata requiring a framework version older than this one.
    */
   public final static String VERSION = "8.0.142.0";

   // operations

   /**
    * Adds System.Timer to System.PoolManager component as a pool provider.
    * Sets up timeouts in System.Timer.
    */
   public void upgradeComponent(Element element, String sName, XMLMetadataHelper helper)
   {
      if (sName.equals("System.PoolManager"))
      {
         Element properties = XMLUtil.findChildElement(element, "Properties");

         if (properties != null && helper.findResource("components/System.Timer.comp") != null)
         {
            Element poolProvider = XMLUtil.findChildElementByName(properties, "Collection", "poolProvider");

            if (poolProvider == null)
            {
               poolProvider = XMLUtil.addChildElement(properties, null, "Collection");
               XMLUtil.setAttribute(poolProvider, "name", "poolProvider", true);
               XMLUtil.addChildElement(poolProvider, null, "Item", "System.Timer");
            }
         }
      }
      else if (sName.equals("System.Timer"))
      {
         Element properties = XMLUtil.findChildElement(element, "Properties");

         if (properties != null)
         {
            Element timeouts = XMLUtil.findChildElementByName(properties, "Collection", "timeout");

            if (timeouts == null)
            {
               timeouts = XMLUtil.addChildElement(properties, null, "Collection");
               XMLUtil.setAttribute(timeouts, "name", "timeout", true);
            }

            addTimeout(timeouts, 10000, "System.PoolManager");

            if (helper.findResource("components/System.ClusterManager.comp") != null)
            {
               addTimeout(timeouts, 3000, "System.ClusterManager");
            }
         }
      }
   }

   /**
    * Adds a timeout component definition to a timeout component collection.
    * @param timeouts The parent element.
    * @param nPeriod The period in milliseconds.
    * @param sComponent The component name.
    */
   protected void addTimeout(Element timeouts, int nPeriod, String sComponent)
   {
      Element timeout = XMLUtil.addChildElement(XMLUtil.addChildElement(timeouts, null, "Item"), null, "Component");

      XMLUtil.setAttribute(timeout, "type", Timeout.class.getName(), true);
      XMLUtil.setAttribute(timeout, "activation", "new", true);

      Element properties = XMLUtil.addChildElement(timeout, null, "Properties");

      XMLUtil.setAttribute(XMLUtil.addChildElement(properties, null, "Property", String.valueOf(nPeriod)), "name", "period", true);
      XMLUtil.setAttribute(XMLUtil.addChildElement(properties, null, "Property", sComponent), "name", "receiver", true);
   }
}
