package nexj.core.meta.xml.upgrade;

import org.w3c.dom.Element;

import nexj.core.meta.xml.XMLMetadataHelper;
import nexj.core.util.Lookup;
import nexj.core.util.XMLUtil;

/**
 * Adds the System.PoolManager component.
 */
public class XMLMetadataUpgradeToPoolManager
{
   // constants

   /**
    * The upgrade applies to all metadata requiring a framework version older than this one.
    */
   public final static String VERSION = "8.0.125.0";

   // operations

   /**
    * Adds the System.PoolManager component to the model.
    */
   public void addResources(Lookup resourceMap)
   {
      resourceMap.put("components/System.PoolManager.comp", getClass().getResource("poolmanager/System.PoolManager.comp"));
   }

   public void upgradeComponent(Element element, String sName, XMLMetadataHelper helper)
   {
      if (sName.equals("System.Lifecycle"))
      {
         Element properties = XMLUtil.findChildElement(element, "Properties");

         if (properties != null)
         {
            Element component = XMLUtil.findChildElementByName(properties, "Collection", "component");

            if (component != null)
            {
               XMLUtil.addChildElement(component, null, "Item", "System.PoolManager");
            }
         }
      }
   }
}
