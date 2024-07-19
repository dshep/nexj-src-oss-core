// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.xml.upgrade;

import nexj.core.meta.xml.XMLMetadataHelper;

import org.w3c.dom.Element;

/**
 * Class to upgrade individual XML DOM elements from some version prior to VERSION into VERSION.
 */
public class XMLMetadataUpgrade_1_1_1_1_Test
{
   /**
    * This upgrade applicable to all Metadata prior to this version.
    */
   public final static String VERSION="1.1.1.1";

   public void upgradeUpgradeTest(Element element, String sName, XMLMetadataHelper helper)
   {
      element.setAttribute("test", "true");
   }
}
