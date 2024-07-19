// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.xml;

import java.io.StringReader;
import java.util.Arrays;

import org.w3c.dom.Element;

import nexj.core.util.SysUtil;

import junit.framework.TestCase;

public class XMLMetadataHelperTest extends TestCase
{
   /**
    * Constructor for XMLMetadataTest.
    * @param name
    */
   public XMLMetadataHelperTest(String name)
   {
      super(name);
   }

   public void testGetRepositoryPath()
   {
      assertEquals(SysUtil.PACKAGE + "/meta/" + "http_www_nexjsystems_com_ns_core", XMLMetadataHelper.getRepositoryPath("http://www.nexjsystems.com/ns/core"));
      assertEquals(SysUtil.PACKAGE + "/meta/" + "_Believe34_green_applesCan_be", XMLMetadataHelper.getRepositoryPath("97_Believe34__green_:_applesCan//be"));
   }
   
   public void testUpgrade()
   {
      String sXML = "<UpgradeTest/>";
      final Object[] handled = new Object[2];
      XMLMetadata metadata = new XMLMetadata("Test", null, null, null, null);
      XMLMetadataHelper helper = new XMLMetadataHelper();
      XMLMetadataHelper.ResourceHandler handler = new XMLMetadataHelper.ResourceHandler()
      {
         public void handleResource(Element rootElement, String sName)
         {
            handled[0] = rootElement;
            handled[1] = sName;
         }
      };

      // upgrade of the same version as metadata should not be invoked
      // attribute hardcoded in test upgrade step
      helper.setUpgrader(new XMLMetadataUpgrader("1.1.1.1+", metadata.getUpgradeResources()));
      helper.loadResource(null, "ResourceName", handler, null, new StringReader(sXML));
      assertNotNull(handled[0]);
      assertEquals("ResourceName", handled[1]);
      assertFalse(((Element)handled[0]).hasAttribute("test"));
      assertFalse(((Element)handled[0]).hasAttribute("newest"));

      // upgrade of a lesser version than metadata should be invoked
      // attribute hardcoded in test upgrade step
      helper.setUpgrader(new XMLMetadataUpgrader("1.1.1.0+", metadata.getUpgradeResources()));
      Arrays.fill(handled, null);
      helper.loadResource(null, "ResourceName", handler, null, new StringReader(sXML));
      assertEquals("true", ((Element)handled[0]).getAttribute("test"));
      assertFalse(((Element)handled[0]).hasAttribute("newest"));
   }
}