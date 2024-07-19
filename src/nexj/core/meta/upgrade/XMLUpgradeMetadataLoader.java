// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.upgrade;

import org.w3c.dom.Element;

import nexj.core.meta.MetadataException;
import nexj.core.meta.persistence.DataSource;
import nexj.core.meta.persistence.XMLPersistenceMetadataLoader;
import nexj.core.meta.xml.XMLMetadata;
import nexj.core.meta.xml.XMLMetadataHelper;
import nexj.core.runtime.InvocationContext;
import nexj.core.scripting.GlobalEnvironment;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Pair;
import nexj.core.util.XMLUtil;

/**
 * XML upgrade metadata loader.
 */
public class XMLUpgradeMetadataLoader
{
   // associations
   
   /**
    * The XML metadata.
    */
   protected XMLMetadata m_metadata;
   
   /**
    * The XML metadata helper.
    */
   protected XMLMetadataHelper m_helper;
   
   /**
    * Global environment for compilation.
    */
   protected GlobalEnvironment m_env;
   
   /**
    * The VM for compilation.
    */
   protected Machine m_machine;

   // constructors
   
   /**
    * Constructs the loader.
    * @param metadata The XML metadata root object.
    * @param helper The XML metadata helper.
    */
   public XMLUpgradeMetadataLoader(XMLMetadata metadata, XMLMetadataHelper helper)
   {
      m_metadata = metadata;
      m_helper = helper;
      m_env = new GlobalEnvironment(m_metadata.getGlobalEnvironment());
      m_machine = new Machine(m_env, (InvocationContext)null);
   }

   // operations

   /**
    * Loads an upgrade from a DOM element.
    * @param sName The upgrade name.
    * @param upgradeElement The DOM element.
    */
   public Upgrade loadUpgrade(String sName, final Element upgradeElement) throws MetadataException
   {
      XMLMetadataHelper.verifyRootElement(upgradeElement, "Upgrade");

      final Upgrade upgrade = new Upgrade(sName);

      upgrade.setMetadata(m_metadata);

      XMLUtil.forEachChildElement(upgradeElement, null, m_helper.new ElementHandler("version")
      {
         protected void handleElement(Element element, String sName)
         {
            if (element.getNodeName().equals("Label"))
            {
               upgrade.addVersion(new LabelUpgrade(sName));
            }
            else if (element.getNodeName().equals("Load")) // request for reset of SysVersion'loaded
            {
               upgrade.addVersion(LoadUpgrade.create(sName, m_metadata, m_machine, upgrade));
            }
            else if (element.getNodeName().equals("Script"))
            {
               ScriptUpgrade scriptUpgrade = new ScriptUpgrade(sName);

               scriptUpgrade.setUpgrade(upgrade);
               scriptUpgrade.setBody((Pair)m_helper.parse(m_helper.getElementValue(element),
                  true, scriptUpgrade.getPosMap(), null, m_env));

               scriptUpgrade.compile(m_machine);
               upgrade.addVersion(scriptUpgrade);
            }
            else
            {
               DataSource dataSource = m_metadata.getDataSource(XMLUtil.getReqStringAttr(element, "dataSource"));
               XMLPersistenceMetadataLoader loader = (XMLPersistenceMetadataLoader)m_helper.getClassInstance(dataSource.getType().getLoader());

               upgrade.addVersion(loader.loadUpgrade(element, sName, dataSource, m_helper));
            }
         }

         protected String getName(Element element)
         {
            if (element.getNodeName().equals("Label"))
            {
               return XMLUtil.getReqStringAttr(element, "version");
            }

            return XMLUtil.getStringAttr(element, "version");
         }
      });

      return upgrade;
   }
}
