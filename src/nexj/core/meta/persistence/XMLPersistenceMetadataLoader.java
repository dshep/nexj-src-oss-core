// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence;

import org.w3c.dom.Element;

import nexj.core.meta.Metaclass;
import nexj.core.meta.xml.XMLMetadataHelper;
import nexj.core.meta.xml.XMLMetadataLoader;

/**
 * Interface implemented by components reading
 * the persistence mapping data from XML DOM.
 */
public interface XMLPersistenceMetadataLoader
{
   // operations

   /**
    * Loads the data source.
    * @param element The element containing the data source.
    * @param sName The data source name.
    * @param type The data source type.
    * @param loader The metadata loader.
    * @return The loaded data source.
    */
   public DataSource loadDataSource(Element element, String sName, DataSourceType type, XMLMetadataLoader loader);
   
   /**
    * Loads the persistence mapping for a given metaclass.
    * @param element The element containing the mapping.
    * @param metaclass The metaclass, with which the mapping is associated.
    * @param dataSource The data source for the metaclass.
    * @param loader The metadata loader.
    * @return The loaded persistence mapping.
    */
   public PersistenceMapping loadMapping(Element element, Metaclass metaclass,
      DataSource dataSource, XMLMetadataLoader loader);
   
   /**
    * Loads the connection.
    * @param element The element containing the connection information.
    * @param source The data source.
    * @param loader The metadata loader.
    */
   public void loadConnection(Element element, DataSource source, XMLMetadataLoader loader);

   /**
    * Loads a data source version upgrade.
    * @param element The element containing the upgrade.
    * @param sName The upgrade version.
    * @param source The data source.
    * @param helper The metadata helper.
    */
   public SchemaUpgrade loadUpgrade(Element element, String sName, DataSource source, XMLMetadataHelper helper);
}
