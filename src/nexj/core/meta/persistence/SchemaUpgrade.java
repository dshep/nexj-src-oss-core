// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence;

import nexj.core.meta.MetadataMarker;
import nexj.core.meta.upgrade.VersionUpgrade;

/**
 * Data source version upgrade.
 */
public abstract class SchemaUpgrade extends VersionUpgrade
{
   // attribute

   /**
    * The data source.
    */
   protected DataSource m_dataSource;

   // constructors

   /**
    * Constructs the upgrade.
    * @param sName The upgrade version.
    */
   public SchemaUpgrade(String sName)
   {
      super(sName);
   }

   /**
    * Constructs the upgrade.
    */
   public SchemaUpgrade()
   {
   }

   // operations

   /**
    * Sets the data source.
    * @param dataSource The data source to set.
    */
   public void setDataSource(DataSource dataSource)
   {
      verifyNotReadOnly();
      m_dataSource = dataSource;
   }

   /**
    * @return The data source.
    */
   public DataSource getDataSource()
   {
      return m_dataSource;
   }

   /**
    * @see nexj.core.meta.MetadataObject#setProperties(nexj.core.meta.MetadataMarker)
    */
   public void setProperties(MetadataMarker marker)
   {
      marker.setTypeName("SchemaUpgrade");
      marker.setProperty("dataSource", m_dataSource.getName());
      marker.setProperty("upgrade", m_upgrade.getName());
      marker.setProperty("version", m_sName);
   }
}
