// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence;

import java.util.Iterator;

import nexj.core.meta.Metadata;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataLookupException;
import nexj.core.meta.NamedMetadataObject;
import nexj.core.util.LinkedHashTab;
import nexj.core.util.Lookup;

/**
 * Describes the data source type.
 */
public class DataSourceType extends NamedMetadataObject
{
   // attributes
   
   /**
    * The persistence flag.
    */
   protected boolean m_bPersistent;

   // associations

   /**
    * The metadata container.
    */
   protected Metadata m_metadata;

   /**
    * The persistence metadata loader class.
    */
   protected Class m_loader;

   /**
    * The persistence metadata exporter class.
    */
   protected Class m_exporter;

   /**
    * The data source adapter map: DataSourceAdapter[String].
    */
   protected Lookup m_adapterMap = new LinkedHashTab(4);

   // constructors
   
   /**
    * Constructs the data source type.
    * @param sName The data source type name.
    */
   public DataSourceType(String sName)
   {
      super(sName);
   }

   // operations
   
   /**
    * Sets the metadata container.
    * @param metadata The metadata container to set.
    */
   public void setMetadata(Metadata metadata)
   {
      verifyNotReadOnly();
      m_metadata = metadata;
   }

   /**
    * @return The metadata container.
    */
   public Metadata getMetadata()
   {
      return m_metadata;
   }

   /**
    * Sets the persistence metadata loader class.
    * @param loader The persistence metadata loader class to set.
    */
   public void setLoader(Class loader)
   {
      verifyNotReadOnly();
      m_loader = loader;
   }

   /**
    * @return The persistence metadata loader class.
    */
   public Class getLoader()
   {
      return m_loader;
   }
   
   /**
    * Sets the persistence metadata exporter class.
    * @param exporter The persistence metadata exporter class to set.
    */
   public void setExporter(Class exporter)
   {
      verifyNotReadOnly();
      m_exporter = exporter;
   }

   /**
    * @return The persistence metadata exporter class.
    */
   public Class getExporter()
   {
      return m_exporter;
   }
   
   /**
    * Adds a new data source adapter to the data source type.
    * @param adapter The data source adapter to add.
    * @throws MetadataException if a data source adapter
    * with the same name already exists.
    */
   public void addAdapter(DataSourceAdapter adapter)
   {
      verifyNotReadOnly();

      Object oldAdapter = m_adapterMap.put(adapter.getName(), adapter);

      if (oldAdapter != null)
      {
         m_adapterMap.put(adapter.getName(), oldAdapter);

         throw new MetadataException("err.meta.adapterDup", new Object[]
         {
            adapter.getName(),
            getName()
         });
      }

      adapter.setType(this);
   }

   /**
    * Gets a data source adapter by name.
    * @param sName The data source adapter name.
    * @return The data source adapter object.
    * @throws MetadataLookupException if the data source adapter does not exist.
    */
   public DataSourceAdapter getAdapter(String sName)
   {
      DataSourceAdapter adapter = (DataSourceAdapter) m_adapterMap.get(sName);

      if (adapter != null)
      {
         return adapter;
      }

      throw new MetadataLookupException("err.meta.adapterLookup", sName, this);
   }

   /**
    * @return The data source adapter count.
    */
   public int getAdapterCount()
   {
      return m_adapterMap.size();
   }

   /**
    * @return An iterator for the contained data source adapter objects.
    */
   public Iterator getAdapterIterator()
   {
      return m_adapterMap.valueIterator();
   }
}
