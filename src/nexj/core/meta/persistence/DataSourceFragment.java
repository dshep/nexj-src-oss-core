// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence;

import nexj.core.meta.Metadata;
import nexj.core.meta.MetadataFinder;
import nexj.core.meta.MetadataObject;
import nexj.core.meta.NamedMetadataObject;
import nexj.core.util.StringUtil;

/**
 * Data source fragment metadata, Describing a separate instance
 * of the data source with identical schema and parameters,
 * typically used for isolated partitioning of data and
 * horizontal data source scaling resulting from this. 
 */
public abstract class DataSourceFragment extends NamedMetadataObject implements MetadataFinder
{
   // associations

   /**
    * The data source.
    */
   protected DataSource m_dataSource;

   // constructors

   /**
    * Constructs a default (nameless) fragment.
    */
   public DataSourceFragment()
   {
   }

   /**
    * Constructs a fragment.
    * @param sName The fragment name.
    */
   public DataSourceFragment(String sName)
   {
      super((StringUtil.isEmpty(sName)) ? null : sName);
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
    * @return True if this is the default fragment.
    */
   public boolean isDefault()
   {
      return m_sName == null;
   }

   /**
    * Returns an empty string for the default fragment, or .name for other fragments.
    * @return The fragment suffix.
    */
   public String getSuffix()
   {
      return (isDefault()) ? "" : '.' + m_sName;
   }

   /**
    * @see nexj.core.meta.MetadataFinder#find(nexj.core.meta.Metadata)
    */
   public MetadataObject find(Metadata metadata)
   {
      if (m_dataSource != null)
      {
         DataSource ds = metadata.findDataSource(m_dataSource.getName());

         if (ds != null && ds.getClass() == m_dataSource.getClass())
         {
            return ds.findFragment(m_sName);
         }
      }

      return null;
   }
}
