// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence;

import nexj.core.meta.Metadata;
import nexj.core.meta.MetadataObject;
import nexj.core.util.ObjUtil;

/**
 * The persistence schema, describing the storage
 * in which the objects are persisted.
 */
public abstract class Schema extends MetadataObject
{
   // associations
   
   /**
    * The data source.
    */
   protected DataSource m_dataSource;

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
    * @return The root metadata object.
    */
   public Metadata getMetadata()
   {
      return m_dataSource.getType().getMetadata();
   }
   
   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuffer buf = new StringBuffer(64);

      buf.append(ObjUtil.getShortClassName(this));
      buf.append(' ');

      if (m_dataSource == null || m_dataSource.getName() == null)
      {
         buf.append("<anonymous>");
      }
      else
      {
         buf.append(m_dataSource.getName());
      }
      
      return buf.toString();
   }
}
