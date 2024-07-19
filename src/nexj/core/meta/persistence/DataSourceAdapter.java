// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence;

import nexj.core.meta.NamedMetadataObject;

/**
 * Data source adapter class representing the different adapter strategies that
 * are available for a data source type.
 */
public final class DataSourceAdapter extends NamedMetadataObject
{
   // associations
   
   /**
    * The data source type.
    */
   protected DataSourceType m_type;

   /**
    * The adapter class object.
    */
   protected Class m_clazz;

   /**
    * The adapter compatibility version.
    */
   protected String m_sVersion;

   // constructors
   
   /**
    * Constructs the data source adapter.
    * @param sName The data source adapter name.
    */
   public DataSourceAdapter(String sName)
   {
      super(sName);
   }
   
   // operations
   
   /**
    * Sets the data source type.
    * @param type The data source type to set.
    */
   public void setType(DataSourceType type)
   {
      verifyNotReadOnly();
      m_type = type;
   }

   /**
    * @return The data source type.
    */
   public DataSourceType getType()
   {
      return m_type;
   }
   
   /**
    * Sets the adapter class object.
    * @param clazz The adapter class object to set.
    */
   public void setClassObject(Class clazz)
   {
      verifyNotReadOnly();
      m_clazz = clazz;
   }

   /**
    * @return The adapter class object.
    */
   public Class getClassObject()
   {
      return m_clazz;
   }
   
   /**
    * Sets the adapter compatibility version.
    * @param sVersion The adapter compatibility version to set.
    */
   public void setVersion(String sVersion)
   {
      verifyNotReadOnly();
      m_sVersion = sVersion;
   }

   /**
    * @return The adapter compatibility version.
    */
   public String getVersion()
   {
      return m_sVersion;
   }
}
