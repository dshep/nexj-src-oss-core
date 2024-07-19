// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import java.util.Iterator;
import java.util.Properties;

import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;

/**
 * Class for storing custom properties.
 */
public class PropertyHolder extends MetadataObject
{
   // associations
   
   /**
    * The property map: String[String].
    */
   protected Lookup m_propertyMap;

   // operations

   /**
    * Adds a property to the holder.
    * @param sName The property name.
    * @param sValue The property value.
    * @throws MetadataException if the property already exists.
    */
   public void addProperty(String sName, String sValue)
   {
      verifyNotReadOnly();
      
      if (m_propertyMap == null)
      {
         m_propertyMap = new HashTab(4);
      }

      Object oldValue = m_propertyMap.put(sName, sValue);

      if (oldValue != null)
      {
         m_propertyMap.put(sName, oldValue);
         
         throw new MetadataException("err.meta.dupProperty", new Object[]{sName});
      }
   }

   /**
    * Adds a property to the holder, if it does not exist already.
    * @param sName The property name.
    * @param sValue The property value.
    */
   public void addDefaultProperty(String sName, String sValue)
   {
      if (findProperty(sName) == null)
      {
         addProperty(sName, sValue);
      }
   }

   /**
    * Adds properties from another holder, if they do not exist already.
    * @param holder The holder containing the default properties. 
    */
   public void addDefaultProperties(PropertyHolder holder)
   {
      for (Lookup.Iterator itr = holder.getPropertyIterator(); itr.hasNext();)
      {
         itr.next();
         addDefaultProperty((String)itr.getKey(), (String)itr.getValue());
      }
   }

   /**
    * Finds a property value by name.
    * @param sName The property name.
    * @return The property value, or null if not found.
    */
   public String findProperty(String sName)
   {
      if (m_propertyMap == null)
      {
         return null;
      }

      return (String)m_propertyMap.get(sName);
   }

   /**
    * @return The property count.
    */
   public int getPropertyCount()
   {
      if (m_propertyMap == null)
      {
         return 0;
      }

      return m_propertyMap.size();
   }

   /**
    * @return True if there are no properties. 
    */
   public boolean isEmpty()
   {
      return m_propertyMap == null || m_propertyMap.size() == 0;
   }

   /**
    * @return The property iterator.
    */
   public Lookup.Iterator getPropertyIterator()
   {
      if (m_propertyMap == null)
      {
         return HashTab.EMPTY_ITERATOR;
      }

      return m_propertyMap.iterator();
   }

   /**
    * @return A new property map containing the properties.
    */
   public Properties getProperties()
   {
      Properties properties = new Properties();

      for (Lookup.Iterator itr = getPropertyIterator(); itr.hasNext();)
      {
         itr.next();
         properties.put(itr.getKey(), itr.getValue());
      }
      
      return properties;
   }

   /**
    * @see nexj.core.meta.MetadataObject#clone()
    */
   public Object clone()
   {
      PropertyHolder holder = (PropertyHolder)super.clone();

      if (m_propertyMap != null)
      {
         holder.m_propertyMap = (Lookup)m_propertyMap.clone();
      }

      return holder;
   }

   /**
    * @see java.lang.Object#equals(java.lang.Object)
    */
   public boolean equals(Object obj)
   {
      if (!(obj instanceof PropertyHolder))
      {
         return false;
      }

      PropertyHolder holder = (PropertyHolder)obj;

      if (getPropertyCount() != holder.getPropertyCount())
      {
         return false;
      }

      for (Iterator itr = getPropertyIterator(); itr.hasNext();)
      {
         String sName = (String)itr.next();

         if (!ObjUtil.equal(findProperty(sName), holder.findProperty(sName)))
         {
            return false;
         }
      }

      return true;
   }
}
