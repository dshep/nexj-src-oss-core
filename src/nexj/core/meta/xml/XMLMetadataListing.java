// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.xml;

import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import nexj.core.meta.MetadataObject;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;

/**
 * A listing of resources from an XMLMetadata directory tree.
 */
public class XMLMetadataListing extends MetadataObject
{
   // associations

   /**
    * The base name to resource map: XMLResource[String].
    */
   protected Lookup m_baseResourceMap;

   /**
    * The root name to resource map: XMLResource[String].
    */
   protected Lookup m_rootResourceMap;

   /**
    * The base, root, and linked mix-in name to resource map: XMLResource[String].
    */
   protected Lookup m_resourceMap;

   /**
    * The map of extensions to lists of resources from m_resourceMap,
    * excluding resources in m_excludedSet.
    */
   protected Lookup m_extensionMap;

   // operations

   /**
    * Sets the map of base resources to URLs.
    * @param baseResourceMap The map of base resources to URLs.
    */
   public void setBaseResourceMap(Lookup baseResourceMap)
   {
      verifyNotReadOnly();
      m_baseResourceMap = baseResourceMap;
   }

   /**
    * @return The map of base resources.
    */
   public Lookup getBaseResourceMap()
   {
      return m_baseResourceMap;
   }

   /**
    * Sets the map of root resources to URLs.
    * @param rootResourceMap The map of root resources to URLs.
    */
   public void setRootResourceMap(Lookup rootResourceMap)
   {
      verifyNotReadOnly();
      m_rootResourceMap = rootResourceMap;
   }

   /**
    * @return The map of root resources of type ModelResource[String].
    */
   public Lookup getRootResourceMap()
   {
      return m_rootResourceMap;
   }

   /**
    * Sets the map of all resources to URLs.
    * @param resourceMap The map of all resources to URLs.
    */
   public void setResourceMap(Lookup resourceMap)
   {
      verifyNotReadOnly();
      m_resourceMap = resourceMap;
   }

   /**
    * @return The map of all resources.
    */
   public Lookup getResourceMap()
   {
      return m_resourceMap;
   }

   /**
    * Adds a resource to the listing.
    * @param sName The resource name.
    * @param resource The resource to add.
    */
   public void addResource(String sName, XMLResource resource)
   {
      if (m_resourceMap == null)
      {
         m_resourceMap = new HashTab();
      }

      m_resourceMap.put(sName, resource);
      m_extensionMap = null;
   }

   /**
    * Adds a resource to the listing.
    * @param sName The resource name.
    * @param url The resource URL.
    */
   public void addRootResource(String sName, URL url)
   {
      verifyNotReadOnly();

      XMLResource resource = new XMLResource(sName, url, true); 

      if (m_resourceMap == null)
      {
         m_resourceMap = new HashTab();
      }

      m_resourceMap.put(sName, resource);

      if (m_rootResourceMap == null)
      {
         m_rootResourceMap = new HashTab();
      }

      m_rootResourceMap.put(sName, resource);
      m_extensionMap = null;
   }

   /**
    * Removes a resource from the root listing.
    * @param sName The resource name.
    */
   public void removeRootResource(String sName)
   {
      verifyNotReadOnly();
      
      if (m_rootResourceMap != null)
      {
         m_rootResourceMap.remove(sName);
      }

      Object baseResource = (m_baseResourceMap == null) ? null : m_baseResourceMap.get(sName);

      if (baseResource != null)
      {
         assert m_resourceMap != null;

         m_resourceMap.put(sName, baseResource);
      }
      else
      {
         if (m_resourceMap != null)
         {
            m_resourceMap.remove(sName);
         }
      }

      m_extensionMap = null;
   }

   /**
    * @return The map of extensions to resource name lists, excluding excluded resources.
    */
   public Lookup getExtensionMap()
   {
      if (m_extensionMap == null)
      {
         verifyNotReadOnly();
         m_extensionMap = new HashTab();

         if (m_resourceMap != null)
         {
            List resourceList = new ArrayList(m_resourceMap.size());

            for (Iterator itr = m_resourceMap.valueIterator(); itr.hasNext(); )
            {
               XMLResource resource = (XMLResource)itr.next();

               if (resource.isEnabled())
               {
                  resourceList.add(resource.getName());
               }
            }

            Collections.sort(resourceList); // add resources in sorted order

            for (int i = 0; i < resourceList.size(); i++)
            {
               String sName = (String)resourceList.get(i);
               int nExtensionIndex = sName.lastIndexOf('.');

               if (nExtensionIndex > 0)
               {
                  String sExtension = sName.substring(nExtensionIndex);
                  List extList = (List)m_extensionMap.get(sExtension);

                  if (extList == null)
                  {
                     extList = new ArrayList();
                     m_extensionMap.put(sExtension, extList);
                  }

                  extList.add(sName);
               }
            }
         }
      }

      return m_extensionMap;
   }

   /**
    * @see MetadataObject#makeReadOnly()
    */
   public void makeReadOnly()
   {
      getExtensionMap(); // populate extension map, if it is not already populated
      super.makeReadOnly();
   }        
}
