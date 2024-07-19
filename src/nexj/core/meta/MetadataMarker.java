// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import nexj.core.util.Lookup;

/**
 * Metadata marker providing properties identifying a metadata object.
 */
public interface MetadataMarker
{
   // constants

   /**
    * The resource name property name.
    */
   final static String RESOURCE_NAME = "resourceName";
   
   /**
    * The type name property name.
    */
   final static String TYPE_NAME = "typeName";

   // operations

   /**
    * Sets a metadata element location property.
    * @param sName Property name.
    * @param value Property value.
    */
   void setProperty(String sName, Object value);
   
   /**
    * Gets a metadata element location property.
    * @param sName Property name.
    * @return The property value. Null if not found.
    */
   Object getProperty(String sName);
   
   /**
    * @return A property iterator, the key and value hold
    * the property name and its value correspondingly.
    */
   Lookup.Iterator getPropertyIterator();
   
   /**
    * @return The property count.
    */
   int getPropertyCount();
   
   /**
    * Sets the metadata resource name (e.g. XML file path
    * relative to the metadata root URL).
    * @param sName The resource name to set.
    */
   void setResourceName(String sName);
   
   /**
    * @return The metadata resource name (e.g. relative XML file path).
    */
   String getResourceName();

   /**
    * Sets the invalid metadata element type name (e.g. Metaclass, Attribute).
    * @param sName The metadata element type name.
    */
   void setTypeName(String sName);
   
   /**
    * @return The metadata element type name (e.g. Metaclass, Attribute).
    */
   String getTypeName();
}
