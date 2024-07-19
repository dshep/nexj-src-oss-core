// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;


/**
 * Property map interface.
 */
public interface PropertyMap
{
   /**
    * Finds a property value.
    * @param sName The property name.
    * @return The property value, or null if not found.
    */
   Object findValue(String sName);

   /**
    * Finds a property value.
    * @param sName The property name.
    * @param defaultValue The default value to return if the property is not found.
    * @return The property value, or defaultValue if not found.
    */
   Object findValue(String sName, Object defaultValue);

   /**
    * Gets a property value.
    * May throw an exception if not found.
    * @param sName The property name.
    * @return The property value.
    */
   Object getValue(String sName);

   /**
    * Sets a property value.
    * @param sName The property name.
    * @param value The property value.
    */
   void setValue(String sName, Object value);

   /**
    * Determines if a named value exists in the map.
    * @param sName The property name.
    * @return True if the value exists.
    */
   boolean hasValue(String sName);

   /**
    * @return The property value count.
    */
   int getValueCount();

   /**
    * @return The property iterator.
    */
   PropertyIterator getIterator();

   /**
    * @return The property map class name.
    */
   String getClassName();
}
