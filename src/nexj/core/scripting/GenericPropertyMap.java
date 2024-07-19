// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import java.io.Serializable;
import java.util.Iterator;

import nexj.core.util.Iteratable;
import nexj.core.util.PropertyHashTab;
import nexj.core.util.PropertyIterator;
import nexj.core.util.PropertyMap;

/**
 * Generic scripted property map implementation.
 */
public class GenericPropertyMap extends ScriptedPropertyHolder implements PropertyMap, Iteratable, Serializable
{
   // constants

   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = 2349243736148455634L;

   /**
    * The variable map.
    */
   protected PropertyHashTab m_map;

   // operations

   /**
    * @see nexj.core.scripting.ScriptedPropertyHolder#getClassName()
    */
   public String getClassName()
   {
      return "var";
   }

   /**
    * @see nexj.core.util.PropertyMap#findValue(java.lang.String)
    */
   public Object findValue(String sName)
   {
      if (m_map == null)
      {
         return null;
      }

      return m_map.findValue(sName);
   }

   /**
    * @see nexj.core.util.PropertyMap#findValue(java.lang.String, java.lang.Object)
    */
   public Object findValue(String sName, Object defaultValue)
   {
      if (m_map == null)
      {
         return defaultValue;
      }

      return m_map.findValue(sName, defaultValue);
   }

   /**
    * @see nexj.core.scripting.ScriptedPropertyHolder#getValue(java.lang.String)
    * @see nexj.core.util.PropertyMap#getValue(java.lang.String)
    */
   public Object getValue(String sName)
   {
      if (m_map == null)
      {
         return null;
      }

      return m_map.getValue(sName);
   }

   /**
    * @see nexj.core.scripting.ScriptedPropertyHolder#setValue(java.lang.String, java.lang.Object)
    * @see nexj.core.util.PropertyMap#setValue(java.lang.String, java.lang.Object)
    */
   public void setValue(String sName, Object value)
   {
      if (m_map == null)
      {
         m_map = new PropertyHashTab();
      }

      m_map.setValue(sName, value);
   }

   /**
    * Removes the specified property.
    * @param sName The property name.
    * @return The removed property value.
    */
   public Object removeValue(String sName)
   {
      if (m_map == null)
      {
         return null;
      }

      return m_map.remove(sName);
   }

   /**
    * @see nexj.core.util.PropertyMap#hasValue(java.lang.String)
    */
   public boolean hasValue(String sName)
   {
      return m_map != null && m_map.hasValue(sName);
   }

   /**
    * @see nexj.core.util.PropertyMap#getValueCount()
    */
   public int getValueCount()
   {
      if (m_map == null)
      {
         return 0;
      }

      return m_map.getValueCount();
   }

   /**
    * @see nexj.core.util.PropertyMap#getIterator()
    */
   public PropertyIterator getIterator()
   {
      if (m_map == null)
      {
         return PropertyHashTab.EMPTY_ITERATOR;
      }

      return m_map.getIterator();
   }

   /**
    * @see nexj.core.util.Iteratable#iterator()
    */
   public Iterator iterator()
   {
      return getIterator();
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      if (m_map == null)
      {
         return "{}";
      }

      return m_map.toString();
   }
}
