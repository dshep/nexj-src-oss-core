// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nexj.core.runtime.InvocationContext;
import nexj.core.util.Lookup;

/**
 * Initializes the property with primitive values
 * via repeated calls to the appender method.
 */
public final class PrimitiveCollectionPropertyInitializer extends PropertyInitializer
{
   // attributes

   /**
    * The list of primitive values used to initialize the collection property.
    */
   private List m_valueList = new ArrayList(4); // of type Object

   // constructors

   /**
    * Constructs the property initializer.
    * @param sName The property name.
    * @param type The property type.
    * @param method The property initialization method.
    * @param component The containing component.
    */
   public PrimitiveCollectionPropertyInitializer(String sName, Class type, Method method, Component component)
   {
      super(sName, type, method, component);
   }

   /**
    * Creates a property initializer for the named property.
    * @param sName The property name.
    * @param component The containing component.
    */
   public PrimitiveCollectionPropertyInitializer(String sName, Component component)
   {
      super(sName, component);
   }

   // operations

   /**
    * @see nexj.core.meta.PropertyInitializer#isCollection()
    */
   public boolean isCollection()
   {
      return true;
   }

   /**
    * @see nexj.core.meta.PropertyInitializer#initializeProperty(java.lang.Object, nexj.core.meta.InvocationContext, nexj.core.util.Lookup, java.lang.Object[])
    */
   public void initializeProperty(Object instance, InvocationContext context, Lookup instanceMap, Object[] argArray)
   {
      int n = m_valueList.size();
      
      for (int i = 0; i < n; ++i)
      {
         invokeMethod(instance, m_valueList.get(i), argArray);
      }
   }

   /**
    * @see nexj.core.meta.PropertyInitializer#initializeValue(java.lang.Object)
    */
   public void initializeValue(Object value)
   {
      addValue(value);
   }

   /**
    * Adds a new property value to the property initializer.
    * @param value The property value to add.
    */
   public void addValue(Object value)
   {
      verifyNotReadOnly();
      m_valueList.add(value);
   }

   /**
    * Gets a property value by ordinal number.
    * @param nOrdinal The property value ordinal number (0-based).
    * @return The property value object.
    */
   public Object getValue(int nOrdinal)
   {
      return (Object)m_valueList.get(nOrdinal);
   }

   /**
    * @return The property value count.
    */
   public int getValueCount()
   {
      return m_valueList.size();
   }

   /**
    * @return An iterator for the contained property value objects.
    */
   public Iterator getValueIterator()
   {
      return m_valueList.iterator();
   }
}
