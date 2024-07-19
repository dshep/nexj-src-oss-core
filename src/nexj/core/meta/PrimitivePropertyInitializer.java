// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import java.lang.reflect.Method;

import nexj.core.runtime.InvocationContext;
import nexj.core.util.Lookup;

/**
 * Initializes a property with a primitive value.
 */
public final class PrimitivePropertyInitializer extends PropertyInitializer
{
   // attributes

   /**
    * The property value.
    */
   private Object m_value;

   // constructors

   /**
    * Constructs the property initializer.
    * @param sName The property name.
    * @param type The property type.
    * @param method The property initialization method.
    * @param component The containing component.
    */
   public PrimitivePropertyInitializer(String sName, Class type, Method method, Component component)
   {
      super(sName, type, method, component);
   }

   /**
    * Creates a property initializer for the named property.
    * @param sName The property name.
    * @param component The containing component.
    */
   public PrimitivePropertyInitializer(String sName, Component component)
   {
      super(sName, component);
   }

   // operations

   /**
    * @see nexj.core.meta.PropertyInitializer#isCollection()
    */
   public boolean isCollection()
   {
      return false;
   }
   
   /**
    * @see nexj.core.meta.PropertyInitializer#initializeProperty(java.lang.Object, nexj.core.meta.InvocationContext, nexj.core.util.Lookup, java.lang.Object[])
    */
   public void initializeProperty(Object instance, InvocationContext context, Lookup instanceMap, Object[] argArray)
   {
      invokeMethod(instance, m_value, argArray);
   }

   /**
    * @see nexj.core.meta.PropertyInitializer#initializeValue(java.lang.Object)
    */
   public void initializeValue(Object value)
   {
      setValue(value);
   }

   /**
    * Sets the property value.
    * @param value The property value to set.
    */
   public void setValue(Object value)
   {
      verifyNotReadOnly();
      m_value = value;
   }

   /**
    * @return The property value.
    */
   public Object getValue()
   {
      return m_value;
   }
}
