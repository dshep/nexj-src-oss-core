// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import java.lang.reflect.Method;

import nexj.core.runtime.InvocationContext;
import nexj.core.util.Lookup;

/**
 * Initializes the property with a component instance.
 */
public final class ComponentPropertyInitializer extends PropertyInitializer
{
   // associations

   /**
    * The component providing the instance
    * with which the property is initialized.
    */
   private Component m_instanceComponent;

   // constructors

   /**
    * Constructs the property initializer.
    * @param sName The property name.
    * @param type The property type.
    * @param method The property initialization method.
    * @param component The containing component.
    */
   public ComponentPropertyInitializer(String sName, Class type, Method method, Component component)
   {
      super(sName, type, method, component);
   }

   /**
    * Creates a property initializer for the named property.
    * @param sName The property name.
    * @param component The containing component.
    */
   public ComponentPropertyInitializer(String sName, Component component)
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
      invokeMethod(instance, m_instanceComponent.getInstance(context, instanceMap, argArray), argArray);
   }

   /**
    * @see nexj.core.meta.PropertyInitializer#initializeValue(java.lang.Object)
    */
   public void initializeValue(Object value)
   {
      setInstanceComponent((Component)value);
   }

   /**
    * Sets the component providing the instance for the property value.
    * @param component The component to set.
    */
   public void setInstanceComponent(Component component)
   {
      verifyNotReadOnly();
      m_instanceComponent = component;
      
      if (component != null && component.getMetadata() == null)
      {
         component.setMetadata(getComponent().getMetadata());
      }
   }

   /**
    * @return The component providing the instance for the property value.
    */
   public Component getInstanceComponent()
   {
      return m_instanceComponent;
   }
}
