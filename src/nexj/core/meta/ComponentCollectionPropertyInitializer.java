// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nexj.core.runtime.InvocationContext;
import nexj.core.util.Lookup;

/**
 * Initializes the property with component instances via repeated calls to
 * the appender method.
 */
public final class ComponentCollectionPropertyInitializer extends PropertyInitializer
{
   // associations

   /**
    * Collection of components providing the instances
    * with which the property collection is initialized.
    */
   private List m_componentList = new ArrayList(4); // of type Component

   // constructors
   
   /**
    * Constructs the property initializer.
    * @param sName The property name.
    * @param type The property type.
    * @param method The property initialization method.
    * @param component The containing component.
    */
   public ComponentCollectionPropertyInitializer(String sName, Class type, Method method, Component component)
   {
      super(sName, type, method, component);
   }

   /**
    * Creates a property initializer for the named property.
    * @param sName The property name.
    * @param component The containing component.
    */
   public ComponentCollectionPropertyInitializer(String sName, Component component)
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
      int n = m_componentList.size();
      
      for (int i = 0; i < n; ++i)
      {
         invokeMethod(instance,
            ((Component)m_componentList.get(i)).getInstance(context, instanceMap, argArray),
            argArray);
      }
   }

   /**
    * @see nexj.core.meta.PropertyInitializer#initializeValue(java.lang.Object)
    */
   public void initializeValue(Object value)
   {
      addComponent((Component)value);
   }

   /**
    * Adds a new component to the property initializer.
    * @param component The component to add.
    */
   public void addComponent(Component component)
   {
      verifyNotReadOnly();
      m_componentList.add(component);

      if (component.getMetadata() == null)
      {
         component.setMetadata(getComponent().getMetadata());
      }
   }

   /**
    * Gets a component by ordinal number.
    * @param nOrdinal The component ordinal number (0-based).
    * @return The component object.
    */
   public Component getComponent(int nOrdinal)
   {
      return (Component)m_componentList.get(nOrdinal);
   }

   /**
    * @return The component count.
    */
   public int getComponentCount()
   {
      return m_componentList.size();
   }

   /**
    * @return An iterator for the contained component objects.
    */
   public Iterator getComponentIterator()
   {
      return m_componentList.iterator();
   }
}