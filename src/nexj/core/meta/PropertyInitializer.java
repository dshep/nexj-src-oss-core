// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import nexj.core.runtime.InvocationContext;
import nexj.core.util.Lookup;

/**
 * Base class for component property initializers.
 */
public abstract class PropertyInitializer extends NamedMetadataObject
{
   // associations

   /**
    * The property setter or appender method.
    */
   private Method m_method;

   /**
    * The property type.
    */
   private Class m_type;

   /**
    * The containing component.
    */
   private Component m_component;

   // constructors

   /**
    * Constructs the property initializer.
    * @param sName The property name.
    * @param type The property type.
    * @param method The property initialization method.
    * @param component The containing component.
    */
   public PropertyInitializer(String sName, Class type, Method method, Component component)
   {
      super(sName);
      m_type = type;
      m_method = method;
      m_component = component;
   }
   
   /**
    * Creates the property initializer for the named property.
    * @param sName The property name.
    * @param component The containing component.
    * @throws MetadataException if the property name is invalid.
    */
   public PropertyInitializer(String sName, Component component)
   {
      super(sName);
      m_component = component;
      m_method = component.getPropertyMethod(sName, isCollection());
      m_type = m_method.getParameterTypes()[0];
   }

   // operations

   /**
    * @return True if the property is a collection.
    */
   public abstract boolean isCollection();

   /**
    * Sets the preconfigured property value on the specified instance.
    * @param instance The component instance.
    * @param context The invocation context.
    * @param instanceMap The component instance map. 
    * @param argArray A cached array for property setter invocation.
    */
   public abstract void initializeProperty(Object instance, InvocationContext context, Lookup instanceMap, Object[] argArray);
   
   /**
    * Initializes the preconfigured value.
    * @param value The value to set.
    */
   public abstract void initializeValue(Object value);
   
   /**
    * Sets the property setter or appender method.
    * @param method The property setter or appender method to set.
    */
   public void setMethod(Method method)
   {
      verifyNotReadOnly();
      m_method = method;
   }

   /**
    * @return The property setter or appender method.
    */
   public Method getMethod()
   {
      return m_method;
   }

   /**
    * @return The property type.
    */
   public Class getType()
   {
      return m_type;
   }
   
   /**
    * Sets the containing component.
    * @param component The containing component to set.
    */
   public void setComponent(Component component)
   {
      verifyNotReadOnly();
      m_component = component;
   }

   /**
    * @return The containing component.
    */
   public Component getComponent()
   {
      return m_component;
   }

   /**
    * Invokes the method with one argument and handles the errors.
    * @param instance The component instance on which to invoke the method.
    * @param value The value to pass to the method.
    * @param argArray Preallocated argument array with one element.
    */
   protected void invokeMethod(Object instance, Object value, Object[] argArray)
   {
      argArray[0] = value;

      try
      {
         m_method.invoke(instance,argArray);
      }
      catch (Exception e)
      {
         Throwable t = e;
            
         if (e instanceof InvocationTargetException)
         {
            InvocationTargetException x = (InvocationTargetException)e;
               
            if (x.getTargetException() != null)
            {
                t = x.getTargetException();
            }
         }

         throw new ComponentException("err.comp.propertyInitialization",
            new Object[]{getName(), m_component.getName()}, t);
      }
   }
}
