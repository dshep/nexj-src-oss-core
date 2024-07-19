// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;

import nexj.core.runtime.Initializable;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.util.ExceptionHolder;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;

/**
 * Component metadata. Components are objects, which can be activated by
 * name. Their initial state is configured in the metadata.
 */
public final class Component extends NamedMetadataObject
{
   // constants

   /**
    * Singleton activation policy - the instance is shared
    * between all the threads in the process.
    */
   public final static int SINGLETON = 0;

   /**
    * Context activation policy - one instance per activation context.
    */
   public final static int CONTEXT = 1;

   /**
    * New instance activation policy - a new instance
    * is created on each activation.
    */
   public final static int NEW = 2;

   // attributes

   /**
    * The class object or the interface that exposes the configuration
    * properties. If no factory is specified, then it must be a class with a
    * public no-argument constructor.
    */
   private Class m_type;

   /**
    * Activation policy, one of the constants SINGLETON, CONTEXT, NEW.
    */
   private int m_nActivation = NEW;

   /**
    * The singleton instance.
    */
   private Object m_instance;

   /**
    * The method object of the factory, which is used to instantiate the
    * component.
    */
   private Method m_factoryMethod;

   // associations

   /**
    * The optional component, which creates instances of this component.
    */
   private Component m_factory;

   /**
    * The property initializer collection.
    */
   private List m_initializerList = new ArrayList(4); // of type PropertyInitializer

   /**
    * The containing metadata object.
    */
   private Metadata m_metadata;

   // constructors

   /**
    * Creates a component with a given name.
    * @param sName The component name.
    */
   public Component(String sName)
   {
      super(sName);
   }
   
   /**
    * Creates a component.
    * @param sName The component name.
    * @param type The component type.
    * @param nActivation The component activation mode, one of the Component.* constants.
    */
   public Component(String sName, Class type, int nActivation)
   {
      super(sName);
      m_type = type;
      m_nActivation = nActivation;
   }

   // operations

   /**
    * Instantiates a component according to its activation policy and
    * initializes it with the configured values.
    * @param context The invocation context, can be null for invocation
    * without a context.
    * @return The activated component instance. Cannot be null.
    * @throws ComponentException if an error occurred during instantiation.
    */
   public Object getInstance(InvocationContext context) throws ComponentException
   {
      if (m_instance != null)
      {
         return m_instance;
      }

      return getInstance(context,
         (m_nActivation == CONTEXT && context != null) ?
            context.getComponentInstanceMap() :
            new HashTab(8),
          new Object[1]);
   }

   /**   
    * Instantiates a component according to its activation policy and
    * initializes it with the configured values.
    * @param context The invocation context.
    * @param instanceMap The component instance map. 
    * @param argArray A cached array for property setter invocation.
    * @return The activated component instance. Cannot be null.
    * @throws ComponentException if an error occurred during instantiation.
    */
   protected Object getInstance(InvocationContext context, Lookup instanceMap, Object[] argArray) throws ComponentException
   {
      if (m_nActivation == SINGLETON && m_instance != null)
      {
         return m_instance;
      }

      Object instance = instanceMap.get(this);

      if (instance != null)
      {
         return instance;
      }

      if (m_factory != null)
      {
         instance = m_factory.getInstance(context, instanceMap, argArray);
         
         try
         {
            instance = m_factoryMethod.invoke(instance, null);
         }
         catch (Throwable t)
         {
            if (t instanceof InvocationTargetException)
            {
               InvocationTargetException e = (InvocationTargetException)t;
               
               if (e.getTargetException() != null)
               {
                   t = e.getTargetException();
               }
            }

            throw new ComponentException("err.comp.instantiation", new Object[]{getName()}, t);
         }
      }
      else
      {
         try
         {
            instance = m_type.newInstance();
         }
         catch (Throwable t)
         {
            throw new ComponentException("err.comp.instantiation", new Object[]{getName()}, t);
         }
      }
      
      instanceMap.put(this, instance);

      boolean bInitializationComplete = false;

      try
      {
         int n = m_initializerList.size();
         
         for (int i = 0; i < n; ++i)
         {
            ((PropertyInitializer)m_initializerList.get(i)).initializeProperty(instance, context, instanceMap, argArray);
         }
         
         if (instance instanceof InvocationContextAware)
         {
            ((InvocationContextAware)instance).setInvocationContext(context);
         }

         if (instance instanceof Initializable)
         {
            try
            {
               ((Initializable)instance).initialize();
            }
            catch (Throwable t)
            {
               throw new ComponentException("err.comp.initialization",
                  new Object[]{getName()}, t);
            }
         }

         bInitializationComplete = true;
      }
      finally
      {
         if (!bInitializationComplete)
         {
            instanceMap.remove(this);
         }
      }
      
      if (m_nActivation == SINGLETON)
      {
         m_instance = instance;
      }

      return instance;
   }

   /**
    * Sets the Java type/class object.
    * @param type The Java type/class object to set.
    */
   public void setType(Class type)
   {
      verifyNotReadOnly();
      m_type = type;
   }

   /**
    * @return The Java type/class object.
    */
   public Class getType()
   {
      return m_type;
   }
   
   /**
    * Sets the activation policy
    * @param nActivation The activation policy to set,
    * one of the constants SINGLETON, CONTEXT, NEW.
    */
   public void setActivation(int nActivation)
   {
      verifyNotReadOnly();
      m_nActivation = nActivation;
   }

   /**
    * @return The activation policy.
    */
   public int getActivation()
   {
      return m_nActivation;
   }
   
   /**
    * Sets the Java factory method.
    * @param sMethodName The name of the factory method.
    */
   public void setFactoryMethod(String sMethodName)
   {
      Method method;
      
      try
      {
         if (sMethodName == null)
         {
            sMethodName = "create";
         }
         
         method = m_factory.getType().getMethod(sMethodName, null);
      }
      catch (Exception e)
      {
         throw new MetadataException("err.meta.javaMethodLookup",
            new Object[]{sMethodName, m_factory.getType().getName()}, e);
      }
      
      setFactoryMethod(method);
   }
   
   /**
    * Sets the Java factory method.
    * @param factoryMethod The Java factory method to set.
    */
   public void setFactoryMethod(Method factoryMethod)
   {
      verifyNotReadOnly();
      m_factoryMethod = factoryMethod;
   }

   /**
    * @return The Java factory method.
    */
   public Method getFactoryMethod()
   {
      return m_factoryMethod;
   }
   
   /**
    * Sets the factory component.
    * @param factory The factory component to set.
    */
   public void setFactory(Component factory)
   {
      verifyNotReadOnly();
      m_factory = factory;
   }

   /**
    * @return The factory component.
    */
   public Component getFactory()
   {
      return m_factory;
   }

   /**
    * Sets the factory component and the factory method.
    * @param factory The factory component to set.
    * @param sMethodName The name of the factory method.
    */
   public void setFactory(Component factory, String sMethodName)
   {
      setFactory(factory);
      setFactoryMethod(sMethodName);
   }
   
   /**
    * Gets a property initialization method.
    * @param sName The property name.
    * @param bCollection True if the property is a collection.
    */
   public Method getPropertyMethod(String sName, boolean bCollection)
   {
      String sMethodName = ((bCollection) ? "add" : "set") +
         sName.substring(0, 1).toUpperCase(Locale.ENGLISH) + sName.substring(1);

      Method[] methodArray = m_type.getMethods();
      Method method = null;
   
      for (int i = 0; i < methodArray.length; ++i)
      {
         Method meth = methodArray[i];
   
         if (Modifier.isPublic(meth.getModifiers()) &&
            !Modifier.isStatic(meth.getModifiers()) &&
            meth.getName().equals(sMethodName))
         {
            Class[] paramArray = meth.getParameterTypes();
   
            if (paramArray.length == 1)
            {
               if (method != null)
               {
                  throw new MetadataException("err.meta.javaMethodDup",
                     new Object[]{sMethodName, m_type.getName()});
               }

               method = meth;
            }
         }
      }
   
      if (method == null)
      {
         throw new MetadataException("err.meta.javaMethodLookup",
            new Object[]{sMethodName, m_type.getName()});
      }
      
      return method;
   }

   /**
    * Gets a final static constant field value.
    * @param sName The field name.
    * @param defaultValue The default value, if the field is not found.
    */
   public Object getConstantValue(String sName, Object defaultValue)
   {
      if (m_type != null)
      {
         Field[] fieldArray = m_type.getFields();

         for (int i = 0; i < fieldArray.length; ++i)
         {
            Field field = fieldArray[i];

            if (field.getName().equals(sName))
            {
               if ((field.getModifiers() & (Modifier.STATIC | Modifier.FINAL)) ==
                  (Modifier.STATIC | Modifier.FINAL))
               {
                  try
                  {
                     return field.get(null);
                  }
                  catch (Throwable t)
                  {
                  }
               }

               break;
            }
         }
      }

      return defaultValue;
   }

   /**
    * Creates and adds a primitive property initializer to the component.
    * @param sName The property name.
    * @param value The property value.
    * @return The created initializer.
    */
   public PrimitivePropertyInitializer addPrimitivePropertyInitializer(String sName, Object value)
   {
      PrimitivePropertyInitializer initializer = new PrimitivePropertyInitializer(sName, this);
      
      initializer.setValue(value);
      addPropertyInitializer(initializer);
      
      return initializer;
   }
   
   /**
    * Creates and adds a component property initializer to the component.
    * @param sName The property name.
    * @param component The initializing component.
    * @return The created initializer.
    */
   public ComponentPropertyInitializer addComponentPropertyInitializer(String sName, Component component)
   {
      ComponentPropertyInitializer initializer = new ComponentPropertyInitializer(sName, this);
      
      initializer.setInstanceComponent(component);
      addPropertyInitializer(initializer);
      
      return initializer;
   }
   
   /**
    * Creates and adds a primitive collection property initializer to the component.
    * @param sName The property name.
    * @return The created initializer.
    */
   public PrimitiveCollectionPropertyInitializer addPrimitiveCollectionPropertyInitializer(String sName)
   {
      PrimitiveCollectionPropertyInitializer initializer = new PrimitiveCollectionPropertyInitializer(sName, this);
      
      addPropertyInitializer(initializer);
      
      return initializer;
   }
   
   /**
    * Creates and adds a component collection property initializer to the component.
    * @param sName The property name.
    * @return The created initializer.
    */
   public ComponentCollectionPropertyInitializer addComponentCollectionPropertyInitializer(String sName)
   {
      ComponentCollectionPropertyInitializer initializer = new ComponentCollectionPropertyInitializer(sName, this);
      
      addPropertyInitializer(initializer);
      
      return initializer;
   }

   /**
    * Adds a new property initializer to the component.
    * @param initializer The property initializer to add.
    */
   public void addPropertyInitializer(PropertyInitializer initializer)
   {
      verifyNotReadOnly();
      m_initializerList.add(initializer);
      initializer.setComponent(this);
   }

   /**
    * Finds a property initializer by name.
    * @param sName The name of the property initializer.
    * @return The property initializer or null if not found.
    */
   public PropertyInitializer findPropertyInitializer(String sName)
   {
      for (int i = m_initializerList.size() - 1; i >= 0; --i)
      {
         PropertyInitializer initializer = (PropertyInitializer)m_initializerList.get(i);

         if (initializer.getName().equals(sName))
         {
            return initializer;
         }
      }

      return null;
   }

   /**
    * Gets a property initializer by ordinal number.
    * @param nOrdinal The property initializer ordinal number (0-based).
    * @return The property initializer object.
    */
   public PropertyInitializer getPropertyInitializer(int nOrdinal)
   {
      return (PropertyInitializer)m_initializerList.get(nOrdinal);
   }

   /**
    * @return The property initializer count.
    */
   public int getPropertyInitializerCount()
   {
      return m_initializerList.size();
   }

   /**
    * @return An iterator for the contained property initializer objects.
    */
   public Iterator getPropertyInitializerIterator()
   {
      return m_initializerList.iterator();
   }
   
   /**
    * Sets the containing metadata object.
    * @param metadata The containing metadata object to set.
    */
   public void setMetadata(Metadata metadata)
   {
      verifyNotReadOnly();
      m_metadata = metadata;
   }

   /**
    * @return The containing metadata object.
    */
   public Metadata getMetadata()
   {
      return m_metadata;
   }

   /**
    * @see nexj.core.meta.MetadataObject#validate(ContextMetadata, ExceptionHolder)
    */
   public void validate(ContextMetadata metadata, ExceptionHolder warnings)
   {
      super.validate(metadata, warnings);
      
      if (m_type != null)
      {
         if (m_factory == null)
         {
            if (Modifier.isInterface(m_type.getModifiers()) ||
               Modifier.isAbstract(m_type.getModifiers()))
            {
               throw new MetadataException("err.meta.abstractComp",
                  new Object[]{getName(), m_type.getName()});
            }
   
            try
            {
               if (!Modifier.isPublic(m_type.getConstructor(null).getModifiers()))
               {
                  throw new MetadataException("err.meta.inaccessibleCompConstructor",
                     new Object[]{getName(), m_type.getName()});
               }
            }
            catch (NoSuchMethodException e)
            {
               throw new MetadataException("err.meta.missingDefaultCompConstructor",
                  new Object[]{getName(), m_type.getName()});
            }
         }
         else
         {
            if (!m_type.isAssignableFrom(m_factoryMethod.getReturnType()) &&
               !m_factoryMethod.getReturnType().isAssignableFrom(m_type))
            {
               throw new MetadataException("err.meta.componentFactoryMethodTypeMismatch",
                  new Object[]{getName(), m_factoryMethod.getReturnType().getName(), m_type.getName()});
            }
         }
      }
   }

   /**
    * @see nexj.core.meta.MetadataObject#makeReadOnly()
    */
   public void makeReadOnly()
   {
      for (Iterator itr = getPropertyInitializerIterator(); itr.hasNext();)
      {
         ((MetadataObject)itr.next()).makeReadOnly();
      }

      if (m_factory != null)
      {
         m_factory.makeReadOnly();
      }

      super.makeReadOnly();

      ((ArrayList)m_initializerList).trimToSize(); // free unused memory
   }
}