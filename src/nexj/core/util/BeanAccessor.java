// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Enumeration;
import java.util.Locale;
import java.util.Properties;

/**
 * Caching bean property accessor.
 */
public class BeanAccessor
{
   // constants

   /**
    * Array of one string class.
    */
   protected final static Class[] ONE_STRING_CLASS_ARRAY = new Class[]{String.class};

   // associations

   /**
    * Setter method arguments: Object[1].
    */
   protected Object[] m_args = new Object[1];

   /**
    * Map of property names and class objects to their info structures: PropertyInfo[String][Class].
    */
   protected Lookup2D m_propertyMap = new HashTab2D();

   // operations

   /**
    * Sets a named property on a bean.
    * @param obj The bean instance. Can be null.
    * @param sName The property name.
    * @param value The property value. Must be of correct type.
    * @throws Throwable if an error occurs.
    */
   public void setProperty(Object obj, String sName, Object value) throws Throwable
   {
      if (obj != null)
      {
         PropertyInfo info = getPropertyInfo(obj.getClass(), sName);
         Method method = info.setter;

         if (method == null)
         {
            throw new IllegalArgumentException("Property \"" + sName + "\" is read-only");
         }

         m_args[0] = value;

         try
         {
            method.invoke(obj, m_args);
         }
         catch (InvocationTargetException e)
         {
            throw e.getTargetException();
         }
      }
   }

   /**
    * Sets a named property on a bean.
    * @param obj The bean instance. Can be null.
    * @param sName The property name.
    * @param sValue The property value.
    * @throws Throwable if an error occurs.
    */
   public void setProperty(Object obj, String sName, String sValue) throws Throwable
   {
      if (obj != null)
      {
         PropertyInfo info = getPropertyInfo(obj.getClass(), sName);
         Method method = info.setter;

         if (method == null)
         {
            throw new IllegalArgumentException("Property \"" + sName + "\" is read-only");
         }

         try
         {
            m_args[0] = convert(info.type, sValue);

            try
            {
               method.invoke(obj, m_args);
            }
            catch (InvocationTargetException e)
            {
               throw e.getTargetException();
            }
         }
         catch (Throwable t)
         {
            throw new IllegalArgumentException("Unable to set property \"" + sName +
               "\" (of type \"" + info.type.getName() + "\")", t);
         }
      }
   }

   /**
    * Template method for type conversion from a string.
    * @param type The target type.
    * @param sValue The string to convert. Can be null.
    * @return The converted value.
    */
   protected Object convert(Class type, String sValue) throws Throwable
   {
      if (sValue == null)
      {
         return null;
      }

      if (type == String.class)
      {
         return sValue;
      }

      if (type == Properties.class)
      {
         return PropertyUtil.fromString(sValue);
      }

      try
      {
         m_args[0] = sValue;

         return type.getConstructor(ONE_STRING_CLASS_ARRAY).newInstance(m_args);
      }
      catch (InvocationTargetException e)
      {
         throw e.getTargetException();
      }
   }

   /**
    * Sets bean properties from a property map.
    * @param obj The bean instance. Can be null.
    * @param properties The property map. Can be null.
    * @throws Throwable On any error.
    */
   public void setProperties(Object obj, Properties properties) throws Throwable
   {
      if (properties != null)
      {
         for (Enumeration enm = properties.propertyNames(); enm.hasMoreElements();)
         {
            String sName = (String)enm.nextElement();

            setProperty(obj, sName, properties.getProperty(sName));
         }
      }
   }

   /**
    * Gets a named property from a bean.
    * @param obj The bean instance. Can be null.
    * @param sName The property name.
    * @return The property value.
    * @throws Throwable if an error occurs.
    */
   public Object getProperty(Object obj, String sName) throws Throwable
   {
      if (obj != null)
      {
         PropertyInfo info = getPropertyInfo(obj.getClass(), sName);
         Method method = info.getter;

         if (method == null)
         {
            throw new IllegalArgumentException("Property \"" + sName + "\" is write-only");
         }

         try
         {
            return method.invoke(obj, null);
         }
         catch (InvocationTargetException e)
         {
            throw e.getTargetException();
         }
      }

      return null;
   }

   /**
    * Gets property info.
    * @param clazz The class object.
    * @param sName The property name.
    */
   protected PropertyInfo getPropertyInfo(Class clazz, String sName)
   {
      PropertyInfo info = (PropertyInfo)m_propertyMap.get(sName, clazz);

      if (info == null)
      {
         Method[] methods = (Method[])m_propertyMap.get(clazz, Boolean.TRUE);

         if (methods == null)
         {
            methods = clazz.getMethods();
            m_propertyMap.put(clazz, Boolean.TRUE, methods);
         }

         info = new PropertyInfo(methods, sName);
         m_propertyMap.put(sName, clazz, info);
      }

      return info;
   }

   // inner classes

   /**
    * Property information.
    */
   protected static class PropertyInfo
   {
      /**
       * Property type.
       */
      public Class type;

      /**
       * Getter method.
       */
      public Method getter;

      /**
       * Setter method.
       */
      public Method setter;

      // constructors

      /**
       * Constructs the property info.
       * @param methods The class public methods.
       * @param sName The property name.
       */
      public PropertyInfo(Method[] methods, String sName)
      {
         String sCapName = sName.substring(0, 1).toUpperCase(Locale.ENGLISH) + sName.substring(1);

         for (int i = 0; i < methods.length; ++i)
         {
            Method method = methods[i];
            String sMethod = method.getName();

            if (this.getter == null)
            {
               if (sMethod.startsWith("get"))
               {
                  if (sMethod.length() == (sCapName.length() + 3) &&
                     sMethod.regionMatches(3, sCapName, 0, sCapName.length()) &&
                     method.getParameterTypes().length == 0)
                  {
                     this.getter = method;
                  }

                  continue;
               }

               if (sMethod.startsWith("is"))
               {
                  if (sMethod.length() == (sCapName.length() + 2) &&
                     sMethod.regionMatches(2, sCapName, 0, sCapName.length()) &&
                     method.getParameterTypes().length == 0 &&
                     (method.getReturnType() == Boolean.class ||
                      method.getReturnType() == Boolean.TYPE))
                  {
                     this.getter = method;
                  }

                  continue;
               }
            }
            else if (this.setter != null)
            {
               break;
            }

            if (this.setter == null)
            {
               if (sMethod.startsWith("set") &&
                  sMethod.length() == (sCapName.length() + 3) &&
                  sMethod.regionMatches(3, sCapName, 0, sCapName.length()))
               {
                  Class[] argTypes = method.getParameterTypes();

                  if (argTypes.length == 1)
                  {
                     Class type = argTypes[0];

                     if (type.isPrimitive())
                     {
                        if (type == Integer.TYPE)
                        {
                           type = Integer.class;
                        }
                        else if (type == Boolean.TYPE)
                        {
                           type = Boolean.class;
                        }
                        else if (type == Long.TYPE)
                        {
                           type = Long.class;
                        }
                        else if (type == Byte.TYPE)
                        {
                           type = Byte.class;
                        }
                        else if (type == Short.TYPE)
                        {
                           type = Short.class;
                        }
                        else if (type == Double.TYPE)
                        {
                           type = Double.class;
                        }
                        else if (type == Float.TYPE)
                        {
                           type = Float.class;
                        }
                        else if (type == Character.TYPE)
                        {
                           type = Character.class;
                        }
                     }

                     this.type = type;
                     this.setter = method;
                  }

                  continue;
               }
            }
         }

         if (this.getter == null && this.setter == null)
         {
            throw new IllegalArgumentException("Unknown property \"" + sName + "\"");
         }
      }
   }
}
