// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/**
 * Provides access to Java class properties via getter, and setter methods.
 */
public class ClassProperty
{
   /**
    * Synchronized cache of Java methods.
    */
   private static Lookup2D s_cache = new HashTab2D(64); // of type Pair[Class, String]
   
   /**
    * Constants for fixed length argument types.
    */
   private final static Class[][] NUM_ARG_TYPE = { new Class[0], new Class[1], new Class[2], new Class[3], new Class[4] };
   
   /**
    * Gets property getter method.
    * @param sPropName Property name.
    * @param claz Class with property.
    * @return Read method name if found. Otherwise an exception is thrown.
    */
   public static Method getReadMethod(String sPropName, Class claz)
   {
      Method method = findReadMethod(sPropName, NUM_ARG_TYPE[0], claz);
      
      if (method == null)
      {
         throw new UncheckedException("err.classproperty.missingReadMethod", new Object[]{sPropName, claz});
      }
      
      return method;
   }
   
   /**
    * Finds property getter method.
    * @param sPropName Property name.
    * @param claz Class with property.
    * @return Read method name, or null if not found.
    */
   public static Method findReadMethod(String sPropName, Class claz)
   {
      return findReadMethod(sPropName, NUM_ARG_TYPE[0], claz);
   }
   
   /**
    * Finds property getter method.
    * @param sPropName Property name.
    * @param argTypes Property argument types. Set to null to ignore matching argument types.
    * Set elements to null to match any type.
    * @param claz Class with property.
    * @return Read method name, or null if not found.
    */
   public static Method findReadMethod(String sPropName, Class[] argTypes, Class claz)
   {
      Method method = null;
      
      synchronized (s_cache)
      {
         Object[] methodList = (Object[]) s_cache.get(claz, sPropName); 
   
         if (methodList != null)
         {
            if (matchArgumentTypes((Method) methodList[0], argTypes))
            {
               method = (Method) methodList[0];
            }
         }
         
         if (method == null)
         {
            Method[] methods = (Method[]) s_cache.get(claz, "$");
            
            if (methods == null)
            {
               methods = claz.getMethods();
               s_cache.put(claz, "$", methods);
            }
            
            method = findMethod("get", sPropName, argTypes, methods);
            
            if (method == null)
            {
               method = findMethod("is", sPropName, argTypes, methods);
               
               if (method == null)
               {
                  method = findMethod("has", sPropName, argTypes, methods);
               }
            }
            
            if (methodList == null)
            {
               methodList = new Object[] {method, null};
               s_cache.put(claz, sPropName, methodList);
            }
            else
            {
               methodList[0] = method;
            }
         }
      }
      
      return method;
   }
   
   /**
    * Gets property setter method.
    * @param sPropName Property name.
    * @param claz Class with property.
    * @param argTypes Property argument types. Set to null to ignore matching argument types.
    * Set elements to null to match any type.
    * @return Property setter method if found. Otherwise an exception is thrown.
    */
   public static synchronized Method getWriteMethod(String sPropName, Class claz, Class[] argTypes)
   {
      Method method = null;
      
      synchronized (s_cache)
      {
         Object[] methodList = (Object[]) s_cache.get(claz, sPropName); 
   
         if (methodList != null)
         {
            if (matchArgumentTypes((Method) methodList[1], argTypes))
            {
               method = (Method) methodList[1];
            }
         }
         
         if (method == null)
         {
            Method[] methods = (Method[]) s_cache.get(claz, "$");
            
            if (methods == null)
            {
               methods = claz.getMethods();
               s_cache.put(claz, "$", methods);
            }
            
            method = findMethod("set", sPropName, argTypes, methods);
            
            if (methodList == null)
            {
               methodList = new Object[] {null, method};
               s_cache.put(claz, sPropName, methodList);
            }
            else
            {
               methodList[1] = method;
            }
         }
         
         if (method == null)
         {
            throw new UncheckedException("err.classproperty.missingWriteMethod", new Object[]{sPropName, claz});
         }
      }
      
      return method;
   }
   
   /**
    * Check the argument types of a method.
    * @param method The method to perform check against.
    * @param argTypes Property argument types. Set to null to ignore matching argument types.
    * Set elements to null to match any type.
    * @return True if the method arguments are of the specified types or argTypes is null. False otherwise
    * and if method is null.
    */
   public static boolean matchArgumentTypes(Method method, Class[] argTypes)
   {
      if (argTypes == null)
      {
         return true;
      }
      
      if (method == null)
      {
         return false;
      }

      Class[] paramTypes = method.getParameterTypes();
      int nArgs = argTypes.length;

      if (paramTypes.length == nArgs)
      {
         boolean bMatch = true;

         for (int j = 0; j < nArgs; j++)
         {
            if (argTypes[j] != null && paramTypes[j] != argTypes[j])
            {
               bMatch = false;
               break;
            }
         }
         
         return bMatch;
      }
      
      return false;
   }
   
   /**
    * Matches a method from an array of methods.
    * @param sPrefix Method prefix, such as "get", set", etc.
    * @param sPropName Property name.
    * @param methods Array of class methods.
    * @param argTypes Property argument types. Set to null to ignore matching argument types.
    * Set elements to null to match any type.
    * @return Method which starts with sPrefix and ends with sPropName, or null if not found.
    */
   public static Method findMethod(String sPrefix, String sPropName, Class[] argTypes, Method[] methods)
   {
      StringBuilder sb = new StringBuilder(sPrefix);
      int l = sPropName.length();
      
      if (l > 0)
      {
         sb.append(Character.toUpperCase(sPropName.charAt(0)));
         
         if (l > 1)
         {
            sb.append(sPropName.substring(1));
         }
      }
      
      String sMethodName = sb.toString();
      
      for (int i = 0, c = methods.length; i < c; i++)
      {
         if (sMethodName.equals(methods[i].getName()))
         {
            if (matchArgumentTypes(methods[i], argTypes))
            {
               return methods[i];
            }
         }
      }
      
      return null;
   }
   
   /**
    * Gets property type.
    * @param sPropName Property name.
    * @param claz Class with property.
    * @return Property type.
    */
   public static Class getPropertyType(String sPropName, Class claz)
   {
      return getReadMethod(sPropName, claz).getReturnType();
   }
   
   /**
    * Gets property value.
    * @param sPropName Property name.
    * @param obj Class instance.
    * @return Property value.
    */
   public static Object getPropertyValue(String sPropName, Object obj)
   {
      try
      {
         return getReadMethod(sPropName, obj.getClass()).invoke(obj, null);
      }
      catch (Exception e)
      {
         Throwable t = e;

         if (t instanceof InvocationTargetException)
         {
            t = ((InvocationTargetException)e).getCause();
         }

         if (t instanceof RuntimeException)
         {
            throw ((RuntimeException)t);
         }

         throw new UncheckedException("err.classproperty.invocationexception",
            new Object[]{ObjUtil.getMessage(t)}, t);
      }
   }
   
   /**
    * Gets property value. If property does not exists returns null.
    * @param sPropName Property name.
    * @param obj Class instance.
    * @return Property value, or null if property does not exist.
    */
   public static Object findPropertyValue(String sPropName, Object obj)
   {
      try
      {
         Method method = findReadMethod(sPropName, obj.getClass());
         
         if (method != null)
         {
            return method.invoke(obj, null);
         }
         
         return null;
      }
      catch (Exception e)
      {
         Throwable t = e;

         if (t instanceof InvocationTargetException)
         {
            t = ((InvocationTargetException)e).getCause();
         }

         if (t instanceof RuntimeException)
         {
            throw ((RuntimeException)t);
         }

         throw new UncheckedException("err.classproperty.invocationexception",
            new Object[]{ObjUtil.getMessage(t)}, t);
      }
   }
   
   /**
    * Gets parameterized property value. If property does not exists returns null.
    * @param sPropName Property name.
    * @param args Property arguments.
    * @param obj Class instance.
    * @return Property value, or null if property does not exist.
    */
   public static Object findPropertyValue(String sPropName, Object[] args, Object obj)
   {
      if (args.length < NUM_ARG_TYPE.length)
      {
         findPropertyValue(sPropName, args, NUM_ARG_TYPE[args.length], obj);
      }

      return findPropertyValue(sPropName, args, new Class[args.length], obj);
   }

   /**
    * Gets parametrized property value. If property does not exists returns null.
    * @param sPropName Property name.
    * @param args Property arguments.
    * @param argTypes Property argument types. Set to null to ignore matching argument types.
    * @param obj Class instance.
    * @return Property value, or null if property does not exist.
    */
   public static Object findPropertyValue(String sPropName, Object[] args, Class[] argTypes, Object obj)
   {
      try
      {
         Method method = findReadMethod(sPropName, argTypes, obj.getClass());
         
         if (method != null)
         {
            return method.invoke(obj, args);
         }
         
         return null;
      }
      catch (Exception e)
      {
         Throwable t = e;

         if (t instanceof InvocationTargetException)
         {
            t = ((InvocationTargetException)e).getCause();
         }

         if (t instanceof RuntimeException)
         {
            throw ((RuntimeException)t);
         }

         throw new UncheckedException("err.classproperty.invocationexception",
            new Object[]{ObjUtil.getMessage(t)}, t);
      }
   }
   
   /**
    * Sets property value.
    * @param sPropName Property name.
    * @param value Property value.
    * @param obj Class instance.
    */
   public static void setPropertyValue(String sPropName, Object value, Object obj)
   {
      try
      {
         getWriteMethod(sPropName, obj.getClass(), NUM_ARG_TYPE[1]).invoke(obj, new Object[]{value});
         return;
      }
      catch (Exception e)
      {
         Throwable t = e;

         if (t instanceof InvocationTargetException)
         {
            t = ((InvocationTargetException)e).getCause();
         }

         if (t instanceof RuntimeException)
         {
            throw ((RuntimeException)t);
         }

         throw new UncheckedException("err.classproperty.invocationexception",
            new Object[]{ObjUtil.getMessage(t)}, t);
      }
   }
   
   /**
    * Calls a property setter with multiple arguments.
    * @param sPropName Property name.
    * @param values Argument array.
    * @param obj Class instance.
    */
   public static void setPropertyValue(String sPropName, Object[] values, Object obj)
   {
      try
      {
         if (values.length < NUM_ARG_TYPE.length)
         {
            getWriteMethod(sPropName, obj.getClass(), NUM_ARG_TYPE[values.length]).invoke(obj, values);
         }
         
         getWriteMethod(sPropName, obj.getClass(), new Class[values.length]).invoke(obj, values);
         
         return;
      }
      catch (Exception e)
      {
         Throwable t = e;

         if (t instanceof InvocationTargetException)
         {
            t = ((InvocationTargetException)e).getCause();
         }

         if (t instanceof RuntimeException)
         {
            throw ((RuntimeException)t);
         }

         throw new UncheckedException("err.classproperty.invocationexception",
            new Object[]{ObjUtil.getMessage(t)}, t);
      }
   }
   
   /**
    * Clears the method cache.
    */
   public static void cleanup()
   {
      synchronized (s_cache)
      {
         s_cache.clear();
      }
   }
}
