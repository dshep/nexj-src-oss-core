// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Member;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.Comparator;

import nexj.core.meta.Primitive;
import nexj.core.util.ObjUtil;

/**
 * A Java method represented as a generic function dispatching on
 * method argument count and argument types.
 */
public final class JavaMethod implements Function
{
   // constants

   /**
    * Primitive type wrapper classes.
    */
   private final static Class INTEGER_CLASS = Integer.class;
   private final static Class BOOLEAN_CLASS = Boolean.class;
   private final static Class CHARACTER_CLASS = Character.class;
   private final static Class LONG_CLASS = Long.class;
   private final static Class FLOAT_CLASS = Float.class;
   private final static Class DOUBLE_CLASS = Double.class;
   private final static Class BYTE_CLASS = Byte.class;
   private final static Class SHORT_CLASS = Short.class;
   private final static Class VOID_CLASS = Void.class;

   /**
    * Empty arrays.
    */
   private final static Field[] EMPTY_FIELD_ARRAY = new Field[0];
   private final static Class[] EMPTY_CLASS_ARRAY = new Class[0];

   /**
    * The comparator for Java method sorting.
    */
   private final static Comparator COMPARATOR = new Comparator()
   {
      public int compare(Object o1, Object o2)
      {
         Descriptor desc1 = (Descriptor)o1;
         Descriptor desc2 = (Descriptor)o2;
         int n = desc1.name.compareTo(desc2.name);

         if (n != 0)
         {
            return n;
         }

         Class[] types1 = desc1.types;
         Class[] types2 = desc2.types;

         if ((n = types1.length - types2.length) != 0)
         {
            return n;
         }

         for (int i = 0; i < types1.length; ++i)
         {
            Class type1 = types1[i];
            Class type2 = types2[i];

            if (type1 != type2)
            {
               if (type1.isAssignableFrom(type2))
               {
                  return 1;
               }

               if (type2.isAssignableFrom(type1))
               {
                  return -1;
               }

               if (type1.isPrimitive())
               {
                  if (!type2.isPrimitive())
                  {
                     return -1;
                  }
               }
               else if (type2.isPrimitive())
               {
                  return 1;
               }

               return type1.getName().compareTo(type2.getName());
            }
         }

         return 0;
      }
   };

   // associations

   /**
    * 2D descriptor array, indexed first by argument count
    * and then by the priority of their argument types.
    */
   private Descriptor[][] m_desc2dArray;

   // constructors

   /**
    * Constructs the Java method wrapper.
    * @param desc2dArray The descriptor dispatch array.
    */
   private JavaMethod(Descriptor[][] desc2dArray)
   {
      m_desc2dArray = desc2dArray;
   }

   // operations

   /**
    * @see nexj.core.scripting.Function#invoke(int, nexj.core.scripting.Machine)
    */
   public boolean invoke(int nArgCount, Machine machine)
   {
      if (nArgCount > m_desc2dArray.length)
      {
         throw new ScriptingException("err.scripting.maxArgCount",
            new Object[]{getName(), Primitive.createInteger(m_desc2dArray.length - 1),
               Primitive.createInteger(nArgCount - 1)});
      }

      Descriptor[] descArray = m_desc2dArray[nArgCount - 1];

      if (descArray == null)
      {
         int nCount;

         for (nCount = nArgCount; nCount < m_desc2dArray.length; ++nCount)
         {
            if (m_desc2dArray[nCount] != null)
            {
               break;
            }
         }

         throw new ScriptingException("err.scripting.minArgCount",
            new Object[]{getName(), Primitive.createInteger(nCount),
               Primitive.createInteger(nArgCount - 1)});
      }

      int nCount = descArray.length;
      Object[] argArray;
      Descriptor desc;

      if (nArgCount == 1)
      {
         argArray = null;
      }
      else
      {
         argArray = new Object[nArgCount - 1];
         machine.getArgs(argArray);
      }

      if (nCount == 1)
      {
         desc = descArray[0];
      }
      else
      {
         desc = null;

         for (int i = 0; i < nCount; ++i)
         {
            desc = descArray[i];
            Class[] typeArray = desc.types;

            for (int k = 0; k < nArgCount - 1; ++k)
            {
               Object arg = argArray[k];
               Class type = typeArray[k];

               if (arg == null)
               {
                  if (type.isPrimitive())
                  {
                     desc = null;
                     break;
                  }
               }
               else if (type.isPrimitive())
               {
                  if (arg.getClass() != getWrapperClass(type))
                  {
                     desc = null;
                     break;
                  }
               }
               else if (!type.isAssignableFrom(arg.getClass()))
               {
                  desc = null;
                  break;
               }
            }

            if (desc != null)
            {
               break;
            }
         }

         if (desc == null)
         {
            desc = descArray[0];
         }
      }

      try
      {
         if (desc.member instanceof Method)
         {
            machine.returnValue(((Method)desc.member).invoke(machine.getArg(0, nArgCount), argArray), nArgCount);
         }
         else
         {
            machine.returnValue(((Field)desc.member).get(machine.getArg(0, nArgCount)), nArgCount);
         }
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

         throw new ScriptingException("err.scripting.exception",
            new Object[]{ObjUtil.getMessage(t)}, t);
      }

      return false;
   }

   /**
    * @return The method name.
    */
   protected String getName()
   {
      int nCount = m_desc2dArray.length;

      for (int i = 0; i < nCount; ++i)
      {
         Descriptor[] descArray = m_desc2dArray[i];

         if (descArray != null)
         {
            return descArray[0].member.toString();
         }
      }

      return "unknown method";
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      if (m_desc2dArray == null)
      {
         return super.toString();
      }

      return getName();
   }

   /**
    * Get the descriptors that this Java method refers to. Caller must NEVER
    * modify this data.
    * 
    * @return 2D descriptor array, indexed first by argument count and then by
    *         the priority of their argument types.
    */
   public Descriptor[][] getDescriptors()
   {
      return m_desc2dArray;
   }

   /**
    * Imports the methods of a Java class to a global environment.
    * @param clazz The class which methods to import.
    * @param holder The holder where to import the methods.
    */
   public static void importJavaMethods(Class clazz, JavaMethodHolder holder)
   {
      importJavaMethods(clazz, getDescriptors(clazz), holder);
   }

   /**
    * Gets the Java method descriptors.
    * @param clazz The class which methods to import.
    * @return The descriptors.
    */
   public static Descriptor[] getDescriptors(Class clazz)
   {
      boolean bPublic = Modifier.isPublic(clazz.getModifiers());
      Field[] fieldArray = (bPublic) ? clazz.getFields() : EMPTY_FIELD_ARRAY;
      Method[] methodArray = clazz.getMethods();
      Descriptor[] descArray = new Descriptor[fieldArray.length + methodArray.length];

      for (int i = 0; i < fieldArray.length; ++i)
      {
         Field field = fieldArray[i];

         descArray[i] = new Descriptor(field, field.getName(), EMPTY_CLASS_ARRAY);
      }

      for (int i = 0; i < methodArray.length; ++i)
      {
         Method method = methodArray[i];
         Class[] typeArray = method.getParameterTypes();
         String sName = method.getName();

         if (!bPublic)
         {
            Method method2 = findAccessibleMethod(clazz, sName, typeArray);

            if (method2 != null)
            {
               method = method2;
            }
         }

         descArray[fieldArray.length + i] = new Descriptor(method,
            normalizeMethodName(sName, typeArray.length), typeArray);
      }

      return descArray;
   }

   /**
    * Creates Java method wrappers.
    * @param clazz The class object.
    * @param descArray The descriptor array. Modified (sorted) by this method.
    * @param holder The method wrapper holder. 
    */
   public static void importJavaMethods(Class clazz, Descriptor[] descArray, JavaMethodHolder holder)
   {
      Arrays.sort(descArray, COMPARATOR);

      int nDesc = descArray.length - 1;

      while (nDesc >= 0)
      {
         Descriptor desc = descArray[nDesc];
         int nArgCount = desc.types.length;
         Descriptor[][] desc2dArray = new Descriptor[nArgCount + 1][];
         int i = nDesc - 1;

         for (;;)
         {
            Descriptor desc2 = null;
            int n = nArgCount;
            boolean bNameChanged = i < 0;

            if (!bNameChanged)
            {
               bNameChanged = !(desc2 = descArray[i]).name.equals(desc.name);
            }

            if (bNameChanged || (n = desc2.types.length) != nArgCount)
            {
               Descriptor[] descDispArray = new Descriptor[nDesc - i];

               while (nDesc != i)
               {
                  descDispArray[nDesc - i - 1] = descArray[nDesc];
                  --nDesc;
               }

               desc2dArray[nArgCount] = descDispArray;
               nArgCount = n;
            }

            if (bNameChanged)
            {
               break;
            }

            --i;
         }

         holder.addJavaMethod(clazz, Symbol.define(desc.name), new JavaMethod(desc2dArray));
      }
   }

   /**
    * Finds a method which can be invoked using reflection.
    * @param clazz The class where to look for the method.
    * @param sName The method name.
    * @param types The argument types.
    * @return The found method.
    */
   protected static Method findAccessibleMethod(Class clazz, String sName, Class[] types)
   {
      if (Modifier.isPublic(clazz.getModifiers()))
      {
         Method[] methodArray = clazz.getMethods();

         for (int i = 0; i < methodArray.length; ++i)
         {
            Method method = methodArray[i];

            if (method.getName().equals(sName))
            {
               Class[] paramTypeArray = method.getParameterTypes();

               if (types.length == paramTypeArray.length)
               {
                  int k;

                  for (k = types.length - 1; k >= 0; --k)
                  {
                     if (types[k] != paramTypeArray[k])
                     {
                        break;
                     }
                  }

                  if (k < 0)
                  {
                     return method;
                  }
               }
            }
         }
      }

      Class[] interfaceArray = clazz.getInterfaces();

      for (int i = 0; i < interfaceArray.length; ++i)
      {
         Method method = findAccessibleMethod(interfaceArray[i], sName, types);

         if (method != null)
         {
            return method;
         }
      }

      Class superclass = clazz.getSuperclass();

      if (superclass != null)
      {
         return findAccessibleMethod(superclass, sName, types);
      }

      return null;
   }

   /**
    * Converts the method name according to the Java Beans convention.
    * @param sName The method name to convert.
    * @param nArgCount The method argument count.
    * @return The converted method name.
    */
   public static String normalizeMethodName(String sName, int nArgCount)
   {
      switch (nArgCount)
      {
         case 0:
            if (sName.startsWith("get"))
            {
               if (sName.length() > 3 && Character.isUpperCase(sName.charAt(3)))
               {
                  sName = convertMethodName(sName.substring(3));
               }
            }
            else if (sName.startsWith("is"))
            {
               if (sName.length() > 2 && Character.isUpperCase(sName.charAt(2)))
               {
                  sName = convertMethodName(sName.substring(2));
               }
            }

            break;

         case 1:
            if (sName.startsWith("set"))
            {
               if (sName.length() > 3 && Character.isUpperCase(sName.charAt(3)))
               {
                  sName = convertMethodName(sName.substring(3));
               }
            }

            break;
      }

      return sName;
   }

   /**
    * Converts the method name according to the Java Beans convention.
    * @param sName The method name to convert, with prefixes already removed.
    * @return The converted method name.
    */
   protected static String convertMethodName(String sName)
   {
      StringBuffer buf = new StringBuffer(sName);

      int k = 0;

      do
      {
         buf.setCharAt(k, Character.toLowerCase(buf.charAt(k)));
      }
      while (++k != buf.length() && Character.isUpperCase(buf.charAt(k)) &&
         (k + 1 == buf.length() || !Character.isLowerCase(buf.charAt(k + 1))));

      return buf.toString();
   }

   /**
    * Computes the wrapper class for a primitive type class.
    * @param type The primitive type class.
    * @return The wrapper class object.
    */
   public static Class getWrapperClass(Class type)
   {
      if (type == Integer.TYPE)
      {
         return INTEGER_CLASS;
      }

      if (type == Boolean.TYPE)
      {
         return BOOLEAN_CLASS;
      }

      if (type == Character.TYPE)
      {
         return CHARACTER_CLASS;
      }

      if (type == Long.TYPE)
      {
         return LONG_CLASS;
      }

      if (type == Float.TYPE)
      {
         return FLOAT_CLASS;
      }

      if (type == Double.TYPE)
      {
         return DOUBLE_CLASS;
      }

      if (type == Byte.TYPE)
      {
         return BYTE_CLASS;
      }

      if (type == Short.TYPE)
      {
         return SHORT_CLASS;
      }

      if (type == Void.TYPE)
      {
         return VOID_CLASS;
      }

      return type;
   }

   // inner classes

   /**
    * Java member descriptor, consisting of a member object,
    * a converted member name, and the argument types.
    */
   public final static class Descriptor
   {
      public Member member;
      public String name;
      public Class[] types;

      public Descriptor(Member member, String sName, Class[] typeArray)
      {
         this.member = member;
         this.name = sName;
         this.types = typeArray;
      }

      public Descriptor(Class clazz, String sJavaName, String sName, Class[] typeArray)
      {
         this.member = findAccessibleMethod(clazz, sJavaName, typeArray);
         this.name = sName;
         this.types = typeArray;

         assert this.member != null;
      }

      public Descriptor(Class clazz, String sName, Class[] typeArray)
      {
         this(clazz, sName, sName, typeArray);
      }
   }
}
