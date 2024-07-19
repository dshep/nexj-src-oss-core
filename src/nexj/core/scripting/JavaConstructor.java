// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.Comparator;

import nexj.core.meta.Primitive;
import nexj.core.util.ObjUtil;

/**
 * A Java constructor represented as a generic function
 * dispatching on method argument count and argument types.
 */
public final class JavaConstructor implements Function
{
   // constants

   /**
    * The comparator for Java method sorting.
    */
   private final static Comparator COMPARATOR = new Comparator()
   {
      public int compare(Object o1, Object o2)
      {
         Class[] types1 = ((Descriptor)o1).types;
         Class[] types2 = ((Descriptor)o2).types;
         int n;

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
    * 2D constructor array, indexed first by argument count
    * and then by the priority of their argument types.
    */
   private Descriptor[][] m_desc2dArray;

   // constructors

   /**
    * Constructs the Java constructor wrapper.
    * @param desc2dArray The descriptor dispatch array.
    */
   private JavaConstructor(Descriptor[][] desc2dArray)
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
      Object[] argArray = new Object[nArgCount - 1];
      Descriptor desc;

      machine.getArgs(argArray);

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
                  if (arg.getClass() != JavaMethod.getWrapperClass(type))
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
         machine.returnValue(desc.constructor.newInstance(argArray), nArgCount);
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
            return descArray[0].constructor.toString();
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
    * Get the descriptors that this Java constructor refers to. Caller must NEVER
    * modify this data.
    *
    * @return 2D constructor array, indexed first by argument count and then by
    *         the priority of their argument types.
    */
   public Descriptor[][] getDescriptors()
   {
      return m_desc2dArray;
   }

   /**
    * Imports the constructors of a Java class to a global environment.
    * @param clazz The class which methods to import.
    * @param holder The holder where to import the methods.
    */
   public static void importJavaConstructors(Class clazz, JavaMethodHolder holder)
   {
      Descriptor[] descArray = getDescriptors(clazz);

      if (descArray.length != 0)
      {
         holder.addJavaMethod(clazz, Symbol.NEW, create(descArray));
      }
   }

   /**
    * Get the constructor descriptors.
    * @param clazz The class object.
    * @return The descriptors.
    */
   public static Descriptor[] getDescriptors(Class clazz)
   {
      Constructor[] constructors = clazz.getConstructors();
      int nCount = constructors.length;
      Descriptor[] descArray = new Descriptor[nCount];

      for (int i = 0; i < nCount; ++i)
      {
         Constructor constructor = constructors[i];

         descArray[i] = new Descriptor(constructor, constructor.getParameterTypes());
      }

      return descArray;
   }

   /**
    * Creates a Java constructor wrapper.
    * @param descArray The descriptor array. Should be non-empty.
    * Modified (sorted) by this constructor.
    */
   public static JavaConstructor create(Descriptor[] descArray)
   {
      int nDesc = descArray.length - 1;
      Descriptor[][] desc2dArray;

      if (nDesc < 0)
      {
         desc2dArray = new Descriptor[0][];
      }
      else
      {
         Arrays.sort(descArray, COMPARATOR);

         Descriptor desc = descArray[nDesc];
         int nArgCount = desc.types.length;
         int i = nDesc - 1;

         desc2dArray = new Descriptor[nArgCount + 1][];

         do
         {
            int n = nArgCount;

            if (i < 0 || (n = descArray[i].types.length) != nArgCount)
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
         }
         while (i-- >= 0);
      }

      return new JavaConstructor(desc2dArray);
   }

   // inner classes

   /**
    * Java constructor descriptor, consisting of a constructor
    * object and the argument types.
    */
   public final static class Descriptor
   {
      public Constructor constructor;
      public Class[] types;

      public Descriptor(Constructor constructor, Class[] types)
      {
         this.constructor = constructor;
         this.types = types;
      }

      public Descriptor(Class clazz, Class[] types)
      {
         try
         {
            this.constructor = clazz.getConstructor(types);
            this.types = types;
         }
         catch (Throwable t)
         {
            ObjUtil.rethrow(t);
         }
      }
   }
}
