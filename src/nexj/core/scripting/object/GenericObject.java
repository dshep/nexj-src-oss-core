// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting.object;

import nexj.core.meta.Primitive;
import nexj.core.meta.Type;
import nexj.core.meta.Typed;
import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Symbol;
import nexj.core.util.ObjUtil;

/**
 * The generic object implementation.
 */
public abstract class GenericObject implements ObjectOriented, Typed
{
   // constants

   /**
    * Symbol "initialize".
    */
   public final static Symbol INITIALIZE_SYMBOL = Symbol.define("initialize");

   // associations

   /**
    * The class object.
    */
   protected ClassObject m_class;

   // constructors

   /**
    * Constructs the instance.
    * @param classObject The class object.
    */
   protected GenericObject(ClassObject classObject)
   {
      m_class = classObject;
   }

   // operations

   /**
    * @see ObjectOriented#getClassObject()
    */
   public ClassObject getClassObject()
   {
      return m_class;
   }

   /**
    * @see nexj.core.meta.Typed#getType()
    */
   public Type getType()
   {
      return m_class;
   }

   /**
    * @see nexj.core.scripting.object.ObjectOriented#initialize(Machine)
    */
   public void initialize(Machine machine)
   {
      initialize(this, machine);
   }

   /**
    * Initializes the object after creation by running the attribute initializers.
    * @param obj The object to initialize.
    * @param machine The VM for initialization.
    */
   public static void initialize(ObjectOriented obj, Machine machine)
   {
      ClassObject clazz = obj.getClassObject();

      for (int i = 0, n = clazz.getInitializerCount(); i < n; ++i)
      {
         int nOffset = clazz.getInitializerOffset(i);

         if (obj.getValue(nOffset, machine) == null)
         {
            AttributeObject attribute = clazz.resolveAttribute(nOffset);
            Object value = machine.invoke(attribute.getInitializerFunction(), obj, (Object[])null);
            Function setter = attribute.getSetterFunction();

            if (setter != null)
            {
               value = machine.invoke(setter, obj, value, null);
            }

            obj.setValue(nOffset, value, machine);
         }
      }
   }

   /**
    * Invokes an instance method dispatching on the first argument,
    * which must be a symbol, and the total argument count.
    * @see nexj.core.scripting.Function#invoke(int, nexj.core.scripting.Machine)
    */
   public boolean invoke(int nArgCount, Machine machine)
   {
      if (nArgCount == 0)
      {
         throw new InvocationException("err.scripting.minArgCount",
            new Object[]{m_class.getName(),
               Primitive.ONE_INTEGER,
               Primitive.createInteger(nArgCount)});
      }

      Object selector = machine.getArg(0, nArgCount);

      if (selector instanceof Symbol)
      {
         machine.setArg(0, nArgCount, this);

         Function fun = machine.getGlobalEnvironment().findFunction(m_class, (Symbol)selector, nArgCount - 1);

         if (fun != null)
         {
            return fun.invoke(nArgCount, machine);
         }

         fun = m_class.resolveFunction((Symbol)selector, nArgCount - 1);

         if (fun != null)
         {
            return fun.invoke(nArgCount, machine);
         }

         throw new InvocationException("err.scripting.methodLookup",
            new Object[]{selector.toString(), Primitive.createInteger(nArgCount - 1), m_class.getName()});
      }

      if (selector instanceof ClassObject && nArgCount >= 1)
      {
         ClassObject classObject = (ClassObject)selector;

         selector = machine.getArg(1, nArgCount);

         if (selector instanceof Symbol)
         {
            machine.shiftArgs(1, nArgCount--);
            machine.setArg(0, nArgCount, this);

            Function fun = machine.getGlobalEnvironment().findFunction(classObject, (Symbol)selector, -nArgCount);

            if (fun != null)
            {
               return fun.invoke(nArgCount, machine);
            }

            fun = classObject.resolveBaseFunction((Symbol)selector, nArgCount - 1);

            if (fun != null)
            {
               return fun.invoke(nArgCount, machine);
            }

            throw new InvocationException("err.scripting.baseMethodLookup",
               new Object[]{selector.toString(), Primitive.createInteger(nArgCount - 1), classObject.getName()});
         }
      }

      throw new InvocationException("err.scripting.funCall");
   }

   /**
    * @see ObjectOriented#setValue(int, Object, Machine)
    */
   public void setValue(int nOffset, Object value, Machine machine)
   {
      throw new UnsupportedOperationException();
   }

   /**
    * @see ObjectOriented#getValue(int, Machine)
    */
   public Object getValue(int nOffset, Machine machine)
   {
      throw new UnsupportedOperationException();
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      String sName = m_class.getName();

      return ObjUtil.getShortClassName(this) + ':' +
         ((sName == null) ? "" : sName) + '@' + System.identityHashCode(this);
   }

   /**
    * Exposes members from ObjectOriented on a class object.
    * @param classObject The destination class object.
    */
   public static void addMembers(ClassObject classObject)
   {
      classObject.addMethod(":class", 0, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            machine.returnValue(((ObjectOriented)machine.getArg(0, nArgCount)).getClassObject(), nArgCount);

            return false;
         }
      });

      classObject.addMethod(":understands?", 2, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            int nMemberArgCount = ((Number)machine.getArg(2, nArgCount)).intValue();

            machine.returnValue(Boolean.valueOf(nMemberArgCount >= 0 &&
               ((ObjectOriented)machine.getArg(0, nArgCount)).getClassObject()
               .resolveFunction((Symbol)machine.getArg(1, nArgCount), nMemberArgCount) != null),
               nArgCount);

            return false;
         }
      });

      classObject.addMethod(INITIALIZE_SYMBOL, 0, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            ((ObjectOriented)machine.getArg(0, nArgCount)).initialize(machine);
            machine.returnValue(null, nArgCount);

            return false;
         }
      });
   }
}
