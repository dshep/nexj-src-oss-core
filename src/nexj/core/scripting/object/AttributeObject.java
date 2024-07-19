// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting.object;

import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Symbol;

/**
 * Attribute implementation.
 */
public class AttributeObject extends MemberObject
{
   // constants 

   /**
    * The attribute class object symbol.
    */
   public final static Symbol CLASS_SYMBOL = Symbol.define("sys:Attribute"); 

   // associations

   /**
    * The initializer function.
    */
   protected Function m_initializerFunction;

   /**
    * The setter function.
    */
   protected Function m_setterFunction;

   // constructors

   /**
    * Constructs the attribute.
    * @param holder The holder class. Can be null.
    * @param symbol The attribute symbol.
    */
   public AttributeObject(ClassObject holder, Symbol symbol)
   {
      super(ClassObject.getEnvironment().findClass(CLASS_SYMBOL), holder, symbol);
   }

   /**
    * Constructs the attribute.
    * @param classObject The attribute class object. This is not the holder.
    * @param holder The holder class. Can be null.
    * @param symbol The attribute symbol.
    */
   protected AttributeObject(ClassObject classObject, ClassObject holder, Symbol symbol)
   {
      super(classObject, holder, symbol);
   }

   // operations

   /**
    * Sets the initializer function.
    * @param initializerFunction The initializer function to set.
    */
   public void setInitializerFunction(Function initializerFunction)
   {
      change();
      m_initializerFunction = initializerFunction;
   }

   /**
    * @return The initializer function.
    */
   public Function getInitializerFunction()
   {
      return m_initializerFunction;
   }

   /**
    * Sets the setter function.
    * @param setterFunction The setter function to set.
    */
   public void setSetterFunction(Function setterFunction)
   {
      change();
      m_setterFunction = setterFunction;
   }

   /**
    * @return The setter function.
    */
   public Function getSetterFunction()
   {
      return m_setterFunction;
   }

   /**
    * Exposes members from AttributeObject.
    * @param classObject The destination class object.
    */
   public static void addMembers(ClassObject classObject)
   {
      MemberObject.addMembers(classObject);

      classObject.addMethod(":initializer", 0, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            machine.returnValue(((AttributeObject)machine.getArg(0, nArgCount)).getInitializerFunction(), nArgCount);

            return false;
         }
      });

      classObject.addMethod(":initializer", 1, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            AttributeObject attribute = (AttributeObject)machine.getArg(0, nArgCount);
            Function fun = attribute.getHolder().compile(machine.getArg(1, nArgCount),
               attribute.getName(), ":initializer", machine);

            attribute.setInitializerFunction(fun);
            machine.returnValue(fun, nArgCount);

            return false;
         }
      });

      classObject.addMethod(":setter", 0, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            machine.returnValue(((AttributeObject)machine.getArg(0, nArgCount)).getSetterFunction(), nArgCount);

            return false;
         }
      });

      classObject.addMethod(":setter", 1, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            AttributeObject attribute = (AttributeObject)machine.getArg(0, nArgCount);
            Function fun = attribute.getHolder().compile(machine.getArg(1, nArgCount),
               attribute.getName(), ":setter", machine);

            attribute.setSetterFunction(fun);
            machine.returnValue(fun, nArgCount);

            return false;
         }
      });
   }
}
