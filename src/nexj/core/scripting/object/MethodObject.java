// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting.object;

import nexj.core.meta.Primitive;
import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Symbol;

/**
 * Method implementation.
 */
public class MethodObject extends MemberObject
{
   // constants 

   /**
    * The method class object symbol.
    */
   public final static Symbol CLASS_SYMBOL = Symbol.define("sys:Method"); 

   // attributes

   /**
    * The argument count.
    */
   protected int m_nArgCount;

   /**
    * True if the method supports variable argument count,
    * in which case the argument values are stored as a pair list in the last argument.
    */
   protected boolean m_bVarArg;

   // associations

   /**
    * The implementation function.
    */
   protected Function m_function;

   // constructors

   /**
    * Constructs the method.
    * @see MethodObject#MethodObject(ClassObject, ClassObject, Symbol, int, boolean, Function)
    */
   public MethodObject(ClassObject holder, Symbol symbol, int nArgCount, boolean bVarArg, Function function)
   {
      this(ClassObject.getEnvironment().findClass(CLASS_SYMBOL), holder, symbol, nArgCount, bVarArg, function);
   }

   /**
    * Constructs the method.
    * @param classObject The method class object.
    * @param holder The method holder. Can be null.
    * @param symbol The method symbol.
    * @param nArgCount The argument count.
    * @param bVarArg True if the method supports variable argument count,
    * in which case the argument values are stored as a pair list in the last argument.
    * @param function The implementation function.
    */
   public MethodObject(ClassObject classObject, ClassObject holder, Symbol symbol, int nArgCount, boolean bVarArg, Function function)
   {
      super(classObject, holder, symbol);
      m_nArgCount = nArgCount;
      m_bVarArg = bVarArg;
      m_function = function;
   }

   // operations

   /**
    * @return The argument count.
    */
   public int getArgCount()
   {
      return m_nArgCount;
   }

   /**
    * @return True if the method supports variable argument count.
    */
   public boolean isVarArg()
   {
      return m_bVarArg;
   }

   /**
    * @return The minimum argument count.
    */
   public int getMinArgCount()
   {
      return (m_bVarArg) ? m_nArgCount - 1 : m_nArgCount;
   }

   /**
    * @return The implementation function.
    */
   public Function getFunction()
   {
      return m_function;
   }

   /**
    * Determines if the method is matching a given signature.
    * @param symbol The method symbol.
    * @param nArgCount The argument count.
    * @return True if the method is matching
    */
   public boolean isMatching(Symbol symbol, int nArgCount)
   {
      return m_symbol.equals(symbol) && (m_nArgCount == nArgCount || m_bVarArg && nArgCount >= m_nArgCount - 1);
   }

   /**
    * Exposes members from MethodObject.
    * @param classObject The destination class object.
    */
   public static void addMembers(ClassObject classObject)
   {
      MemberObject.addMembers(classObject);

      classObject.addMethod(":arg-count", 0, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            machine.returnValue(Primitive.createInteger(((MethodObject)machine.getArg(0, nArgCount))
               .getArgCount()), nArgCount);

            return false;
         }
      });

      classObject.addMethod(":var-arg?", 0, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            machine.returnValue(Boolean.valueOf(((MethodObject)machine.getArg(0, nArgCount)).isVarArg()), nArgCount);

            return false;
         }
      });

      classObject.addMethod(":function", 0, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            machine.returnValue(((MethodObject)machine.getArg(0, nArgCount)).getFunction(), nArgCount);

            return false;
         }
      });
   }
}
