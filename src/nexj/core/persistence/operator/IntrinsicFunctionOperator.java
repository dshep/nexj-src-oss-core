// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.operator;

import nexj.core.meta.Primitive;
import nexj.core.meta.Type;
import nexj.core.meta.TypeMismatchException;
import nexj.core.persistence.InvalidQueryException;
import nexj.core.persistence.Operator;
import nexj.core.runtime.ThreadContextHolder;
import nexj.core.scripting.IntrinsicFunction;
import nexj.core.scripting.Symbol;

/**
 * Operator corresponding to an intrinsic function.
 */
public class IntrinsicFunctionOperator extends FunctionOperator
{
   // associations

   /**
    * The function implementation.
    */
   protected IntrinsicFunction m_fun;

   /**
    * The argument type array, indexed by the argument ordinal number.
    */
   protected Primitive[] m_argTypeArray;

   /**
    * Argument value array.
    */
   protected Object[] m_argValueArray;

   // constructors

   /**
    * Creates a function operator.
    * @param fun The function implementation.
    * @param type The function return type.
    * @param argTypeArray The argument type array, indexed by the argument ordinal number.
    */
   public IntrinsicFunctionOperator(IntrinsicFunction fun, Primitive type, Primitive[] argTypeArray)
   {
      assert fun != null;
      assert type != null;
      assert argTypeArray != null;

      m_fun = fun;
      m_type = type;
      m_argTypeArray = argTypeArray;
   }

   // operations

   /**
    * @see nexj.core.persistence.Operator#getSymbol()
    */
   public Symbol getSymbol()
   {
      return m_fun.getSymbol();
   }

   /**
    * Gets the type of an operand with a given ordinal number.
    * @param nOperand The operand ordinal number.
    * @return The operand type.
    */
   public Primitive getOperandType(int nOperand)
   {
      return m_argTypeArray[nOperand];
   }

   /**
    * @see FunctionOperator#verifyOperandCount()
    */
   public void verifyOperandCount() throws InvalidQueryException
   {
      if (m_nOperandCount != m_argTypeArray.length)
      {
         throw new InvalidQueryException("err.persistence.functionArgCount",
            new Object[]{getSymbol().getName(), Primitive.createInteger(m_nOperandCount),
               Primitive.createInteger(m_argTypeArray.length)});
      }
   }

   /**
    * @see nexj.core.persistence.operator.MultiArgOperator#normalize(int)
    */
   public Operator normalize(int nFlags)
   {
      super.normalize(nFlags);

      boolean bConstant = true;

      for (int i = 0; i < m_nOperandCount; ++i)
      {
         Operator op = getOperand(i);
         Type fromType = op.getType();

         if (fromType != null)
         {
            if (!fromType.isPrimitive())
            {
               throw new TypeMismatchException(getSymbol());
            }

            Primitive toType = m_argTypeArray[i];

            if (toType != fromType)
            {
               if (toType.findStrictConverter((Primitive)fromType) == null)
               {
                  throw new TypeMismatchException(getSymbol());
               }

               op = new TypeConversionOperator(toType, op);
               setOperand(i, op);
               op = op.normalize(nFlags | NORMALIZE_NORECUR);
               setOperand(i, op);
            }

            if (!op.isConstant())
            {
               bConstant = false;
            }
         }
      }

      if (bConstant)
      {
         setConstantValue(evaluate());
      }

      return this;
   }

   /**
    * @see nexj.core.persistence.Operator#evaluate()
    */
   protected Object evaluate()
   {
      if (m_argValueArray == null && m_nOperandCount != 0)
      {
         m_argValueArray = new Object[m_nOperandCount];
      }

      for (int i = 0; i < m_nOperandCount; ++i)
      {
         m_argValueArray[i] = m_argTypeArray[i].strictConvert(getOperand(i).getValue());
      }

      return ((Primitive)m_type).strictConvert(ThreadContextHolder.getContext()
         .getMachine().invoke(m_fun, m_argValueArray));
   }

   /**
    * @see nexj.core.persistence.operator.MultiArgOperator#copy(nexj.core.persistence.Operator)
    */
   public void copy(Operator src)
   {
      super.copy(src);

      IntrinsicFunctionOperator op = (IntrinsicFunctionOperator)src;

      m_fun = op.m_fun;
      m_argTypeArray = op.m_argTypeArray;
   }
}
