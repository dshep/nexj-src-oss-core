// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.operator;

import java.io.IOException;

import nexj.core.meta.Primitive;
import nexj.core.meta.Type;
import nexj.core.meta.UnaryFunction;
import nexj.core.persistence.Operator;
import nexj.core.scripting.Symbol;
import nexj.core.util.PrintWriter;

/**
 * Type conversion expression node.
 */
public final class TypeConversionOperator extends UnaryOperator
{
   // constants
   
   /**
    * The operator ordinal number.
    */
   public final static int ORDINAL = 3;
   
   /**
    * The operator priority.
    */
   public final static int PRIORITY = 7;
   
   /**
    * The operator symbol.
    */
   public final static Symbol SYMBOL = Symbol.CAST;

   // associations
   
   /**
    * The conversion function.
    */
   private UnaryFunction m_function;
   
   // constructors
   
   /**
    * Creates a type conversion operator.
    * @param type The type to which to convert.
    * @param operand The operand to convert.
    */
   public TypeConversionOperator(Type type, Operator operand)
   {
      assert type != null;
      
      m_type = type;
      m_operand = operand;
   }

   // operations
   
   /**
    * @see nexj.core.persistence.Operator#getOrdinal()
    */
   public int getOrdinal()
   {
      return ORDINAL;
   }

   /**
    * @see nexj.core.persistence.Operator#getPriority()
    */
   public int getPriority()
   {
      return PRIORITY;
   }

   /**
    * @see nexj.core.persistence.Operator#getSymbol()
    */
   public Symbol getSymbol()
   {
      return SYMBOL;
   }

   /**
    * @see nexj.core.persistence.Operator#normalize(int)
    */
   public Operator normalize(int nFlags)
   {
      if ((nFlags & NORMALIZE_NORECUR) == 0)
      {
         m_operand = m_operand.normalize(nFlags);
      }

      Type type = m_operand.getType();

      if (type == null)
      {
         setConstantValue(null);
      }
      else if (type.isPrimitive())
      {
         setSource(m_operand.getSource(), nFlags);
         m_function = ((Primitive)m_type).getConverter((Primitive)type);

         if (m_operand.isConstant())
         {
            setConstantValue(evaluate());
         }
      }
      else
      {
         return normalizeMetaclass(nFlags);
      }

      return this;
   }

   /**
    * @see nexj.core.persistence.Operator#evaluate()
    */
   protected Object evaluate()
   {
      return m_function.invoke(m_operand.getValue());
   }

   /**
    * @see nexj.core.persistence.operator.UnaryOperator#copy(nexj.core.persistence.Operator)
    */
   public void copy(Operator src)
   {
      super.copy(src);

      TypeConversionOperator op = (TypeConversionOperator)src;

      m_function = op.m_function;
   }

   /**
    * @see nexj.core.persistence.operator.UnaryOperator#printOn(PrintWriter)
    */
   public void printOn(PrintWriter writer) throws IOException
   {
      writer.write("(cast ");
      writer.write(m_type.getName());
      writer.write(' ');
      writer.print(getOperand());
      writer.write(')');
   }
}
