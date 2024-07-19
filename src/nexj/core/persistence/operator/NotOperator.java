// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.operator;

import nexj.core.meta.Primitive;
import nexj.core.persistence.Operator;
import nexj.core.scripting.Symbol;

/**
 * NOT expression node.
 */
public final class NotOperator extends UnaryOperator implements Logical
{
   // constants
   
   /**
    * The operator ordinal number.
    */
   public final static int ORDINAL = 5;
   
   /**
    * The operator priority.
    */
   public final static int PRIORITY = 7;
   
   /**
    * The operator symbol.
    */
   public final static Symbol SYMBOL = Symbol.NOT;
   
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
    * @see nexj.core.persistence.operator.UnaryOperator#normalize(int)
    */
   public Operator normalize(int nFlags)
   {
      if (m_operand.getOrdinal() == ORDINAL)
      {
         Operator op = ((NotOperator)m_operand).getOperand();

         if (op instanceof Logical || op.getType() == Primitive.BOOLEAN)
         {
            op.setParent(m_parent);

            if ((nFlags & NORMALIZE_NORECUR) == 0)
            {
               return op.normalize(nFlags);
            }

            return op;
         }
      }
      else if (m_operand instanceof ComparisonOperator)
      {
         ComparisonOperator op = ((ComparisonOperator)m_operand).createInverse();

         if (op != null)
         {
            op.copy(m_operand);
            op.setParent(m_parent);

            return op.normalize(nFlags);
         }
      }

      if ((nFlags & NORMALIZE_NORECUR) == 0)
      {
         m_operand = m_operand.normalize(nFlags);
      }

      if (m_operand.getOrdinal() == ORDINAL)
      {
         Operator op = ((NotOperator)m_operand).getOperand();

         op.setParent(m_parent);

         return op;
      }

      return super.normalize(nFlags | NORMALIZE_NORECUR);
   }

   /**
    * @see nexj.core.persistence.Operator#evaluate()
    */
   protected Object evaluate()
   {
      return ((Primitive)m_type).getNotFunction().invoke(m_operand.getValue());
   }
}
