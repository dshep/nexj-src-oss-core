// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.operator;

import nexj.core.meta.Primitive;
import nexj.core.meta.TypeMismatchException;
import nexj.core.persistence.Operator;
import nexj.core.scripting.Symbol;

/**
 * AND expression node.
 */
public final class AndOperator extends MultiArgOperator implements Logical
{
   // constants
   
   /**
    * The operator ordinal number.
    */
   public final static int ORDINAL = 18;
   
   /**
    * The operator priority.
    */
   public final static int PRIORITY = 2;
   
   /**
    * The operator symbol.
    */
   public final static Symbol SYMBOL = Symbol.AND;
   
   // constructors
   
   /**
    * Constructs the operator.
    */
   public AndOperator()
   {
      m_type = Primitive.BOOLEAN;
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
         for (int i = 0; i < m_nOperandCount; ++i)
         {
            setOperand(i, m_operandArray[i].normalize(nFlags));
         }
      }

      dissociate();
      setConstantValue(Boolean.TRUE);
      m_source = null;

      boolean bMixed = false;

      for (int i = 0; i < m_nOperandCount; ++i)
      {
         Operator op = m_operandArray[i];

         if (op.getType() == null)
         {
            removeOperand(i--);
         }
         else
         {
            if (op.getType() != Primitive.BOOLEAN)
            {
               throw new TypeMismatchException(getSymbol());
            }

            if (op.isConstant())
            {
               if (Boolean.FALSE.equals(op.getValue()))
               {
                  setConstantValue(Boolean.FALSE);

                  return this;
               }

               removeOperand(i--);
            }
            else
            {
               m_bConstant = false;
               bMixed = addSource(op, bMixed);

               if (op.getOrdinal() == ORDINAL)
               {
                  int nCount = mergeOperand(i);

                  if (nCount != 0)
                  {
                     i += nCount - 1;
                  }
               }
            }
         }
      }

      if (m_nOperandCount == 0)
      {
         setConstantValue(Boolean.TRUE);
      }
      else if (m_nOperandCount == 1)
      {
         Operator op = m_operandArray[0];

         op.setParent(m_parent);

         return op;
      }

      setSource(m_source, nFlags);

      return this;
   }

   /**
    * @see nexj.core.persistence.Operator#evaluate()
    */
   protected Object evaluate()
   {
      for (int i = 0; i < m_nOperandCount; ++i)
      {
         if (Boolean.FALSE.equals(getOperand(i).getValue()))
         {
            return Boolean.FALSE;
         }
      }

      return Boolean.TRUE;
   }

   /**
    * Combines destructively two operators into a conjunction.
    * @param left The left operator. Can be null.
    * @param right The right operator. Can be null.
    * @return The resulting conjunction, or null if both operators are null.
    */
   public static Operator conjunction(Operator left, Operator right)
   {
      if (left == null)
      {
         return right;
      }
      
      if (right == null)
      {
         return left;
      }
      
      if (left.getOrdinal() == AndOperator.ORDINAL)
      {
         if (right.getOrdinal() == AndOperator.ORDINAL)
         {
            AndOperator conj = (AndOperator)left;
            AndOperator op = (AndOperator)right;
            
            for (int i = 0, nCount = op.getOperandCount(); i < nCount; ++i)
            {
               conj.addOperand(op.getOperand(i));
            }
         }
         else
         {
            ((AndOperator)left).addOperand(right);
         }
         
         return left;
      }
      else
      {
         if (right.getOrdinal() == AndOperator.ORDINAL)
         {
            ((AndOperator)right).addOperand(left);
            
            return right;
         }

         AndOperator conj = new AndOperator();
         
         conj.addOperand(left);
         conj.addOperand(right);
         
         return conj;
      }
   }
}
