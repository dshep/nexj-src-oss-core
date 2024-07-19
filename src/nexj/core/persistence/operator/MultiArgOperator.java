// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.operator;

import java.io.IOException;

import nexj.core.persistence.Operator;
import nexj.core.persistence.Query;
import nexj.core.scripting.Pair;
import nexj.core.util.PrintWriter;

/**
 * Expression node with multiple arguments.
 */
public abstract class MultiArgOperator extends Operator
{
   // attributes
   
   /**
    * Operand count.
    */
   protected int m_nOperandCount = 0;
   
   // associations
   
   /**
    * Operand array.
    */
   protected Operator[] m_operandArray = new Operator[4];

   // operations

   /**
    * Reserve space for more operands.
    * @param nCount The reserved operand count (on top of the current count).
    */
   protected void reserve(int nCount)
   {
      nCount += m_nOperandCount;

      if (nCount > m_operandArray.length)
      {
         Operator[] operandArray = new Operator[Math.max(m_nOperandCount << 1, nCount)];
         System.arraycopy(m_operandArray, 0, operandArray, 0, m_nOperandCount);
         m_operandArray = operandArray;
      }
   }

   /**
    * Adds a new operand to the operator.
    * @param operand The operand to add. Can be null, in which case it is not added.
    * @return The ordinal number of the newly added operand. Negative if not added. 
    */
   public int addOperand(Operator operand)
   {
      if (operand == null)
      {
         return -1;
      }

      operand.setParent(this);

      reserve(1);
      m_operandArray[m_nOperandCount] = operand;

      return ++m_nOperandCount;
   }

   /**
    * Adds a new operand to the operator.
    * @param nOrdinal The operand ordinal number.
    * @param operand The operand to add.
    */
   public void addOperand(int nOrdinal, Operator operand)
   {
      assert nOrdinal >= 0 && nOrdinal <= m_nOperandCount;
      assert operand != null;

      operand.setParent(this);

      reserve(1);

      if (nOrdinal < m_nOperandCount)
      {
         System.arraycopy(m_operandArray, nOrdinal, m_operandArray,
            nOrdinal + 1, m_nOperandCount - nOrdinal);
      }

      m_operandArray[nOrdinal] = operand;
      ++m_nOperandCount;
   }

   /**
    * Replaces an operator with a given ordinal number.
    * @param nOrdinal The operand ordinal number.
    * @param operand The operand to set.
    */
   public void setOperand(int nOrdinal, Operator operand)
   {
      assert nOrdinal >= 0 && nOrdinal <= m_nOperandCount;
      assert operand != null;

      operand.setParent(this);
      m_operandArray[nOrdinal] = operand;
   }

   /**
    * Removes the operand with the specified ordinal number.
    * @param nOrdinal The ordinal number of the operand.
    */
   public void removeOperand(int nOrdinal)
   {
      assert nOrdinal >= 0 && nOrdinal < m_nOperandCount;

      System.arraycopy(m_operandArray, nOrdinal + 1, m_operandArray, nOrdinal, --m_nOperandCount - nOrdinal);
      m_operandArray[m_nOperandCount] = null;
   }

   /**
    * Merges an operand of the same type by replacing it with its operands.
    * @param nOrdinal the operand ordinal number.
    * @return The number of merged operands (0 if no merge has taken place).
    */
   protected int mergeOperand(int nOrdinal)
   {
      Operator op = getOperand(nOrdinal);
      MultiArgOperator mop = (MultiArgOperator)op;
      int nCount = mop.getOperandCount();

      if (nCount != 0)
      {
         if (nCount > 1)
         {
            reserve(nCount - 1);
            System.arraycopy(m_operandArray, nOrdinal + 1, m_operandArray,
               nOrdinal + nCount, m_nOperandCount - nOrdinal - 1);
         }

         for (int i = 0; i < nCount; ++i)
         {
            setOperand(nOrdinal + i, mop.getOperand(i));
         }
      }

      return nCount;
   }

   /**
    * Gets the operand with the specified ordinal number.
    * @param nOrdinal The ordinal number of the operand.
    * @return The operand.
    */
   public Operator getOperand(int nOrdinal)
   {
      assert nOrdinal >= 0 && nOrdinal < m_nOperandCount;

      return m_operandArray[nOrdinal];
   }

   /**
    * @return The operand count.
    */
   public int getOperandCount()
   {
      return m_nOperandCount;
   }

   /**
    * Sets the parent of all the operands.
    */
   public void setOperandParent()
   {
      for (int i = 0; i < m_nOperandCount; ++i)
      {
         m_operandArray[i].setParent(this);
      }
   }

   /**
    * @see nexj.core.persistence.Operator#visit(nexj.core.persistence.Operator.Visitor, int)
    */
   public boolean visit(Visitor visitor, int nFlags)
   {
      if ((nFlags & VISIT_PREORDER) != 0)
      {
         if (!visitor.visit(this))
         {
            return false;
         }
      }

      for (int i = 0; i < m_nOperandCount; ++i)
      {
         Operator op = m_operandArray[i];

         if (visitor.isEligible(op))
         {
            if (!op.visit(visitor, nFlags))
            {
               return false;
            }
         }
      }

      if ((nFlags & VISIT_POSTORDER) != 0)
      {
         if (!visitor.visit(this))
         {
            return false;
         }
      }

      return true;
   }

   /**
    * Removes operator association: (f a b (f c d)) -> (f a b c d)
    */
   protected void dissociate()
   {
      for (int i = 0; i < m_nOperandCount; ++i)
      {
         Operator op = m_operandArray[i];

         if (op.getOrdinal() == getOrdinal())
         {
            MultiArgOperator mop = (MultiArgOperator)op;
            int nCount = mop.getOperandCount();

            if (nCount == 0)
            {
               removeOperand(i);
            }
            else
            {
               reserve(nCount - 1);
               System.arraycopy(m_operandArray, i + 1, m_operandArray,
                  i + nCount, m_nOperandCount - i - 1);

               for (int k = 0; k < nCount; ++k)
               {
                  Operator operand = mop.getOperand(k);

                  operand.setParent(this);
                  m_operandArray[i++] = operand; 
               }

               m_nOperandCount += nCount - 1;
            }

            --i;
         }
      }
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
            m_operandArray[i] = m_operandArray[i].normalize(nFlags);
         }
      }

      m_source = null;

      boolean bMixed = false;

      for (int i = 0; i < m_nOperandCount; ++i)
      {
         bMixed = addSource(m_operandArray[i], bMixed);
      }

      setSource(m_source, nFlags);

      return this;
   }

   /**
    * @see nexj.core.persistence.Operator#copy(nexj.core.persistence.Operator)
    */
   public void copy(Operator src)
   {
      super.copy(src);

      MultiArgOperator op = (MultiArgOperator)src;

      m_nOperandCount = op.m_nOperandCount;
      m_operandArray = new Operator[Math.max(2, m_nOperandCount)];
      System.arraycopy(op.m_operandArray, 0, m_operandArray, 0, m_nOperandCount);

      for (int i = 0; i < m_nOperandCount; ++i)
      {
         m_operandArray[i].setParent(this);
      }
   }

   /**
    * @see java.lang.Object#clone()
    */
   public Object clone()
   {
      MultiArgOperator op = (MultiArgOperator)super.clone();

      if (m_operandArray != null)
      {
         op.m_operandArray = new Operator[m_operandArray.length];
         op.m_nOperandCount = 0;
      }

      for (int i = 0; i < m_nOperandCount; ++i)
      {
         op.addOperand((Operator)m_operandArray[i].clone());
      }

      return op;
   }

   /**
    * @see nexj.core.persistence.Operator#getExpression(nexj.core.persistence.Query)
    */
   public Object getExpression(Query root)
   {
      Pair expression = null;

      for (int i = m_nOperandCount - 1; i >= 0; i--)
      {
         expression = new Pair(m_operandArray[i].getExpression(root),expression);
      }

      return new Pair(getSymbol(), expression);
   }

   /**
    * @see nexj.core.persistence.Operator#compareTo(java.lang.Object)
    */
   public int compareTo(Object obj)
   {
      int n = super.compareTo(obj);

      if (n != 0)
      {
         return n;
      }

      MultiArgOperator op = (MultiArgOperator)obj;

      n = getSymbol().getName().compareTo(op.getSymbol().getName());

      if (n != 0)
      {
         return n;
      }

      n = m_nOperandCount - op.getOperandCount();

      if (n != 0)
      {
         return n;
      }

      for (int i = 0; i < m_nOperandCount; ++i)
      {
         n = getOperand(i).compareTo(op.getOperand(i));

         if (n != 0)
         {
            break;
         }
      }

      return n;
   }

   /**
    * @see nexj.core.util.Printable#printOn(PrintWriter)
    */
   public void printOn(PrintWriter writer) throws IOException
   {
      writer.write('(');
      writer.print(getSymbol());

      for (int i = 0; i < m_nOperandCount; ++i)
      {
         writer.write(' ');
         writer.print(m_operandArray[i]);
      }

      writer.write(')');
   }
}
