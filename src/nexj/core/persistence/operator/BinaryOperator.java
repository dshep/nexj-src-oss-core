// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.operator;

import java.io.IOException;

import nexj.core.meta.Primitive;
import nexj.core.meta.Type;
import nexj.core.meta.TypeMismatchException;
import nexj.core.persistence.Operator;
import nexj.core.persistence.Query;
import nexj.core.scripting.Pair;
import nexj.core.util.PrintWriter;

/**
 * Binary operator expression node.
 */
public abstract class BinaryOperator extends Operator
{
   // associations

   /**
    * The left operand.
    */
   protected Operator m_left;

   /**
    * The right operand.
    */
   protected Operator m_right;

   // operations

   /**
    * Sets the left operand.
    * @param left The left operand to set. Can be null.
    * @return False if the operand is null.
    */
   public boolean setLeft(Operator left)
   {
      m_left = left;

      if (left == null)
      {
         return false;
      }

      left.setParent(this);

      return true;
   }

   /**
    * @return The left operand.
    */
   public Operator getLeft()
   {
      return m_left;
   }

   /**
    * Sets the right operand.
    * @param right The right operand to set. Can be null.
    * @return False if the operand is null.
    */
   public boolean setRight(Operator right)
   {
      m_right = right;

      if (right == null)
      {
         return false;
      }

      right.setParent(this);

      return true;
   }

   /**
    * @return The right operand.
    */
   public Operator getRight()
   {
      return m_right;
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

      if (m_left != null)
      {
         if (visitor.isEligible(m_left))
         {
            if (!m_left.visit(visitor, nFlags))
            {
               return false;
            }
         }
      }

      if (m_right != null)
      {
         if (visitor.isEligible(m_right))
         {
            if (!m_right.visit(visitor, nFlags))
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
    * @see nexj.core.persistence.Operator#normalize(int)
    */
   public Operator normalize(int nFlags)
   {
      if ((nFlags & NORMALIZE_NORECUR) == 0)
      {
         m_left = m_left.normalize(nFlags);
         m_right = m_right.normalize(nFlags);
      }

      Type leftType = m_left.getType();
      Type rightType = m_right.getType();

      if (leftType == null || rightType == null)
      {
         m_type = null;
         setConstantValue(null);
      }
      else if (leftType.isPrimitive())
      {
         if (rightType.isPrimitive())
         {
            setSource(findCommonSource(m_left, m_right), nFlags);

            ConversionMapper mapper = null;
            BinaryDescriptor descriptor = null;
            Primitive resultType = null;

            if (m_source != null)
            {
               mapper = m_source.getAdapter().getConversionMapper();
               descriptor = mapper.getBinaryDescriptor(this);
            }

            if (descriptor == null)
            {
               m_source = null;
               mapper = getConversionMapper();
               descriptor = mapper.getBinaryDescriptor(this);

               if (descriptor == null)
               {
                  throw new TypeMismatchException(getSymbol());
               }
            }
            else
            {
               resultType = getConversionMapper().getBinaryDescriptor(this).getResultType();
            }

            if (mapper.getType((Primitive)leftType) != mapper.getType(descriptor.getLeftType()))
            {
               setLeft(new TypeConversionOperator(descriptor.getLeftType(), m_left));
               m_left = m_left.normalize(nFlags | NORMALIZE_NORECUR);
            }

            if (mapper.getType((Primitive)rightType) != mapper.getType(descriptor.getRightType()))
            {
               setRight(new TypeConversionOperator(descriptor.getRightType(), m_right));
               m_right = m_right.normalize(nFlags | NORMALIZE_NORECUR);
            }

            m_type = descriptor.getResultType();

            if (m_left.isConstant() && m_right.isConstant())
            {
               setConstantValue(evaluate());
            }

            if (m_source != null && mapper.getType((Primitive)m_type) != mapper.getType(resultType))
            {
               TypeConversionOperator result = new TypeConversionOperator(resultType, this);

               result.setParent(m_parent);
               m_parent = result;
               result.normalize(nFlags | NORMALIZE_NORECUR);

               return result;
            }
         }
         else
         {
            normalizeRightMetaclass(nFlags);
         }
      }
      else
      {
         return normalizeLeftMetaclass(nFlags);
      }

      return this;
   }

   /**
    * Normalizes the left operand with class type.
    * @see nexj.core.persistence.Operator#normalize(int)
    */
   protected Operator normalizeLeftMetaclass(int nFlags)
   {
      throw new TypeMismatchException(getSymbol());
   }

   /**
    * Normalizes the right operand with class type.
    * The left one if primitive.
    * @see nexj.core.persistence.Operator#normalize(int)
    */
   protected Operator normalizeRightMetaclass(int nFlags)
   {
      throw new TypeMismatchException(getSymbol());
   }

   /**
    * @see nexj.core.persistence.Operator#copy(nexj.core.persistence.Operator)
    */
   public void copy(Operator src)
   {
      super.copy(src);

      BinaryOperator op = (BinaryOperator)src;

      m_left = op.m_left;

      if (m_left != null)
      {
         m_left.setParent(this);
      }

      m_right = op.m_right;

      if (m_right != null)
      {
         m_right.setParent(this);
      }
   }

   /**
    * @see java.lang.Object#clone()
    */
   public Object clone()
   {
      BinaryOperator op = (BinaryOperator)super.clone();

      if (m_left != null)
      {
         op.setLeft((Operator)m_left.clone());
      }

      if (m_right != null)
      {
         op.setRight((Operator)m_right.clone());
      }

      return op;
   }

   /**
    * @see nexj.core.persistence.Operator#getExpression(nexj.core.persistence.Query)
    */
   public Object getExpression(Query root)
   {
      return Pair.binary(getSymbol(), m_left.getExpression(root), m_right.getExpression(root));
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

      BinaryOperator op = (BinaryOperator)obj;

      n = m_left.compareTo(op.getLeft());

      if (n != 0)
      {
         return n;
      }

      return m_right.compareTo(op.getRight());
   }

   /**
    * @see nexj.core.util.Printable#printOn(PrintWriter)
    */
   public void printOn(PrintWriter writer) throws IOException
   {
      writer.write('(');
      writer.print(getSymbol());
      writer.write(' ');
      writer.print(m_left);
      writer.write(' ');
      writer.print(m_right);
      writer.write(')');
   }
}
