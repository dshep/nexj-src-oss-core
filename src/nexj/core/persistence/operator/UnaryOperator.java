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
 * Unary operator expression node.
 */
public abstract class UnaryOperator extends Operator
{
   // associations

   /**
    * The operand.
    */
   protected Operator m_operand;

   // operations

   /**
    * Sets the operand.
    * @param operand The operand to set. Can be null.
    * @return False if the operand is null.
    */
   public boolean setOperand(Operator operand)
   {
      m_operand = operand;

      if (operand == null)
      {
         return false;
      }

      operand.setParent(this);

      return true;
   }

   /**
    * @return The operand.
    */
   public Operator getOperand()
   {
      return m_operand;
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

      if (m_operand != null)
      {
         if (visitor.isEligible(m_operand))
         {
            if (!m_operand.visit(visitor, nFlags))
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
   protected void normalizeOperand(int nFlags)
   {
      if ((nFlags & NORMALIZE_NORECUR) == 0)
      {
         m_operand = m_operand.normalize(nFlags);
      }

      setSource(m_operand.getSource(), nFlags);
   }

   /**
    * @see nexj.core.persistence.Operator#normalize(int)
    */
   public Operator normalize(int nFlags)
   {
      normalizeOperand(nFlags);

      Type type = m_operand.getType();

      if (type == null)
      {
         m_type = null;
         setConstantValue(null);
      }
      else if (type.isPrimitive())
      {
         ConversionMapper mapper = null;
         UnaryDescriptor descriptor = null;
         Primitive resultType = null;

         if (m_source != null)
         {
            mapper = m_source.getAdapter().getConversionMapper();
            descriptor = mapper.getUnaryDescriptor(this);
         }

         if (descriptor == null)
         {
            m_source = null;
            mapper = getConversionMapper();
            descriptor = mapper.getUnaryDescriptor(this);

            if (descriptor == null)
            {
               throw new TypeMismatchException(getSymbol());
            }
         }
         else
         {
            resultType = getConversionMapper().getUnaryDescriptor(this).getResultType();
         }

         if (mapper.getType((Primitive)type) != mapper.getType(descriptor.getOperandType()))
         {
            setOperand(new TypeConversionOperator(descriptor.getOperandType(), m_operand));
            m_operand = m_operand.normalize(nFlags | NORMALIZE_NORECUR);
         }

         m_type = descriptor.getResultType();

         if (m_operand.isConstant())
         {
            foldConstant();
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
         return normalizeMetaclass(nFlags);
      }

      return this;
   }

   /**
    * Normalizes a class attribute.
    * @see nexj.core.persistence.Operator#normalize(int)
    */
   protected Operator normalizeMetaclass(int nFlags)
   {
      throw new TypeMismatchException(getSymbol());
   }

   /**
    * Folds a constant value.
    */
   protected void foldConstant()
   {
      setConstantValue(evaluate());
   }

   /**
    * @see nexj.core.persistence.Operator#getExpression(nexj.core.persistence.Query)
    */
   public Object getExpression(Query root)
   {
      return Pair.list(getSymbol(), m_operand.getExpression(root));
   }

   /**
    * @see nexj.core.persistence.Operator#copy(nexj.core.persistence.Operator)
    */
   public void copy(Operator src)
   {
      super.copy(src);

      UnaryOperator op = (UnaryOperator)src;

      m_operand = op.m_operand;

      if (m_operand != null)
      {
         m_operand.setParent(this);
      }
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

      return m_operand.compareTo(((UnaryOperator)obj).getOperand());
   }

   /**
    * @see java.lang.Object#clone()
    */
   public Object clone()
   {
      UnaryOperator op = (UnaryOperator)super.clone();

      if (m_operand != null)
      {
         op.setOperand((Operator)m_operand.clone());
      }

      return op;
   }

   /**
    * @see nexj.core.util.Printable#printOn(PrintWriter)
    */
   public void printOn(PrintWriter writer) throws IOException
   {
      writer.write('(');
      writer.print(getSymbol());
      writer.write(' ');
      writer.print(m_operand);
      writer.write(')');
   }
}
