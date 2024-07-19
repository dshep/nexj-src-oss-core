// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.operator;

import java.io.IOException;

import nexj.core.meta.Primitive;
import nexj.core.meta.Type;
import nexj.core.persistence.Operator;
import nexj.core.persistence.Query;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.PrintWriter;

/**
 * Ternary conditional operator: (if cond-expr then-expr else-exp)
 */
public class IfOperator extends Operator
{
   // constants

   /**
    * The operator ordinal number.
    */
   public final static int ORDINAL = 22;

   /**
    * The operator priority.
    */
   public final static int PRIORITY = 8;

   /**
    * The operator symbol.
    */
   public final static Symbol SYMBOL = Symbol.IF;

   // associations

   /**
    * The condition operand.
    */
   protected Operator m_condition;

   /**
    * The true branch.
    */
   protected Operator m_then;

   /**
    * The false branch.
    */
   protected Operator m_else;

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
    * Sets the condition operand.
    * @param condition The condition operand to set.
    * @return False if the operand is null.
    */
   public boolean setCondition(Operator condition)
   {
      m_condition = condition;

      if (condition == null)
      {
         return false;
      }

      condition.setParent(this);

      return true;
   }

   /**
    * @return The condition operand.
    */
   public Operator getCondition()
   {
      return m_condition;
   }

   /**
    * Sets the true branch.
    * @param then The true branch to set.
    * @return False if the operand is null.
    */
   public boolean setThen(Operator then)
   {
      m_then = then;

      if (then == null)
      {
         return false;
      }

      then.setParent(this);

      return true;
   }

   /**
    * @return The true branch.
    */
   public Operator getThen()
   {
      return m_then;
   }

   /**
    * Sets the false branch.
    * @param elseOp The false branch to set.
    * @return False if the operand is null.
    */
   public boolean setElse(Operator elseOp)
   {
      m_else = elseOp;

      if (elseOp == null)
      {
         return false;
      }

      elseOp.setParent(this);

      return true;
   }

   /**
    * @return The false branch.
    */
   public Operator getElse()
   {
      return m_else;
   }

   /**
    * @see nexj.core.persistence.Operator#normalize(int)
    */
   public Operator normalize(int nFlags)
   {
      if ((nFlags & NORMALIZE_NORECUR) == 0)
      {
         m_condition = m_condition.normalize(nFlags);
         m_then = m_then.normalize(nFlags);
         m_else = m_else.normalize(nFlags);
      }

      if (m_condition.isConstant())
      {
         Operator op = (Boolean.FALSE.equals(m_condition.getValue())) ? m_else : m_then;

         op.setParent(m_parent);

         return op;
      }

      Type thenType = m_then.getType();
      Type elseType = m_else.getType();

      m_source = null;

      if (thenType == null)
      {
         m_type = elseType;
      }
      else if (elseType == null)
      {
         m_type = thenType;
      }
      else if (!thenType.isPrimitive() || !elseType.isPrimitive())
      {
         if (thenType.isPrimitive() == elseType.isPrimitive())
         {
            m_type = thenType;

            while (!m_type.isUpcast(elseType))
            {
               m_type = m_type.getBaseType();
            }
         }
         else
         {
            m_type = Primitive.ANY;
         }
      }
      else
      {
         m_source = null;
         addSource(m_condition, addSource(m_then, addSource(m_else, false)));
         setSource(m_source, nFlags);

         ConversionMapper mapper = null;
         BinaryDescriptor descriptor = null;

         if (m_source != null)
         {
            mapper = m_source.getAdapter().getConversionMapper();
            descriptor = mapper.getIfDescriptor(this);
         }

         if (descriptor == null)
         {
            m_source = null;
            mapper = getConversionMapper();
            descriptor = mapper.getIfDescriptor(this);
         }

         if (descriptor == null)
         {
            m_type = Primitive.ANY;
         }
         else
         {
            if (mapper.getType((Primitive)thenType) != mapper.getType(descriptor.getLeftType()))
            {
               setThen(new TypeConversionOperator(descriptor.getLeftType(), m_then));
               m_then = m_then.normalize(nFlags | NORMALIZE_NORECUR);
            }

            if (mapper.getType((Primitive)elseType) != mapper.getType(descriptor.getRightType()))
            {
               setElse(new TypeConversionOperator(descriptor.getRightType(), m_else));
               m_else = m_else.normalize(nFlags | NORMALIZE_NORECUR);
            }

            m_type = descriptor.getResultType();
         }
      }

      // Truth table optimization
      if (m_type == Primitive.BOOLEAN)
      {
         MultiArgOperator op = null;
         NotOperator not = null;
         Operator right = null;

         if (m_then.isConstant())
         {
            if (Boolean.FALSE.equals(m_then.getValue()))
            {
               not = new NotOperator();
               op = new AndOperator();
            }
            else
            {
               op = new OrOperator();
            }

            right = m_else;
         }
         else if (m_else.isConstant())
         {
            if (Boolean.FALSE.equals(m_else.getValue()))
            {
               op = new AndOperator();
            }
            else
            {
               op = new OrOperator();
               not = new NotOperator();
            }

            right = m_then;
         }

         if (op != null)
         {
            op.setParent(m_parent);

            if (not == null)
            {
               op.addOperand(m_condition);
            }
            else
            {
               not.setOperand(m_condition);
               op.addOperand(not.normalize(nFlags | NORMALIZE_NORECUR));
            }

            op.addOperand(right);

            return op.normalize(nFlags | NORMALIZE_NORECUR);
         }
      }

      return this;
   }

   /**
    * @see nexj.core.persistence.Operator#evaluate()
    */
   protected Object evaluate()
   {
      return (Boolean.FALSE.equals(m_condition.getValue())) ? m_else.getValue() : m_then.getValue();
   }

   /**
    * @see nexj.core.persistence.Operator#visit(nexj.core.persistence.Operator.Visitor, int)
    */
   public boolean visit(Visitor visitor, int nFlags)
   {
      if ((nFlags & VISIT_PREORDER) != 0 && !visitor.visit(this))
      {
         return false;
      }

      if (visitor.isEligible(m_condition) && !m_condition.visit(visitor, nFlags))
      {
         return false;
      }

      if (visitor.isEligible(m_then) && !m_then.visit(visitor, nFlags))
      {
         return false;
      }

      if (visitor.isEligible(m_else) && !m_else.visit(visitor, nFlags))
      {
         return false;
      }

      if ((nFlags & VISIT_POSTORDER) != 0 && !visitor.visit(this))
      {
         return false;
      }

      return true;
   }

   /**
    * @see nexj.core.persistence.Operator#getExpression(nexj.core.persistence.Query)
    */
   public Object getExpression(Query root)
   {
      return Pair.list(getSymbol(), m_condition.getExpression(root),
         m_then.getExpression(root), m_else.getExpression(root));
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

      IfOperator op = (IfOperator)obj;

      n = m_condition.compareTo(op.getCondition());

      if (n != 0)
      {
         return n;
      }

      n = m_then.compareTo(op.getThen());

      if (n != 0)
      {
         return n;
      }

      return m_else.compareTo(op.getElse());
   }

   /**
    * @see nexj.core.util.Printable#printOn(nexj.core.util.PrintWriter)
    */
   public void printOn(PrintWriter writer) throws IOException
   {
      writer.write('(');
      writer.print(getSymbol());

      writer.write(' ');
      writer.print(m_condition);

      writer.write(' ');
      writer.print(m_then);

      writer.write(' ');
      writer.print(m_else);

      writer.write(')');
   }
}
