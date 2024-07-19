// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.operator;

import java.math.BigDecimal;

import nexj.core.meta.Primitive;
import nexj.core.persistence.Operator;
import nexj.core.persistence.Query;
import nexj.core.scripting.Symbol;

/**
 * EQ expression node.
 */
public final class EqualsOperator extends ComparisonOperator
{
   // constants
   
   /**
    * The operator ordinal number.
    */
   public final static int ORDINAL = 16;
   
   /**
    * The operator priority.
    */
   public final static int PRIORITY = 3;
   
   /**
    * The operator symbol.
    */
   public final static Symbol SYMBOL = Symbol.EQ;
   
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
    * @see nexj.core.persistence.Operator#initialize()
    */
   public void initialize()
   {
      super.initialize();

      AttributeOperator aop;

      if (m_left instanceof AttributeOperator && m_right.isConstant())
      {
         aop = (AttributeOperator)m_left;
      }
      else if (m_right instanceof AttributeOperator && m_left.isConstant())
      {
         aop = (AttributeOperator)m_right;
         m_right = m_left;
         m_left = aop;
      }
      else
      {
         aop = null;
      }

      if (aop != null && aop.getSource() instanceof Query)
      {
         Query query = (Query)aop.getSource();

         if (query.getConstraint() == null)
         {
            query.setConstraint(this);
         }
      }
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#isSymmetric()
    */
   protected boolean isSymmetric()
   {
      return true;
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#createSymmetric()
    */
   protected ComparisonOperator createSymmetric()
   {
      return new EqualsOperator();
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#isEquivalence()
    */
   protected boolean isEquivalence()
   {
      return true;
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#isCombinatorDisjunctive()
    */
   protected boolean isCombinatorDisjunctive()
   {
      return false;
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#createSame()
    */
   protected ComparisonOperator createSame()
   {
      return new EqualsOperator();
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#createInverse()
    */
   public ComparisonOperator createInverse()
   {
      return new NotEqualsOperator();
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#createStrict()
    */
   protected ComparisonOperator createStrict()
   {
      return new EqualsOperator();
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#createNullComparison()
    */
   protected Operator createNullComparison()
   {
      return new EqualsOperator();
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#nullPredicate(boolean)
    */
   protected boolean nullPredicate(boolean bNull)
   {
      return bNull;
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#booleanPredicate(boolean)
    */
   protected int booleanPredicate(boolean bRHS)
   {
      return (bRHS) ? BOOL_LHS : BOOL_NOT;
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#compareToPredicate(int)
    */
   protected boolean compareToPredicate(int nSign)
   {
      return nSign == 0;
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#compare(java.lang.Object, java.lang.Object)
    */
   protected Object compare(Object left, Object right)
   {
      return Primitive.eq(left, right);
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#compareStrict(java.lang.Object, java.lang.Object)
    */
   protected Object compareStrict(Object left, Object right)
   {
      return Primitive.eq(left, right);
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#stopValue()
    */
   protected boolean stopValue()
   {
      return false;
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#evaluatePrimitive()
    */
   protected Object evaluatePrimitive()
   {
      return ((Primitive)m_left.getType()).getEQFunction((Primitive)m_right.getType()).invoke(m_left.getValue(), m_right.getValue());
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#isOutOfBounds(int, int)
    */
   protected boolean isOutOfBounds(int nLower, int nUpper)
   {
      if (nLower > 0 || nUpper < 0)
      {
         m_value = Boolean.FALSE;
         return true;
      }
   
      return false;
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#round(java.math.BigDecimal)
    */
   protected long round(BigDecimal value)
   {
      BigDecimal dec = value.setScale(0, BigDecimal.ROUND_DOWN);
      
      if (dec.compareTo(value) != 0)
      {
         setConstantValue(Boolean.FALSE);
      }
      
      return dec.longValue();
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#round(double)
    */
   protected long round(double value)
   {
      double d = Math.floor(value);
      
      if (d != value)
      {
         setConstantValue(Boolean.FALSE);
      }
      
      return (long)d;
   }
}
