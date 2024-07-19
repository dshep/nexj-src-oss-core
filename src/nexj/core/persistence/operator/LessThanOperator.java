// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.operator;

import java.math.BigDecimal;

import nexj.core.meta.Primitive;
import nexj.core.persistence.Operator;
import nexj.core.scripting.Symbol;

/**
 * LT expression node.
 */
public final class LessThanOperator extends ComparisonOperator
{
   // constants
   
   /**
    * The operator ordinal number.
    */
   public final static int ORDINAL = 14;
   
   /**
    * The operator priority.
    */
   public final static int PRIORITY = 3;
   
   /**
    * The operator symbol.
    */
   public final static Symbol SYMBOL = Symbol.LT;
   
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
    * @see nexj.core.persistence.operator.ComparisonOperator#isSymmetric()
    */
   protected boolean isSymmetric()
   {
      return false;
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#createSymmetric()
    */
   protected ComparisonOperator createSymmetric()
   {
      return new GreaterThanOperator();
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#isEquivalence()
    */
   protected boolean isEquivalence()
   {
      return false;
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
      return new LessThanOperator();
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#createStrict()
    */
   protected ComparisonOperator createStrict()
   {
      return new LessThanOperator();
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#createInverse()
    */
   public ComparisonOperator createInverse()
   {
      return new GreaterThanOrEqualsOperator();
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#createNullComparison()
    */
   protected Operator createNullComparison()
   {
      return new ConstantOperator(Boolean.FALSE);
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#nullPredicate(boolean)
    */
   protected boolean nullPredicate(boolean bNull)
   {
      return !bNull;
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#booleanPredicate(boolean)
    */
   protected int booleanPredicate(boolean bRHS)
   {
      return (bRHS) ? BOOL_NOT : BOOL_FALSE;
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#compareToPredicate(int)
    */
   protected boolean compareToPredicate(int nSign)
   {
      return nSign < 0;
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#compare(java.lang.Object, java.lang.Object)
    */
   protected Object compare(Object left, Object right)
   {
      return Primitive.lt(left, right);
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#compareStrict(java.lang.Object, java.lang.Object)
    */
   protected Object compareStrict(Object left, Object right)
   {
      return Primitive.lt(left, right);
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#stopValue()
    */
   protected boolean stopValue()
   {
      return true;
   }
   
   /**
    * @see nexj.core.persistence.ComparisonOperator#evaluatePrimitive()
    */
   protected Object evaluatePrimitive()
   {
      return ((Primitive)m_left.getType()).getLTFunction((Primitive)m_right.getType()).invoke(m_left.getValue(), m_right.getValue());
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#isOutOfBounds(int, int)
    */
   protected boolean isOutOfBounds(int nLower, int nUpper)
   {
      if (nLower >= 0)
      {
         m_value = Boolean.FALSE;
         return true;
      }
   
      if (nUpper < 0)
      {
         m_value = Boolean.TRUE;
         return true;
      }
   
      return false;
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#round(java.math.BigDecimal)
    */
   protected long round(BigDecimal value)
   {
      return value.setScale(0, BigDecimal.ROUND_CEILING).longValue();
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#round(double)
    */
   protected long round(double value)
   {
      return (long)Math.ceil(value);
   }
}
