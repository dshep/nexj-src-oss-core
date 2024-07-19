// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.operator;

import java.math.BigDecimal;

import nexj.core.meta.Primitive;
import nexj.core.persistence.InvalidQueryException;
import nexj.core.persistence.Operator;
import nexj.core.scripting.Symbol;

/**
 * LIKE_P expression node: (like? x c)
 */
public final class LikeOperator extends ComparisonOperator
{
   // constants
   
   /**
    * The operator ordinal number.
    */
   public final static int ORDINAL = 11;
   
   /**
    * The operator priority.
    */
   public final static int PRIORITY = 3;
   
   /**
    * The operator symbol.
    */
   public final static Symbol SYMBOL = Symbol.LIKE_P;
   
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
         m_left = m_left.normalize(nFlags);
         m_right = m_right.normalize(nFlags);
         nFlags |= NORMALIZE_NORECUR;
      }

      if (!m_right.isConstant())
      {
         throw new InvalidQueryException("err.persistence.variableLikePattern");
      }

      return super.normalize(nFlags);
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
      return new LikeOperator();
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
      return new LikeOperator();
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#createInverse()
    */
   public ComparisonOperator createInverse()
   {
      return null;
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#createStrict()
    */
   protected ComparisonOperator createStrict()
   {
      return new LikeOperator();
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
      throw new UnsupportedOperationException(); // evaluate() is overridden
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#compare(java.lang.Object, java.lang.Object)
    */
   protected Object compare(Object left, Object right)
   {
      return Primitive.like(left, right);
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#compareStrict(java.lang.Object, java.lang.Object)
    */
   protected Object compareStrict(Object left, Object right)
   {
      return Primitive.like(left, right);
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#stopValue()
    */
   protected boolean stopValue()
   {
      return false;
   }

   /**
    * @see nexj.core.persistence.ComparisonOperator#evaluatePrimitive()
    */
   protected Object evaluatePrimitive()
   {
      throw new UnsupportedOperationException(); // evaluate() is overridden
   }

   /**
    * @see nexj.core.persistence.Operator#evaluate()
    */
   protected Object evaluate()
   {
      return ((Primitive)m_left.getType()).getLikeFunction((Primitive)m_right.getType()).invoke(m_left.getValue(), m_right.getValue());
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#isOutOfBounds(int, int)
    */
   protected boolean isOutOfBounds(int nLower, int nUpper)
   {
      throw new IllegalStateException("LikeOperator.isOutOfBounds()");
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#round(java.math.BigDecimal)
    */
   protected long round(BigDecimal value)
   {
      throw new IllegalStateException("LikeOperator.round()");
   }

   /**
    * @see nexj.core.persistence.operator.ComparisonOperator#round(double)
    */
   protected long round(double value)
   {
      throw new IllegalStateException("LikeOperator.round()");
   }
}
