// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.operator;

import java.math.BigDecimal;

import nexj.core.meta.Primitive;
import nexj.core.meta.Type;
import nexj.core.meta.TypeMismatchException;
import nexj.core.persistence.Converter;
import nexj.core.persistence.Field;
import nexj.core.persistence.OID;
import nexj.core.persistence.OIDHolder;
import nexj.core.persistence.Operator;
import nexj.core.persistence.PersistenceAdapter;
import nexj.core.persistence.Query;

/**
 * Comparison expression node.
 */
public abstract class ComparisonOperator extends BinaryOperator
{
   // constants

   /**
    * The boolean comparison is equivalent to the LHS.
    */
   protected final static int BOOL_LHS = 0;

   /**
    * The boolean comparison is equivalent to the negated LHS.
    */
   protected final static int BOOL_NOT = 1;

   /**
    * The boolean comparison is equivalent to TRUE.
    */
   protected final static int BOOL_TRUE = 2;

   /**
    * The boolean comparison is equivalent to FALSE.
    */
   protected final static int BOOL_FALSE = 3;

   /**
    * Constant limits
    */
   private final static Long LONG_MIN_INTEGER = new Long(Integer.MIN_VALUE);
   private final static Long LONG_MAX_INTEGER = new Long(Integer.MAX_VALUE);
   private final static BigDecimal DECIMAL_MIN_INTEGER = BigDecimal.valueOf(Integer.MIN_VALUE);
   private final static BigDecimal DECIMAL_MAX_INTEGER = BigDecimal.valueOf(Integer.MAX_VALUE);
   private final static BigDecimal DECIMAL_MIN_LONG = BigDecimal.valueOf(Long.MIN_VALUE);
   private final static BigDecimal DECIMAL_MAX_LONG = BigDecimal.valueOf(Long.MAX_VALUE);
   private final static Float FLOAT_MIN_INTEGER = new Float(Integer.MIN_VALUE);
   private final static Float FLOAT_MAX_INTEGER = new Float(Integer.MAX_VALUE);
   private final static Float FLOAT_MIN_LONG = new Float(Long.MIN_VALUE);
   private final static Float FLOAT_MAX_LONG = new Float(Long.MAX_VALUE);
   private final static Double DOUBLE_MIN_INTEGER = new Double(Integer.MIN_VALUE);
   private final static Double DOUBLE_MAX_INTEGER = new Double(Integer.MAX_VALUE);
   private final static Double DOUBLE_MIN_LONG = new Double(Long.MIN_VALUE);
   private final static Double DOUBLE_MAX_LONG = new Double(Long.MAX_VALUE);
   private final static Double DOUBLE_MIN_FLOAT = new Double(-Float.MAX_VALUE);
   private final static Double DOUBLE_MAX_FLOAT = new Double(Float.MAX_VALUE);

   // constructors

   /**
    * Constructs the operator.
    */
   protected ComparisonOperator()
   {
      m_type = Primitive.BOOLEAN;
   }

   // operations

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

      if (m_left.isConstant() && !m_right.isConstant())
      {
         if (isSymmetric())
         {
            Operator left = m_left;
            m_left = m_right;
            m_right = left;
         }
         else
         {
            ComparisonOperator op = createSymmetric();

            op.setParent(m_parent);
            op.setLeft(m_right);
            op.setRight(m_left);

            return op.normalize(nFlags | NORMALIZE_NORECUR);
         }
      }

      Type leftType = m_left.getType();
      Type rightType = m_right.getType();

      if (leftType == null)
      {
         assert m_left.isConstant();
         assert m_right.isConstant();

         setConstantValue(Boolean.valueOf(nullPredicate(m_right.getValue() == null)));

         return this;
      }

      if (rightType == null)
      {
         rightType = leftType;
         m_right.setType(rightType);
      }

      if (!isEquivalence())
      {
         if (rightType == null || m_right.isConstant() && m_right.getValue() == null)
         {
            Operator op = createNullComparison();

            op.setParent(m_parent);

            if (op.getOrdinal() != ConstantOperator.ORDINAL)
            {
               ComparisonOperator cmpOp = (ComparisonOperator)op;

               cmpOp.setLeft(m_left);
               cmpOp.setRight(m_right);
            }

            return op.normalize(nFlags | NORMALIZE_NORECUR);
         }
      }

      setSource(findCommonSource(m_left, m_right), nFlags);

      if (leftType.isPrimitive())
      {
         if (rightType != null)
         {
            if (!rightType.isPrimitive())
            {
               throw new TypeMismatchException(getSymbol());
            }
         }

         if (leftType == Primitive.ANY && m_left.isConstant() && m_right.isConstant())
         {
            foldObjectComparison();
            return this;
         }

         ConversionMapper mapper = null;
         BinaryDescriptor descriptor = null;

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

         Primitive rightPrimitive = (Primitive)rightType;

         if (mapper.getType(rightPrimitive) != mapper.getType(descriptor.getRightType()))
         {
            setRight(new TypeConversionOperator(descriptor.getRightType(), m_right));
            m_right = m_right.normalize(nFlags | NORMALIZE_NORECUR);
         }

         Primitive leftPrimitive = (Primitive)leftType;

         if (mapper.getType(leftPrimitive) != mapper.getType(descriptor.getLeftType()))
         {
            if (m_right.isConstant() && Primitive.isOrderPreserved(leftPrimitive, descriptor.getLeftType()))
            {
               if (m_right.getValue() != null)
               {
                  m_bConstant = false;

                  if (unconvertConstantComparison(leftPrimitive, rightPrimitive, m_right.getValue()))
                  {
                     if (m_bConstant)
                     {
                        return this;
                     }
                  }
                  else
                  {
                     setLeft(new TypeConversionOperator(descriptor.getLeftType(), m_left));
                     m_left = m_left.normalize(nFlags | NORMALIZE_NORECUR);
                  }
               }
            }
            else
            {
               setLeft(new TypeConversionOperator(descriptor.getLeftType(), m_left));
               m_left = m_left.normalize(nFlags | NORMALIZE_NORECUR);
            }
         }

         if (m_left.isConstant())
         {
            assert m_right.isConstant();

            setConstantValue(evaluate());
         }
         else if (m_right.isConstant())
         {
            if (m_left.getOrdinal() == AttributeOperator.ORDINAL)
            {
               AttributeOperator left = (AttributeOperator)m_left;
               Converter converter = left.getConverter();

               if (converter != null && (isEquivalence() ||
                  Primitive.isOrderPreserved(converter.getSourceType(), converter.getDestinationType())))
               {
                  left.setType(converter.getSourceType());
                  left.setNoConversion(true);
                  m_right.setValue(converter.getInverseFunction().invoke(m_right.getValue()));
                  m_right.setType(converter.getSourceType());
               }
            }
            else if (m_left.getType() == Primitive.BOOLEAN && m_right.getValue() != null)
            {
               switch (booleanPredicate(((Boolean)m_right.getValue()).booleanValue()))
               {
                  case BOOL_LHS:
                     return m_left;

                  case BOOL_NOT:
                     NotOperator op = new NotOperator();

                     op.setOperand(m_left);
                     return op.normalize(nFlags | NORMALIZE_NORECUR);

                  case BOOL_TRUE:
                     setConstantValue(Boolean.TRUE);
                     break;

                  case BOOL_FALSE:
                     setConstantValue(Boolean.FALSE);
                     break;
               }
            }
         }
      }
      else
      {
         assert m_left.getOrdinal() == AttributeOperator.ORDINAL;

         if (rightType != null)
         {
            if (rightType.isPrimitive() &&
               (rightType != Primitive.ANY || !m_right.isConstant() ||
                m_right.getValue() != null && !(m_right.getValue() instanceof OIDHolder)))
            {
               throw new TypeMismatchException(getSymbol());
            }
         }

         if (m_left.isConstant())
         {
            foldObjectComparison();
            return this;
         }

         if ((nFlags & NORMALIZE_PERSISTENCE) != 0 && m_source != null)
         {
            PersistenceAdapter adapter = m_source.getAdapter();
            Field[] leftFieldArray = adapter.getFields(m_left.getSource());

            if (leftFieldArray != null)
            {
               Field[] rightFieldArray = null;
               Object[] valueArray = null;

               if (m_right.isConstant())
               {
                  if (m_right.getValue() != null)
                  {
                     OID oid = ((OIDHolder)m_right.getValue()).getOID();

                     if (oid != null)
                     {
                        valueArray = adapter.getValues(oid, m_left.getSource());

                        if (leftFieldArray.length != valueArray.length)
                        {
                           throw new TypeMismatchException(getSymbol());
                        }
                     }
                     else if (isEquivalence())
                     {
                        setConstantValue(Boolean.valueOf(nullPredicate(false)));

                        return this;
                     }
                  }
               }
               else
               {
                  assert m_right.getOrdinal() == AttributeOperator.ORDINAL;

                  rightFieldArray = adapter.getFields((Query)m_right.getSource());

                  assert rightFieldArray != null;

                  if (leftFieldArray.length != rightFieldArray.length)
                  {
                     throw new TypeMismatchException(getSymbol());
                  }
               }

               assert leftFieldArray.length > 0;

               Operator result = null;

               if (leftFieldArray.length == 1)
               {
                  Field field = leftFieldArray[0];

                  if (m_left.getQuery().getConstraint() != this)
                  {
                     m_left.setSource(field);
                     m_left.setType(field.getType());

                     if (rightFieldArray != null)
                     {
                        field = rightFieldArray[0];
                        m_right.setSource(field);
                        m_right.setType(field.getType());
                     }
                     else
                     {
                        ConstantOperator cons;
                        Object value = (valueArray == null) ? null : valueArray[0];

                        if (m_right.getOrdinal() == ConstantOperator.ORDINAL)
                        {
                           cons = (ConstantOperator)m_right;
                           cons.setType(Primitive.primitiveOf(value));
                           cons.setValue(value);
                        }
                        else
                        {
                           cons = new ConstantOperator(value);
                           setRight(cons);
                        }
                     }

                     result = this;
                  }
                  else
                  {
                     ComparisonOperator cmp = createSame();

                     cmp.setParent(m_parent);
                     cmp.setLeft(new AttributeOperator(field));

                     if (rightFieldArray != null)
                     {
                        cmp.setRight(new AttributeOperator(rightFieldArray[0]));
                     }
                     else
                     {
                        cmp.setRight(new ConstantOperator((valueArray == null) ? null : valueArray[0]));
                     }

                     result = cmp;
                  }
               }
               else if (isEquivalence())
               {
                  MultiArgOperator combinator;

                  if (rightFieldArray == null && valueArray == null)
                  {
                     combinator = new AndOperator();
                  }
                  else
                  {
                     if (isCombinatorDisjunctive())
                     {
                        combinator = new OrOperator();
                     }
                     else
                     {
                        combinator = new AndOperator();
                     }
                  }

                  combinator.setParent(m_parent);

                  for (int i = 0; i < leftFieldArray.length; ++i)
                  {
                     ComparisonOperator cmp = createSame();

                     cmp.setLeft(new AttributeOperator(leftFieldArray[i]));

                     if (rightFieldArray != null)
                     {
                        cmp.setRight(new AttributeOperator(rightFieldArray[i]));
                     }
                     else
                     {
                        cmp.setRight(new ConstantOperator((valueArray == null) ? null : valueArray[i]));
                     }

                     combinator.addOperand(cmp);
                  }

                  result = combinator;
               }
               else
               {
                  OrOperator or = new OrOperator();

                  or.setParent(m_parent);

                  int nCount = leftFieldArray.length - 1;

                  for (int i = 0; i <= nCount; ++i)
                  {
                     MultiArgOperator combinator;

                     if (i == 0)
                     {
                        combinator = or;
                     }
                     else
                     {
                        combinator = new AndOperator();
                        or.addOperand(combinator);
                     }

                     for (int k = 0; k <= i; ++k)
                     {
                        ComparisonOperator cmp;

                        if (k == i)
                        {
                           if (i == nCount)
                           {
                              cmp = createSame();
                           }
                           else
                           {
                              cmp = createStrict();
                           }
                        }
                        else
                        {
                           cmp = new EqualsOperator();
                        }

                        cmp.setLeft(new AttributeOperator(leftFieldArray[k]));

                        if (rightFieldArray != null)
                        {
                           cmp.setRight(new AttributeOperator(rightFieldArray[k]));
                        }
                        else
                        {
                           cmp.setRight(new ConstantOperator((valueArray == null) ? null : valueArray[k]));
                        }

                        combinator.addOperand(cmp);
                     }
                  }

                  result = or;
               }

               return result.normalize(nFlags & ~NORMALIZE_NORECUR);
            }
         }
      }

      return this;
   }

   /**
    * Folds a constant OID comparison.
    */
   private void foldObjectComparison()
   {
      boolean bReference = false;
      Object[] leftValues = null;

      if (m_left.getValue() != null)
      {
         if (!(m_left.getValue() instanceof OIDHolder))
         {
            throw new TypeMismatchException(getSymbol());
         }

         OID oid = ((OIDHolder)m_left.getValue()).getOID();

         if (oid != null)
         {
            leftValues = oid.getValueArray();
         }
         else
         {
            bReference = true;
         }
      }

      Object[] rightValues = null;

      if (m_right.getValue() != null)
      {
         if (!(m_right.getValue() instanceof OIDHolder))
         {
            throw new TypeMismatchException(getSymbol());
         }

         OID oid = ((OIDHolder)m_right.getValue()).getOID();

         if (oid != null)
         {
            rightValues = oid.getValueArray();
         }
         else
         {
            bReference = true;
         }
      }

      if (bReference && isEquivalence())
      {
         m_value = Boolean.valueOf(nullPredicate(m_left.getValue() == m_right.getValue()));
      }
      else if (leftValues != null)
      {
         int nCount = leftValues.length;

         if (rightValues != null && rightValues.length != nCount)
         {
            throw new TypeMismatchException(getSymbol());
         }
         else
         {
            m_value = compare(leftValues, rightValues, nCount);
         }
      }
      else
      {
         if (rightValues != null)
         {
            m_value = compare(leftValues, rightValues, rightValues.length);
         }
         else
         {
            m_value = compare(null, null);
         }
      }

      setConstant(true);
   }

   /**
    * Compares value arrays of the same dimension.
    * @param leftValues The left value array.
    * @param rightValues The right value array.
    * @param nCount The number of elements to compare.
    * @return The comparison result.
    */
   private Object compare(Object[] leftValues, Object[] rightValues, int nCount)
   {
      --nCount;

      for (int i = 0; i < nCount; ++i)
      {
         Object left = (leftValues == null) ? null : leftValues[i];
         Object right = (rightValues == null) ? null : rightValues[i];
         Object value = compareStrict(left, right);

         if (((Boolean)value).booleanValue() == stopValue() ||
            !isEquivalence() && !((Boolean)Primitive.eq(left, right)).booleanValue())
         {
            return value;
         }
      }

      return compare((leftValues == null) ? null : leftValues[nCount],
         (rightValues == null) ? null : rightValues[nCount]);
   }

   /**
    * Attemts to remove a LHS operand type cast in the constant comparison: (Type)left op const.
    * @param fromType The left operand type.
    * @param toType The right operand type, to which the left operand should be converted.
    * @param value The constant value.
    * @return true if the cast has been removed successfully.
    */
   private boolean unconvertConstantComparison(Primitive fromType, Primitive toType, Object value)
   {
      Comparable min = null;
      Comparable max = null;

      switch (fromType.getOrdinal())
      {
         case Primitive.INTEGER_ORDINAL:
            switch (toType.getOrdinal())
            {
               case Primitive.LONG_ORDINAL:
                  min = LONG_MIN_INTEGER;
                  max = LONG_MAX_INTEGER;
                  break;

               case Primitive.DECIMAL_ORDINAL:
                  min = DECIMAL_MIN_INTEGER;
                  max = DECIMAL_MAX_INTEGER;
                  break;

               case Primitive.FLOAT_ORDINAL:
                  min = FLOAT_MIN_INTEGER;
                  max = FLOAT_MAX_INTEGER;
                  break;

               case Primitive.DOUBLE_ORDINAL:
                  min = DOUBLE_MIN_INTEGER;
                  max = DOUBLE_MAX_INTEGER;
                  break;

               default:
                  return false;
            }

            break;

         case Primitive.LONG_ORDINAL:
            switch (toType.getOrdinal())
            {
               case Primitive.DECIMAL_ORDINAL:
                  min = DECIMAL_MIN_LONG;
                  max = DECIMAL_MAX_LONG;
                  break;

               case Primitive.FLOAT_ORDINAL:
                  min = FLOAT_MIN_LONG;
                  max = FLOAT_MAX_LONG;
                  break;

               case Primitive.DOUBLE_ORDINAL:
                  min = DOUBLE_MIN_LONG;
                  max = DOUBLE_MAX_LONG;
                  break;

               default:
                  return false;
            }

            break;

         case Primitive.DECIMAL_ORDINAL:
            break;

         case Primitive.FLOAT_ORDINAL:
            switch (toType.getOrdinal())
            {
               case Primitive.DOUBLE_ORDINAL:
                  min = DOUBLE_MIN_FLOAT;
                  max = DOUBLE_MAX_FLOAT;
                  break;

               default:
                  return false;
            }

            break;

         default:
            return false;
      }

      if (min != null)
      {
         if (isOutOfBounds(min.compareTo(value), max.compareTo(value)))
         {
            setConstant(true);

            return true;
         }
      }

      switch (fromType.getOrdinal())
      {
         case Primitive.INTEGER_ORDINAL:
         case Primitive.LONG_ORDINAL:

            boolean bRound = false;
            long lValue = 0;

            switch (toType.getOrdinal())
            {
               case Primitive.DECIMAL_ORDINAL:
                  lValue = round((BigDecimal)value);
                  bRound = true;
                  break;

               case Primitive.FLOAT_ORDINAL:
                  lValue = round(((Float)value).doubleValue());
                  bRound = true;
                  break;

               case Primitive.DOUBLE_ORDINAL:
                  lValue = round(((Double)value).doubleValue());
                  bRound = true;
                  break;
            }

            if (bRound)
            {
               if (fromType == Primitive.INTEGER)
               {
                  m_right.setValue(Primitive.createInteger((int)lValue));
               }
               else
               {
                  m_right.setValue(Primitive.createLong(lValue));
               }

               m_right.setType(fromType);

               return true;
            }

            break;
      }

      m_right.setType(fromType);
      m_right.setValue(fromType.getConverter(toType).invoke(value));

      return true;
   }

   /**
    * @see nexj.core.persistence.Operator#evaluate()
    */
   protected Object evaluate()
   {
      if (m_left.getType().isPrimitive())
      {
         return evaluatePrimitive();
      }

      Object leftValue = m_left.getValue();

      if (leftValue instanceof OIDHolder)
      {
         Object rightValue = m_right.getValue();
         OID rightOID = (rightValue instanceof OIDHolder) ? ((OIDHolder)rightValue).getOID() : null;

         return Boolean.valueOf(compareToPredicate(((OIDHolder)leftValue).getOID().compareTo(rightOID)));
      }

      if (leftValue == null)
      {
         return Boolean.valueOf(nullPredicate(m_right.getValue() == null));
      }

      throw new TypeMismatchException(getSymbol());
   }

   /**
    * Processes a comparison between the left operand and the right operand.
    * @param nSign The result of m_left.compareTo(m_right).
    * @return True if the predicate is satisfied.
    */
   protected abstract boolean compareToPredicate(int nSign);

   /**
    * Evaluates the operator. Assumes left side is primitive.
    * @return The result.
    */
   protected abstract Object evaluatePrimitive();

   /**
    * @return True if the operator is symmetric.
    */
   protected abstract boolean isSymmetric();

   /**
    * @return A symmetric operator instance.
    */
   protected abstract ComparisonOperator createSymmetric();

   /**
    * @return True if the operator is an equivalence comparison.
    */
   protected abstract boolean isEquivalence();

   /**
    * @return True if the combinator is disjunctive.
    */
   protected abstract boolean isCombinatorDisjunctive();

   /**
    * @return A new instance of the same class.
    */
   protected abstract ComparisonOperator createSame();

   /**
    * @return A new instance of the inverse comparison operator,
    * or null if such does not exist.
    */
   public abstract ComparisonOperator createInverse();

   /**
    * @return A new instance of the strict version of the operator.
    */
   protected abstract ComparisonOperator createStrict();

   /**
    * @return The equivalent operator for a null right operand.
    */
   protected abstract Operator createNullComparison();

   /**
    * Compares null to null or non-null.
    * @param bNull True if the right operand is null.
    * @return True if the predicate is satisfied.
    */
   protected abstract boolean nullPredicate(boolean bNull);

   /**
    * Compares an expression to a RHS boolean constant.
    * @param bRHS The RHS constant.
    * @return The result of the comparison, one of the BOOL_* constants.
    */
   protected abstract int booleanPredicate(boolean bRHS);

   /**
    * Performs the comparison.
    * @param left The left operand.
    * @param right The right operand.
    * @return The comparison result.
    */
   protected abstract Object compare(Object left, Object right);

   /**
    * Performs the strict version of the comparison.
    * @param left The left operand.
    * @param right The right operand.
    * @return The comparison result.
    */
   protected abstract Object compareStrict(Object left, Object right);

   /**
    * @return The comparison stop value.
    */
   protected abstract boolean stopValue();

   /**
    * Determines whether the value is out of comparison bounds and sets the result value.
    * @param nLower Result of the comparison to the lower bound value.
    * @param nUpper Result of the comparison to the upper bound value.
    * @return True if the value is out of bounds.
    */
   protected abstract boolean isOutOfBounds(int nLower, int nUpper);

   /**
    * Rounds the value according to the operator and sets the constant flag and value if necessary.
    * @param value The value to round.
    * @return The rounded value.
    */
   protected abstract long round(BigDecimal value);

   /**
    * Rounds the value according to the operator and sets the constant flag and value if necessary.
    * @param value The value to round.
    * @return The rounded value.
    */
   protected abstract long round(double value);
}
