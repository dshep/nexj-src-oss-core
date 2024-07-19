// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.operator;

import nexj.core.meta.BinaryFunction;
import nexj.core.meta.Primitive;
import nexj.core.meta.Type;
import nexj.core.meta.TypeMismatchException;
import nexj.core.persistence.Converter;
import nexj.core.persistence.Field;
import nexj.core.persistence.InvalidQueryException;
import nexj.core.persistence.OID;
import nexj.core.persistence.OIDHolder;
import nexj.core.persistence.Operator;
import nexj.core.persistence.PersistenceAdapter;
import nexj.core.scripting.Symbol;

/**
 * IN_P expression node: (in? x c1 c2 ... cN)
 */
public final class InOperator extends MultiArgOperator
{
   // constants

   /**
    * The operator ordinal number.
    */
   public final static int ORDINAL = 10;

   /**
    * The operator priority.
    */
   public final static int PRIORITY = 4;

   /**
    * The operator symbol.
    */
   public final static Symbol SYMBOL = Symbol.IN_P;

   // constructors

   /**
    * Constructs the operator.
    */
   public InOperator()
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
      assert m_nOperandCount >= 1;

      if ((nFlags & NORMALIZE_NORECUR) == 0)
      {
         for (int i = 0; i < m_nOperandCount; ++i)
         {
            setOperand(i, m_operandArray[i].normalize(nFlags));
         }
      }

      Operator first = m_operandArray[0];
      Type type = first.getType();

      if (type == null || m_nOperandCount <= 1)
      {
         setConstantValue(Boolean.FALSE);

         for (int i = 1; i < m_nOperandCount; ++i)
         {
            Operator op = m_operandArray[i];

            if (!op.isConstant())
            {
               throw new InvalidQueryException("err.persistence.variableInList");
            }

            if (op.getValue() == null)
            {
               m_value = Boolean.TRUE;
               break;
            }
         }

         return this;
      }

      setSource(first.getSource(), nFlags);

      if (type.isPrimitive())
      {
         if (type == Primitive.ANY && first.isConstant())
         {
            foldObjectIn(first);
            return this;
         }

         Primitive firstType = (Primitive)type;
         boolean bNull = false;

         for (int i = 1; i < m_nOperandCount; ++i)
         {
            Operator op = m_operandArray[i];

            if (!op.isConstant() || op.getType() != null && !op.getType().isPrimitive())
            {
               throw new InvalidQueryException("err.persistence.variableInList");
            }

            Primitive opType = (Primitive)op.getType();

            if (firstType.findEQFunction(opType) == null)
            {
               throw new TypeMismatchException(getSymbol());
            }

            op.setValue(firstType.getConverter(opType).invoke(op.getValue()));
            op.setType(firstType);

            if (op.getValue() == null && !first.isConstant())
            {
               bNull = true;
               removeOperand(i--);
            }
         }

         if (first.isConstant())
         {
            setConstantValue(evaluate());
         }
         else
         {
            if (first.getOrdinal() == AttributeOperator.ORDINAL)
            {
               AttributeOperator aop = (AttributeOperator)first;
               Converter converter = aop.getConverter();

               if (converter != null)
               {
                  aop.setType(converter.getSourceType());
                  aop.setNoConversion(true);

                  for (int i = 1; i < m_nOperandCount; ++i)
                  {
                     Operator op = m_operandArray[i];

                     op.setValue(converter.getInverseFunction().invoke(op.getValue()));
                     op.setType(converter.getSourceType());
                  }
               }
            }

            if (bNull)
            {
               if (m_nOperandCount > 1)
               {
                  OrOperator or = new OrOperator();

                  or.setParent(m_parent);

                  EqualsOperator eq = new EqualsOperator();

                  eq.setParent(or);
                  eq.setLeft((Operator)first.clone());
                  eq.setRight(new ConstantOperator(null));

                  or.addOperand(eq.normalize(nFlags | NORMALIZE_NORECUR));
                  or.addOperand(this);

                  return or.normalize(nFlags | NORMALIZE_NORECUR);
               }
               else
               {
                  EqualsOperator eq = new EqualsOperator();

                  eq.setParent(m_parent);
                  eq.setLeft(first);
                  eq.setRight(new ConstantOperator(null));
                  eq.normalize(nFlags | NORMALIZE_NORECUR);

                  return eq;
               }
            }
         }

      }
      else
      {
         assert first.getOrdinal() == AttributeOperator.ORDINAL;

         for (int i = 1; i < m_nOperandCount; ++i)
         {
            Operator op = m_operandArray[i];

            if (op.getType() != null)
            {
               if (op.getType().isPrimitive() &&
                  (op.getType() != Primitive.ANY || !op.isConstant() ||
                   op.getValue() != null && !(op.getValue() instanceof OIDHolder)))
               {
                  throw new TypeMismatchException(getSymbol());
               }
            }
         }

         if (first.isConstant())
         {
            foldObjectIn(first);
            return this;
         }

         if ((nFlags & NORMALIZE_PERSISTENCE) != 0 && m_source != null)
         {
            PersistenceAdapter adapter = m_source.getAdapter();
            Field[] fieldArray = adapter.getFields(m_source);

            if (fieldArray != null)
            {
               int nCount = fieldArray.length;

               assert nCount > 0;

               if (nCount == 1)
               {
                  Field field = fieldArray[0];

                  first.setSource(field);
                  first.setType(field.getType());

                  for (int i = 1; i < m_nOperandCount; ++i)
                  {
                     Operator op = m_operandArray[i];

                     if (op.getValue() == null)
                     {
                        op.setType(null);
                     }
                     else
                     {
                        OID oid = ((OIDHolder)op.getValue()).getOID();

                        if (oid != null)
                        {
                           Object[] valueArray = adapter.getValues(oid, m_source);

                           if (valueArray.length != 1)
                           {
                              throw new TypeMismatchException(getSymbol());
                           }

                           op.setValue(valueArray[0]);
                           op.setType(Primitive.primitiveOf(op.getValue()));
                        }
                        else
                        {
                           removeOperand(i--);
                        }
                     }
                  }

                  return normalize(nFlags | NORMALIZE_NORECUR);
               }
               else
               {
                  OrOperator or = new OrOperator();

                  for (int i = 1; i < m_nOperandCount; ++i)
                  {
                     Operator op = m_operandArray[i];
                     Object[] valueArray = null;

                     if (op.getValue() != null)
                     {
                        OID oid = ((OIDHolder)op.getValue()).getOID();

                        if (oid != null)
                        {
                           valueArray = adapter.getValues(oid, m_source);

                           if (valueArray.length != nCount)
                           {
                              throw new TypeMismatchException(getSymbol());
                           }
                        }
                        else
                        {
                           continue;
                        }
                     }

                     AndOperator and = new AndOperator();

                     for (int k = 0; k < nCount; ++k)
                     {
                        EqualsOperator eq = new EqualsOperator();

                        eq.setLeft(new AttributeOperator(fieldArray[k]));
                        eq.setRight(new ConstantOperator((valueArray == null) ? null : valueArray[k]));
                        and.addOperand(eq);
                     }

                     or.addOperand(and);
                  }

                  if (or.getOperandCount() == 1)
                  {
                     Operator op = or.getOperand(0);

                     op.setParent(m_parent);

                     return op.normalize(nFlags & ~NORMALIZE_NORECUR);
                  }
                  else
                  {
                     or.setParent(m_parent);

                     return or.normalize(nFlags & ~NORMALIZE_NORECUR);
                  }
               }
            }
         }
      }

      return this;
   }

   /**
    * Folds a constant OID in? expression.
    */
   private void foldObjectIn(Operator first)
   {
      Object[] firstValues = null;
      boolean bFirstReference = false;

      setConstantValue(Boolean.FALSE);

      if (first.getValue() != null)
      {
         if (!(first.getValue() instanceof OIDHolder))
         {
            throw new TypeMismatchException(getSymbol());
         }

         OID oid = ((OIDHolder)first.getValue()).getOID();

         if (oid != null)
         {
            firstValues = oid.getValueArray();
         }
         else
         {
            bFirstReference = true;
         }
      }

      for (int i = 1; i < m_nOperandCount; ++i)
      {
         Operator op = m_operandArray[i];
         Object[] opValues = null;
         boolean bReference = false;

         if (op.getValue() != null)
         {
            if (!(op.getValue() instanceof OIDHolder))
            {
               throw new TypeMismatchException(getSymbol());
            }

            OID oid = ((OIDHolder)op.getValue()).getOID();

            if (oid != null)
            {
               opValues = oid.getValueArray();
            }
            else
            {
               bReference = true;
            }
         }

         if (bFirstReference & bReference)
         {
            m_value = Boolean.valueOf(first.getValue() == op.getValue());
         }
         else if (firstValues != null)
         {
            int nCount = firstValues.length;

            if (opValues != null && opValues.length != nCount)
            {
               throw new TypeMismatchException(getSymbol());
            }
            else
            {
               m_value = equal(firstValues, opValues, nCount);
            }
         }
         else
         {
            if (opValues != null)
            {
               m_value = equal(firstValues, opValues, opValues.length);
            }
            else
            {
               m_value = Boolean.TRUE;
            }
         }

         if (((Boolean)m_value).booleanValue())
         {
            break;
         }
      }
   }

   /**
    * Compares value arrays of the same dimension.
    * @param leftValues The left value array.
    * @param rightValues The right value array.
    * @param nCount The number of elements to compare.
    * @return True if the values in the arrays are equal.
    */
   private Object equal(Object[] leftValues, Object[] rightValues, int nCount)
   {
      for (int i = 0; i < nCount; ++i)
      {
         Object left = (leftValues == null) ? null : leftValues[i];
         Object right = (rightValues == null) ? null : rightValues[i];

         if (!((Boolean)Primitive.eq(left, right)).booleanValue())
         {
            return Boolean.FALSE;
         }
      }

      return Boolean.TRUE;
   }

   /**
    * @see nexj.core.persistence.Operator#evaluate()
    */
   protected Object evaluate()
   {
      Operator first = m_operandArray[0];
      Object firstValue = first.getValue();
      Type type = first.getType();

      if (type.isPrimitive())
      {
         Primitive primitiveType = (Primitive)type;
         BinaryFunction f = primitiveType.getEQFunction(primitiveType);

         for (int i = 1; i < m_nOperandCount; ++i)
         {
            if (((Boolean)f.invoke(firstValue, m_operandArray[i].getValue())).booleanValue())
            {
               return Boolean.TRUE;
            }
         }

         return Boolean.FALSE;
      }

      if (firstValue instanceof OIDHolder)
      {
         OID firstOID = ((OIDHolder)firstValue).getOID();

         for (int i = 1; i < m_nOperandCount; ++i)
         {
            Object argValue = m_operandArray[i].getValue();
            OID oid = (argValue instanceof OIDHolder) ? ((OIDHolder)argValue).getOID() : null;

            if (firstOID.compareTo(oid) == 0)
            {
               return Boolean.TRUE;
            }
         }

         return Boolean.FALSE;
      }

      throw new TypeMismatchException(getSymbol());
   }
}
