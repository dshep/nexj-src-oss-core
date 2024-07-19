// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.operator;

import nexj.core.meta.Primitive;
import nexj.core.meta.Type;
import nexj.core.meta.TypeMismatchException;
import nexj.core.persistence.Operator;
import nexj.core.persistence.Query;
import nexj.core.scripting.Symbol;

/**
 * Sum aggregate function.
 */
public class SumOperator extends AggregateOperator
{
   // constructors

   /**
    * Constructs the operator.
    * @param query The quantor query.
    */
   public SumOperator(Query query)
   {
      super(Symbol.SUM, null, query);
   }

   // operations

   /**
    * @see nexj.core.persistence.operator.FunctionOperator#normalize(int)
    */
   public Operator normalize(int nFlags)
   {
      verifyNesting();
      normalizeOperand(nFlags);

      Operator op = getOperand();
      Type type = op.getType();

      if (type == null)
      {
         setConstantValue(null);
         m_type = null;
      }
      else
      {
         if (type instanceof Primitive)
         {
            switch (((Primitive)type).getOrdinal())
            {
               case Primitive.INTEGER_ORDINAL:
               case Primitive.LONG_ORDINAL:
               case Primitive.FLOAT_ORDINAL:
               case Primitive.DOUBLE_ORDINAL:
               case Primitive.DECIMAL_ORDINAL:
                  m_bConstant = false;
                  m_type = type;
                  break;

               default:
                  throw new TypeMismatchException(getSymbol());
            }

            if (op.isConstant())
            {
               foldConstant();
            }
         }
         else
         {
            throw new TypeMismatchException(getSymbol());
         }
      }

      return this;
   }

   /**
    * @see nexj.core.persistence.operator.AggregateOperator#foldConstant()
    */
   protected void foldConstant()
   {
      Operator op = getOperand();

      if (op.isConstant() && op.getValue() == null)
      {
         setConstantValue(null);
      }
   }
}
