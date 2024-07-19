// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.operator;

import nexj.core.meta.Primitive;
import nexj.core.meta.Type;
import nexj.core.meta.TypeMismatchException;
import nexj.core.persistence.Operator;
import nexj.core.persistence.Query;
import nexj.core.scripting.Symbol;

/**
 * Lookup aggregate function, i.e. returning a value from a set.
 */
public class LookupAggregateOperator extends AggregateOperator
{
   // constructors

   /**
    * Constructs the operator.
    * @param symbol The function symbol.
    * @param query The quantor query.
    */
   public LookupAggregateOperator(Symbol symbol, Query query)
   {
      super(symbol, null, query);
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

      m_type = type;

      if (op.isConstant())
      {
         setConstantValue(op.getValue());
      }
      else
      {
         m_bConstant = false;

         if (!(type instanceof Primitive))
         {
            throw new TypeMismatchException(getSymbol());
         }
      }

      return this;
   }
}
