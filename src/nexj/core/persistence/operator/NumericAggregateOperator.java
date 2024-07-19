// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.operator;

import nexj.core.persistence.Operator;
import nexj.core.persistence.Query;
import nexj.core.scripting.Symbol;

/**
 * Numeric aggregate function. 
 */
public class NumericAggregateOperator extends AggregateOperator
{
   // constructors

   /**
    * Constructs the operator.
    * @param symbol The function symbol.
    * @param query The quantor query.
    */
   public NumericAggregateOperator(Symbol symbol, Query query)
   {
      super(symbol, null, query);
   }

   // operations

   /**
    * @see nexj.core.persistence.operator.UnaryOperator#normalize(int)
    */
   public Operator normalize(int nFlags)
   {
      verifyNesting();

      return super.normalize(nFlags);
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
