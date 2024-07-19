// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.operator;

import nexj.core.meta.Primitive;
import nexj.core.meta.Type;
import nexj.core.meta.TypeMismatchException;
import nexj.core.persistence.Field;
import nexj.core.persistence.Operator;
import nexj.core.persistence.Query;
import nexj.core.scripting.Symbol;

/**
 * Aggregate function for counting non-null value occurrences.
 */
public class CountOperator extends AggregateOperator
{
   // constructors

   /**
    * Constructs the operator.
    * @param query The quantor query.
    */
   public CountOperator(Query query)
   {
      super(Symbol.COUNT, Primitive.LONG, query);
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

      if (type == null || op.isConstant() && op.getValue() == null)
      {
         setConstantValue(Primitive.ZERO_LONG);
      }
      else if (!type.isPrimitive() && (nFlags & NORMALIZE_PERSISTENCE) != 0 && m_source != null)
      {
         Field[] fieldArray = m_source.getAdapter().getFields(op.getSource());

         if (fieldArray != null)
         {
            if (m_bUnique && fieldArray.length > 1)
            {
               // TODO: Support multi-part keys
               throw new TypeMismatchException(getSymbol());
            }

            op = new AttributeOperator(fieldArray[0]);
            setOperand(op);
            op = op.normalize(nFlags | NORMALIZE_NORECUR);
            setOperand(op);
         }
      }

      return this;
   }
}
