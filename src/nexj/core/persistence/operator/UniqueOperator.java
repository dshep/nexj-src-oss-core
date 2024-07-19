package nexj.core.persistence.operator;

import nexj.core.persistence.InvalidQueryException;
import nexj.core.persistence.Operator;
import nexj.core.persistence.Query;
import nexj.core.scripting.Symbol;

/**
 * Unique element aggregate function.
 */
public class UniqueOperator extends AggregateOperator
{
   // constructors

   /**
    * Constructs the operator.
    * @param query The operator query.
    */
   public UniqueOperator(Query query)
   {
      super(Symbol.UNIQUE, null, query);
   }

   // operations

   /**
    * @see nexj.core.persistence.operator.FunctionOperator#normalize(int)
    */
   public Operator normalize(int nFlags)
   {
      if (!(m_parent instanceof AggregateOperator))
      {
         throw new InvalidQueryException("err.persistence.nestedAggregate", new Object[]{getSymbol().getName()});
      }

      normalizeOperand(nFlags);

      AggregateOperator parent = (AggregateOperator)m_parent;

      parent.setUnique(true);

      Operator op = getOperand();

      op.setParent(parent);

      return op;
   }
}
