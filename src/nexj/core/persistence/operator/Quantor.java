package nexj.core.persistence.operator;

import nexj.core.persistence.Operator;
import nexj.core.persistence.Query;

/**
 * Interface implemented by quantor operators.
 */
public interface Quantor
{
   /**
    * @return The query, to which the operator applies.
    */
   Query getQuantorQuery();

   /**
    * @return The quantor operand.
    */
   Operator getOperand();
}
