// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.operator;

import nexj.core.persistence.InvalidQueryException;
import nexj.core.persistence.Operator;


/**
 * Function operator: (fn x1 x2 ... xN).
 */
public abstract class FunctionOperator extends MultiArgOperator
{
   // constants

   /**
    * The operator ordinal number.
    */
   public final static int ORDINAL = 2;

   /**
    * The operator priority.
    */
   public final static int PRIORITY = 8;

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
    * Verifies the argument count.
    * @throws InvalidQueryException if the argument count is invalid.
    */
   public abstract void verifyOperandCount() throws InvalidQueryException;

   /**
    * @see nexj.core.persistence.operator.MultiArgOperator#normalize(int)
    */
   public Operator normalize(int nFlags)
   {
      verifyOperandCount();

      return super.normalize(nFlags);
   }
}
