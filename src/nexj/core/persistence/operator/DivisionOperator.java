// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.operator;

import nexj.core.meta.Primitive;
import nexj.core.scripting.Symbol;

/**
 * Division expression node.
 */
public final class DivisionOperator extends BinaryOperator
{
   // constants
   
   /**
    * The operator ordinal number.
    */
   public final static int ORDINAL = 7;
   
   /**
    * The operator priority.
    */
   public final static int PRIORITY = 6;
   
   /**
    * The operator symbol.
    */
   public final static Symbol SYMBOL = Symbol.DIVIDE;
   
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
    * @see nexj.core.persistence.Operator#evaluate()
    */
   protected Object evaluate()
   {
      return ((Primitive)m_left.getType()).getDivisionFunction((Primitive)m_right.getType()).invoke(m_left.getValue(), m_right.getValue());
   }
}
