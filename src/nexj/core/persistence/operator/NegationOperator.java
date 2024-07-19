// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.operator;

import nexj.core.meta.Primitive;
import nexj.core.persistence.Operator;
import nexj.core.scripting.Symbol;

/**
 * Negation expression node.
 */
public final class NegationOperator extends UnaryOperator
{
   // constants
   
   /**
    * The operator ordinal number.
    */
   public final static int ORDINAL = 4;
   
   /**
    * The operator priority.
    */
   public final static int PRIORITY = 7;
   
   /**
    * The operator symbol.
    */
   public final static Symbol SYMBOL = Symbol.MINUS;

   // constructors
   
   /**
    * Creates a negation operator.
    */
   public NegationOperator()
   {
   }
   
   /**
    * Creates a negation operator with a given operand.
    * @param operand The operand to negate.
    */
   public NegationOperator(Operator operand)
   {
      m_operand = operand;
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
    * @see nexj.core.persistence.Operator#evaluate()
    */
   protected Object evaluate()
   {
      return ((Primitive)m_type).getNegationFunction().invoke(m_operand.getValue());
   }
}
