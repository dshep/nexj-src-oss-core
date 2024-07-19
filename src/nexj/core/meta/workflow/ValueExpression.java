// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

import nexj.core.scripting.Machine;
import nexj.core.scripting.PCodeFunction;
import nexj.core.scripting.PCodeHolder;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;

/**
 * Step argument represented by a Scheme expression.
 */
public class ValueExpression implements PCodeHolder
{
   // associations

   /**
    * Scheme function body to evaluate. Can be null.
    */
   protected Object m_value;

   /**
    * The compiled expression.
    */
   protected PCodeFunction m_function;

   // constructors

   /**
    * @param expr The expression.
    */
   public ValueExpression(Object expr)
   {
      m_value = expr;
   }

   // operations

   /**
    * @return The script for compilation.
    */
   protected Pair getBody()
   {
      return Pair.list(m_value);
   }

   /**
    * @return The expression passed into the constructor.
    */
   public Object getValue()
   {
      return m_value;
   }

   /**
    * Compiles the expression.
    * @param step The step with which this expression is associated.
    * @param machine The VM for compilation.
    * @param args The function arguments.
    */
   public void compile(Step step, Machine machine, Pair arguments)
   {
      if (((m_value instanceof Pair) || (m_value instanceof Symbol)) && (m_function == null))
      {
         m_function = step.compile(arguments, getBody(), machine);
         m_value = null;
      }
   }

   /**
    * Invokes the expression with given arguments.
    */ 
   public Object invoke(Machine machine, State state, Object[] args)
   {
      return (m_function == null) ? m_value : machine.invoke(state.bind(m_function), args);
   }

   /**
    * @see nexj.core.scripting.PCodeHolder#getPCode()
    */
   public PCodeFunction getPCode()
   {
      return m_function;
   }
}
