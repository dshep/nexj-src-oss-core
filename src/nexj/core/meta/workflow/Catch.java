// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;

/**
 * Catch branch. 
 */
public class Catch extends Branch
{
   // associations

   /**
    * The exception class to catch.
    */
   protected Class m_exception;

   // operations

   /**
    * Sets the exception class to catch.
    * @param exception The exception class to catch to set.
    */
   public void setException(Class exception)
   {
      verifyNotReadOnly();
      m_exception = exception;
   }

   /**
    * @return The exception class to catch.
    */
   public Class getException()
   {
      return m_exception;
   }

   /**
    * ((and (instance? exception '<m_exception>) <condition>) ('<this>'branch :state))
    * @see nexj.core.meta.workflow.Branch#generate()
    */
   public Pair generate()
   {
      return Pair.list(Pair.commutative(Symbol.AND,
         (m_exception == null) ? null : Pair.list(Symbol.INSTANCE_P,
            Symbol._EXCEPTION, Pair.quote(m_exception)), m_condition),
         Pair.list(Pair.quote(this), Pair.quote(Symbol.BRANCH), Symbol._STATE));
   }

   /**
    * @see nexj.core.meta.workflow.Branch#branch(nexj.core.meta.workflow.State)
    */
   public void branch(State state)
   {
      state.remove(getRootConcurrent());
      state.remove(m_decision);

      Step next = getFirstStep();
      
      if (next == null)
      {
         next = m_decision.getNext();
      }

      state.add(next);
      state.setLastStep(next);
   }
}
