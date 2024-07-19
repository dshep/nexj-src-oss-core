// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

import nexj.core.scripting.ConstPair;
import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;
import nexj.core.scripting.PCodeFunction;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;

/**
 * Try-catch block. 
 */
public class TryCatch extends Decision
{
   // constants

   /**
    * Arguments for the catch selector function.
    */
   protected final static Pair CATCH_ARGUMENTS = new ConstPair(Symbol._EXCEPTION, ARGUMENTS);

   /**
    * Try-catch default catch clause: ((else #f))
    */
   protected final static Pair DEFAULT_CATCH = Pair.list(Pair.list(Symbol.ELSE, Boolean.FALSE));

   // associations

   /**
    * The exception variable. May be null.
    */
   protected Variable m_exceptionVariable;

   /**
    * Try branch, executed always.
    */
   protected final Try m_try = new Try();

   // constructors

   /**
    * Constructs the try-catch step.
    * @param sName The step name.
    */
   public TryCatch(String sName)
   {
      super(sName);
   }

   /**
    * Constructs the try-catch step.
    */
   public TryCatch()
   {
   }

   // operations

   /**
    * @return The try branch.
    */
   public Try getTry()
   {
      return m_try;
   }

   /**
    * @return The function for selecting a catch branch.
    */
   public Function getFunction()
   {
      return m_function;
   }

   /**
    * Sets the exception variable.
    * @param exceptionVariable The exception variable to set.
    */
   public void setExceptionVariable(Variable exceptionVariable)
   {
      verifyNotReadOnly();
      m_exceptionVariable = exceptionVariable;
   }

   /**
    * @return The exception variable.
    */
   public Variable getExceptionVariable()
   {
      return m_exceptionVariable;
   }
   
   /**
    * @see nexj.core.meta.workflow.Decision#generate(nexj.core.scripting.Machine)
    */
   public void generate(Machine machine)
   {
      verifyNotReadOnly();

      boolean bDefault = false;

      for (int i = getBranchCount() - 1; i >= 0; --i)
      {
         Catch branch = (Catch)getBranch(i);
         
         if (branch.getException() == null && branch.getCondition() == Boolean.TRUE)
         {
            bDefault = true;
            break;
         }
      }

      Pair code = (bDefault) ? null : DEFAULT_CATCH;

      for (int i = getBranchCount() - 1; i >= 0; --i)
      {
         code = new Pair(getBranch(i).generate(), code);
      }

      code = new Pair(Symbol.COND, code);

      if (m_activity.getFlow().isPrivileged())
      {
         code = Pair.list(Symbol.BEGIN_PRIVILEGED, code);
      }
      
      code = Pair.list(code);
      
      if (m_exceptionVariable != null)
      {
         code = new Pair(Pair.list(Symbol.SET, m_exceptionVariable.getSymbol(), Symbol._EXCEPTION), code);
      }

      m_function = compile(CATCH_ARGUMENTS, code, machine);
   }

   /**
    * @see nexj.core.meta.workflow.Decision#step(nexj.core.meta.workflow.State)
    */
   public PCodeFunction step(State state)
   {
      state.remove(this);

      Step next = m_try.getFirstStep();

      state.add((next != null) ? next : m_next);

      return null;
   }

   /**
    * @see nexj.core.meta.workflow.Decision#visit(nexj.core.meta.workflow.Step.Visitor)
    */
   public void visit(Visitor visitor)
   {
      m_try.visit(visitor);
      super.visit(visitor);
   }

   /**
    * @see nexj.core.meta.workflow.Decision#visit(nexj.core.meta.workflow.Activity.Visitor)
    */
   public void visit(Activity.Visitor visitor)
   {
      m_try.visit(visitor);
      super.visit(visitor);
   }

   // inner classes

   /**
    * Try branch.
    */
   protected final class Try extends Activity
   {
      /**
       * @see nexj.core.meta.workflow.Activity#getContainer()
       */
      public Step getContainer()
      {
         return TryCatch.this;
      }

      /**
       * @see nexj.core.meta.workflow.Activity#getFlow()
       */
      public Flow getFlow()
      {
         return getActivity().getFlow();
      }

      /**
       * @see nexj.core.meta.workflow.Activity#getFork()
       */
      public Fork getFork()
      {
         return TryCatch.this.getFork();
      }

      /**
       * @see nexj.core.meta.workflow.Activity#getParent()
       */
      public Activity getParent()
      {
         return getActivity();
      }

      /**
       * @see nexj.core.meta.workflow.Activity#getTryCatch()
       */
      public TryCatch getTryCatch()
      {
         return TryCatch.this;
      }
   }
}
