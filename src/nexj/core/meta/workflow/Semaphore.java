// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

import nexj.core.meta.Primitive;
import nexj.core.scripting.Machine;
import nexj.core.scripting.PCodeFunction;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;

/**
 * Workflow and Service Semaphore Step.
 */
public class Semaphore extends Step
{
   // constants

   /**
    * "SysWorkflowAssignment" symbol.
    */
   protected final static Symbol ASSIGNMENT_METACLASS = Symbol.define("SysWorkflowAssignment");

   /**
    * "cleanup" event symbol for SysWorkflowAssignment.
    */
   protected final static Symbol CLEANUP = Symbol.define("cleanup");

   /**
    * "startup" event symbol for SysWorkflowAssignment.
    */
   protected final static Symbol STARTUP = Symbol.define("startup");

   // associations

   /**
    * The state management function.
    */
   protected PCodeFunction m_function;

   /**
    * The assignment step.
    */
   protected Assignment m_assignment;

   // constructors

   /**
    * Constructs the Semaphore flow step.
    * 
    * @param sName The Semaphore name.
    * @param assignment The Assignment from which the Semaphore will read its tasks.
    */
   public Semaphore(String sName, Assignment assignment)
   {
      super(sName);
      m_assignment = assignment;
   }

   /**
    * Constructs the Semaphore flow step.
    * 
    * @param assignment The Assignment from which the Semaphore will read its tasks.
    */
   public Semaphore(Assignment assignment)
   {
      super();
      m_assignment = assignment;
   }

   // operations

   /**
    * @return The assignment step.
    */
   public Assignment getAssignment()
   {
      return m_assignment;
   }

   /**
    * @see nexj.core.meta.workflow.Step#step(nexj.core.meta.workflow.State)
    */
   public PCodeFunction step(State state)
   {
      state.remove(this);
      state.add(m_next);

      return m_function;
   }

   /**
    * Creates code for the function to execute when execution leaves
    * the Semaphore. Basically, this function must remove the
    * assignment.
    * 
    * @return The exit function.
    */
   public Pair getExitCode()
   {
      /*
       * (SysWorkflowAssignment'cleanup :flow <m_assignment.m_nOrdinal>)
       * this
       */
      return new Pair(
         Pair.list(
            ASSIGNMENT_METACLASS,
            Pair.quote(CLEANUP),
            Symbol._FLOW,
            Primitive.createInteger(m_assignment.m_nOrdinal)
         ),
         m_activity.getFlow().getDefaultReturnCode()
      );
   }

   /**
    * @see nexj.core.meta.workflow.Step#generate(nexj.core.scripting.Machine)
    */
   public void generate(Machine machine)
   {
      /*
       * (SysWorkflowAssignment'startup :flow <m_assignment.m_nOrdinal>)
       * this
       */
      Pair body = new Pair(
         Pair.list(
            ASSIGNMENT_METACLASS,
            Pair.quote(STARTUP),
            Symbol._FLOW,
            Primitive.createInteger(m_assignment.m_nOrdinal)
         ),
         m_activity.getFlow().getDefaultReturnCode()
      );

      m_function = compile(ARGUMENTS, body, machine);
   }

   /**
    * @see nexj.core.meta.workflow.Step#isPersistent()
    */
   public boolean isPersistent()
   {
      return true;
   }
}
