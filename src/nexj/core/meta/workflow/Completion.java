// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

import java.util.Collections;
import java.util.List;

import nexj.core.scripting.PCodeFunction;
import nexj.core.scripting.Symbol;

/**
 * Immediate assignment completion step.
 */
public class Completion extends Step
{
   // constants

   /**
    * The unassignment step cleanup list, consisting of the assignments association.
    */
   protected final static List CLEANUP_ASSOC_LIST = Collections.singletonList(Symbol.ASSIGNMENTS);

   // associations

   /**
    * The assignment step.
    */
   protected Assignment m_assignment;

   // constructors
   
   /**
    * Constructs the step.
    */
   public Completion(Assignment assignment)
   {
      super();
      m_assignment = assignment;
      setName(assignment.getName());
      setActivity(assignment.getActivity());
      m_cleanupAssocList = CLEANUP_ASSOC_LIST;
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
    * @see nexj.core.meta.workflow.Step#getCleanupOrdinal()
    */
   public int getCleanupOrdinal()
   {
      return m_assignment.m_nOrdinal;
   }

   /**
    * @see nexj.core.meta.workflow.Step#isDependent()
    */
   public boolean isDependent()
   {
      return true;
   }

   /**
    * @see nexj.core.meta.workflow.Step#step(nexj.core.meta.workflow.State)
    */
   public PCodeFunction step(State state)
   {
      state.remove(this);
      state.add(m_next);

      return null;
   }
}
