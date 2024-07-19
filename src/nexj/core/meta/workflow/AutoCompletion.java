// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

import java.util.Collections;
import java.util.List;

import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;

/**
 * Completion trigger. Removes an item from the work queue
 * by deleting an instance of SysWorkflowAssignment with a given ordinal number.
 */
public class AutoCompletion extends HandlerStep
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
    * @param assignment The preceding assignment step.
    */
   public AutoCompletion(Assignment assignment)
   {
      super(assignment.getName(), assignment.getActivity());

      m_assignment = assignment;
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
    * @see nexj.core.meta.workflow.HandlerStep#setBody(Pair)
    */
   protected void setBody(Pair body)
   {
   }

   /**
    * @see nexj.core.meta.workflow.HandlerStep#getBody()
    */
   protected Pair getBody()
   {
      return null;
   }
}
