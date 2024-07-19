// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

import java.util.Collections;
import java.util.List;

import nexj.core.meta.Primitive;
import nexj.core.scripting.ConstPair;
import nexj.core.scripting.Machine;
import nexj.core.scripting.PCodeFunction;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.HashHolder;
import nexj.core.util.Holder;

/**
 * Manual completion step. Removes an item from the work queue
 * by deleting an instance of SysWorkflowAssignment with a given ordinal number.
 */
public class ManualCompletion extends Step
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

   /**
    * The manual states list of pairs: ((ordinal . caption) ...)
    */
   protected Pair m_states;

   // constructors

   /**
    * Constructs the step.
    * @param assignment The preceding assignment step.
    */
   public ManualCompletion(Assignment assignment)
   {
      super();
      setAssignment(assignment);
      setName(assignment.getName());
      setActivity(assignment.getActivity());
      m_cleanupAssocList = CLEANUP_ASSOC_LIST;
   }

   // operations

   /**
    * Sets the assignment step.
    * @param assignment The assignment step to set.
    */
   public void setAssignment(Assignment assignment)
   {
      verifyNotReadOnly();
      m_assignment = assignment;
      
      if (assignment != null)
      {
         assignment.setManualCompletion(this);
      }
   }

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
    * @return The states list.
    */
   public Pair getStates()
   {
      return m_states;
   }
   
   /**
    * Collects recursively the manual states in a given decision.
    * @param decision The decision to process.
    * @param identitySet The set for tracking object identity.
    */
   protected void collectStates(Decision decision, Holder identitySet)
   {
      if (!identitySet.add(decision))
      {
         return;
      }

      for (int i = 0, nCount = decision.getBranchCount(); i < nCount; ++i)
      {
         Branch branch = decision.getBranch(i);
         String sCaption = branch.getCaption();
         int nOrdinal;

         if (branch.getStepCount() == 0)
         {
            if (decision.getNext() instanceof Decision)
            {
               Decision next = (Decision)decision.getNext();
               
               if (next.isManual())
               {
                  collectStates(next, identitySet);
                  
                  continue;
               }
            }
            
            if (decision.getNext() == null)
            {
               if (sCaption == null)
               {
                  sCaption = "ids.flow.done";
               }

               m_states = new ConstPair(new ConstPair(Primitive.createInteger(-1), sCaption), m_states);
               
               continue;
            }
            
            nOrdinal = decision.getNext().getOrdinal();
            
            if (sCaption == null)
            {
               sCaption = decision.getNext().getName();
            }
         }
         else
         {
            Step step = branch.getStep(0);
            
            if (step instanceof Decision && ((Decision)step).isManual())
            {
               collectStates((Decision)step, identitySet);

               continue;
            }
            
            nOrdinal = step.getOrdinal();
            
            if (sCaption == null)
            {
               sCaption = step.getName();
            }
         }
         
         if (sCaption == null)
         {
            sCaption = "Decision " + nOrdinal;
         }
         
         m_states = new ConstPair(new ConstPair(Primitive.createInteger(nOrdinal), sCaption), m_states);
      }
   }
   
   /**
    * @see nexj.core.meta.workflow.Step#generate(nexj.core.scripting.Machine)
    */
   public void generate(Machine machine)
   {
      verifyNotReadOnly();

      if (m_next instanceof Decision)
      {
         Decision decision = (Decision)m_next;

         if (decision.isManual())
         {
            collectStates(decision, new HashHolder());

            return;
         }
      }

      m_states = new ConstPair(new ConstPair(
         Primitive.createInteger((m_next == null) ? -1 : m_next.getOrdinal()), "ids.flow.done"));
   }
   
   /**
    * @see nexj.core.meta.workflow.Step#isPersistent()
    */
   public boolean isPersistent()
   {
      return true;
   }

   /**
    * Jumps to the specified state.
    * @param state The current state.
    * @param nOrdinal The ordinal number of the next state. -1 for final.
    */
   public void jump(State state, int nOrdinal)
   {
      state.remove(this);

      if (nOrdinal >= 0)
      {
         state.add(m_activity.getFlow().getFlowStep(nOrdinal));
      }
   }

   /**
    * @see nexj.core.meta.workflow.Step#step(nexj.core.meta.workflow.State)
    */
   public PCodeFunction step(State state)
   {
      throw new IllegalStateException();
   }
}
