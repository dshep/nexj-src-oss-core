// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;

/**
 * Decision branch.
 */
public class Branch extends Activity
{
   // attributes

   /**
    * The caption.
    */
   protected String m_sCaption;

   // associations

   /**
    * The decision step.
    */
   protected Decision m_decision;

   /**
    * The condition.
    */
   protected Object m_condition;

   // operations

   /**
    * Sets the decision step.
    * @param decision The decision step to set.
    */
   public void setDecision(Decision decision)
   {
      verifyNotReadOnly();
      m_decision = decision;
   }

   /**
    * @return The decision step.
    */
   public Decision getDecision()
   {
      return m_decision;
   }

   /**
    * Sets the condition.
    * @param condition The condition to set.
    */
   public void setCondition(Object condition)
   {
      verifyNotReadOnly();
      m_condition = condition;
   }

   /**
    * @return The condition.
    */
   public Object getCondition()
   {
      return m_condition;
   }

   /**
    * Sets the caption.
    * @param sCaption The caption to set.
    */
   public void setCaption(String sCaption)
   {
      verifyNotReadOnly();
      m_sCaption = sCaption;
   }

   /**
    * @return The caption.
    */
   public String getCaption()
   {
      return m_sCaption;
   }
   
   /**
    * @see nexj.core.meta.workflow.Activity#getFlow()
    */
   public Flow getFlow()
   {
      return m_decision.getActivity().getFlow();
   }

   /**
    * @see nexj.core.meta.workflow.Activity#getParent()
    */
   public Activity getParent()
   {
      return m_decision.getActivity();
   }

   /**
    * @see nexj.core.meta.workflow.Activity#getContainer()
    */
   public Step getContainer()
   {
      return m_decision;
   }

   /**
    * @see nexj.core.meta.workflow.Activity#getFork()
    */
   public Fork getFork()
   {
      return m_decision.getFork();
   }
   
   /**
    * Generates a condition for this branch:
    * (<condition> ('<this>'branch :state))
    * @return The generated code.
    */
   public Pair generate()
   {
      return Pair.list(m_condition, Pair.list(Pair.quote(this),
         Pair.quote(Symbol.BRANCH), Symbol._STATE));
   }

   /**
    * Switches to this branch.
    * @param state The flow state.
    */
   public void branch(State state)
   {
      state.remove(m_decision);

      Step next = getFirstStep();

      state.add((next != null) ? next : m_decision.getNext());
   }
}
