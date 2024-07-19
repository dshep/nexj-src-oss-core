// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

import nexj.core.scripting.PCodeFunction;

/**
 * Join step for terminating a fork.
 */
public class Join extends Step
{
   // attributes

   /**
    * The any flag.
    */
   protected boolean m_bAny;

   // associations

   /**
    * The joined fork.
    */
   protected Fork m_joinedFork;

   // constructors
   
   /**
    * Constructs the join.
    * @param fork The preceding fork.
    */
   public Join(Fork fork)
   {
      super();
      m_joinedFork = fork;
      setName(fork.getName());
      setActivity(fork.getActivity());
   }
   
   // operations
   
   /**
    * Sets the any flag.
    * @param bAny The any flag to set.
    */
   public void setAny(boolean bAny)
   {
      verifyNotReadOnly();
      m_bAny = bAny;
   }

   /**
    * @return The any flag.
    */
   public boolean isAny()
   {
      return m_bAny;
   }

   /**
    * @return The joined fork.
    */
   public Fork getJoinedFork()
   {
      return m_joinedFork;
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
      while (state.remove(this));

      if (m_joinedFork.isParallel() && state.getThreadInfo(getName()) != null)
      {
         return null;
      }

      if (m_bAny)
      {
         state.removeConcurrent(m_joinedFork);
         state.add(m_next);
      }
      else if (!state.containsConcurrent(m_joinedFork))
      {
         state.add(m_next);
      }

      return null;
   }
}
