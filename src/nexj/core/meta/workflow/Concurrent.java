// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

/**
 * Concurrent activity. 
 */
public class Concurrent extends Activity
{
   // associations

   /**
    * The fork element.
    */
   protected Fork m_fork;

   // constructors

   /**
    * Construct a Concurrent object.
    * @param fork The fork element.
    */
   public Concurrent(Fork fork)
   {
      m_fork = fork;
   }

   // operations

   /**
    * Sets the fork step.
    * @param fork The fork step to set.
    */
   public void setFork(Fork fork)
   {
      verifyNotReadOnly();
      m_fork = fork;
   }

   /**
    * @return The fork step.
    */
   public Fork getFork()
   {
      return m_fork;
   }

   /**
    * @see nexj.core.meta.workflow.Activity#getFlow()
    */
   public Flow getFlow()
   {
      return m_fork.getActivity().getFlow();
   }

   /**
    * @see nexj.core.meta.workflow.Activity#getParent()
    */
   public Activity getParent()
   {
      return m_fork.getActivity();
   }

   /**
    * @see nexj.core.meta.workflow.Activity#getContainer()
    */
   public Step getContainer()
   {
      return m_fork;
   }
}
