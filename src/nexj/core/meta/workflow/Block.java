// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

import nexj.core.scripting.Machine;
import nexj.core.scripting.PCodeFunction;
import nexj.core.scripting.PCodeHolder;
import nexj.core.scripting.Pair;

/**
 * A block that contains an activity. When execution leaves the activity (due to
 * normal execution or an exception), the function specified on this block is
 * executed.
 */
public class Block extends Step implements PCodeHolder
{
   // associations

   /**
    * When this activity is exited
    */
   protected final BlockActivity m_contained = new BlockActivity();

   /**
    * The code to execute on exit.
    */
   protected Pair m_code;

   /**
    * The compiled function to execute on exit.
    */
   protected PCodeFunction m_cleanupFunction;

   // constructors

   /**
    * Constructs the block step.
    * 
    * @param sName The name of the block step.
    */
   public Block(String sName)
   {
      super(sName);
   }

   /**
    * Constructs the block step. 
    */
   public Block()
   {
      super();
   }

   // operations

   /**
    * Gets the Activity that holds the steps to be executed by this block.
    * 
    * @return The Block's Activity.
    */
   public Activity getContainedActivity()
   {
      return m_contained;
   }

   /**
    * Sets the code to execute when the Block is exited.
    * 
    * @param code The code to execute when the Block is exited.
    */
   public void setCleanupCode(Pair code)
   {
      verifyNotReadOnly();
      m_code = code;
   }

   /**
    * @see nexj.core.meta.workflow.Step#generate(nexj.core.scripting.Machine)
    */
   public void generate(Machine machine)
   {
      if (m_code != null)
      {
         m_cleanupFunction = compile(ARGUMENTS, m_code, machine);
      }
   }

   /**
    * @see nexj.core.meta.workflow.Step#step(nexj.core.meta.workflow.State)
    */
   public PCodeFunction step(State state)
   {
      state.remove(this);

      Step next = m_contained.getFirstStep();

      state.add((next != null) ? next : m_next);

      return null;
   }

   /**
    * @see nexj.core.meta.workflow.Decision#visit(nexj.core.meta.workflow.Step.Visitor)
    */
   public void visit(Visitor visitor)
   {
      m_contained.visit(visitor);
      super.visit(visitor);
   }

   /**
    * @see nexj.core.meta.workflow.Decision#visit(nexj.core.meta.workflow.Activity.Visitor)
    */
   public void visit(Activity.Visitor visitor)
   {
      m_contained.visit(visitor);
      super.visit(visitor);
   }

   /**
    * @see nexj.core.scripting.PCodeHolder#getPCode()
    */
   public PCodeFunction getPCode()
   {
      return m_cleanupFunction;
   }

   // inner classes

   /**
    * The Activity that holds the steps to be executed by this block.
    */
   protected class BlockActivity extends Activity
   {
      // operations

      /**
       * @see nexj.core.meta.workflow.Activity#getContainer()
       */
      public Step getContainer()
      {
         return Block.this;
      }

      /**
       * @see nexj.core.meta.workflow.Activity#getFlow()
       */
      public Flow getFlow()
      {
         return Block.this.getActivity().getFlow();
      }

      /**
       * @see nexj.core.meta.workflow.Activity#getFork()
       */
      public Fork getFork()
      {
         return Block.this.getFork();
      }

      /**
       * @see nexj.core.meta.workflow.Activity#getParent()
       */
      public Activity getParent()
      {
         return Block.this.getActivity();
      }

      /**
       * @see nexj.core.meta.workflow.Activity#cleanup(nexj.core.meta.workflow.State)
       */
      public PCodeFunction cleanup(State state)
      {
         return m_cleanupFunction;
      }

      /**
       * @see nexj.core.meta.workflow.Activity#isCleanupRequired()
       */
      public boolean isCleanupRequired()
      {
         return true;
      }
   }
}
