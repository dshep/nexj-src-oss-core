// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

import nexj.core.scripting.PCodeFunction;

/**
 * Step for jumping to another step.
 */
public class Goto extends Step implements Redirector
{
   // constructors
   
   /**
    * Constructs the step.
    * @param sName The step name.
    */
   public Goto(String sName)
   {
      super(sName);
   }

   /**
    * Constructs the step.
    */
   public Goto()
   {
      super();
   }

   // operations

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
