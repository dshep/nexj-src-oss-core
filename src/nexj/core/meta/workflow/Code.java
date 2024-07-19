// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

import nexj.core.scripting.Machine;
import nexj.core.scripting.PCodeFunction;
import nexj.core.scripting.PCodeHolder;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;

/**
 * Base class for scripts.
 */
public abstract class Code extends Step implements PCodeHolder
{
   // associations
   
   /**
    * The state management function.
    */
   protected PCodeFunction m_function;

   // constructors
   
   /**
    * Constructs the step.
    * @param sName The step name.
    */
   public Code(String sName)
   {
      super(sName);
   }

   /**
    * Constructs the step.
    */
   public Code()
   {
      super();
   }

   // operations
   
   /**
    * Returns the code.
    */
   protected abstract Pair getBody();
   
   /**
    * @see nexj.core.meta.workflow.Step#generate(nexj.core.scripting.Machine)
    */
   public void generate(Machine machine)
   {
      verifyNotReadOnly();

      Pair body = getBody();

      if (body != null)
      {
         if (m_activity.getFlow().isPrivileged())
         {
            body = Pair.list(new Pair(Symbol.BEGIN_PRIVILEGED, body));
         }

         m_function = compile(ARGUMENTS, body, machine);
      }
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
    * @see nexj.core.scripting.PCodeHolder#getPCode()
    */
   public PCodeFunction getPCode()
   {
      return m_function;
   }
}
