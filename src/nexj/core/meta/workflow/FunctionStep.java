// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

import nexj.core.scripting.Function;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;

/**
 * Step implementing the Function interface.
 */
public abstract class FunctionStep extends Code
{
   // constructors
   
   /**
    * Constructs the step.
    * @param sName The step name.
    */
   public FunctionStep(String sName)
   {
      super(sName);
   }

   /**
    * Constructs the step.
    */
   public FunctionStep()
   {
      super();
   }

   // operations
   
   /**
    * @see nexj.core.meta.workflow.Code#getBody()
    */
   protected final Pair getBody()
   {
      // ('<function> <arguments...>)

      Pair body = new Pair(Pair.quote(getFunction()), getArguments());

      if (m_activity.getFlow().isPrivileged())
      {
         body = Pair.list(Symbol.BEGIN_PRIVILEGED, body);
      }

      return Pair.list(body);
   }

   /**
    * @return The function implementation.
    */
   protected abstract Function getFunction();
   
   /**
    * @return The function arguments.
    */
   protected Pair getArguments()
   {
      return ARGUMENTS;
   }
}
