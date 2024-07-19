// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

import nexj.core.util.Executable;

/**
 * Superclass for command objects that do not require security check on InvocationContext.
 */
public abstract class TrustedExecutable implements Executable, InvocationContextAware
{
   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   /**
    * Sets the invocation context.
    * @param context The invocation context to set.
    */
   public void setInvocationContext(InvocationContext context)
   {
      m_context = context;
   }

   /**
    * @return The invocation context.
    */
   public InvocationContext getInvocationContext()
   {
      return m_context;
   }
   
   /**
    * @see nexj.core.util.Executable#execute()
    */
   public final void execute()
   {
      boolean wasSecure = m_context.isSecure();
      m_context.setSecure(false);
      try
      {
         executeTrusted();
      }
      finally
      {
         m_context.setSecure(wasSecure);
      }
   }

   /**
    * Execute secure command 
    */
   protected abstract void executeTrusted();
}
