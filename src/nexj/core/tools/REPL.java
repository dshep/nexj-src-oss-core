// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.tools;

import java.util.Locale;

import nexj.core.meta.Repository;
import nexj.core.runtime.Context;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.ThreadContextHolder;
import nexj.core.util.auth.SimplePrincipal;

/**
 * Read-Eval-Print Loop for the scripting engine.
 */
public class REPL extends ContextREPL
{
   // operations

   /**
    * @see nexj.core.tools.ContextREPL#initializeContext()
    */
   protected Context initializeContext()
   {
      InvocationContext context = (InvocationContext)Repository.getMetadata().getComponent("System.InvocationContext")
         .getInstance(null);

      context.setAudited(false);
      context.initialize((m_sUser == null) ? null : new SimplePrincipal(m_sUser));
      context.setLocale(Locale.getDefault());
      context.getUnitOfWork().commit();

      ThreadContextHolder.setContext(context);

      assert context.getMachine() != null;

      return context;
   }

   /**
    * @see nexj.core.tools.GenericTool#dispose()
    */
   protected void dispose()
   {
      super.dispose();

      ThreadContextHolder.setContext(null);
   }

   public static void main(String[] args)
   {
      new REPL().run(args);
   }

}
