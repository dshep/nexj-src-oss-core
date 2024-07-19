// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.tools;

import nexj.core.runtime.Context;
import nexj.core.runtime.MinimalContext;
import nexj.core.util.ObjUtil;

/**
 * Minimal scheme language REPL.
 */
public class MinimalREPL extends ContextREPL
{
   // operations

   /**
    * @see nexj.core.tools.ContextREPL#initializeContext()
    */
   protected Context initializeContext()
   {
      Context ctx = new MinimalContext();

      ctx.initialize(null, null);

      return ctx;
   }

   /**
    * @see nexj.core.tools.ContextREPL#getMessage(java.lang.Throwable)
    */
   protected String getMessage(Throwable t)
   {
      return ObjUtil.getMessage(t);
   }

   public static void main(String[] args)
   {
      new MinimalREPL().run(args);
   }
}
