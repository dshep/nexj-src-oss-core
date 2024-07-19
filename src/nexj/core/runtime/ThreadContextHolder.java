// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

/**
 * Associates the context with the current thread.
 */
public final class ThreadContextHolder
{
   // associations

   /**
    * The thread invocation context.
    */
   private final static ThreadLocal s_threadContext = new ThreadLocal();

   // constructors

   /**
    * Prevents construction.
    */
   private ThreadContextHolder()
   {
   }

   // operations

   /**
    * Sets the thread context.
    * @param context The context to set. Can be null. 
    */
   public static void setContext(Context context)
   {
      s_threadContext.set(context);
   }

   /**
    * @return The thread context. Can be null.
    */
   public static Context getContext()
   {
      return (Context)s_threadContext.get();
   }
}
