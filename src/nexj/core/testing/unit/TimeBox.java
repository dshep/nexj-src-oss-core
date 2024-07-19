package nexj.core.testing.unit;

import nexj.core.runtime.InvocationContext;
import nexj.core.util.LinkedHashTab;
import nexj.core.util.Lookup;
import nexj.core.util.LookupDeque;

/**
 * A utility class that ensures time boxed execution of the current thread
 */
public final class TimeBox implements Runnable
{   
   /**
    * Time box duration, milliseconds
    */
   private final long m_lTimeout;
   
   /**
    * True if the timeout was exceeded before box was interrupted.
    */
   private boolean m_bTimedOut = false;
   
   /**
    * A collection of time boxed invocation contexts.  Key is InvocationContext,
    * value is whether to rollback the unit of work.
    */
   private final LookupDeque m_contextMap = new LinkedHashTab();

   /**
    * Time box thread
    */
   private final Thread m_thread;

   /**
    * Constructor
    * @param lTimeout timeout duration in milliseconds
    * @param ctx an InvocationContext
    * @param bRollbackUnitOfWork true if unit of work is to be rolled back after canceling context
    */
   public TimeBox(long lTimeout, InvocationContext ctx, boolean bRollbackUnitOfWork)
   {
      m_lTimeout = lTimeout;
      m_thread = new Thread(this);
      m_thread.start();
      addContext(ctx, bRollbackUnitOfWork);
   }
   
   /**
    * @return the TimedOut flag.
    */
   public boolean isTimedOut()
   {
      return m_bTimedOut;
   }
   
   /**
    * Interrupts the time box thread
    */
   public void interrupt()
   {
      m_thread.interrupt();
   }
   
   /**
    * Adds invocation context to the collection of boxed invocation contexts
    * @param ctx an InvocationContext
    * @param bRollbackUnitOfWork true if unit of work is to be rolled back after canceling context
    */
   public void addContext(InvocationContext ctx, boolean bRollbackUnitOfWork)
   {
      Boolean rollback = (Boolean)m_contextMap.get(ctx);
      
      rollback = (rollback != null && rollback.booleanValue()) ? rollback : Boolean.valueOf(bRollbackUnitOfWork);
      m_contextMap.putFirst(ctx, rollback);
   }

   /**
    * @see java.lang.Runnable#run()
    */
   public void run()
   {
      try
      {
         for(;;)
         {
            Thread.sleep(m_lTimeout);
            m_bTimedOut = true;
            
            for (Lookup.Iterator itr = m_contextMap.iterator(); itr.hasNext(); )
            {
               InvocationContext context = (InvocationContext)itr.next();
               
               try
               {
                  context.cancel();
                  
                  if (((Boolean)itr.getValue()).booleanValue())
                  {
                     context.complete(false);
                  }
               }
               catch (Throwable t)
               {  // do nothing
               }
            }
         }
      }
      catch (InterruptedException e)
      {
         // This thread will be interrupted by the main thread during normal processing
      }
   }
}
