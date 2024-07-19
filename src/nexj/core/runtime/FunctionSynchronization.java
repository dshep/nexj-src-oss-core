// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

import javax.transaction.NotSupportedException;
import javax.transaction.RollbackException;
import javax.transaction.Synchronization;
import javax.transaction.SystemException;

import nexj.core.scripting.Function;
import nexj.core.scripting.Pair;

/**
 * @deprecated: use UnitOfWork addCompensator instead.
 * A distributed transaction listener that executes a function on transaction completion 
 */
public class FunctionSynchronization implements Synchronization
{
   /**
    * A function called on transaction synchronization
    */
   private final Function m_function;
   
   /**
    * A list of arguments to be passed to the function, in addition to the transaction status
    */
   private final Pair m_argumentPair;
   
   /**
    * The original invocation context
    */
   private final InvocationContext m_context;
   
   /**
    * Constructor
    * @param function a function whose first argument is transaction status (int)
    * @param argumentPair values of function arguments, starting from the second
    * @param context the current invocation context
    */
   public FunctionSynchronization(Function function, Pair argumentPair, InvocationContext context)
   {
      m_function = function;
      m_argumentPair = argumentPair;
      m_context = context;
   }


   /**
    * Registers the given function to receive transaction synchronization notifications
    * @param function a function whose first argument is transaction status (int)
    * @param argumentPair values of function arguments, starting from the second
    * @param context the current invocation context
    * 
    * The following exceptions can be thrown by transaction manager:
    * @throws SystemException 
    * @throws RollbackException 
    * @throws IllegalStateException 
    * @throws NotSupportedException 
    * 
    */
   public static void submit(Function function, Pair argumentPair, InvocationContext context) throws IllegalStateException, RollbackException, SystemException, NotSupportedException
   {
      context.getTransactionManager().getTransaction().registerSynchronization(new FunctionSynchronization(function, argumentPair, context));
   }

   /**
    * @see javax.transaction.Synchronization#beforeCompletion()
    */
   public void beforeCompletion()
   {
   }

   /**
    * @see javax.transaction.Synchronization#afterCompletion(int)
    */
   public void afterCompletion(int nStatus)
   {
      Context contextSaved = ThreadContextHolder.getContext(); 
      
      try
      {
         ThreadContextHolder.setContext(m_context);
         m_context.getMachine().invoke(m_function, new Pair(Integer.valueOf(nStatus), m_argumentPair));
      }
      finally
      {
         ThreadContextHolder.setContext(contextSaved);
      }
   }
}
