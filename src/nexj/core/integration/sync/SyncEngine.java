// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.sync;

import nexj.core.integration.IntegrationException;
import nexj.core.integration.ObjectMismatchException;
import nexj.core.persistence.AvailabilityException;
import nexj.core.persistence.DeadlockException;
import nexj.core.persistence.DuplicateKeyException;
import nexj.core.persistence.LockTimeoutException;
import nexj.core.persistence.OptimisticLockException;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.UnitOfWork;

/**
 * Synchronization utilities class.
 */
public class SyncEngine
{
   // operations

   /**
    * Creates and submits synchronization commands for near-realtime sync.
    * @param context The invocation context.
    */
   public static void submitSyncCommands(InvocationContext context)
   {
   }

   /**
    * Process instance changes in the unit of work.
    * @param uow The unit of work for which instance changes need to be processed.
    */
   public static void processChanges(UnitOfWork uow)
   {
   }

   /**
    * Determine if the given exception is transient (is likely to go away after a retry)
    * @param e The exception.
    * @return True, if the exception is transient. Otherwise, false.
    */
   public static boolean isTransient(Throwable e)
   {
      if (e instanceof IntegrationException)
      {
         Throwable cause = e.getCause();

         if (cause != null)
         {
            return isTransient(cause);
         }
      }

      return e instanceof OptimisticLockException
         || e instanceof DuplicateKeyException
         || e instanceof DeadlockException
         || e instanceof LockTimeoutException
         || e instanceof OutOfMemoryError
         || e instanceof AvailabilityException
         || e instanceof ObjectMismatchException;
   }
}
