// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.sync;

import nexj.core.persistence.DeadlockException;
import nexj.core.persistence.DuplicateKeyException;
import nexj.core.persistence.LockTimeoutException;
import nexj.core.persistence.OptimisticLockException;
import nexj.core.runtime.InvocationContext;

/**
 * Synchronization utilities class.
 * @deprecated
 * @see nexj.core.integration.sync.SyncEngine
 */
public class SyncUtil
{
   // operations

   /**
    * Creates and submits synchronization commands for near-realtime sync.
    * @param context The invocation context.
    * @deprecated
    * @see nexj.core.integration.sync.SyncEngine#submitSyncCommands(InvocationContext)
    */
   public static void submitSyncCommands(InvocationContext context)
   {
   }

   /**
    * Determine if the given exception is transient (is likely to go away after a retry)
    * @param e The exception.
    * @return True, if the exception is transient. Otherwise, false.
    * @deprecated
    * @see nexj.core.integration.sync.SyncEngine#isTransient(Throwable)
    */
   public static boolean isTransient(Throwable e)
   {
      return e instanceof OptimisticLockException
         || e instanceof DuplicateKeyException
         || e instanceof DeadlockException
         || e instanceof LockTimeoutException
         || e instanceof OutOfMemoryError;
   }
}
