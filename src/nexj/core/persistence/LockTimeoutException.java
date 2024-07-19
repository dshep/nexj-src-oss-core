// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;

/**
 * Exception indicating a timed out attempt to acquire a lock.
 */
public class LockTimeoutException extends PersistenceException
{
   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = 1765366365250659497L;

   public LockTimeoutException()
   {
      super("err.persistence.lockTimeout");
   }

   public LockTimeoutException(Throwable cause)
   {
      super("err.persistence.lockTimeout", cause);
   }

   // operations

   /**
    * @see nexj.core.util.ErrorCode#isSystem()
    */
   public boolean isSystem()
   {
      return false;
   }
}
