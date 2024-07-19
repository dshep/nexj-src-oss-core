// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;

/**
 * Exception indicating that concurrent updates to shared data have caused a deadlock.
 */
public class DeadlockException extends PersistenceException
{
   // constants

   /**
    * The serialization UID.
    */
   private final static long serialVersionUID = -4896400169730461266L;

   // constructors

   public DeadlockException()
   {
      super("err.persistence.deadlock");
   }

   public DeadlockException(Throwable cause)
   {
      super("err.persistence.deadlock", cause);
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
