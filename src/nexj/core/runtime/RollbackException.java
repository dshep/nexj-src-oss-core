// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

/**
 * Exception indicating an attempt to do transactional work on a rolled back transaction.
 */
public class RollbackException extends TransactionException
{
   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = 4992046809428763592L;

   public RollbackException()
   {
      super("err.runtime.txRollback");
   }
}
