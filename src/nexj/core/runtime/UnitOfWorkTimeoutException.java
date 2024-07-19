// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;


/**
 * Exception thrown when the unit of work timeout has been exceeded.
 * This is different from a transaction timeout, which usually is larger.
 */
public class UnitOfWorkTimeoutException extends TransactionException
{
   /**
    * Serialization UID.
    */
   private static final long serialVersionUID = 259416924914387831L;

   public UnitOfWorkTimeoutException(long lTimeout)
   {
      super("err.runtime.uowTimeout", new Object[]{new Long((lTimeout + 999) / 1000)});
   }
}
