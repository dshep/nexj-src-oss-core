// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration;

/**
 * Exception thrown when no matching persistent object was found while formatting an object message.
 */
public class ObjectMismatchException extends IntegrationException
{
   // constants

   /**
    * Serialization version.
    */
   private static final long serialVersionUID = -2126064511296409219L;

   // constructors

   public ObjectMismatchException(String sErrCode)
   {
      super(sErrCode);
   }

   public ObjectMismatchException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public ObjectMismatchException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public ObjectMismatchException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
}
