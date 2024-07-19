// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration;

/**
 * Integration state exception.
 */
public class StateException extends IntegrationException
{
   // constants
   
   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = -3719330163960320902L;

   // constructors
   
   public StateException(String sErrCode)
   {
      super(sErrCode);
   }

   public StateException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public StateException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public StateException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
}
