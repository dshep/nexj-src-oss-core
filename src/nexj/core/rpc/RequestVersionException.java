// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc;

/**
 * Exception indicating a request version mismatch.
 */
public class RequestVersionException extends RPCException
{
   // constants

   /**
    * Serialization UID.
    */
   private static final long serialVersionUID = -6118481380470930210L;

   // constructors

   public RequestVersionException(String sErrCode)
   {
      super(sErrCode);
   }

   public RequestVersionException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public RequestVersionException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public RequestVersionException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }

   // operations

   /**
    * @see nexj.core.util.UncheckedException#isSystem()
    */
   public boolean isSystem()
   {
      return false;
   }
}
