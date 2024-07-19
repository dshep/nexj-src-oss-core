// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc;

/**
 * Exception indicating a problem with the request.
 */
public class RequestException extends RPCException
{
   // constants

   /**
    * Serialization UID.
    */
   private static final long serialVersionUID = 7718207083924858858L;

   // constructors

   public RequestException(String sErrCode)
   {
      super(sErrCode);
   }

   public RequestException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public RequestException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public RequestException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
}
