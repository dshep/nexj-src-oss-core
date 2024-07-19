// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc;

/**
 * Generic exception indicating a problem with the server, as opposed to the request.
 */
public class ServerException extends RPCException
{
   // constants

   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = 4013468714028343710L;

   // constructors

   public ServerException(String sErrCode)
   {
      super(sErrCode);
   }

   public ServerException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public ServerException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public ServerException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
}
