// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc;

/**
 * Exception thrown if an RPC transport connection cannot be established.
 */
public class ConnectivityException extends RPCException
{
   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = -1658124023986364462L;

   public ConnectivityException()
   {
      super("err.rpc.connect");
   }
   
   public ConnectivityException(String sErrCode)
   {
      super(sErrCode);
   }

   public ConnectivityException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public ConnectivityException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public ConnectivityException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
}
