// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc;

import nexj.core.util.UncheckedException;

/**
 * Exception thrown when an invalid RPC message is encountered.
 */
public class RPCException extends UncheckedException
{
   // constants
   
   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = 7944508118944601945L;

   // constructors
   
   public RPCException(String sErrCode)
   {
      super(sErrCode);
   }

   public RPCException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public RPCException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public RPCException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
}
