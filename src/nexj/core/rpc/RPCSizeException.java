// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc;

/**
 * Thrown when a too large RPC message is detected.
 */
public class RPCSizeException extends RequestException
{
   // constants

   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = -2803306071186718825L;

   // constructors
   
   public RPCSizeException(String sErrCode)
   {
      super(sErrCode);
   }

   public RPCSizeException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public RPCSizeException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public RPCSizeException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
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
