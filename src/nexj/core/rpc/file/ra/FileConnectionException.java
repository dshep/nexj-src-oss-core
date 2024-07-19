// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.file.ra;

import nexj.core.rpc.RPCException;

/**
 * Exception thrown when a problem is encountered by the file
 * resource adapter.
 */
public class FileConnectionException extends RPCException
{
   // constants

   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = 1923056194371284246L;

   // constructors

   public FileConnectionException(String sErrCode)
   {
      super(sErrCode);
   }

   public FileConnectionException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public FileConnectionException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public FileConnectionException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
}