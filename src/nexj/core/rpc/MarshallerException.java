// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc;

import nexj.core.util.UncheckedException;

/**
 * Exception thrown to indicate a marshalling problem.
 */
public class MarshallerException extends UncheckedException
{
   // constants
   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = 6058452036908178852L;

   // constructors
   
   public MarshallerException(String sErrCode)
   {
      super(sErrCode);
   }

   public MarshallerException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public MarshallerException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public MarshallerException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
}
