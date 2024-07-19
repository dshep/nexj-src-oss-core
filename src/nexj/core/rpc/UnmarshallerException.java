// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc;

import nexj.core.util.UncheckedException;

/**
 * Exception thrown to indicate an unmarshalling problem.
 */
public class UnmarshallerException extends UncheckedException
{
   /**
    * Serialization version.
    */
   private final static long serialVersionUID = 1494901965865003096L;

   public UnmarshallerException(String sErrCode)
   {
      super(sErrCode);
   }

   public UnmarshallerException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public UnmarshallerException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public UnmarshallerException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
}
