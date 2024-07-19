// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.soap;

import nexj.core.rpc.UnmarshallerException;

/**
 * Exception thrown to indicate a SOAP unmarshalling problem.
 */
public class SOAPUnmarshallerException extends UnmarshallerException
{
   /**
    * Serial version.
    */
   private static final long serialVersionUID = -3776888521949784699L;

   public SOAPUnmarshallerException(String sErrCode)
   {
      super(sErrCode);
   }

   public SOAPUnmarshallerException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public SOAPUnmarshallerException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public SOAPUnmarshallerException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
}
