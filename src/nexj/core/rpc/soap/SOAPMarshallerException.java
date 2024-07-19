// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.soap;

import nexj.core.rpc.MarshallerException;

/**
 * Exception thrown to indicate a SOAP marshalling problem.
 */
public class SOAPMarshallerException extends MarshallerException
{
   // constants

   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = -6641954773643623057L;

   // constructors
   
   public SOAPMarshallerException(String sErrCode)
   {
      super(sErrCode);
   }

   public SOAPMarshallerException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public SOAPMarshallerException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public SOAPMarshallerException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
}
