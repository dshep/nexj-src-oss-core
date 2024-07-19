// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.text;

import nexj.core.rpc.MarshallerException;

/**
 * Exception thrown when a text marshalling error occurs.
 */
public class TextMarshallerException extends MarshallerException
{
   // constants
   
   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = 7195623810526920508L;

   // constructors
   
   public TextMarshallerException(String sErrCode)
   {
      super(sErrCode);
   }

   public TextMarshallerException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public TextMarshallerException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public TextMarshallerException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
}
