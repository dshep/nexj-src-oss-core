// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.text;

import nexj.core.rpc.UnmarshallerException;

/**
 * Exception thrown when a text unmarshalling error occurs.
 */
public class TextUnmarshallerException extends UnmarshallerException
{
   /**
    * Serialization version.
    */
   private static final long serialVersionUID = 7977383118288060159L;

   public TextUnmarshallerException(String sErrCode)
   {
      super(sErrCode);
   }

   public TextUnmarshallerException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public TextUnmarshallerException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public TextUnmarshallerException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
}
