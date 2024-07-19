// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

/**
 * Exception thrown when a process has been cancelled by the user.
 */
public class CancellationException extends UncheckedException
{
   // constants
   
   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = -3688856971234069737L;

   // constructors
   
   public CancellationException(String sErrCode)
   {
      super(sErrCode);
   }

   public CancellationException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public CancellationException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public CancellationException(String sErrCode, Object[] argArray, Throwable cause)
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
