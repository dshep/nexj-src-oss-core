// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

import nexj.core.util.UncheckedException;

/**
 * Exception thrown when a security violation is detected.
 */
public class SecurityViolationException extends UncheckedException
{
   // constants
   
   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = 4599306191390607086L;

   // constructors
   
   public SecurityViolationException(String sErrCode)
   {
      super(sErrCode);
   }

   public SecurityViolationException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public SecurityViolationException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public SecurityViolationException(String sErrCode, Object[] argArray, Throwable cause)
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
