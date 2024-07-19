// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util.auth;

import nexj.core.util.UncheckedException;

/**
 * Exception thrown of authentication failure.
 */
public class AuthenticationException extends UncheckedException
{
   // constants
   
   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = -4700839242279836047L;

   // constructors
   
   public AuthenticationException(String sErrCode)
   {
      super(sErrCode);
   }

   public AuthenticationException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public AuthenticationException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public AuthenticationException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
}
