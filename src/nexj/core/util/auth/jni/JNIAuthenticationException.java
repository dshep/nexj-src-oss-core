// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util.auth.jni;

/**
 * Authentication exception thrown from the JNIAuthenticator.
 */
public class JNIAuthenticationException extends RuntimeException
{
   // constants

   /**
    * The serialization UID.
    */
   private final static long serialVersionUID = 600669947152240214L;

   // constructors

   /**
    * Constructs the exception.
    * @param sErrorCode The error code.
    */
   public JNIAuthenticationException(String sErrorCode)
   {
      super(sErrorCode);
   }

   // operations

   /**
    * @return The error code.
    */
   public String getErrorCode()
   {
      return getMessage();
   }
}
