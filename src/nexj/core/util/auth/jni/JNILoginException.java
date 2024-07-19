// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util.auth.jni;

/**
 * Exception thrown by the JNI authenticator when the login is denied.
 */
public class JNILoginException extends JNIAuthenticationException
{
   // constants

   /**
    * The serialization UID.
    */
   private final static long serialVersionUID = 4592467617835932219L;

   // constructors

   /**
    * Constructs the exception.
    */
   public JNILoginException()
   {
      this("err.auth.login");
   }

   /**
    * Constructs the exception.
    * @param sErrorCode The error code.
    */
   public JNILoginException(String sErrorCode)
   {
      super(sErrorCode);
   }
}
