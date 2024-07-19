// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util.auth;

/**
 * Exception thrown when the login is denied.
 */
public class LoginException extends AuthenticationException
{
   // constants
   
   /**
    * The serialization UID.
    */
   private final static long serialVersionUID = -7298882428096373046L;

   // constructors

   public LoginException()
   {
      super("err.auth.login");
   }

   public LoginException(String sErrCode)
   {
      super(sErrCode);
   }
}
