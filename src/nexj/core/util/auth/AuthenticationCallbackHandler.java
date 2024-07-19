// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util.auth;

import java.io.IOException;

import javax.security.auth.callback.Callback;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.callback.NameCallback;
import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.callback.UnsupportedCallbackException;

/**
 * Callback handler for authenticating with a user name and password.
 */
public class AuthenticationCallbackHandler implements CallbackHandler
{
   // attributes
   
   /**
    * The user name.
    */
   private String m_sUser;

   /**
    * The password.
    */
   private char[] m_chPasswordArray;

   // constructors

   /**
    * Creates an authentication handler with a user name and password.
    * @param sUser The user name.
    * @param chPasswordArray The password.
    */
   public AuthenticationCallbackHandler(String sUser, char[] chPasswordArray)
   {
      m_sUser = sUser;
      m_chPasswordArray = chPasswordArray;
   }
   
   /**
    * Creates an unitialized authentication handler.
    */
   public AuthenticationCallbackHandler()
   {
   }

   // operations

   /**
    * Sets the user name.
    * @param sUser The user name to set.
    */
   public void setUser(String sUser)
   {
      m_sUser = sUser;
   }

   /**
    * Sets the password.
    * @param chPasswordArray The password to set.
    */
   public void setPassword(char[] chPasswordArray)
   {
      m_chPasswordArray = chPasswordArray;
   }

   /**
    * @see javax.security.auth.callback.CallbackHandler#handle(javax.security.auth.callback.Callback[])
    */
   public void handle(Callback[] callbacks) throws IOException, UnsupportedCallbackException
   {
      for (int i = 0; i < callbacks.length; ++i)
      {
         Callback cb = callbacks[i];
         
         if (cb instanceof NameCallback)
         {
            ((NameCallback)cb).setName(m_sUser);
         }
         else if (cb instanceof PasswordCallback)
         {
            ((PasswordCallback)cb).setPassword(m_chPasswordArray);
         }
      }
   }
}
