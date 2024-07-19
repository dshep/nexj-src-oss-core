// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util.auth;

import nexj.core.util.auth.jni.JNIAuthenticationException;
import nexj.core.util.auth.jni.JNIAuthenticator;
import nexj.core.util.auth.jni.JNILoginException;

/**
 * Authenticator using the OS facilities.
 */
public class NativeAuthenticator implements Authenticator
{
   // associations

   /**
    * The nested authenticator.
    */
   protected JNIAuthenticator m_auth = new JNIAuthenticator();

   // operations

   /**
    * Initializes the authentication sequence.
    * @param nProtocol The protocol, one of the PROTOCOL_* constants.
    * @param nMode The authentication mode, combination of the MODE_* flags.
    * @param sService The peer service name.
    * @param sDomain The authentication domain.
    * @param sUser The user name. Null to use the SSO cache.
    * @param achPassword The password. Only valid when sUser is specified. 
    * @throws AuthenticationException if an error occurs.
    */
   public void init(int nProtocol, int nMode, String sService,
      String sDomain, String sUser, char[] achPassword) throws AuthenticationException
   {
      try
      {
         m_auth.init(nProtocol, nMode, sService, sDomain, sUser, achPassword);
      }
      catch (JNILoginException e)
      {
         throw new LoginException(e.getErrorCode());
      }
      catch (JNIAuthenticationException e)
      {
         throw new AuthenticationException(e.getErrorCode());
      }
   }

   /**
    * Generates the next token for the authentication sequence.
    * @param token The token received from the peer, null to get the first one. Cannot be null is server mode.
    * @return The next token. Null if the authentication is successful.
    * @throws AuthenticationException if an error occurs.
    */
   public byte[] nextToken(byte[] token) throws AuthenticationException
   {
      try
      {
         return m_auth.nextToken(token);
      }
      catch (JNILoginException e)
      {
         throw new LoginException(e.getErrorCode());
      }
      catch (JNIAuthenticationException e)
      {
         throw new AuthenticationException(e.getErrorCode());
      }
   }

   /**
    * @return True if the authentication sequence has completed.
    */
   public boolean isDone()
   {
      return m_auth.isDone();
   }
   
   /**
    * @return The principal name.
    */
   public String getPrincipal()
   {
      return m_auth.getPrincipal();
   }

   /**
    * @return The group names.
    */
   public String[] getGroups()
   {
      return m_auth.getGroups();
   }

   /**
    * @see nexj.core.util.jni.JNIObject#dispose()
    */
   public void dispose()
   {
      if (m_auth != null)
      {
         m_auth.dispose();
         m_auth = null;
      }
   }
}
