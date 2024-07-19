// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util.auth.jni;

import nexj.core.util.jni.JNIObject;

/**
 * Authenticator using the OS facilities.
 * The whole package has to be installed in a location
 * that is loaded by the system class loaded and never reloaded
 * to avoid the JDK single-class-loader-per-library design goof.
 */
public class JNIAuthenticator extends JNIObject
{
   // initalizers

   static
   {
      System.loadLibrary("nexjauth");
   }

   // operations

   /**
    * Initializes the authentication sequence.
    * @param nProtocol The protocol, one of the PROTOCOL_* constants.
    * @param nMode The authentication mode, combination of the MODE_* flags.
    * @param sService The peer service name.
    * @param sDomain The authentication domain.
    * @param sUser The user name. Null to use the SSO cache.
    * @param achPassword The password. Only valid when sUser is specified. 
    * @throws JNIAuthenticationException if an error occurs.
    */
   public native void init(int nProtocol, int nMode, String sService,
      String sDomain, String sUser, char[] achPassword) throws JNIAuthenticationException;

   /**
    * Generates the next token for the authentication sequence.
    * @param token The token received from the peer, null to get the first one. Cannot be null is server mode.
    * @return The next token. Null if the authentication is successful.
    * @throws JNIAuthenticationException if an error occurs.
    */
   public native byte[] nextToken(byte[] token) throws JNIAuthenticationException;
   
   /**
    * @return True if the authentication sequence has completed.
    */
   public native boolean isDone();
   
   /**
    * @return The principal name.
    */
   public native String getPrincipal();

   /**
    * @return The group names.
    */
   public native String[] getGroups(); 

   /**
    * @see nexj.core.util.jni.JNIObject#dispose()
    */
   public native void dispose();
}
