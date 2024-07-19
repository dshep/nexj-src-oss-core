// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util.auth;

/**
 * Interface implemented by handshake authenticators.
 */
public interface Authenticator
{
   // constants
   
   /**
    * SPNEGO protocol.
    */
   final static int PROTOCOL_SPNEGO = 0;
   
   /**
    * Kerberos protocol.
    */
   final static int PROTOCOL_KERBEROS = 1;
   
   /**
    * NTLM protocol.
    */
   final static int PROTOCOL_NTLM = 2;

   /**
    * Server authentication mode.
    */
   final static int MODE_SERVER = 0x0001;

   /**
    * Mutual authentication mode.
    */
   final static int MODE_MUTUAL = 0x0002;

   /**
    * Delegation authentication mode.
    */
   final static int MODE_DELEGATE = 0x0004;
   
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
   void init(int nProtocol, int nMode, String sService, String sDomain,
      String sUser, char[] achPassword) throws AuthenticationException;

   /**
    * Generates the next token for the authentication sequence.
    * @param token The token received from the peer, null to get the first one. Cannot be null is server mode.
    * @return The next token. Null if the authentication is successful.
    * @throws AuthenticationException if an error occurs.
    */
   byte[] nextToken(byte[] token) throws AuthenticationException;
   
   /**
    * @return True if the authentication sequence has completed.
    */
   boolean isDone();
   
   /**
    * @return The principal name.
    */
   String getPrincipal();

   /**
    * @return The group names.
    */
   String[] getGroups(); 
   
   /**
    * Disposes of any allocated resources.
    */
   void dispose();
}
