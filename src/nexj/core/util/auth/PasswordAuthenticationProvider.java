// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util.auth;

import java.net.PasswordAuthentication;

/**
 * Interface implemented by user/password providers.
 */
public interface PasswordAuthenticationProvider
{
   /**
    * @return The password authentication object.
    */
   PasswordAuthentication getPasswordAuthentication();

   /**
    * @return True if the authentication information is deterministic,
    * i.e. the same on each call.
    */
   boolean isAuthenticationDeterministic();
}
