// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.mail;

import java.util.Properties;

import javax.resource.ResourceException;

/**
 * A factory for Mail connections.
 * An interface to be used outside of the ResourceAdapter.
 */
public interface MailConnectionFactory
{
   // operations

   /**
    * Opens a connection to the mail store.
    * @param conf The mail session configuration properties.
    * @param sUser The authentication user name override.
    * @param sPassword The authentication password override (used if sUser not null).
    * @param bInFirst Mail send requires authentication against mail store.
    * @return The connection to the mail store.
    * @throws ResourceException if an error occurs.
    */
   public MailConnection open(Properties conf, String sUser, String sPassword, boolean bInFirst)
      throws ResourceException;
}