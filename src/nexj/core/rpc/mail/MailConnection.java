// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.mail;

import javax.mail.MessagingException;
import javax.mail.Store;
import javax.mail.internet.MimeMessage;

/**
 * A connection to a mail resource.
 * An interface to be used outside of the ResourceAdapter.
 */
public interface MailConnection
{
   // operations

   /**
    * Closes the connection.
    */
   public void close();

   /**
    * Create a new message to be sent over this connection.
    * @return Message sendable over this connection or null for closed connections.
    * @throws MessagingException On mail creation error.
    */
   public MimeMessage createMessage() throws MessagingException;

   /**
    * Get the Mail Store of this connection (the store will be closed upon connection handle close).
    * @return The Mail store of this connection or null for closed connections.
    * @throws MessagingException On Mail Store open failure.
    */
   public Store getStore() throws MessagingException;
}