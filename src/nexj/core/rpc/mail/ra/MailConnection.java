// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.mail.ra;

import javax.mail.MessagingException;
import javax.mail.Store;
import javax.mail.internet.MimeMessage;
import javax.resource.ResourceException;

import nexj.core.rpc.ra.GenericConnection;
import nexj.core.util.Logger;

/**
 * This is a connection handle for working with a single Mail session.
 * It can be reassociated to a different Session, but should not be unless it is closed first.
 */
public class MailConnection extends GenericConnection implements nexj.core.rpc.mail.MailConnection
{
   // associations

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(MailConnection.class);

   // operations

   /**
    * @see nexj.core.rpc.mail.MailConnection#close()
    */
   public void close()
   {
      try
      {
         closeHandle();
      }
      catch (ResourceException e)
      {
         m_managedConnection = null; // unlink in the event this was not performed
      }
   }

   /**
    * @see nexj.core.rpc.mail.MailConnection#createMessage()
    */
   public MimeMessage createMessage() throws MessagingException
   {
      if (m_managedConnection == null)
      {
         return null;
      }

      MailManagedConnection con = (MailManagedConnection)m_managedConnection;

      if (con.m_conInfo.isInFirst())
      {
         getStore(); // POP/IMAP before SMTP
      }

      return new MimeMessage(con.m_session);
   }

   /**
    * @see nexj.core.rpc.mail.MailConnection#getStore()
    */
   public Store getStore() throws MessagingException
   {
      return (m_managedConnection != null)
             ? ((MailManagedConnection)m_managedConnection).getStore()
             : null;

   }
}