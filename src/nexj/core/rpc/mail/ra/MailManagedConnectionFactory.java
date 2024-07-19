// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.mail.ra;

import javax.mail.Authenticator;
import javax.mail.PasswordAuthentication;
import javax.mail.Session;
import javax.resource.ResourceException;
import javax.resource.spi.ConnectionManager;
import javax.resource.spi.ConnectionRequestInfo;
import javax.resource.spi.ManagedConnection;
import javax.security.auth.Subject;

import nexj.core.rpc.ra.GenericManagedConnectionFactory;
import nexj.core.util.Logger;

/**
 * Mail managed connection factory.
 */
public class MailManagedConnectionFactory extends GenericManagedConnectionFactory
{
   // attributes

   /**
    * Serialization UID.
    */
   private static final long serialVersionUID = 7471658880077649335L;

   /**
    * The default class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(MailManagedConnectionFactory.class);

   // operations

   /**
    * @see nexj.core.rpc.ra.GenericManagedConnectionFactory#getDefaultConnectionManager()
    */
   protected ConnectionManager getDefaultConnectionManager()
   {
      return MailResourceAdapter.getDefaultConnectionManager();
   }

   /**
    * @see javax.resource.spi.ManagedConnectionFactory#createConnectionFactory(javax.resource.spi.ConnectionManager)
    */
   public Object createConnectionFactory(ConnectionManager manager) throws ResourceException
   {
      return new MailConnectionFactory(this, manager);
   }

   /**
    * @see javax.resource.spi.ManagedConnectionFactory#createManagedConnection(javax.security.auth.Subject, javax.resource.spi.ConnectionRequestInfo)
    */
   public ManagedConnection createManagedConnection(Subject subject, ConnectionRequestInfo cri)
      throws ResourceException
   {
      MailConnectionRequestInfo conf = (MailConnectionRequestInfo)cri;
      Session session = null;

      if (conf.getUser() != null)
      {
         final PasswordAuthentication auth =
            new PasswordAuthentication(conf.getUser(), conf.getPassword());
         Authenticator authenticator = new Authenticator()
         {
            protected PasswordAuthentication getPasswordAuthentication()
            {
               return auth;
            }
         };

         session = Session.getInstance(conf.getProperties(), authenticator);
      }
      else
      {
         session = Session.getInstance(conf.getProperties());
      }

      session.setDebug(s_logger.isDebugEnabled()); // inherit debug from factory

      return new MailManagedConnection(session, conf);
   }
}