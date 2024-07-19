// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.mail.ra;

import javax.mail.MessagingException;
import javax.mail.Session;
import javax.mail.Store;
import javax.mail.URLName;
import javax.resource.ResourceException;
import javax.resource.spi.ConnectionRequestInfo;
import javax.security.auth.Subject;

import nexj.core.rpc.ra.GenericConnection;
import nexj.core.rpc.ra.GenericConnectionRequestInfo;
import nexj.core.rpc.ra.UnsharedManagedConnection;
import nexj.core.util.Logger;
import nexj.core.util.ObjUtil;

/**
 * A Mail Session wrapper (non-transactional).
 */
public class MailManagedConnection extends UnsharedManagedConnection
{
   // associations

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(MailManagedConnection.class);

   /**
    * The connection request information used for connection identification, e.g. matches().
    */
   protected MailConnectionRequestInfo m_conInfo;

   /**
    * The mail session.
    */
   protected Session m_session;

   /**
    * The Mail Store of this connection (lazy init).
    */
   protected Store m_store;

   // constructors

   /**
    * Constructor.
    * @param session The mail session that will provide handles (not null).
    * @param conInfo Connection request information used for connection identification, matches().
    */
   public MailManagedConnection(Session session, MailConnectionRequestInfo conInfo)
   {
      assert session != null;
      assert conInfo != null;

      m_session = session;
      m_conInfo = conInfo;
   }

   // operations

   /**
    * @see nexj.core.rpc.ra.UnsharedManagedConnection#closeConnections()
    */
   protected synchronized void closeConnections() throws ResourceException
   {
      super.closeConnections();

      if (m_store != null)
      {
         try
         {
            m_store.close();
         }
         catch (MessagingException e) // ignore exception since closing handle anyway
         {
            s_logger.debug("Error closing the store", e);
         }

         m_store = null;
      }
   }

   /**
    * @see nexj.core.rpc.ra.GenericManagedConnection#createConnection(javax.security.auth.Subject, javax.resource.spi.ConnectionRequestInfo)
    */
   protected GenericConnection createConnection(Subject subject, ConnectionRequestInfo cri)
      throws ResourceException
   {
      return new MailConnection(); // the empty handle will be told its managed connection
   }

   /**
    * Get the Mail Store of this connection (the store will be closed upon connection handle close).
    * @return The Mail store of this connection or null for closed connections.
    * @throws MessagingException On Mail Store open failure.
    */
   public Store getStore() throws MessagingException
   {
      if (m_store == null)
      {
         // kludge for javamaildir/mstor and anyone else needing to specify URLName explicitly
         String sURL = m_conInfo.getProperties().getProperty("mail.store.url");

         m_store = (sURL == null) ? m_session.getStore() : m_session.getStore(new URLName(sURL));
      }

      if (!m_store.isConnected())
      {
         m_store.connect();
      }

      return m_store;
   }

   /**
    * @see nexj.core.rpc.ra.GenericManagedConnection#matches(javax.security.auth.Subject, nexj.core.rpc.ra.GenericConnectionRequestInfo)
    */
   public boolean matches(Subject subject, GenericConnectionRequestInfo cri)
   {
      return ObjUtil.equal(m_conInfo, cri);
   }
}