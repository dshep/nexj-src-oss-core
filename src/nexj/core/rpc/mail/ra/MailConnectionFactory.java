// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.mail.ra;

import java.util.Properties;

import javax.resource.ResourceException;
import javax.resource.spi.ConnectionManager;

import nexj.core.rpc.mail.MailConnection;
import nexj.core.rpc.ra.GenericConnectionFactory;

/**
 * Mail connection factory.
 */
public class MailConnectionFactory
   extends GenericConnectionFactory implements nexj.core.rpc.mail.MailConnectionFactory
{
	// attributes

   /**
    * Serialization UID.
    */
   private static final long serialVersionUID = -6459772823724396484L;

   // associations

   /**
    * The factory used in creating new connections by the connection manager.
    */
   protected MailManagedConnectionFactory m_factory;

   // constructors

   /**
    * Constructor
    * @param factory The factory used in creating new connections.
    * @param manager The connection manager used in requesting connections from factory.
    */
   protected MailConnectionFactory(MailManagedConnectionFactory factory, ConnectionManager manager)
   {
      super(manager);

      m_factory = factory;
   }

   // operations

   /**
    * @see nexj.core.rpc.mail.MailConnectionFactory#open(java.util.Properties, java.lang.String, java.lang.String, boolean)
    */
   public MailConnection open(Properties conf, String sUser, String sPassword, boolean bInFirst)
      throws ResourceException
   {
      MailConnectionRequestInfo info =
         new MailConnectionRequestInfo(sUser, sPassword, bInFirst, conf);

      return (nexj.core.rpc.mail.MailConnection)m_manager.allocateConnection(m_factory, info);
   }
}