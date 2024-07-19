// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.jms.ra;

import javax.jms.Connection;
import javax.jms.ConnectionFactory;
import javax.jms.JMSException;
import javax.resource.ResourceException;
import javax.resource.spi.ConnectionManager;

import nexj.core.rpc.ra.GenericConnectionFactory;

/**
 * The JMS connection factory handle used by JMS clients.
 */
public class JMSConnectionFactory extends GenericConnectionFactory implements ConnectionFactory
{
   // constants
   
   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = -789909540091588892L;

   // associations
   
   /**
    * The managed connection factory.
    */
   protected JMSManagedConnectionFactory m_managedConnectionFactory;
   
   // constructors
   
   /**
    * Creates a new JMS connection factory.
    * 
    * @param managedConnectionFactory The managed connection factory.
    * @param connectionManager The app server's connection manager.
    */
   protected JMSConnectionFactory(JMSManagedConnectionFactory managedConnectionFactory, ConnectionManager connectionManager)
   {
      super(connectionManager);
      m_managedConnectionFactory = managedConnectionFactory;
   }
   
   // opearations

   /**
    * @see javax.jms.ConnectionFactory#createConnection()
    */
   public Connection createConnection() throws JMSException
   {
      try
      {
         return (Connection)m_manager.allocateConnection(m_managedConnectionFactory, null);
      }
      catch (ResourceException e)
      {
         throw JMSResourceAdapter.createJMSException(e);
      }
   }

   /**
    * @see javax.jms.ConnectionFactory#createConnection(java.lang.String, java.lang.String)
    */
   public Connection createConnection(String sUserName, String sPassword) throws JMSException
   {
      try
      {
         return (Connection)m_manager.allocateConnection(m_managedConnectionFactory, new JMSConnectionRequestInfo(sUserName, sPassword));
      }
      catch (ResourceException e)
      {
         throw JMSResourceAdapter.createJMSException(e);
      }
   }
}
