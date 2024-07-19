// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.jms.ra;

import javax.jms.Connection;
import javax.jms.ConnectionConsumer;
import javax.jms.ConnectionFactory;
import javax.jms.Destination;
import javax.jms.ExceptionListener;
import javax.jms.JMSException;
import javax.jms.ServerSession;
import javax.jms.ServerSessionPool;
import javax.jms.Topic;
import javax.jms.XAConnectionFactory;
import javax.resource.ResourceException;
import javax.resource.spi.endpoint.MessageEndpointFactory;

import nexj.core.rpc.jms.ra.engine.JMSEngineAdapter;
import nexj.core.rpc.ra.GenericConsumer;
import nexj.core.util.Logger;

/**
 * JMS consumer endpoint pool, corresponding to the MDBs with the same configuration object.
 */
public class JMSConsumerPool extends JMSDynamicSelectorConsumerPool implements ServerSessionPool, ExceptionListener
{
   // attributes

   /**
    * True if the message delivery is transactional (either distributed or jms local).
    */
   protected boolean m_bTransactional;
   
   /**
    * True if message delivery is to be performed within a distributed transaction.
    */
   protected boolean m_bXA;
   
   /**
    * The maximun number of concurrent consumers for this pool.
    */
   protected int m_nMaxPoolSize;

   // associations
   
   /**
    * The JMS engine adapter.
    */
   protected JMSEngineAdapter m_jmsEngineAdapter;

   /**
    * The destination connection.
    */
   protected Connection m_connection;

   /**
    * The connection consumer.
    */
   protected ConnectionConsumer m_consumer;

   /**
    * The JMS destination.
    */
   protected Destination m_destination;
   
   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(JMSConsumerPool.class);

   // constructors

   /**
    * Constructs the consumer pool.
    * @param adapter The resource adapter.
    * @param factory The endpoint factory.
    * @param config The pool configuration.
    * @throws ResourceException if an initialization error occurs.
    */
   public JMSConsumerPool(JMSResourceAdapter adapter, MessageEndpointFactory factory,
      JMSConsumerConfig config) throws ResourceException
   {
      super(adapter, factory, config, s_logger);

      try
      {
         m_bTransactional = factory.isDeliveryTransacted(JMSConsumer.JMSLISTENER_ONMESSAGE_METHOD);
      }
      catch (NoSuchMethodException e)
      {
         throw new ResourceException("Unable to determine if the delivery is transactional", e);
      }
   }

   // operations

   /**
    * @see nexj.core.rpc.ra.GenericConsumerPool#isTransactional()
    */
   public boolean isTransactional()
   {
      return m_bTransactional;
   }

   /**
    * @return The JMS engine adapter.
    */
   public JMSEngineAdapter getJMSEngineAdapter()
   {
      return m_jmsEngineAdapter;
   }
   
   /**
    * @return The JMS destination.
    */
   public synchronized Destination getDestination()
   {
      return m_destination;
   }
   
   /**
    * @return The JMS connection.
    */
   public synchronized Connection getConnection()
   {
      return m_connection;
   }

   /**
    * @return True if the message delivery should be done in a distributed transaction.
    */
   public synchronized boolean isXA()
   {
      return m_bXA;
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumerPool#createConsumer()
    */
   protected GenericConsumer createConsumer() throws Throwable
   {
      return new JMSConsumer(this);
   }

   /**
    * @see javax.jms.ServerSessionPool#getServerSession()
    */
   public ServerSession getServerSession() throws JMSException
   {
      try
      {
         return (ServerSession)getConsumer();
      }
      catch (JMSException e)
      {
         throw e;
      }
      catch (Throwable e)
      {
         throw JMSResourceAdapter.createJMSException("Unable to get a JMS consumer", e);
      }
   }
   
   /**
    * @see nexj.core.rpc.ra.GenericConsumerPool#getMaxPoolSize()
    */
   protected int getMaxPoolSize()
   {
      return m_nMaxPoolSize;
   }

   /**
    * @see javax.jms.ExceptionListener#onException(javax.jms.JMSException)
    */
   public void onException(JMSException e)
   {
      s_logger.error("Error in " + this, e);
      reconnect();
   }

   /**
    * Connects to the destination.
    */
   protected synchronized void connect() throws Throwable
   {
      JMSConsumerConfig config = (JMSConsumerConfig)m_config;

      m_jmsEngineAdapter = (JMSEngineAdapter)JMSResourceAdapter.getJMSEngineAdapter(
         config.getConnectionFactory(), config.getConnectionFactoryPropertyMap());

      Object jmsConnFactory = m_jmsEngineAdapter.getJMSConnectionFactory(m_bTransactional, false);

      m_destination = (Destination)JMSResourceAdapter.getResource(
         config.getDestination(), config.getDestinationPropertyMap());

      m_bXA = jmsConnFactory instanceof XAConnectionFactory &&
         (isTransactional() || !(jmsConnFactory instanceof ConnectionFactory));

      if (m_bXA)
      {
         if (config.getUser() != null)
         {
            m_connection = ((XAConnectionFactory)jmsConnFactory).createXAConnection(
               config.getUser(), config.getPassword());
         }
         else
         {
            m_connection = ((XAConnectionFactory)jmsConnFactory).createXAConnection();
         }
      }
      else
      {
         if (config.getUser() != null)
         {
            m_connection = ((ConnectionFactory)jmsConnFactory).createConnection(
               config.getUser(), config.getPassword());
         }
         else
         {
            m_connection = ((ConnectionFactory)jmsConnFactory).createConnection();
         }
      }

      if (config.getClientId() != null)
      {
         m_connection.setClientID(config.getClientId());
      }

      m_connection.setExceptionListener(this);

      if (config.isDurable())
      {
         m_consumer = m_connection.createDurableConnectionConsumer((Topic)m_destination,
            config.getSubscriptionName(), config.getMessageSelector(), this, m_jmsEngineAdapter.getConnectionConsumerMaxMessageCount(config));
      }
      else
      {
         m_consumer = m_connection.createConnectionConsumer(m_destination,
            config.getMessageSelector(), this, m_jmsEngineAdapter.getConnectionConsumerMaxMessageCount(config));
      }

      m_nMaxPoolSize = m_jmsEngineAdapter.getMaxPoolSize(config);

      m_connection.start();

      s_logger.debug("Connection started for " + this);
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumerPool#stop()
    */
   protected void stop()
   {
      if (m_connection != null)
      {
         try
         {
            if (m_logger.isDebugEnabled())
            {
               m_logger.debug("Stopping connection in " + this);
            }
            
            m_connection.stop();
         }
         catch (Throwable e)
         {
            s_logger.error("Unable to stop the connection of " + this, e);
         }
      }

      if (m_consumer != null)
      {
         try
         {
            if (m_logger.isDebugEnabled())
            {
               m_logger.debug("Closing connection consumer in " + this);
            }
            
            m_consumer.close();
         }
         catch (Throwable e)
         {
            s_logger.error("Unable to close the consumer of " + this, e);
         }

         m_consumer = null;
      }
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumerPool#close()
    */
   protected void close()
   {
      if (m_connection != null)
      {
         try
         {
            m_connection.close();
         }
         catch (Throwable e)
         {
            s_logger.error("Unable to close the connection of " + this, e);
         }

         m_connection = null;
      }

      m_destination = null;
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumerPool#resetMessageSelector()
    */
   protected void resetMessageSelector()
   {
      stop();

      try
      {
         String sMessageSelector = getMessageSelector();

         m_consumer = m_connection.createConnectionConsumer(m_destination, sMessageSelector, this, 1);
         m_connection.start();

         if (m_logger.isDebugEnabled())
         {
            m_logger.debug("Resetting message selector to " + sMessageSelector + " in " + this);
         }
      }
      catch (JMSException e)
      {
         s_logger.error("Unable to restart consumer pool after selector update in " + this, e);
      }
   }
}