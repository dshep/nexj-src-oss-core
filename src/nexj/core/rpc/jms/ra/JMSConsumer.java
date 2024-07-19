// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.jms.ra;

import java.lang.reflect.Method;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.ServerSession;
import javax.jms.Session;
import javax.jms.XAConnection;
import javax.jms.XASession;
import javax.resource.spi.endpoint.MessageEndpoint;
import javax.transaction.xa.XAResource;

import nexj.core.rpc.jms.JMS;
import nexj.core.rpc.jms.JMSListener;
import nexj.core.rpc.jms.JMSUtil;
import nexj.core.rpc.jms.JMSUtil.MessagePropertyUpdater;
import nexj.core.rpc.ra.TransactionalConsumer;
import nexj.core.rpc.ra.TransactionalResourceAdapter;
import nexj.core.util.Logger;

/**
 * JMS consumer, corresponding to one active instance of an MDB.
 */
public class JMSConsumer extends TransactionalConsumer implements ServerSession, MessageListener
{
   // constants

   /**
    * The JMSListener.onMessage() method object.
    */
   public final static Method JMSLISTENER_ONMESSAGE_METHOD = getMethod(
      JMSListener.class, "onMessage", new Class[]{Message.class});
   
   // attributes

   /**
    * True if the transaction is distributed.
    */
   protected boolean m_bXA;

   // associations

   /**
    * The JMS session.
    */
   protected Session m_session;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(JMSConsumer.class);
   
   /**
    * Message rejection message updater.
    */
   protected final static MessagePropertyUpdater s_rejectionMessageUpdater = new MessagePropertyUpdater()
   {
      public void getProperties(Message message) throws JMSException
      {
      }

      public void setProperties(Message message) throws JMSException
      {
         message.setBooleanProperty(JMS.MESSAGE_REJECTED, true);
      }
   };

   // constructors

   /**
    * Constructs and initializes the consumer.
    * @param pool The consumer pool.
    */
   public JMSConsumer(JMSConsumerPool pool) throws Throwable
   {
      super(pool, s_logger);
   }

   // operations

   /**
    * @see nexj.core.rpc.ra.GenericConsumer#init()
    */
   protected MessageEndpoint init() throws Throwable
   {
      JMSConsumerPool pool = (JMSConsumerPool)m_pool;
      
      XAResource xar = null;

      if (pool.isXA())
      {
         m_session = ((XAConnection)pool.getConnection()).createXASession();
         m_bXA = true;
         xar = ((XASession)m_session).getXAResource();
         m_session = pool.getJMSEngineAdapter().getSession(m_session);
      }
      else
      {
         m_session = pool.getConnection().createSession(m_pool.isTransactional(),
            ((JMSConsumerConfig)m_pool.getConfig()).getAckMode());
      }
      
      m_session.setMessageListener(this);

      return m_pool.getFactory().createEndpoint((m_bXA) ? xar : null);
   }

   /**
    * @see javax.jms.ServerSession#getSession()
    */
   public Session getSession() throws JMSException
   {
      return m_session;
   }

   /**
    * @see javax.jms.ServerSession#start()
    */
   public void start() throws JMSException
   {
      try
      {
         activate();
      }
      catch (Throwable e)
      {
         throw JMSResourceAdapter.createJMSException("Unable to run the JMS consumer", e);
      }
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumer#getMethod()
    */
   protected Method getMethod()
   {
      return JMSLISTENER_ONMESSAGE_METHOD;
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumer#deliver(java.lang.Object)
    */
   protected void deliver(Object obj) throws Throwable
   {
      JMSConsumerPool pool = (JMSConsumerPool)m_pool;
      Message message = pool.getJMSEngineAdapter().prepareMessage((Message)obj);

      pool.checkForDelayedDelivery(message);
      
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Delivering a message to an MDB of " + this);
         s_logger.dump(message);
      }

      ((JMSListener)m_endpoint).onMessage(message);
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumer#reject(java.lang.Object)
    * Marks the message for rejection.  The actual rejection will take place in the MDB.
    */
   protected void reject(Object message)
   {
      Message msg = (Message)message;
      
      try
      {
         JMSUtil.update(msg, s_rejectionMessageUpdater);
         ((JMSListener)m_endpoint).onMessage(msg);
      }
      catch (Throwable t)
      {
         s_logger.error("Unable to reject the message in " + this, t);
      }
   }

   /**
    * @see javax.jms.MessageListener#onMessage(javax.jms.Message)
    */
   public void onMessage(Message message)
   {
      consume(message);
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumer#consume()
    */
   protected boolean consume() throws Throwable
   {
      m_session.run();

      return m_pool.isTransactional() && m_bXA && 
         ((TransactionalResourceAdapter)m_pool.getAdapter()).getTransactionManager() != null;
   }

   /**
    * @see nexj.core.rpc.ra.TransactionalConsumer#commit()
    */
   protected void commit() throws Throwable
   {
      m_session.commit();
   }

   /**
    * @see nexj.core.rpc.ra.TransactionalConsumer#rollback()
    */
   protected void rollback() throws Throwable
   {
      m_session.rollback();
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumer#close()
    */
   protected void close()
   {
      if (m_session != null)
      {
         try
         {
            m_session.close();
         }
         catch (Throwable e)
         {
            if (s_logger.isWarnEnabled())
            {
               s_logger.warn("Unable to close the session of " + this, e);
            }
         }

         m_session = null;
      }
   }
}