// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.jms.ra.engine;

import java.util.Properties;

import javax.jms.ConnectionFactory;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.Session;
import javax.resource.NotSupportedException;

import nexj.core.rpc.jms.ra.JMSConsumerConfig;

/**
 * This class is used to wrap a managed connection factory into a JMSEngineAdapter interface
 */
public class GenericJMSEngineAdapter implements JMSEngineAdapter
{
   // associations
   
   /**
    * The JMS connection factory.
    */
   protected ConnectionFactory m_jmsConnectionFactory;
   
   // constructors
   
   public GenericJMSEngineAdapter(ConnectionFactory jmsConnectionFactory)
   {
      m_jmsConnectionFactory = jmsConnectionFactory;
   }
   
   // operations

   /**
    * @see nexj.core.util.PropertiesAware#setProperties(java.util.Properties)
    */
   public void setProperties(Properties properties)
   {
   }
   
   /**
    * @see nexj.core.rpc.jms.ra.engine.JMSEngineAdapter#getConnectionConsumerMaxMessageCount(nexj.core.rpc.jms.ra.JMSConsumerConfig)
    */
   public int getConnectionConsumerMaxMessageCount(JMSConsumerConfig consumerConfig)
   {
      return 1;
   }
   
   /**
    * @see nexj.core.rpc.jms.ra.engine.JMSEngineAdapter#getMaxPoolSize(nexj.core.rpc.jms.ra.JMSConsumerConfig)
    */
   public int getMaxPoolSize(JMSConsumerConfig consumerConfig)
   {
      return consumerConfig.getMaxPoolSize();
   }
   
   /**
    * @see nexj.core.rpc.jms.ra.engine.JMSEngineAdapter#getJMSConnectionFactory(boolean, boolean)
    */
   public Object getJMSConnectionFactory(boolean bTransacted, boolean bRecovery) throws Throwable
   {
      return m_jmsConnectionFactory;
   }

   /**
    * @see nexj.core.rpc.jms.ra.engine.JMSEngineAdapter#isSameRMUsed()
    */
   public boolean isSameRMUsed()
   {
      return true;
   }

   /**
    * @see nexj.core.rpc.jms.ra.engine.JMSEngineAdapter#setSharedTransactions(javax.jms.Session, javax.jms.Session)
    */
   public void setSharedTransactions(Session newJMSSession, Session sharedJMSSession) throws Throwable
   {
      throw new NotSupportedException();
   }
   
   /**
    * @see nexj.core.rpc.jms.ra.engine.JMSEngineAdapter#prepareMessage(javax.jms.Message)
    */
   public Message prepareMessage(Message message)
   {
      return message;
   }

   /**
    * @see nexj.core.rpc.jms.ra.engine.JMSEngineAdapter.getSession()
    */
   public Session getSession(Session session) throws JMSException
   {
      return session;
   }
}