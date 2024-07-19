// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.jms.ra;

import java.io.Serializable;

import javax.jms.BytesMessage;
import javax.jms.Destination;
import javax.jms.IllegalStateException;
import javax.jms.JMSException;
import javax.jms.MapMessage;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageListener;
import javax.jms.MessageProducer;
import javax.jms.ObjectMessage;
import javax.jms.Queue;
import javax.jms.QueueBrowser;
import javax.jms.Session;
import javax.jms.StreamMessage;
import javax.jms.TemporaryQueue;
import javax.jms.TemporaryTopic;
import javax.jms.TextMessage;
import javax.jms.Topic;
import javax.jms.TopicSubscriber;


/**
 * This class wraps a physical JMS session. The main reason for this class is to implement
 * logical closing so that an underlying physical session can be cached (in JMSManagedConnection).
 */
public class JMSSession implements Session
{
   // attributes
   
   /**
    * True if this session is logically closed.
    */
   protected boolean m_bClosed;
   
   // associations
   
   /**
    * The JMS connection.
    */
   protected JMSConnection m_jmsConnection;
   
   /**
    * The physical JMS session.
    */
   protected Session m_physicalJMSSession;
   
   // constructors
   
   /**
    * @param jmsConnection The JMS connection.
    * @param physicalJMSSession The physical JMS session.
    */
   public JMSSession(JMSConnection jmsConnection, Session physicalJMSSession)
   {
      m_jmsConnection = jmsConnection;
      m_physicalJMSSession = physicalJMSSession;
   }

   // operations

   /**
    * Used to check if an attempt to use a (logically) closed session has ben made.
    * @throws IllegalStateException
    */
   protected void checkClosed() throws IllegalStateException
   {
      if (m_bClosed)
      {
         throw new IllegalStateException("Attempt to use a closed JMS session " + this);
      }
   }
   
   /**
    * @see javax.jms.Session#close()
    */
   public void close() throws JMSException
   {
      if (!m_bClosed)
      {
         m_bClosed = true;

         if (!m_jmsConnection.isCachedSession(m_physicalJMSSession))
         {
            m_physicalJMSSession.close();
         }
         
         m_jmsConnection.removeSession(this);
      }
   }

   /**
    * @see javax.jms.Session#commit()
    */
   public void commit() throws JMSException
   {
      checkClosed();
      
      m_physicalJMSSession.commit();
   }

   /**
    * @see javax.jms.Session#createBrowser(javax.jms.Queue)
    */
   public QueueBrowser createBrowser(Queue queue) throws JMSException
   {
      checkClosed();
      
      return m_physicalJMSSession.createBrowser(JMSResourceAdapter.getJMSDestination(queue));
   }

   /**
    * @see javax.jms.Session#createBrowser(javax.jms.Queue, java.lang.String)
    */
   public QueueBrowser createBrowser(Queue queue, String sMessageSelector) throws JMSException
   {
      checkClosed();
      
      return m_physicalJMSSession.createBrowser(JMSResourceAdapter.getJMSDestination(queue), sMessageSelector);
   }

   /**
    * @see javax.jms.Session#createBytesMessage()
    */
   public BytesMessage createBytesMessage() throws JMSException
   {
      return m_physicalJMSSession.createBytesMessage();
   }

   /**
    * @see javax.jms.Session#createConsumer(javax.jms.Destination)
    */
   public MessageConsumer createConsumer(Destination destination) throws JMSException
   {
      checkClosed();
      
      return m_physicalJMSSession.createConsumer(JMSResourceAdapter.getJMSDestination(destination));
   }

   /**
    * @see javax.jms.Session#createConsumer(javax.jms.Destination, java.lang.String)
    */
   public MessageConsumer createConsumer(Destination destination, String sMessageSelector) throws JMSException
   {
      checkClosed();
      
      return m_physicalJMSSession.createConsumer(JMSResourceAdapter.getJMSDestination(destination), sMessageSelector);
   }

   /**
    * @see javax.jms.Session#createConsumer(javax.jms.Destination, java.lang.String, boolean)
    */
   public MessageConsumer createConsumer(Destination destination, String sMessageSelector, boolean bNoLocal) throws JMSException
   {
      checkClosed();
      
      return m_physicalJMSSession.createConsumer(
         JMSResourceAdapter.getJMSDestination(destination), sMessageSelector, bNoLocal);
   }

   /**
    * @see javax.jms.Session#createDurableSubscriber(javax.jms.Topic, java.lang.String)
    */
   public TopicSubscriber createDurableSubscriber(Topic topic, String sName) throws JMSException
   {
      checkClosed();
      
      return m_physicalJMSSession.createDurableSubscriber(JMSResourceAdapter.getJMSDestination(topic), sName);
   }

   /**
    * @see javax.jms.Session#createDurableSubscriber(javax.jms.Topic, java.lang.String, java.lang.String, boolean)
    */
   public TopicSubscriber createDurableSubscriber(Topic topic, String sName, String sMessageSelector, boolean bNoLocal) throws JMSException
   {
      checkClosed();
      
      return m_physicalJMSSession.createDurableSubscriber(
         JMSResourceAdapter.getJMSDestination(topic), sName, sMessageSelector, bNoLocal);
   }

   /**
    * @see javax.jms.Session#createMapMessage()
    */
   public MapMessage createMapMessage() throws JMSException
   {
      return m_physicalJMSSession.createMapMessage();
   }

   /**
    * @see javax.jms.Session#createMessage()
    */
   public Message createMessage() throws JMSException
   {
      return m_physicalJMSSession.createMessage();
   }

   /**
    * @see javax.jms.Session#createObjectMessage()
    */
   public ObjectMessage createObjectMessage() throws JMSException
   {
      return m_physicalJMSSession.createObjectMessage();
   }

   /**
    * @see javax.jms.Session#createObjectMessage(java.io.Serializable)
    */
   public ObjectMessage createObjectMessage(Serializable object) throws JMSException
   {
      return m_physicalJMSSession.createObjectMessage(object);
   }

   /**
    * @see javax.jms.Session#createProducer(javax.jms.Destination)
    */
   public MessageProducer createProducer(Destination destination) throws JMSException
   {
      checkClosed();
      
      return m_physicalJMSSession.createProducer(JMSResourceAdapter.getJMSDestination(destination));
   }

   /**
    * @see javax.jms.Session#createQueue(java.lang.String)
    */
   public Queue createQueue(String sQueueName) throws JMSException
   {
      checkClosed();
      
      return m_physicalJMSSession.createQueue(sQueueName);
   }

   /**
    * @see javax.jms.Session#createStreamMessage()
    */
   public StreamMessage createStreamMessage() throws JMSException
   {
      return m_physicalJMSSession.createStreamMessage();
   }

   /**
    * @see javax.jms.Session#createTemporaryQueue()
    */
   public TemporaryQueue createTemporaryQueue() throws JMSException
   {
      checkClosed();
      
      return m_physicalJMSSession.createTemporaryQueue();
   }

   /**
    * @see javax.jms.Session#createTemporaryTopic()
    */
   public TemporaryTopic createTemporaryTopic() throws JMSException
   {
      checkClosed();
      
      return m_physicalJMSSession.createTemporaryTopic();
   }

   /**
    * @see javax.jms.Session#createTextMessage()
    */
   public TextMessage createTextMessage() throws JMSException
   {
      return m_physicalJMSSession.createTextMessage();
   }

   /**
    * @see javax.jms.Session#createTextMessage(java.lang.String)
    */
   public TextMessage createTextMessage(String sText) throws JMSException
   {
      return m_physicalJMSSession.createTextMessage(sText);
   }

   /**
    * @see javax.jms.Session#createTopic(java.lang.String)
    */
   public Topic createTopic(String sTopicName) throws JMSException
   {
      checkClosed();
      
      return m_physicalJMSSession.createTopic(sTopicName);
   }

   /**
    * @see javax.jms.Session#getAcknowledgeMode()
    */
   public int getAcknowledgeMode() throws JMSException
   {
      checkClosed();
      
      return m_physicalJMSSession.getAcknowledgeMode();
   }

   /**
    * @see javax.jms.Session#getMessageListener()
    */
   public MessageListener getMessageListener() throws JMSException
   {
      checkClosed();
      
      return m_physicalJMSSession.getMessageListener();
   }

   /**
    * @see javax.jms.Session#getTransacted()
    */
   public boolean getTransacted() throws JMSException
   {
      checkClosed();
      
      return m_physicalJMSSession.getTransacted();
   }

   /**
    * @see javax.jms.Session#recover()
    */
   public void recover() throws JMSException
   {
      checkClosed();
      
      m_physicalJMSSession.recover();
   }

   /**
    * @see javax.jms.Session#rollback()
    */
   public void rollback() throws JMSException
   {
      checkClosed();
      
      m_physicalJMSSession.rollback();
   }

   /**
    * @see javax.jms.Session#run()
    */
   public void run()
   {
      m_physicalJMSSession.run();
   }

   /**
    * @see javax.jms.Session#setMessageListener(javax.jms.MessageListener)
    */
   public void setMessageListener(MessageListener listener) throws JMSException
   {
      checkClosed();
      
      m_physicalJMSSession.setMessageListener(listener);
   }

   /**
    * @see javax.jms.Session#unsubscribe(java.lang.String)
    */
   public void unsubscribe(String sName) throws JMSException
   {
      checkClosed();
      
      m_physicalJMSSession.unsubscribe(sName);
   }
}
