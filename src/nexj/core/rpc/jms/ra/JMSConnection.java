// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.jms.ra;

import java.util.Iterator;
import java.util.Set;

import javax.jms.Connection;
import javax.jms.ConnectionConsumer;
import javax.jms.ConnectionMetaData;
import javax.jms.Destination;
import javax.jms.ExceptionListener;
import javax.jms.JMSException;
import javax.jms.Queue;
import javax.jms.QueueConnection;
import javax.jms.QueueSession;
import javax.jms.ServerSessionPool;
import javax.jms.Session;
import javax.jms.Topic;
import javax.jms.TopicConnection;
import javax.jms.TopicSession;
import javax.resource.ResourceException;

import nexj.core.rpc.ra.GenericConnection;
import nexj.core.util.IdentityHashHolder;

/**
 * The JMS connection handle used by JMS clients.
 */
public class JMSConnection extends GenericConnection implements Connection, QueueConnection, TopicConnection
{
   // associations
   
   /**
    * Holds JMS sessions created with this connection.
    */
   protected Set m_jmsSessionSet = new IdentityHashHolder(2);

   // opearations
   
   /**
    * @return The underlying physical JMS connection.
    */
   public Connection getPhysicalJMSConnection()
   {
      return ((JMSManagedConnection)m_managedConnection).m_physicalJMSConnection;
   }
   
   /**
    * @return The cached physical JMS session.
    */
   public Session getCachedPhysicalJMSSession()
   {
      return ((JMSManagedConnection)m_managedConnection).m_physicalJMSSession;
   }
   
   /**
    * @param session The session to compare with the cached one
    * @return true is the session is the same as the cached one, false otherwise
    */
   public boolean isCachedSession(Session session) throws JMSException
   {
      return ((JMSManagedConnection)m_managedConnection).getJmsEngineAdapter().getSession(getCachedPhysicalJMSSession()) == session;
   }
   
   /**
    * @see javax.jms.Connection#close()
    */
   public synchronized void close() throws JMSException
   {
      for (Iterator iter = m_jmsSessionSet.iterator(); iter.hasNext();)
      {
         JMSSession jmsSession = (JMSSession)iter.next();
         
         iter.remove();
         jmsSession.close();
      }
      
      try
      {
         closeHandle();
      }
      catch (ResourceException e)
      {
         throw JMSResourceAdapter.createJMSException(e);
      }
   }
   
   /**
    * Removes a JMS session originally created with this connection from m_jmsSessionsList.
    * If this method is called indirectly as a result of this.close(), the session will be already removed (by this.close())
    * @param jmsSession The JMS session to remove.
    */
   protected synchronized void removeSession(JMSSession jmsSession)
   {
      m_jmsSessionSet.remove(jmsSession);
   }

   /**
    * @see javax.jms.Connection#createConnectionConsumer(javax.jms.Destination, java.lang.String, javax.jms.ServerSessionPool, int)
    */
   public ConnectionConsumer createConnectionConsumer(Destination destination, String sMessageSelector, ServerSessionPool sessionPool, int nMaxMessages) throws JMSException
   {
      return getPhysicalJMSConnection().createConnectionConsumer(
         JMSResourceAdapter.getJMSDestination(destination), sMessageSelector, sessionPool, nMaxMessages);
   }

   /**
    * @see javax.jms.QueueConnection#createConnectionConsumer(javax.jms.Queue, java.lang.String, javax.jms.ServerSessionPool, int)
    */
   public ConnectionConsumer createConnectionConsumer(Queue queue, String sMessageSelector, ServerSessionPool sessionPool, int nMaxMessages) throws JMSException
   {
      return getPhysicalJMSConnection().createConnectionConsumer(
         JMSResourceAdapter.getJMSDestination(queue), sMessageSelector, sessionPool, nMaxMessages);
   }

   /**
    * @see javax.jms.TopicConnection#createConnectionConsumer(javax.jms.Topic, java.lang.String, javax.jms.ServerSessionPool, int)
    */
   public ConnectionConsumer createConnectionConsumer(Topic topic, String sMessageSelector, ServerSessionPool sessionPool, int nMaxMessages) throws JMSException
   {
      return getPhysicalJMSConnection().createConnectionConsumer(
         JMSResourceAdapter.getJMSDestination(topic), sMessageSelector, sessionPool, nMaxMessages);
   }

   /**
    * @see javax.jms.Connection#createDurableConnectionConsumer(javax.jms.Topic, java.lang.String, java.lang.String, javax.jms.ServerSessionPool, int)
    */
   public ConnectionConsumer createDurableConnectionConsumer(Topic topic, String sSubscriptionName, String sMessageSelector, ServerSessionPool sessionPool, int nMaxMessages) throws JMSException
   {
      return getPhysicalJMSConnection().createDurableConnectionConsumer(
         JMSResourceAdapter.getJMSDestination(topic), sSubscriptionName, sMessageSelector, sessionPool, nMaxMessages);
   }

   /**
    * @see javax.jms.Connection#createSession(boolean, int)
    */
   public synchronized Session createSession(boolean bTransacted, int nAcknowledgeMode) throws JMSException
   {
      JMSSession jmsSession = new JMSSession(this, ((JMSManagedConnection)m_managedConnection)
         .getPhysicalJMSSession(m_jmsSessionSet.isEmpty(), bTransacted, nAcknowledgeMode));

      m_jmsSessionSet.add(jmsSession);

      return jmsSession;
   }

   /**
    * @see javax.jms.QueueConnection#createQueueSession(boolean, int)
    */
   public QueueSession createQueueSession(boolean bTransacted, int nAcknowledgeMode) throws JMSException
   {
      return (QueueSession)createSession(bTransacted, nAcknowledgeMode);
   }

   /**
    * @see javax.jms.TopicConnection#createTopicSession(boolean, int)
    */
   public TopicSession createTopicSession(boolean bTransacted, int nAcknowledgeMode) throws JMSException
   {
      return (TopicSession)createSession(bTransacted, nAcknowledgeMode);
   }

   /**
    * @see javax.jms.Connection#getClientID()
    */
   public String getClientID() throws JMSException
   {
      return getPhysicalJMSConnection().getClientID();
   }

   /**
    * @see javax.jms.Connection#getExceptionListener()
    */
   public ExceptionListener getExceptionListener() throws JMSException
   {
      return getPhysicalJMSConnection().getExceptionListener();
   }

   /**
    * @see javax.jms.Connection#getMetaData()
    */
   public ConnectionMetaData getMetaData() throws JMSException
   {
     return getPhysicalJMSConnection().getMetaData();
   }

   /**
    * @see javax.jms.Connection#setClientID(java.lang.String)
    */
   public void setClientID(String sClientID) throws JMSException
   {
      getPhysicalJMSConnection().setClientID(sClientID);
   }

   /**
    * @see javax.jms.Connection#setExceptionListener(javax.jms.ExceptionListener)
    */
   public void setExceptionListener(ExceptionListener exceptionListener) throws JMSException
   {
      getPhysicalJMSConnection().setExceptionListener(exceptionListener);
   }

   /**
    * @see javax.jms.Connection#start()
    */
   public void start() throws JMSException
   {
      getPhysicalJMSConnection().start();
   }

   /**
    * @see javax.jms.Connection#stop()
    */
   public void stop() throws JMSException
   {
      getPhysicalJMSConnection().stop();
   }
}
