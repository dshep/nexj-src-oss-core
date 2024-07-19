// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.jms;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.math.BigDecimal;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;

import javax.jms.BytesMessage;
import javax.jms.Connection;
import javax.jms.ConnectionFactory;
import javax.jms.DeliveryMode;
import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.MapMessage;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.MessageProducer;
import javax.jms.Session;
import javax.jms.StreamMessage;
import javax.jms.XAConnection;
import javax.jms.XASession;
import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.resource.ResourceException;
import javax.transaction.Status;
import javax.transaction.Synchronization;
import javax.transaction.Transaction;
import javax.transaction.TransactionManager;

import nexj.core.integration.IntegrationException;
import nexj.core.integration.SecureSender;
import nexj.core.integration.io.ObjectOutput;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.channel.jms.MessageQueue;
import nexj.core.monitoring.ThreadLocalCounter;
import nexj.core.monitoring.jmx.StatUtil;
import nexj.core.rpc.RPCException;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.Initializable;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.ThreadContextHolder;
import nexj.core.scripting.GlobalEnvironment;
import nexj.core.scripting.Symbol;
import nexj.core.util.Binary;
import nexj.core.util.J2EEUtil;
import nexj.core.util.Logger;
import nexj.core.util.PropertyIterator;
import nexj.core.util.Undefined;

/**
 * JMS client and server adapter.
 */
public class JMSSender implements Initializable, SecureSender
{
   // constants

   /**
    * The message properties property: TransferObject.
    */
   public final static String PROPERTIES = "properties";

   /**
    * The message priority property: 0..9.
    */
   public final static String PRIORITY = "priority";

   /**
    * The message persistence flag property: boolean.
    */
   public final static String PERSISTENT = "persistent";

   /**
    * The message time-to-live property: long.
    */
   public final static String TTL = "ttl";

   /**
    * The message type property: String.
    */
   public final static String TYPE = "type";

   /**
    * The message reply-to destination name property: String.
    */
   public final static String REPLY_TO = "replyTo";

   /**
    * The message source node property: String.
    */
   public final static String NODE = "node";

   /**
    * The message UOW state: String.
    */
   public final static String STATE = "state";

   /**
    * Symbol used to store JMS exception
    */
   protected final static Symbol ASYNC_EXCEPTION = Symbol.define("sys:async-exception");

   // attributes

   /**
    * The JMS engine
    */
   private EngineHelper m_jmsStrategy;

   /**
    * The connection factory name.
    */
   protected String m_sConnectionFactory;

   /**
    * The destination name.
    */
   protected String m_sDestination;

   /**
    * The connection user name.
    */
   protected String m_sUser;

   /**
    * The connection password.
    */
   protected String m_sPassword;

   /**
    * The destination type.
    */
   protected String m_sTypeName = "destination";

   /**
    * The enablement flag.
    */
   protected boolean m_bEnabled = J2EEUtil.isContained();

   // associations

   /**
    * The message queue channel.
    */
   protected MessageQueue m_channel;

   /**
    * The naming context.
    */
   protected Context m_namingContext;

   /**
    * The connection factory.
    */
   protected ConnectionFactory m_connectionFactory;

   /**
    * The destination.
    */
   protected Destination m_destination;

   /**
    * The transaction manager.
    */
   protected TransactionManager m_txManager;

   /**
    * Counter of messages sent since the creation of this component
    */
   protected ThreadLocalCounter m_sentCounter = new ThreadLocalCounter();

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(JMSSender.class);

   // operations

   /**
    * Sets the message queue channel.
    * @param channel The message queue channel to set.
    */
   public void setChannel(MessageQueue channel)
   {
      m_channel = channel;
   }

   /**
    * @return The message queue channel.
    */
   public MessageQueue getChannel()
   {
      return m_channel;
   }

   /**
    * Sets the connection factory name.
    * @param sConnectionFactory The connection factory name to set.
    */
   public void setConnectionFactory(String sConnectionFactory)
   {
      m_sConnectionFactory = sConnectionFactory;
   }

   /**
    * @return The connection factory name.
    */
   public String getConnectionFactory()
   {
      return m_sConnectionFactory;
   }

   /**
    * @return The connection factory object.
    */
   public synchronized ConnectionFactory getConnectionFactoryObject()
   {
      return m_connectionFactory;
   }

   /**
    * Sets the destination name.
    * @param sDestination The destination name to set.
    */
   public void setDestination(String sDestination)
   {
      m_sDestination = sDestination;
   }

   /**
    * @return The destination name.
    */
   public String getDestination()
   {
      return m_sDestination;
   }

   /**
    * @return The destination object.
    */
   public synchronized Destination getDestinationObject()
   {
      return m_destination;
   }

   /**
    * Sets the transaction manager.
    * @param txManager The transaction manager to set.
    */
   public void setTransactionManager(TransactionManager txManager)
   {
      m_txManager = txManager;
   }

   /**
    * @return The transaction manager.
    */
   public TransactionManager getTransactionManager()
   {
      return m_txManager;
   }

   /**
    * Sets the enablement flag.
    * @param bEnabled The enablement flag to set.
    */
   public void setEnabled(boolean bEnabled)
   {
      m_bEnabled = bEnabled;
   }

   /**
    * @return The enablement flag.
    */
   public boolean isEnabled()
   {
      return m_bEnabled;
   }

   /**
    * Sets the connection user name.
    * @param sUser The connection user name to set.
    */
   public void setUser(String sUser)
   {
      m_sUser = sUser;
   }

   /**
    * @return The connection user name.
    */
   public String getUser()
   {
      return m_sUser;
   }

   /**
    * Sets the connection password.
    * @param sPassword The connection password to set.
    */
   public void setPassword(String sPassword)
   {
      m_sPassword = sPassword;
   }

   /**
    * @see nexj.core.runtime.Initializable#initialize()
    */
   public void initialize() throws Exception
   {
      if (m_channel.isBroadcast())
      {
         m_sTypeName = "topic";
      }
      else
      {
         m_sTypeName = "queue";
      }

      if (m_sConnectionFactory == null)
      {
         m_sConnectionFactory = m_channel.getConnectionFactory();

         if (m_sConnectionFactory == null || m_sConnectionFactory.startsWith("class:"))
         {
            m_sConnectionFactory = J2EEUtil.JNDI_ENV_PREFIX + "jms/cf/" + m_channel.getName();
         }
      }

      if (m_channel.getConnectionFactory() != null && m_channel.getConnectionFactory().contains("SonicMQ"))
      {
         m_jmsStrategy = new SonicMQEngineHelper();
      }
      else
      {
         m_jmsStrategy = new GenericEngineHelper();
      }

      if (m_sDestination == null)
      {
         m_sDestination = m_channel.getDestination();

         if (m_sDestination == null || m_sDestination.startsWith("class:"))
         {
            m_sDestination = J2EEUtil.JNDI_ENV_PREFIX + "jms/" + m_sTypeName + '/' + m_channel.getName();
         }
      }

      if (m_sUser == null)
      {
         m_sUser = m_channel.getUser();
      }

      if (m_sPassword == null)
      {
         m_sPassword = m_channel.getPassword();
      }

      if (m_bEnabled)
      {
         if (s_logger.isInfoEnabled())
         {
            s_logger.info("Binding to " + m_sTypeName + " \"" +
               m_sDestination + "\" (CF=\"" + m_sConnectionFactory + "\")");
         }

         m_namingContext = new InitialContext();

         try
         {
            bind();
         }
         catch (Exception e)
         {
            s_logger.error("Failed to bind to " + m_sTypeName + " \"" +
               m_sDestination + "\" (CF=\"" + m_sConnectionFactory + "\")", e);
         }
      }
   }

   /**
    * Binds to the connection factory and the queue.
    */
   protected synchronized void bind() throws NamingException, JMSException
   {
      m_connectionFactory = (ConnectionFactory)m_namingContext.lookup(m_sConnectionFactory);
      m_destination = (Destination)m_namingContext.lookup(m_sDestination);
   }

   /**
    * @see nexj.core.integration.Sender#createOutput()
    */
   public ObjectOutput createOutput()
   {
      return new ObjectOutput();
   }

   /**
    * @see nexj.core.integration.Sender#prepare(nexj.core.rpc.TransferObject, nexj.core.rpc.TransferObject, nexj.core.meta.integration.Message)
    */
   public void prepare(TransferObject raw, TransferObject tobj, nexj.core.meta.integration.Message message) throws IntegrationException
   {
   }

   /**
    * @see nexj.core.integration.Sender#send(nexj.core.rpc.TransferObject)
    */
   public void send(TransferObject tobj) throws IntegrationException
   {
      send(Collections.singletonList(tobj));
   }

   /**
    * Sends a collection of messages to the destination.
    * @param col Collection of objects implementing the Envelope interface.
    * @throws RPCException if a sending error has occurred.
    */
   public void send(final Collection col)
   {
      if (!m_channel.isSendable())
      {
         throw new RPCException("err.rpc.notSender", new Object[]{m_channel.getName()});
      }

      long lStartTime = System.nanoTime();
      String sSenderStatPath = m_channel.getSenderStatPath();

      try
      {
         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Sending " + col.size() + " message(s) on channel \"" +
               m_channel.getName() + "\" to destination \"" + m_sDestination + "\"");

            if (s_logger.isDumpEnabled())
            {
               int nCount = 0;

               for (Iterator itr = col.iterator(); itr.hasNext();)
               {
                  s_logger.dump("Message [" + nCount++ + "]: " + ((TransferObject)itr.next()));
               }
            }
         }

         if (m_bEnabled)
         {
            boolean bRetry = true;
            ConnectionFactory factory;
            Destination destination;

            synchronized (this)
            {
               if (m_destination == null)
               {
                  try
                  {
                     bind();
                  }
                  catch (Exception t)
                  {
                     s_logger.error("Failed to bind to " + m_sTypeName + " \"" +
                        m_sDestination + "\" (CF=\"" + m_sConnectionFactory + "\")", t);

                     throw new RPCException("err.rpc.jms", t);
                  }
               }

               factory = m_connectionFactory;
               destination = m_destination;
            }

         retry:
            do
            {
               try
               {
                  Connection connection = null;
                  Session session = null;
                  MessageProducer producer = null;

                  try
                  {
                     if (m_sUser == null)
                     {
                        connection = factory.createConnection();
                     }
                     else
                     {
                        connection = factory.createConnection(m_sUser, m_sPassword);
                     }

                     session = m_jmsStrategy.createSession(connection);
                     producer = session.createProducer(destination);
                     bRetry = false;

                     InvocationContext context = (InvocationContext)ThreadContextHolder.getContext();

                     for (Iterator itr = col.iterator(); itr.hasNext();)
                     {
                        TransferObject tobj = (TransferObject)itr.next();
                        Boolean persistent = (Boolean)tobj.findValue(PERSISTENT);
                        int nPersistenceMode =
                           ((persistent == null) ? m_channel.isPersistent() : persistent.booleanValue()) ?
                           DeliveryMode.PERSISTENT : DeliveryMode.NON_PERSISTENT;
                        Number priority = (Number)tobj.findValue(PRIORITY);
                        int nPriority = (priority == null) ? m_channel.getPriority() : priority.intValue();
                        Number ttl = (Number)tobj.findValue(TTL);
                        long lTTL = (ttl == null) ? m_channel.getTimeToLive() : ttl.longValue();

                        m_sentCounter.add(1);

                        if (context != null)
                        {
                           context.addRPCCount(1);
                        }

                        StatUtil.incrCounter(m_channel.getType().getMetadata(),
                           sSenderStatPath, Channel.STAT_TOTAL_COUNT, 1);

                        producer.send(createMessage(session, tobj), nPersistenceMode, nPriority, lTTL);
                     }
                  }
                  finally
                  {
                     if (producer != null)
                     {
                        try
                        {
                           producer.close();
                        }
                        catch (Exception e)
                        {
                           s_logger.error("Error closing the producer for destination \"" + m_sDestination + "\"", e);
                        }
                     }

                     if (session != null)
                     {
                        try
                        {
                           m_jmsStrategy.closeSession(session);
                        }
                        catch (Exception e)
                        {
                           s_logger.error("Error closing the session for connection factory \"" +
                              m_sConnectionFactory + "\"", e);
                        }
                     }

                     if (connection != null)
                     {
                        try
                        {
                           connection.close();
                        }
                        catch (Exception e)
                        {
                           s_logger.error("Error closing the connection for connection factory \"" +
                              m_sConnectionFactory + "\"", e);
                        }
                     }
                  }
               }
               catch (Exception e)
               {
                  if (bRetry)
                  {
                     bRetry = false;

                     for (Throwable x = e.getCause(); x != null; x = x.getCause())
                     {
                        if (x instanceof ResourceException || x instanceof IOException)
                        {
                           // The connection factory is stale, get a new one

                           if (s_logger.isInfoEnabled())
                           {
                              s_logger.info("Rebinding the stale " + m_sTypeName + " \"" +
                                 m_sDestination + "\" (CF=\"" + m_sConnectionFactory + "\")");
                           }

                           try
                           {
                              synchronized (this)
                              {
                                 if (m_connectionFactory == factory &&
                                    m_destination == destination)
                                 {
                                    bind();
                                 }

                                 factory = m_connectionFactory;
                                 destination = m_destination;
                              }
                           }
                           catch (Exception mx)
                           {
                              s_logger.error("Failed to bind to " + m_sTypeName + " \"" +
                                 m_sDestination + "\" (CF=\"" + m_sConnectionFactory + "\")", mx);

                              throw new RPCException("err.rpc.jms", e);
                           }

                           continue retry;
                        }
                     }
                  }

                  throw new RPCException("err.rpc.jms", e);
               }
            }
            while (bRetry);
         }
         else
         {
            boolean bDone = false;

            if (m_channel.isReceivable())
            {
               if (m_txManager != null)
               {
                  try
                  {
                     if (m_txManager.getStatus() == Status.STATUS_ACTIVE)
                     {
                        Transaction tx = m_txManager.getTransaction();

                        tx.registerSynchronization(new Synchronization()
                        {
                           public void beforeCompletion()
                           {
                           }

                           public void afterCompletion(int nStatus)
                           {
                              if (nStatus == Status.STATUS_COMMITTED)
                              {
                                 receive(col);
                              }
                           }
                        });
                     }
                     else
                     {
                        receive(col);
                     }
                  }
                  catch (Throwable t)
                  {
                     s_logger.error("Error receiving the JMS message on channel \"" +
                        m_channel.getName() + "\"", t);
                  }

                  bDone = true;
               }
            }

            if (!bDone && s_logger.isDebugEnabled())
            {
               s_logger.debug("Channel \"" + m_channel.getName() + "\" is disabled, ignoring the messages.");
            }
         }
      }
      finally
      {
         StatUtil.updateAverage(m_channel.getType().getMetadata(), sSenderStatPath,
               Channel.STAT_AVERAGE_SEND_TIME, (double)(System.nanoTime() - lStartTime) / 1000000);
      }
   }

   /**
    * Creates a new message from an object implementing Envelope.
    * @param session The JMS session.
    * @param tobj The message transfer object.
    * @throws JMSException if an error occurs.
    */
   protected Message createMessage(Session session, TransferObject tobj) throws JMSException
   {
      Object obj = tobj.findValue(BODY);
      Message message;

      if (obj == null)
      {
         message = session.createMessage();
      }
      else if (obj instanceof String)
      {
         message = session.createTextMessage((String)obj);
      }
      else if (obj instanceof Binary)
      {
         BytesMessage bytes = session.createBytesMessage();

         bytes.writeBytes(((Binary)obj).getData());
         message = bytes;
      }
      else if (obj instanceof TransferObject && isMapMessage((TransferObject)obj))
      {
         MapMessage map = session.createMapMessage();

         for (PropertyIterator itr = ((TransferObject)obj).getIterator(); itr.hasNext();)
         {
            itr.next();
            map.setObject(itr.getName(), convert(itr.getValue()));
         }

         message = map;
      }
      else if (obj instanceof Collection && isStreamMessage((Collection)obj))
      {
         StreamMessage stm = session.createStreamMessage();

         for (Iterator itr = ((Collection)obj).iterator(); itr.hasNext();)
         {
            stm.writeObject(convert(itr.next()));
         }

         message = stm;
      }
      else
      {
         message = session.createObjectMessage((java.io.Serializable)obj);
      }

      Object correlationId = tobj.findValue(CORRELATION_ID);

      if (correlationId != null)
      {
         if (!(correlationId instanceof String))
         {
            if (correlationId instanceof Binary)
            {
               correlationId = ((Binary)correlationId).toString();
            }
            else
            {
               throw new RPCException("err.rpc.jms.correlationIdType");
            }
         }

         message.setJMSCorrelationID((String)correlationId);
      }

      String sType = (String)tobj.findValue(TYPE);

      if (sType != null)
      {
         message.setJMSType(sType);
      }

      Object replyTo = tobj.findValue(REPLY_TO);

      if (replyTo == null)
      {
         if (m_channel.getReplyQueue() != null)
         {
            message.setJMSReplyTo(getDestination(m_channel.getReplyQueue()));
         }
      }
      else if (replyTo instanceof Destination)
      {
         message.setJMSReplyTo((Destination)replyTo);
      }
      else if (replyTo instanceof Channel)
      {
         message.setJMSReplyTo(getDestination((Channel)replyTo));
      }
      else
      {
         message.setJMSReplyTo(getDestination((String)replyTo));
      }

      TransferObject properties = (TransferObject)tobj.findValue(PROPERTIES);

      if (properties != null)
      {
         for (PropertyIterator itr = properties.getIterator(); itr.hasNext();)
         {
            String sName = (String)itr.next();
            Object value = itr.getValue();

            if (value instanceof BigDecimal)
            {
               message.setDoubleProperty(sName, ((BigDecimal)value).doubleValue());
            }
            else
            {
               message.setObjectProperty(sName, value);
            }
         }
      }

      if (!m_channel.isLoopback() && m_channel.isBroadcast() &&
         !message.propertyExists(NODE))
      {
         message.setStringProperty(NODE, J2EEUtil.NODE_ID);
      }

      return message;
   }

   /**
    * Converts a framework primitive value to a JMS value.
    * @param value The value to convert.
    * @return The converted value.
    */
   public static Object convert(Object value)
   {
      if (value instanceof Binary)
      {
         return ((Binary)value).getData();
      }

      return value;
   }

   /**
    * Determines if a value can be converted to a JMS primitive.
    * @param value The value to test.
    * @return True if the value can be converted to a JMS primitive.
    */
   public static boolean isPrimitive(Object value)
   {
      if (value instanceof Number)
      {
         return value instanceof Integer ||
            value instanceof Long ||
            value instanceof Double ||
            value instanceof Float ||
            value instanceof Short ||
            value instanceof Byte;
      }

      return value instanceof String ||
         value instanceof Binary ||
         value instanceof byte[] ||
         value instanceof Boolean ||
         value instanceof Character;
   }

   /**
    * Determines if a transfer object can be represented as a JMS map message.
    * @param tobj The transfer object to test.
    * @return True if the transfer object can be represented as a JMS map message.
    */
   public static boolean isMapMessage(TransferObject tobj)
   {
      if (tobj.getClassName() != null ||
         tobj.getEventName() != null ||
         tobj.getOID() != null ||
         tobj.getVersion() != 0)
      {
         return false;
      }

      for (PropertyIterator itr = tobj.getIterator(); itr.hasNext();)
      {
         itr.next();

         if (!isPrimitive(itr.getValue()))
         {
            return false;
         }
      }

      return true;
   }

   /**
    * Determines if a collection can be represented as a JMS stream message.
    * @param col The collection to test.
    * @return True if the collection can be represented as a JMS stream message.
    */
   public static boolean isStreamMessage(Collection col)
   {
      for (Iterator itr = col.iterator(); itr.hasNext();)
      {
         if (!isPrimitive(itr.next()))
         {
            return false;
         }
      }

      return true;
   }

   /**
    * Gets a destination from a message queue metadata object.
    * @param mq The message queue metadata.
    * @return The destination object.
    * @throws RPCException if the channel is incompatible.
    */
   protected Destination getDestination(MessageQueue mq) throws RPCException
   {
      return ((JMSSender)mq.getSender().getInstance(null)).getDestinationObject();
   }

   /**
    * Gets a destination from a channel.
    * @param channel The channel.
    * @return The destination object.
    * @throws RPCException if the channel is incompatible.
    */
   protected Destination getDestination(Channel channel) throws RPCException
   {
      if (!(channel instanceof MessageQueue))
      {
         throw new RPCException("err.rpc.jms.channel", new Object[]{channel.getName()});
      }

      return getDestination((MessageQueue)channel);
   }

   /**
    * Gets a destination by channel name.
    * @param sChannel The channel name.
    * @return The destination object.
    * @throws RPCException if the channel is incompatible.
    */
   protected Destination getDestination(String sChannel) throws RPCException
   {
      return getDestination(m_channel.getType().getMetadata().getChannel(sChannel));
   }

   /**
    * Forwards the messages to the receiver.
    * @param col The message collection.
    */
   protected void receive(Collection col)
   {
      nexj.core.runtime.Context contextSaved = ThreadContextHolder.getContext();

      for (Iterator itr = col.iterator(); itr.hasNext();)
      {
         TransferObject tobj = (TransferObject)itr.next();
         Transaction tx = null;

         try
         {
            // Clone the object graph
            ByteArrayOutputStream bos = new ByteArrayOutputStream(1024);
            ObjectOutputStream oos = new ObjectOutputStream(bos);

            oos.writeObject(tobj);
            oos.close();

            tobj = (TransferObject)new ObjectInputStream(new ByteArrayInputStream(bos.toByteArray())).readObject();
            bos = null;
            oos = null;

            if (m_channel.isTransactional())
            {
               m_txManager.begin();
               tx = m_txManager.getTransaction();
            }

            ((MessageListener)m_channel.getReceiver().getInstance(null))
               .onMessage(new TransferObjectMessage(tobj,
                  (contextSaved instanceof InvocationContext) ? (InvocationContext)contextSaved : null));

            if (tx != null)
            {
               switch (tx.getStatus())
               {
                  case Status.STATUS_ACTIVE:
                     if (tx == m_txManager.getTransaction())
                     {
                        m_txManager.commit();
                     }
                     else
                     {
                        tx.commit();
                     }

                     tx = null;
                     break;

                  case Status.STATUS_MARKED_ROLLBACK:
                     break;

                  default:
                     tx = null;
                     break;
               }
            }
         }
         catch (Throwable t)
         {
            s_logger.error("Error receiving the JMS message on channel \"" +
               m_channel.getName() + "\"", t);

            if (contextSaved != null)
            {
               GlobalEnvironment env = contextSaved.getMachine().getGlobalEnvironment();

               if (env.findVariable(ASYNC_EXCEPTION, Undefined.VALUE) != Undefined.VALUE)
               {
                  env.setVariable(ASYNC_EXCEPTION, t);
               }
            }
         }
         finally
         {
            if (tx != null)
            {
               try
               {
                  if (tx == m_txManager.getTransaction())
                  {
                     m_txManager.rollback();
                  }
                  else
                  {
                     tx.rollback();
                  }

               }
               catch (Throwable t)
               {
               }
            }

            ThreadContextHolder.setContext(contextSaved);
         }
      }
   }

   /**
    * @see nexj.core.integration.Sender#getSentCount()
    */
   public long getSentCount()
   {
      return m_sentCounter.get();
   }

   // inner classes

   /**
    * The inner abstract class that represents a helper strategy for a specific JMS engine.
    *
    * As different JMS engines require different approaches to handle JMS objects
    * (for example, SonicMQ sessions should be opened and closed in a different way
    * than that of other JMS engines) we have several classes that would implement
    * the strategy necessary for a specific JMS engine.
    */
   private interface EngineHelper
   {
      /**
       * Creates session from the given connection in a JMS engine specific way
       * @param connection - connection on which the method will create a session
       * @return the session created.
       */
      public Session createSession(Connection connection) throws Exception;

      /**
       * Closes the given session in a JMS provider specific way.
       * @param session - session to be closed.
       */
      public void closeSession(Session session) throws Exception;
   }

   /**
    * Generic JMS Engine strategy.
    */
   private class GenericEngineHelper implements EngineHelper
   {
      /**
       * @see nexj.core.rpc.jms.JMSSender.EngineHelper#createSession(javax.jms.Connection)
       */
      public Session createSession(Connection connection) throws Exception
      {
         return connection.createSession(false, Session.AUTO_ACKNOWLEDGE);
      }

      /**
       * @see nexj.core.rpc.jms.JMSSender.EngineHelper#closeSession(javax.jms.Session)
       */
      public void closeSession(Session session) throws Exception
      {
         session.close();
      }
   }

   /**
    * SonicMQ specific JMS Engine strategy.
    */
   private class SonicMQEngineHelper implements EngineHelper
   {
      /**
       * @see nexj.core.rpc.jms.JMSSender.EngineHelper#createSession(javax.jms.Connection)
       */
      public Session createSession(Connection connection) throws Exception
      {
         Session session = null;

         if (m_channel.isTransactional() && connection instanceof XAConnection)
         {
            XASession xaSession = ((XAConnection)connection).createXASession();
            session = xaSession.getSession();
         }
         else
         {
            session = connection.createSession(false, Session.AUTO_ACKNOWLEDGE);
         }

         return session;
      }

      /**
       * @see nexj.core.rpc.jms.JMSSender.EngineHelper#closeSession(javax.jms.Session)
       */
      public void closeSession(Session session) throws Exception
      {
      }
   }
}
