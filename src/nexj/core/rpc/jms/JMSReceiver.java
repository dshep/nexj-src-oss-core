// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.jms;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;

import javax.jms.BytesMessage;
import javax.jms.DeliveryMode;
import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.MapMessage;
import javax.jms.Message;
import javax.jms.MessageEOFException;
import javax.jms.MessageListener;
import javax.jms.ObjectMessage;
import javax.jms.Queue;
import javax.jms.StreamMessage;
import javax.jms.TextMessage;
import javax.jms.Topic;

import nexj.core.integration.ContextReceiver;
import nexj.core.meta.Primitive;
import nexj.core.meta.integration.channel.jms.MessageQueue;
import nexj.core.rpc.RequestException;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.GenericSerializablePropertyMap;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextHolder;
import nexj.core.runtime.UnitOfWork;
import nexj.core.util.Binary;
import nexj.core.util.J2EEUtil;
import nexj.core.util.Logger;
import nexj.core.util.PropertyIterator;

/**
 * Generic message listener component.
 */
public class JMSReceiver extends ContextReceiver implements MessageListener
{
   // constants

   /**
    * The message id property: String.
    */
   public final static String MESSAGE_ID = "messageId";
   
   /**
    * The message expiration property: Timestamp.
    */
   public final static String EXPIRATION = "expiration";
   
   /**
    * The message redelivery property: boolean.
    */
   public final static String REDELIVERED = "redelivered";
   
   /**
    * The message timestamp property: Timestamp.
    */
   public final static String TIMESTAMP = "timestamp";
   
   // associations

   /**
    * The message queue channel.
    */
   protected MessageQueue m_channel;

   /**
    * The JMS server list.
    */
   protected List m_serverList;

   /**
    * The receiver logger.
    */
   protected Logger m_logger;

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
    * Adds a JMS server to the list of servers to consider.
    * @param server The server to add.
    */
   public void addServer(JMSServer server)
   {
      server.setReceiver(this);

      if (m_serverList == null)
      {
         m_serverList = new ArrayList(4);
      }

      m_serverList.add(server);
   }

   /**
    * @see nexj.core.integration.ContextReceiver#initialize()
    */
   public void initialize() throws Exception
   {
      super.initialize();
      m_logger = m_channel.getLogger();
   }

   /**
    * @see javax.jms.MessageListener#onMessage(javax.jms.Message)
    */
   public void onMessage(final Message message)
   {
      run(new ContextRunnable()
      {
         public boolean isEnabled() throws Throwable
         {
            return m_channel.isLoopback() ||
               !J2EEUtil.NODE_ID.equals(message.getStringProperty(JMSSender.NODE));
         }

         public String getClientAddress() throws Throwable
         {
            Destination destination = message.getJMSDestination();

            if (destination == null)
            {
               return null;
            }

            if (destination instanceof Queue)
            {
               return "jms:queue:" + ((Queue)destination).getQueueName();
            }

            return "jms:topic:" + ((Topic)destination).getTopicName();
         }

         public String getUser() throws Throwable
         {
            String sUser = null;

            if (m_channel.isTrusted())
            {
               sUser = message.getStringProperty(JMSSender.USER);
            }

            if (sUser == null)
            {
               sUser = m_channel.getDefaultUser();
            }

            return sUser;
         }

         public void run(InvocationContext context) throws Throwable
         {
            if (message instanceof InvocationContextHolder)
            {
               InvocationContext ctx = ((InvocationContextHolder)message).getInvocationContext();

               if (ctx != null && ctx.getTester() != null)
               {
                  context.setTester(ctx.getTester());
               }
            }

            if (m_channel.isTrusted())
            {
               if (message.propertyExists(JMSSender.PROTECTED))
               {
                  context.setProtected(message.getBooleanProperty(JMSSender.PROTECTED));
               }

               if (message.propertyExists(JMSSender.STATE))
               {
                  GenericSerializablePropertyMap map = new GenericSerializablePropertyMap();

                  map.deserializeValues(message.getStringProperty(JMSSender.STATE), context);

                  UnitOfWork uow = context.getUnitOfWork();

                  for (PropertyIterator itr = map.getIterator(); itr.hasNext();)
                  {
                     itr.next();
                     uow.setValue(itr.getName(), itr.getValue());
                  }
               }
            }

            boolean bProcessed = false;

            if (m_serverList != null)
            {
               for (int i = 0, nCount = m_serverList.size(); i < nCount; ++i)
               {
                  bProcessed = ((JMSServer)m_serverList.get(i)).receive(message, context);
                  
                  if (bProcessed)
                  {
                     break;
                  }
               }
            }

            if (!bProcessed)
            {
               if (isBound(m_channel, context))
               {
                  receive(createMessage(message), m_channel, context);
               }
               else
               {
                  throw new RequestException("err.rpc.jms.unknownMessage");
               }
            }
         }

         public void err(Throwable t, InvocationContext context) throws Throwable
         {
            int nErrorCount = 1;
            int nMaxErrorCount = JMSUtil.getMaxErrorCount(m_channel, message);

            // 0 means unlimited delivery so we don't care what the actual error count is.
            if (nMaxErrorCount > 0)
            {
               nErrorCount = JMSUtil.getErrorCount(m_channel, message, (message.getJMSRedelivered()) ? Integer.MAX_VALUE - 1 : 0, false) + 1;
            }

            int nLevel;

            // For unlimited delivery we never redirect to error Q.
            if (nMaxErrorCount > 0 && nErrorCount >= nMaxErrorCount)
            {
               if (m_channel.getErrorQueue() != null)
               {
                  m_logger.error("Redirecting message " + message.getJMSMessageID() +
                     " to error queue \"" + m_channel.getErrorQueue().getName() +
                     "\" after " + nMaxErrorCount + " processing error(s)", t);
               }
               else
               {
                  m_logger.error("Abandoning message " + message.getJMSMessageID() +
                     " after " + nMaxErrorCount + " processing error(s)", t);
               }

               nLevel = Logger.DEBUG;
            }
            else
            {
               if (m_logger.isDebugEnabled())
               {
                  m_logger.debug("Error processing message " + message.getJMSMessageID(), t);
               }

               nLevel = Logger.DUMP;
            }

            if (m_logger.isLevelEnabled(nLevel))
            {
               m_logger.log(nLevel, createMessage(message));
            }
         }
      }, m_channel, "JMS");
   }

   /**
    * Converts a JMS message to an object.
    * @param message The message to convert.
    * @return The converted message.
    * @throws JMSException if an error occurs.
    */
   public TransferObject createMessage(Message message) throws JMSException
   {
      return transfer(message, m_channel);
   }

   /**
    * Converts a JMS message to an object.
    * @param message The message to convert.
    * @param mq The message queue.
    * @return The converted message.
    * @throws JMSException if an error occurs.
    */
   public static TransferObject transfer(Message message, MessageQueue mq) throws JMSException
   {
      TransferObject tobj = new TransferObject();

      tobj.setClassName("MessageQueue");
      tobj.setValue(JMSSender.CHANNEL, mq.getName());
      setJMSProperties(tobj, message);

      if (message instanceof TextMessage)
      {
         tobj.setValue(JMSSender.BODY, ((TextMessage)message).getText());
      }
      else if (message instanceof ObjectMessage)
      {
         tobj.setValue(JMSSender.BODY, ((ObjectMessage)message).getObject());
      }
      else if (message instanceof BytesMessage)
      {
         BytesMessage msg = (BytesMessage)message;

         byte[] buf = new byte[2048];
         byte[] data = null;
         int nCount = 0;
         int nRead;

         do
         {
            nRead = msg.readBytes(buf);

            if (nRead > 0)
            {
               if (data == null)
               {
                  nCount = nRead;
                  data = buf;
                  buf = new byte[2048];
               }
               else
               {
                  if (data.length - nCount < nRead)
                  {
                     byte[] tmp = new byte[data.length << 1];
   
                     System.arraycopy(data, 0, tmp, 0, nCount);
                     data = tmp;
                  }

                  System.arraycopy(buf, 0, data, nCount, nRead);
                  nCount += nRead;
               }
            }
         }
         while (nRead == buf.length);

         buf = new byte[nCount];

         if (nCount > 0)
         {
            System.arraycopy(data, 0, buf, 0, nCount);
         }

         tobj.setValue(JMSSender.BODY, new Binary(buf));
      }
      else if (message instanceof MapMessage)
      {
         MapMessage msg = (MapMessage)message;
         TransferObject map = new TransferObject();
         
         for (Enumeration e = msg.getMapNames(); e.hasMoreElements();)
         {
            String sName = (String)e.nextElement();
            map.setValue(sName, convert(msg.getObject(sName)));
         }

         tobj.setValue(JMSSender.BODY, map);
      }
      else if (message instanceof StreamMessage)
      {
         StreamMessage msg = (StreamMessage)message;
         List list = new ArrayList();
         
         for (;;)
         {
            try
            {
               list.add(convert(msg.readObject()));
            }
            catch (MessageEOFException e)
            {
               break;
            }
         }

         tobj.setValue(JMSSender.BODY, list);
      }
      else if (message instanceof TransferObjectMessage)
      {
         for (PropertyIterator itr = ((TransferObjectMessage)message)
            .getTransferObject().getIterator(); itr.hasNext();)
         {
            itr.next();
            tobj.setValue(itr.getName(), itr.getValue());
         }
      }

      return tobj;
   }

   /**
    * Converts a primitive value to a framework-compatible format.
    * @param value The value to convert.
    * @return The converted value. 
    */
   public static Object convert(Object value)
   {
      if (value instanceof Number)
      {
         if (value instanceof Byte || value instanceof Short)
         {
            return Primitive.createInteger(((Number)value).intValue());
         }
         
         return value;
      }
   
      if (value instanceof byte[])
      {
         return new Binary((byte[])value);
      }
   
      return value;
   }

   /**
    * Sets the system properties on a JMS message.
    * @param tobj The message on which to set the properties.
    * @param message The JMS message.
    */
   public static void setJMSProperties(TransferObject tobj, Message message) throws JMSException
   {
      String sCorrelationId = message.getJMSCorrelationID();
      
      if (sCorrelationId != null && sCorrelationId.length() != 0)
      {
         tobj.setValue(JMSSender.CORRELATION_ID, sCorrelationId);
      }

      long lExpiration = message.getJMSExpiration();

      if (lExpiration != 0)
      {
         tobj.setValue(EXPIRATION, new Timestamp(lExpiration));
      }

      tobj.setValue(MESSAGE_ID, message.getJMSMessageID());
      tobj.setValue(JMSSender.PERSISTENT, Boolean.valueOf(message.getJMSDeliveryMode() == DeliveryMode.PERSISTENT));
      tobj.setValue(JMSSender.PRIORITY, Primitive.createInteger(message.getJMSPriority()));

      TransferObject properties = getProperties(message);

      if (properties != null)
      {
         tobj.setValue(JMSSender.PROPERTIES, properties);
      }

      tobj.setValue(REDELIVERED, Boolean.valueOf(message.getJMSRedelivered()));

      Destination replyTo = message.getJMSReplyTo();

      if (replyTo != null)
      {
         tobj.setValue(JMSSender.REPLY_TO, replyTo);
      }

      long lTimestamp = message.getJMSTimestamp();

      if (lTimestamp != 0)
      {
         tobj.setValue(TIMESTAMP, new Timestamp(lTimestamp));
      }

      String sType = message.getJMSType();

      if (sType != null && sType.length() != 0)
      {
         tobj.setValue(JMSSender.TYPE, sType);
      }
   }
   
   /**
    * Gets the JMS message properties.
    * @param message The JMS message.
    * @param context The invocation context.
    * @return The properties transfer object, or null if not properties have been found.
    */
   public static TransferObject getProperties(Message message) throws JMSException
   {
      Enumeration e = message.getPropertyNames();

      if (!e.hasMoreElements())
      {
         return null;
      }

      TransferObject properties = new TransferObject();

      while (e.hasMoreElements())
      {
         String sName = (String)e.nextElement();
         
         properties.setValue(sName, convert(message.getObjectProperty(sName)));
      }

      return properties;
   }

   /**
    * Gets the object from a JMS message body.
    * @param message The JMS message.
    * @return The object, or null if it is not an object message.
    */
   public static Object getObject(Message message) throws JMSException
   {
      if (message instanceof ObjectMessage)
      {
         return ((ObjectMessage)message).getObject();
      }

      if (message instanceof TransferObjectMessage)
      {
         return ((TransferObjectMessage)message).getTransferObject().findValue(JMSSender.BODY);
      }

      return null;
   }
}
