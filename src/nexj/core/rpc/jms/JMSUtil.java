// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.jms;

import java.util.Enumeration;

import javax.jms.BytesMessage;
import javax.jms.JMSException;
import javax.jms.MapMessage;
import javax.jms.Message;
import javax.jms.MessageEOFException;
import javax.jms.ObjectMessage;
import javax.jms.Session;
import javax.jms.StreamMessage;
import javax.jms.TextMessage;

import nexj.core.meta.integration.channel.jms.MessageQueue;
import nexj.core.rpc.jms.ra.platform.JMSPlatformAdapter;
import nexj.core.util.HashTab;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;

/**
 * JMS utility class.
 */
public class JMSUtil
{
   //constructors

   protected JMSUtil()
   {
   }

   // associations

   /**
    * The logger.
    */
   protected final static Logger s_logger = Logger.getLogger(JMSUtil.class);

   // operations

   /**
    * Creates a message copy on a given session.
    * @param message The message to copy.
    * @param session The session on which to create the new message.
    * @param updater The property updater. Can be null.
    * @return The new message.
    * @throws JMSException if an error occurs.
    */
   public static Message copy(Message message, Session session, MessagePropertyUpdater updater) throws JMSException
   {
      Message copy;

      if (message instanceof TextMessage)
      {
         TextMessage src = (TextMessage)message;
         TextMessage dst = session.createTextMessage();

         dst.setText(src.getText());
         copy = dst;
      }
      else if (message instanceof ObjectMessage)
      {
         ObjectMessage src = (ObjectMessage)message;
         ObjectMessage dst = session.createObjectMessage();

         dst.setObject(src.getObject());
         copy = dst;
      }
      else if (message instanceof BytesMessage)
      {
         BytesMessage src = (BytesMessage)message;
         BytesMessage dst = session.createBytesMessage();
         byte[] buf = new byte[2048];

         for (;;)
         {
            int nCount = src.readBytes(buf, buf.length);

            if (nCount < 0)
            {
               break;
            }

            dst.writeBytes(buf, 0, nCount);
         }

         copy = dst;
      }
      else if (message instanceof MapMessage)
      {
         MapMessage src = (MapMessage)message;
         MapMessage dst = session.createMapMessage();

         for (Enumeration e = src.getMapNames(); e.hasMoreElements();)
         {
            String sName = (String)e.nextElement();

            dst.setObject(sName, src.getObject(sName));
         }

         copy = dst;
      }
      else if (message instanceof StreamMessage)
      {
         StreamMessage src = (StreamMessage)message;
         StreamMessage dst = session.createStreamMessage();

         for (;;)
         {
            try
            {
               dst.writeObject(src.readObject());
            }
            catch (MessageEOFException e)
            {
               break;
            }
         }

         copy = dst;
      }
      else
      {
         throw new JMSException("Unknown message type " + message.getClass().getName());
      }

      for (Enumeration enm = message.getPropertyNames(); enm.hasMoreElements();)
      {
         String sName = (String)enm.nextElement();

         copy.setObjectProperty(sName, message.getObjectProperty(sName));
      }

      String s = message.getJMSCorrelationID();

      if (s != null)
      {
         copy.setJMSCorrelationID(s);
      }

      s = message.getJMSType();

      if (s != null)
      {
         copy.setJMSType(s);
      }

      if (updater != null)
      {
         updater.getProperties(message);
         updater.setProperties(copy);
      }

      return copy;
   }

   /**
    * Sets message properties through an updater.
    * @param message The message to set properties on.
    * @param updater The property updater.
    * @throws JMSException if an error occurs.
    */
   public static void update(Message message, MessagePropertyUpdater updater) throws JMSException
   {
      Lookup map = new HashTab();

      for (Enumeration enm = message.getPropertyNames(); enm.hasMoreElements();)
      {
         String sName = (String)enm.nextElement();

         map.put(sName, message.getObjectProperty(sName));
      }

      updater.getProperties(message);
      message.clearProperties();

      for (Lookup.Iterator itr = map.iterator(); itr.hasNext();)
      {
         itr.next();
         message.setObjectProperty((String)itr.getKey(), itr.getValue());
      }

      updater.setProperties(message);
   }

   /**
    * @return The JMS message id of the message or "<n/a>" if an error occurred.
    */
   protected static String getId(Message message)
   {
      try
      {
         return message.getJMSMessageID();
      }
      catch (JMSException e)
      {
         return "<n/a>";
      }
   }

   /**
    * Returns the number of failed delivery attempts of a message on a message queue. 
    * @param mq The MessageQueue of the message.
    * @param message The Message for which to determine the error count.
    * @param nDefaultErrorCount The fallback error count to use if the error count cannot be determined from the message.
    * @param bRedelivery True if the error count is being used during redelivery (i.e. after the JMS engine has redelivered the failed message).
    *                    This is needed when not using standard (or third-party) JMS properties to track the error.
    *                    A failed message is always redelivered by the JMS engine first which means that another error
    *                    has already occured.
    * @return The number of times delivery of this message has failed prior to the current delivery attempt.
    */
   public static int getErrorCount(MessageQueue mq, Message message, int nDefaultErrorCount, boolean bRedelivery)
      throws JMSException
   {
      if (mq.isTransactional())
      {
         if (message.propertyExists(JMS.ERROR_COUNT))
         {
            return (bRedelivery) ? message.getIntProperty(JMS.ERROR_COUNT) + 1 : message.getIntProperty(JMS.ERROR_COUNT);
         }
         
         if (message.propertyExists(JMS.JMS_DELIVERY_COUNT))
         {
            return message.getIntProperty(JMS.JMS_DELIVERY_COUNT) - 1;
         }
         
         if (message.propertyExists(JMS.JBOSS_REDELIVERY_COUNT))
         {
            return message.getIntProperty(JMS.JBOSS_REDELIVERY_COUNT);
         }

         return nDefaultErrorCount;
      }

      return (bRedelivery) ? 1 : 0;
   }

   /**
    * Returns the maximum error count of message on mq.
    * @param mq The MessageQueue of message.
    * @param message The Message for which to determine the error count.
    * @return The maximum error count.
    */
   public static int getMaxErrorCount(MessageQueue mq, Message message) throws JMSException
   {
      if (mq.isTransactional())
      {
         if (message.propertyExists(JMS.MAX_ERROR_COUNT))
         {
            return message.getIntProperty(JMS.MAX_ERROR_COUNT);
         }

         return mq.getErrorCount();
      }

      return 1;
   }

   /**
    * A method that returns true if connection factory implements a platform adapter.
    *
    * @param sFactory The message queue name.
    * @return True if the connection factory implements a platform adapter.
    */
   public static boolean isPlatformAdapter(String sFactory)
   {
      if (sFactory != null && sFactory.startsWith("class:"))
      {
         Class factoryClass;

         try
         {
            factoryClass = Class.forName(sFactory.substring("class:".length()), false, JMSUtil.class.getClassLoader());
         }
         catch (Throwable t)
         {
            throw new RuntimeException("Invalid connection factory \"" + sFactory + "\"", t);
         }

         if (JMSPlatformAdapter.class.isAssignableFrom(factoryClass))
         {
            return true;
         }
      }

      return false;
   }

   /**
    * Fix the context class loader, depending on the platform.
    * @param loader The new context loader to set. Can be null to avoid any change.
    * @return The old context class loader, or null if the argument was null.
    */
   public static ClassLoader setContextClassLoader(ClassLoader loader)
   {
      if (loader == null)
      {
         return null;
      }

      Thread thread = Thread.currentThread();
      ClassLoader oldClassLoader = thread.getContextClassLoader();

      thread.setContextClassLoader(loader);

      return oldClassLoader;
   }

   // inner classes

   /**
    * Interface implemented by strategies for JMS property updating.
    */
   public interface MessagePropertyUpdater
   {
      /**
       * Gets the properties from a message.
       * @param message The message.
       */
      void getProperties(Message message) throws JMSException;

      /**
       * Sets the properties on a message.
       * @param message The message to update.
       */
      void setProperties(Message message) throws JMSException;
   }
}