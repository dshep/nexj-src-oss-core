// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.channel.jms;

import javax.resource.ResourceException;

import nexj.core.meta.MetadataException;
import nexj.core.meta.Primitive;
import nexj.core.meta.PropertyHolder;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.TransactionalChannel;
import nexj.core.rpc.jms.JMSUtil;
import nexj.core.rpc.jms.ra.JMSResourceAdapter;
import nexj.core.rpc.jms.ra.JMSConsumerConfig;
import nexj.core.rpc.ra.GenericResourceAdapter;

/**
 * Message queue metadata.
 */
public class MessageQueue extends TransactionalChannel
{
   // constants

   /**
    * Transactional acknowledgement mode.
    */
   public final static byte ACK_TX = 0;
   
   /**
    * Auto-acknowledgement mode.
    */
   public final static byte ACK_AUTO = 1;

   /**
    * Lazy acknowledgement mode, duplicate messages are ok.
    */
   public final static byte ACK_LAZY = 2;
   
   // attributes
   
   /**
    * The message time to live in ms.
    */
   protected long m_lTimeToLive;

   /**
    * The JNDI alias of the queue.
    */
   protected String m_sAlias;

   /**
    * The user name for the JMS connection.
    */
   protected String m_sUser;

   /**
    * The JMS connection password.
    */
   protected String m_sPassword;

   /**
    * The default user name.
    */
   protected String m_sDefaultUser;

   /**
    * The message selector.
    */
   protected String m_sSelector;

   /**
    * The connection factory JNDI name.
    */
   protected String m_sConnectionFactory;

   /**
    * The destination JNDI name.
    */
   protected String m_sDestination;

   /**
    * The durable subscription name.
    */
   protected String m_sSubscription;

   /**
    * The client Id.
    */
   protected String m_sClientId;

   /**
    * The priority.
    */
   protected int m_nPriority = 4;

   /**
    * The maximum receiver thread pool size.
    */
   protected int m_nMaxReceivers = 4;

   /**
    * The maximum sender connection pool size.
    */
   protected int m_nMaxSenders = 16;

   /**
    * The error count before a message is redirected to the error queue.
    */
   protected int m_nErrorCount = 3;

   /**
    * The acknowledgement mode, one of the ACK_* constants.
    */
   protected byte m_nAckMode = ACK_TX;

   /**
    * The broadcast flag.
    */
   protected boolean m_bBroadcast;

   /**
    * The trust flag.
    */
   protected boolean m_bTrusted;

   /**
    * The persistent delivery flag.
    */
   protected boolean m_bPersistent = true;

   /**
    * The loopback flag.
    */
   protected boolean m_bLoopback = true;

   /**
    * True if this queue is the first with a given alias.
    */
   protected boolean m_bFirst = true;

   // associations

   /**
    * The reply queue.
    */
   protected MessageQueue m_replyQueue;

   /**
    * The optional error message queue.
    */
   protected MessageQueue m_errorQueue;

   /**
    * The connection factory property holder.
    */
   protected PropertyHolder m_connectionFactoryPropertyHolder = new PropertyHolder();

   /**
    * The destination property holder.
    */
   protected PropertyHolder m_destinationPropertyHolder = new PropertyHolder();

   // constructors

   /**
    * Constructs the message queue metadata.
    * @param sName The message queue name.
    */
   public MessageQueue(String sName)
   {
      super(sName);
   }

   // operations

   /**
    * @see nexj.core.meta.integration.Channel#isSynchronous()
    */
   public boolean isSynchronous()
   {
      return false;
   }

   /**
    * @see nexj.core.meta.integration.Channel#isTransactional()
    */
   public boolean isTransactional()
   {
      return m_nAckMode == ACK_TX;
   }

   /**
    * Sets the broadcast flag.
    * @param bBroadcast The broadcast flag to set.
    */
   public void setBroadcast(boolean bBroadcast)
   {
      verifyNotReadOnly();
      m_bBroadcast = bBroadcast;
   }

   /**
    * @return The broadcast flag.
    */
   public boolean isBroadcast()
   {
      return m_bBroadcast;
   }

   /**
    * Sets the trust flag.
    * @param bTrusted The trust flag to set.
    */
   public void setTrusted(boolean bTrusted)
   {
      verifyNotReadOnly();
      m_bTrusted = bTrusted;
   }

   /**
    * @return The trust flag.
    */
   public boolean isTrusted()
   {
      return m_bTrusted;
   }

   /**
    * Sets the persistent delivery flag.
    * @param bPersistent The persistent delivery flag to set.
    */
   public void setPersistent(boolean bPersistent)
   {
      verifyNotReadOnly();
      m_bPersistent = bPersistent;
   }

   /**
    * @return The persistent delivery flag.
    */
   public boolean isPersistent()
   {
      return m_bPersistent;
   }

   /**
    * Sets the loopback flag.
    * @param bLoopback The loopback flag to set.
    */
   public void setLoopback(boolean bLoopback)
   {
      verifyNotReadOnly();
      m_bLoopback = bLoopback;
   }

   /**
    * @return The loopback flag.
    */
   public boolean isLoopback()
   {
      return m_bLoopback;
   }

   /**
    * Sets whether or not this queue is the first with a given alias.
    * @param bFirst True if this queue is the first with a given alias.
    */
   public void setFirst(boolean bFirst)
   {
      verifyNotReadOnly();
      m_bFirst = bFirst;
   }

   /**
    * Gets whether or not this queue is the first with a given alias.
    * @return True if this queue is the first with a given alias.
    */
   public boolean isFirst()
   {
      return m_bFirst;
   }

   /**
    * Sets the acknowledgement mode, one of the ACK_* constants.
    * @param nAckMode The acknowledgement mode, one of the ACK_* constants to set.
    */
   public void setAckMode(byte nAckMode)
   {
      verifyNotReadOnly();
      m_nAckMode = nAckMode;
   }

   /**
    * @return The acknowledgement mode, one of the ACK_* constants.
    */
   public byte getAckMode()
   {
      return m_nAckMode;
   }
   
   /**
    * Sets the priority.
    * @param nPriority The priority to set.
    */
   public void setPriority(int nPriority)
   {
      verifyNotReadOnly();
      m_nPriority = nPriority;
      
      if (nPriority < 0 || nPriority > 9)
      {
         throw new MetadataException("err.meta.integration.mq.priority",
            new Object[]{Primitive.createInteger(nPriority)});
      }
   }

   /**
    * @return The priority.
    */
   public int getPriority()
   {
      return m_nPriority;
   }

   /**
    * Sets the message time to live in ms.
    * @param lTimeToLive The message time to live in ms to set.
    */
   public void setTimeToLive(long lTimeToLive)
   {
      verifyNotReadOnly();
      m_lTimeToLive = lTimeToLive;
   }

   /**
    * @return The message time to live in ms.
    */
   public long getTimeToLive()
   {
      return m_lTimeToLive;
   }

   /**
    * Sets the maximum receiver thread pool size.
    * @param nMaxReceivers The maximum receiver thread pool size to set.
    */
   public void setMaxReceivers(int nMaxReceivers)
   {
      verifyNotReadOnly();
      m_nMaxReceivers = nMaxReceivers;
   }

   /**
    * @return The maximum receiver thread pool size.
    */
   public int getMaxReceivers()
   {
      return m_nMaxReceivers;
   }

   /**
    * Sets the maximum sender connection pool size.
    * @param nMaxSenders The maximum sender connection pool size to set.
    */
   public void setMaxSenders(int nMaxSenders)
   {
      verifyNotReadOnly();
      m_nMaxSenders = nMaxSenders;
   }

   /**
    * @return The maximum sender connection pool size.
    */
   public int getMaxSenders()
   {
      return m_nMaxSenders;
   }
   
   /**
    * Sets the default user name.
    * @param sDefaultUser The default user name to set.
    */
   public void setDefaultUser(String sDefaultUser)
   {
      verifyNotReadOnly();
      m_sDefaultUser = sDefaultUser;
   }

   /**
    * @return The default user name.
    */
   public String getDefaultUser()
   {
      return m_sDefaultUser;
   }
   
   /**
    * Sets the reply queue.
    * @param replyQueue The reply queue to set.
    */
   public void setReplyQueue(Channel replyQueue)
   {
      verifyNotReadOnly();

      if (!(replyQueue instanceof MessageQueue))
      {
         throw new MetadataException("err.meta.integration.mq.replyQueueChannel", new Object[]{getName()});
      }
      
      m_replyQueue = (MessageQueue)replyQueue;
   }

   /**
    * Sets the reply queue.
    * @param replyQueue The reply queue to set.
    */
   public void setReplyQueue(MessageQueue replyQueue)
   {
      verifyNotReadOnly();
      m_replyQueue = replyQueue;
   }

   /**
    * @return The reply queue.
    */
   public MessageQueue getReplyQueue()
   {
      return m_replyQueue;
   }

   /**
    * Sets the optional error message queue.
    * @param errorQueue The optional error message queue to set.
    */
   public void setErrorQueue(Channel errorQueue)
   {
      verifyNotReadOnly();

      if (errorQueue != null && !(errorQueue instanceof MessageQueue))
      {
         throw new MetadataException("err.meta.integration.mq.errorQueueChannel", new Object[]{getName()});
      }

      setErrorQueue((MessageQueue)errorQueue);
   }

   /**
    * Sets the optional error message queue.
    * @param errorQueue The optional error message queue to set.
    */
   public void setErrorQueue(MessageQueue errorQueue)
   {
      verifyNotReadOnly();
      m_errorQueue = errorQueue;
   }

   /**
    * @return The optional error message queue.
    */
   public MessageQueue getErrorQueue()
   {
      return m_errorQueue;
   }
   
   /**
    * Sets the error count before a message is redirected to the error queue.
    * @param nErrorCount The error count before a message is redirected to the error queue to set.
    */
   public void setErrorCount(int nErrorCount)
   {
      verifyNotReadOnly();
      m_nErrorCount = nErrorCount;
   }

   /**
    * @return The error count before a message is redirected to the error queue.
    */
   public int getErrorCount()
   {
      return m_nErrorCount;
   }
   
   /**
    * Sets the JNDI alias of the queue.
    * @param sAlias The JNDI alias of the queue to set.
    */
   public void setAlias(String sAlias)
   {
      verifyNotReadOnly();
      m_sAlias = sAlias;
   }

   /**
    * @return The JNDI alias of the queue.
    */
   public String getAlias()
   {
      return m_sAlias;
   }
   
   /**
    * Sets the user name for the JMS connection.
    * @param sUser The user name for the JMS connection to set.
    */
   public void setUser(String sUser)
   {
      verifyNotReadOnly();
      m_sUser = sUser;
   }

   /**
    * @return The user name for the JMS connection.
    */
   public String getUser()
   {
      return m_sUser;
   }
   
   /**
    * Sets the JMS connection password.
    * @param sPassword The JMS connection password to set.
    */
   public void setPassword(String sPassword)
   {
      verifyNotReadOnly();
      m_sPassword = sPassword;
   }

   /**
    * @return The JMS connection password.
    */
   public String getPassword()
   {
      return m_sPassword;
   }

   /**
    * Sets the message selector.
    * @param sSelector The message selector to set.
    */
   public void setSelector(String sSelector)
   {
      verifyNotReadOnly();
      m_sSelector = sSelector;
   }

   /**
    * @return The message selector.
    */
   public String getSelector()
   {
      return m_sSelector;
   }
   
   /**
    * Sets the connection factory JNDI name.
    * @param sConnectionFactory The connection factory JNDI name to set.
    */
   public void setConnectionFactory(String sConnectionFactory)
   {
      verifyNotReadOnly();
      m_sConnectionFactory = sConnectionFactory;
   }

   /**
    * @return The connection factory JNDI name.
    */
   public String getConnectionFactory()
   {
      return m_sConnectionFactory;
   }

   /**
    * @return True to create the connection factory in the container,
    *    false if the connection factory is external.
    */
   public boolean isConnectionFactoryManaged()
   {
      return m_sConnectionFactory == null || (m_sConnectionFactory.startsWith("class:") && !JMSUtil.isPlatformAdapter(m_sConnectionFactory));
   }

   /**
    * Sets the destination JNDI name.
    * @param sDestination The destination JNDI name to set.
    */
   public void setDestination(String sDestination)
   {
      verifyNotReadOnly();
      m_sDestination = sDestination;
   }

   /**
    * @return The destination JNDI name.
    */
   public String getDestination()
   {
      return m_sDestination;
   }

   /**
    * @return True to create the destination in the container,
    *    false if the destination is external.
    */
   public boolean isDestinationManaged()
   {
      return (m_sDestination == null || m_sDestination.startsWith("class:")) && !JMSUtil.isPlatformAdapter(m_sConnectionFactory);
   }

   /**
    * @return True if a sender is required.
    */
   public boolean isClient()
   {
      return isSendable() || isEnabled() && isConnectionFactoryManaged();
   }

   /**
    * Sets the durable subscription name.
    * @param sSubscription The durable subscription name to set.
    */
   public void setSubscription(String sSubscription)
   {
      verifyNotReadOnly();
      
      if (sSubscription != null && !m_bBroadcast)
      {
         throw new MetadataException("err.meta.integration.mq.subscription",
            new Object[]{m_sName});
      }

      m_sSubscription = sSubscription;
   }

   /**
    * @return The durable subscription name.
    */
   public String getSubscription()
   {
      return m_sSubscription;
   }

   /**
    * Sets the client Id.
    * @param sClientId The client Id to set.
    */
   public void setClientId(String sClientId)
   {
      verifyNotReadOnly();
      
      if (sClientId != null && !m_bBroadcast)
      {
         throw new MetadataException("err.meta.integration.mq.clientId",
            new Object[]{m_sName});
      }

      m_sClientId = sClientId;
   }

   /**
    * @return The client Id.
    */
   public String getClientId()
   {
      return m_sClientId;
   }

   /**
    * @return The connection factory property holder.
    */
   public PropertyHolder getConnectionFactoryPropertyHolder()
   {
      return m_connectionFactoryPropertyHolder;
   }

   /**
    * @return The destination property holder.
    */
   public PropertyHolder getDestinationPropertyHolder()
   {
      return m_destinationPropertyHolder;
   }

   /**
    * @see nexj.core.meta.MetadataObject#makeReadOnly()
    */
   public void makeReadOnly()
   {
      super.makeReadOnly();
      m_connectionFactoryPropertyHolder.makeReadOnly();
      m_destinationPropertyHolder.makeReadOnly();
   }

   /**
    * @see nexj.core.meta.integration.Channel#update(nexj.core.meta.integration.Channel)
    */
   public void update(Channel channel) throws ResourceException
   {
      JMSConsumerConfig config = new JMSConsumerConfig();

      // set new values in consumer config
      config.setChannel(m_sName);
      config.setMaxPoolSize(((MessageQueue)channel).getMaxReceivers());

      // update existing consumer config
      GenericResourceAdapter.getInstance(JMSResourceAdapter.class).update(config);
   }
}
