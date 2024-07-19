// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.channel.udp;

import java.nio.charset.Charset;

import nexj.core.meta.MetadataException;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.channel.jms.MessageQueue;
import nexj.core.rpc.udp.UDPConnectionPool;
import nexj.core.rpc.udp.UDPConsumerPool;
import nexj.core.util.ObjUtil;
import nexj.core.util.pool.consumer.ConsumerAdapter;
import nexj.core.util.pool.consumer.ConsumerConfig;
import nexj.core.util.pool.consumer.ConsumerPool;
import nexj.core.util.pool.consumer.ConsumerPoolProvider;
import nexj.core.util.pool.resource.ResourcePool;
import nexj.core.util.pool.resource.ResourcePoolProvider;

/**
 * UDP channel metadata
 */
public class UDPChannel extends Channel implements ResourcePoolProvider, ConsumerPoolProvider
{
   // attributes

   /**
    * The remote host.
    */
   protected String m_sHost;

   /**
    * The remote port.
    */
   protected int m_nPort = -1;
   
   /**
    * The local host for binding.
    */
   protected String m_sLocalHost;

   /**
    * The local port for binding (0 is ephemeral).
    */
   protected int m_nLocalPort;
   
   /**
    * The multicast group.
    */
   protected String m_sGroup;

   /**
    * The string message encoding.
    */
   protected String m_sEncoding;

   /**
    * The maximum received packet size in bytes.
    */
   protected int m_nMaxPacketSize = 8192;

   /**
    * The packet time-to-live in hops.
    */
   protected int m_nTTL = 128;

   /**
    * The RFC 1349 type-of-service.
    */
   protected int m_nTOS;
   
   /**
    * The maximum connection pool size.
    */
   protected int m_nMaxSenders = 16;
   
   /**
    * The maximum receiver thread pool size.
    */
   protected int m_nMaxReceivers = 4;

   /**
    * The default user account for processing the messages.
    */
   protected String m_sDefaultUser;

   // associations
   
   /**
    * The optional queue for forwarding the received messages.
    */
   protected MessageQueue m_queue;

   // constructors
   
   /**
    * Constructs the metadata object.
    * @param sName The channel name.
    */
   public UDPChannel(String sName)
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
      return false;
   }

   /**
    * Sets the remote host.
    * @param sHost The remote host to set.
    */
   public void setHost(String sHost)
   {
      verifyNotReadOnly();
      m_sHost = sHost;
   }

   /**
    * @return The remote host.
    */
   public String getHost()
   {
      return m_sHost;
   }

   /**
    * Sets the remote port.
    * @param nPort The remote port to set.
    */
   public void setPort(int nPort)
   {
      verifyNotReadOnly();
      m_nPort = nPort;
   }

   /**
    * @return The remote port.
    */
   public int getPort()
   {
      return m_nPort;
   }

   /**
    * Sets the local host for binding.
    * @param sLocalHost The local host for binding to set.
    */
   public void setLocalHost(String sLocalHost)
   {
      verifyNotReadOnly();
      m_sLocalHost = sLocalHost;
   }

   /**
    * @return The local host for binding.
    */
   public String getLocalHost()
   {
      return m_sLocalHost;
   }

   /**
    * Sets the local port for binding (0 is ephemeral).
    * @param nLocalPort The local port for binding (0 is ephemeral) to set.
    */
   public void setLocalPort(int nLocalPort)
   {
      verifyNotReadOnly();
      m_nLocalPort = nLocalPort;
   }

   /**
    * @return The local port for binding (0 is ephemeral).
    */
   public int getLocalPort()
   {
      return m_nLocalPort;
   }

   /**
    * Sets the multicast group.
    * @param sGroup The multicast group to set.
    */
   public void setGroup(String sGroup)
   {
      verifyNotReadOnly();
      m_sGroup = sGroup;
      
      if (m_sHost == null)
      {
         m_sHost = sGroup;
      }
   }

   /**
    * @return The multicast group.
    */
   public String getGroup()
   {
      return m_sGroup;
   }

   /**
    * Sets the string message encoding.
    * @param sEncoding The string message encoding to set.
    */
   public void setEncoding(String sEncoding)
   {
      verifyNotReadOnly();

      if (sEncoding != null)
      {
         try
         {
            Charset.forName(sEncoding);
         }
         catch (IllegalArgumentException e)
         {
            throw new MetadataException("err.meta.encoding", new Object[]{sEncoding});
         }
      }

      m_sEncoding = sEncoding;
   }

   /**
    * @return The string message encoding.
    */
   public String getEncoding()
   {
      return m_sEncoding;
   }

   /**
    * Sets the maximum received packet size in bytes.
    * @param nMaxPacketSize The maximum received packet size in bytes to set.
    */
   public void setMaxPacketSize(int nMaxPacketSize)
   {
      verifyNotReadOnly();
      m_nMaxPacketSize = nMaxPacketSize;
   }

   /**
    * @return The maximum received packet size in bytes.
    */
   public int getMaxPacketSize()
   {
      return m_nMaxPacketSize;
   }

   /**
    * Sets the packet time-to-live in hops.
    * @param nTTL The packet time-to-live in hops to set.
    */
   public void setTTL(int nTTL)
   {
      verifyNotReadOnly();
      m_nTTL = nTTL;
   }

   /**
    * @return The packet time-to-live in hops.
    */
   public int getTTL()
   {
      return m_nTTL;
   }

   /**
    * Sets the RFC 1349 type-of-service.
    * @param nTOS The RFC 1349 type-of-service to set.
    */
   public void setTOS(int nTOS)
   {
      verifyNotReadOnly();
      m_nTOS = nTOS;
   }

   /**
    * @return The RFC 1349 type-of-service.
    */
   public int getTOS()
   {
      return m_nTOS;
   }
   
   /**
    * Sets the maximum connection pool size.
    * @param nMaxSenders The maximum connection pool size to set.
    */
   public void setMaxSenders(int nMaxSenders)
   {
      verifyNotReadOnly();
      m_nMaxSenders = nMaxSenders;
   }

   /**
    * @return The maximum connection pool size.
    */
   public int getMaxSenders()
   {
      return m_nMaxSenders;
   }
   
   /**
    * Sets the maximum receiver thread pool size.
    * @param nMaxReceivers The maximum receiver pool size to set.
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
    * Sets the default user account for processing the messages.
    * @param sDefaultUser The default user account for processing the messages to set.
    */
   public void setDefaultUser(String sDefaultUser)
   {
      verifyNotReadOnly();
      m_sDefaultUser = sDefaultUser;
   }

   /**
    * @return The default user account for processing the messages.
    */
   public String getDefaultUser()
   {
      return m_sDefaultUser;
   }

   /**
    * Sets the optional queue for forwarding the received messages.
    * @param queue The optional queue for forwarding the received messages to set.
    */
   public void setQueue(Channel queue)
   {
      verifyNotReadOnly();

      if (queue != null && !(queue instanceof MessageQueue))
      {
         throw new MetadataException("err.meta.integration.udp.queueChannel", new Object[]{getName()});
      }

      setQueue((MessageQueue)queue);
   }
   
   /**
    * Sets the optional queue for forwarding the received messages.
    * @param queue The optional queue for forwarding the received messages to set.
    */
   public void setQueue(MessageQueue queue)
   {
      verifyNotReadOnly();

      if (queue != null)
      {
         if (!m_bReceivable)
         {
            throw new MetadataException("err.meta.integration.udp.queue",
               new Object[]{queue.getName(), getName()});
         }
         
         if (!queue.isSendable())
         {
            throw new MetadataException("err.meta.integration.udp.nonSenderQueue",
               new Object[]{queue.getName(), getName()});
         }
      }

      m_queue = queue;
   }

   /**
    * @return The optional queue for forwarding the received messages.
    */
   public MessageQueue getQueue()
   {
      return m_queue;
   }

   /**
    * @see nexj.core.meta.integration.Channel#isCompatible(nexj.core.util.pool.consumer.ConsumerConfig)
    */
   public boolean isCompatible(ConsumerConfig config)
   {
      UDPChannel udp = (UDPChannel)config;

      return ObjUtil.equal(m_sLocalHost, udp.getLocalHost()) &&
         ObjUtil.equal(m_sGroup, udp.getGroup()) &&
         m_nLocalPort == udp.getLocalPort() &&
         m_nMaxPacketSize == udp.getMaxPacketSize();
   }

   /**
    * @see nexj.core.util.pool.resource.ResourcePoolProvider#createResourcePool()
    */
   public ResourcePool createResourcePool()
   {
      return new UDPConnectionPool((UDPChannel)clone());
   }

   /**
    * @see nexj.core.util.pool.consumer.ConsumerPoolProvider#createConsumerPool(nexj.core.util.pool.consumer.ConsumerAdapter)
    */
   public ConsumerPool createConsumerPool(ConsumerAdapter adapter)
   {
      return new UDPConsumerPool(this, adapter);
   }
}
