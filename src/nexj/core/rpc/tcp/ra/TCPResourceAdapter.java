// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.tcp.ra;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.Iterator;

import javax.resource.ResourceException;
import javax.resource.spi.ActivationSpec;
import javax.resource.spi.ConnectionManager;
import javax.resource.spi.endpoint.MessageEndpointFactory;

import nexj.core.rpc.ra.GenericConnectionManager;
import nexj.core.rpc.ra.GenericConsumerPool;
import nexj.core.rpc.ra.GenericResourceAdapter;
import nexj.core.util.HashTab;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;

/**
 * TCP resource adapter.
 */
public class TCPResourceAdapter extends GenericResourceAdapter
{
   // constants

   /**
    * The minimum TCP port value.
    */
   public static int MIN_PORT = 0;

   /**
    * The maximum TCP port value.
    */
   public static int MAX_PORT = 65535;

   // associations

   /**
    * Reusable lookup key.
    */
   private static SocketKey s_key;

   /**
    * Two-way map between SocketKeys and Sockets.
    * Only contains server sockets.
    */
   private static Lookup s_socketMap = new HashTab();  // SocketKey[GenericSocketChannel] & GenericSocketChannel[SocketKey]

   /**
    * The default connection manager.
    */
   private static GenericConnectionManager s_defaultConnectionManager;

   /**
    * The class logger.
    */
   private final static Logger s_logger = Logger.getLogger(TCPResourceAdapter.class);

   // constructors

   /**
    * Constructs the resource adapter.
    */
   public TCPResourceAdapter()
   {
      super(s_logger);
   }

   // operations

   /**
    * Inserts a socket object into the cache.
    * @param socketChannel The socket object to insert.
    */
   public static void addSocketChannel(GenericSocketChannel socketChannel)
   {
      synchronized (s_socketMap)
      {
         if (s_key == null)
         {
            s_key = new SocketKey((InetSocketAddress)socketChannel.getRemoteSocketAddress(),
               (InetSocketAddress)socketChannel.getLocalSocketAddress());
         }
         else
         {
            s_key.setAddresses((InetSocketAddress)socketChannel.getRemoteSocketAddress(),
               (InetSocketAddress)socketChannel.getLocalSocketAddress());
         }

         GenericSocketChannel cachedSocketChannel = (GenericSocketChannel)s_socketMap.get(s_key);

         if (cachedSocketChannel != null)
         {
            removeSocketChannel(cachedSocketChannel);
         }

         s_socketMap.put(s_key, socketChannel);
         s_socketMap.put(socketChannel, s_key);
         s_key = null;
      }
   }

   /**
    * Gets a socket from the cache.
    * @param remoteAddr The remote socket address.
    * @param localAddr The local socket address.
    * @return The cached socket or null if it's not found.
    */
   public static GenericSocketChannel getSocketChannel(InetSocketAddress remoteAddr, InetSocketAddress localAddr)
   {
      synchronized (s_socketMap)
      {
         if (s_key == null)
         {
            s_key = new SocketKey(remoteAddr, localAddr);
         }
         else
         {
            s_key.setAddresses(remoteAddr, localAddr);
         }

         return (GenericSocketChannel)s_socketMap.get(s_key);
      }
   }

   /**
    * Removes a socket channel from the cache.
    * @param socketChannel The socket channel to be removed.
    */
   public static void removeSocketChannel(GenericSocketChannel socketChannel)
   {
      synchronized (s_socketMap)
      {
         SocketKey key = (SocketKey)s_socketMap.remove(socketChannel);

         if (key != null)
         {
            s_socketMap.remove(key);
         }
      }
   }

   /**
    * @return The default connection manager.
    */
   public static synchronized ConnectionManager getDefaultConnectionManager()
   {
      if (s_defaultConnectionManager == null)
      {
         s_defaultConnectionManager = new GenericConnectionManager();
      }

      return s_defaultConnectionManager;
   }

   /**
    * Activates an endpoint and waits for the consumer pool to connect before returning.
    * @param factory The message endpoint factory.
    * @param cfg The activation specification.
    */
   public void startConsumerPool(MessageEndpointFactory factory, ActivationSpec cfg) throws ResourceException
   {
      endpointActivation(factory, cfg);

      TCPConsumerPool pool = null;

      synchronized (m_poolMap)
      {
         pool = (TCPConsumerPool)m_poolMap.get(cfg);
      }

      try
      {
         pool.waitForConnection();
      }
      catch (InterruptedException e)
      {
         throw new ResourceException("Failed to start consumer pool", e);
      }
   }

   /**
    * @see nexj.core.rpc.ra.GenericResourceAdapter#createConsumerPool(javax.resource.spi.endpoint.MessageEndpointFactory, javax.resource.spi.ActivationSpec)
    */
   protected GenericConsumerPool createConsumerPool(MessageEndpointFactory factory, ActivationSpec cfg) throws ResourceException
   {
      return new TCPConsumerPool(this, factory, (TCPConsumerConfig)cfg);
   }

   /**
    * @see nexj.core.rpc.ra.GenericResourceAdapter#stop()
    */
   public void stop()
   {
      synchronized (TCPResourceAdapter.class)
      {
         if (s_defaultConnectionManager != null)
         {
            s_defaultConnectionManager.clear();
            s_defaultConnectionManager = null;
         }
      }

      synchronized (s_socketMap)
      {
         for (Iterator itr = s_socketMap.valueIterator(); itr.hasNext(); )
         {
            Object obj = itr.next();

            if (obj instanceof GenericSocketChannel)
            {
               try
               {
                  ((GenericSocketChannel)obj).close();
               }
               catch (IOException e)
               {
               }
            }
         }

         s_socketMap.clear();
      }

      super.stop();
   }

   // inner classes

   /**
    * The socket key (for the lookup).
    */
   protected static class SocketKey
   {
      // associations

      /**
       * The local address.
       */
      protected InetSocketAddress m_localAddr;

      /**
       * The remote address.
       */
      protected InetSocketAddress m_remoteAddr;

      // constructors

      /**
       * Constructs the address.
       * @param remoteAddr The remote socket address.
       * @param localAddr The local socket address.
       */
      public SocketKey(InetSocketAddress remoteAddr, InetSocketAddress localAddr)
      {
         m_localAddr = localAddr;
         m_remoteAddr = remoteAddr;
      }

      // operations

      /**
       * Sets the addresses.
       * @param remoteAddr The remote socket address.
       * @param localAddr The local socket address.
       */
      public void setAddresses(InetSocketAddress remoteAddr, InetSocketAddress localAddr)
      {
         m_localAddr = localAddr;
         m_remoteAddr = remoteAddr;
      }

      /**
       * @return The local address.
       */
      public InetSocketAddress getLocalAddress()
      {
         return m_localAddr;
      }

      /**
       * @return The remote address.
       */
      public InetSocketAddress getRemoteAddress()
      {
         return m_remoteAddr;
      }

      /**
       * @see java.lang.Object#equals(java.lang.Object)
       */
      public boolean equals(Object obj)
      {
         if (obj instanceof SocketKey)
         {
            SocketKey key = (SocketKey)obj;

            if (ObjUtil.equal(key.getRemoteAddress().getAddress().getHostAddress(),
               m_remoteAddr.getAddress().getHostAddress())
               && key.getRemoteAddress().getPort() == m_remoteAddr.getPort())
            {
               if (ObjUtil.equal(key.getLocalAddress().getAddress().getHostAddress(),
                  m_localAddr.getAddress().getHostAddress()))
               {
                  return true;
               }
            }
         }

         return false;
      }

      /**
       * @see java.lang.Object#hashCode()
       */
      public int hashCode()
      {
         return (m_remoteAddr.getAddress().getHostAddress().hashCode() + m_remoteAddr.getPort())
            << 16 ^ m_localAddr.getAddress().getHostAddress().hashCode();
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         return "SocketKey(" + m_localAddr.getAddress().getHostAddress() + " <-> " + m_remoteAddr + ')';
      }
   }
}
