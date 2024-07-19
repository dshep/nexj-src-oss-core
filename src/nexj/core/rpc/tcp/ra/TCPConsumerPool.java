// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.tcp.ra;

import nexj.core.meta.Primitive;
import nexj.core.rpc.ra.GenericConsumer;
import nexj.core.rpc.ra.GenericConsumerPool;
import nexj.core.util.GenericLinkedHashTab;
import nexj.core.util.IdentityHashHolder;
import nexj.core.util.LinkedHashTab;
import nexj.core.util.Logger;
import nexj.core.util.NetUtil;

import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.nio.channels.CancelledKeyException;
import java.nio.channels.ClosedSelectorException;
import java.nio.channels.SelectableChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.security.AccessController;
import java.security.PrivilegedExceptionAction;
import java.util.Iterator;
import java.util.Set;
import java.util.concurrent.ConcurrentLinkedQueue;

import javax.resource.spi.endpoint.MessageEndpointFactory;

/**
 * TCP consumer endpoint pool, corresponding to the MDBs with the same configuration object.
 */
public class TCPConsumerPool extends GenericConsumerPool
{
   // associations

   /**
    * The multiplexing selector object.
    */
   protected Selector m_selector;

   /**
    * The listener socket.
    */
   protected ServerSocket m_listenerSocket;

   /**
    * Set of active sockets currently being used by TCPConsumers.
    */
   protected Set m_activeSet = new IdentityHashHolder();  // of type GenericSocketChannel[]

   /**
    * The queue of channels waiting to be registered with the Selector.
    */
   protected ConcurrentLinkedQueue m_readyQueue = new ConcurrentLinkedQueue();  // of type GenericSocketChannel[]

   /**
   * A map to keep track of the time of most recent activity on each channel.
   */
   protected GenericLinkedHashTab m_lastActiveTimeQueue = new LinkedHashTab();   // Long[GenericSocketChannel]

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(TCPConsumerPool.class);

   // constructors

   /**
    * Constructs the consumer pool.
    * @param adapter The TCP resource adapter.
    * @param factory The message endpoint factory.
    * @param config The TCP consumer config object.
    */
   public TCPConsumerPool(TCPResourceAdapter adapter, MessageEndpointFactory factory, TCPConsumerConfig config)
   {
      super(adapter, factory, config, s_logger);
   }

   // operations

   /**
    * Add an active socket channel to the list.
    * @param channel A new active socket channel.
    */
   public void addActiveSocketChannel(GenericSocketChannel channel)
   {
      synchronized (m_activeSet)
      {
         m_activeSet.add(channel);
      }
   }

   /**
    * Removes a previously added socket channel that is no longer active.
    * @param socketChannel The socket channel to remove.
    */
   public void removeActiveSocketChannel(GenericSocketChannel socketChannel)
   {
      synchronized (m_activeSet)
      {
         m_activeSet.remove(socketChannel);
      }

      if (!socketChannel.isOpen() || !socketChannel.isConnected())
      {
         // Finished with socket
         TCPResourceAdapter.removeSocketChannel(socketChannel);
      }
      else
      {
         // Tell the TCPConsumerPool to continue listening on this socket
         try
         {
            socketChannel.configureBlocking(false);
         }
         catch (IOException e)
         {
            s_logger.debug("I/O error, closing socket channel", e);
            TCPResourceAdapter.removeSocketChannel(socketChannel);

            return;
         }

         m_readyQueue.offer(socketChannel);
         m_selector.wakeup();
      }
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumerPool#close()
    */
   protected void close()
   {
      synchronized (this)
      {
         if (!m_bShutdown)
         {
            return;
         }
      }

      // close listener socket
      if (m_listenerSocket != null)
      {
         try
         {
            m_listenerSocket.close();
         }
         catch (IOException e)
         {
         }
      }

      // close sockets in selector
      if (m_selector != null && m_selector.isOpen())
      {
         synchronized (m_selector)
         {
            Set keySet = m_selector.keys();
            Iterator itr = keySet.iterator();

            while (itr.hasNext())
            {
               try
               {
                  SelectionKey key = (SelectionKey)itr.next();
                  SelectableChannel channel = key.channel();

                  channel.close();
               }
               catch (IOException e)
               {
               }
            }
         }

         // close selector
         try
         {
            m_selector.close();
         }
         catch (IOException e)
         {
         }
      }

      // close active socket channels
      synchronized (m_activeSet)
      {
         for (Iterator itr = m_activeSet.iterator(); itr.hasNext();)
         {
            try
            {
               GenericSocketChannel channel = (GenericSocketChannel)itr.next();

               channel.close();
            }
            catch (IOException e)
            {
            }
         }
      }

      // empty listen more queue
      while (!m_readyQueue.isEmpty())
      {
         try
         {
            GenericSocketChannel channel = (GenericSocketChannel)m_readyQueue.poll();

            channel.close();
         }
         catch (IOException e)
         {
         }
      }
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumerPool#connect()
    */
   protected void connect() throws Throwable
   {
      AccessController.doPrivileged(new PrivilegedExceptionAction()
      {
         public Object run() throws Exception
         {
            TCPConsumerConfig config = (TCPConsumerConfig)m_config;
            InetAddress host = null;

            if (config.getLocalHost() != null)
            {
               host = InetAddress.getByName(config.getLocalHost());
            }

            InetSocketAddress bindAddress = new InetSocketAddress(host, NetUtil.getBindPort(config.getLocalPort()));
            ServerSocketChannel channel = ServerSocketChannel.open();

            m_listenerSocket = channel.socket();

            if (config.getReceiverBufferSize() > 0)
            {
               m_listenerSocket.setReceiveBufferSize(config.getReceiverBufferSize());
            }

            m_listenerSocket.bind(bindAddress, config.getBacklog());

            // Initialize Selector
            if (m_selector == null || !m_selector.isOpen())
            {
               try
               {
                  m_selector = Selector.open();
               }
               catch (IOException e)
               {
                  s_logger.error("Failed to open a selector", e);
               }
            }

            m_listenerSocket.getChannel().configureBlocking(false);
            m_listenerSocket.getChannel().register(m_selector, SelectionKey.OP_ACCEPT, null);

            return null;
         }
      });

      synchronized (this)
      {
         notifyAll();
      }
   }

   /**
    * Blocks until this is connected or is shutdown.
    */
   public synchronized void waitForConnection() throws InterruptedException
   {
      while ((m_listenerSocket == null || m_listenerSocket.isClosed()) && !m_bShutdown)
      {
         wait();
      }
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumerPool#createConsumer()
    */
   protected GenericConsumer createConsumer() throws Throwable
   {
      return new TCPConsumer(this);
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumerPool#isTransactional()
    */
   public boolean isTransactional()
   {
      return false;
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumerPool#stop()
    */
   protected void stop()
   {
      close();
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumerPool#isSynchronized()
    */
   protected boolean isSynchronized()
   {
      return false;
   }

   /**
    * Blocks on select() to listen for new connections (OP_ACCEPT)
    * and existing connections' read availability (OP_READ).
    * @see nexj.core.rpc.ra.GenericConsumerPool#listen()
    */
   protected void listen()
   {
      int nKeyCount = 0;
      long lIdleTimeout = ((TCPConsumerConfig)m_config).getIdleTimeout() * 60000;
      long lCurrentTime = System.currentTimeMillis();

      try
      {
         while (!m_readyQueue.isEmpty())
         {
            GenericSocketChannel channel = (GenericSocketChannel)m_readyQueue.poll();

            // re-set the read timeout
            channel.setSoTimeout(((TCPConsumerConfig)m_config).getReadTimeout());

            if (channel.getInputStream().available() > 0)
            {
               // no need to go back into the selector
               startConsumer(channel);

               continue;
            }

            if (lIdleTimeout > 0)
            {
               // update channel's last active time
               m_lastActiveTimeQueue.putLast(channel, Primitive.createLong(lCurrentTime));
            }

            try
            {
               channel.register(m_selector, SelectionKey.OP_READ);
            }
            catch (CancelledKeyException e)
            {
               // A canceled key is only removed at the next select() operation
               // Therefore, if a register() is attempted before then, this
               // exception will be raised
               m_selector.select(1);
               channel.register(m_selector, SelectionKey.OP_READ);
            }
         }

         if (lIdleTimeout > 0 && m_lastActiveTimeQueue.size() > 0)
         {
            // Determine timeout value for select
            long lTimeDiff = lCurrentTime - ((Long)m_lastActiveTimeQueue.firstValue()).longValue();
            long lTimeout = Math.max(lIdleTimeout - lTimeDiff, 1);

            nKeyCount = m_selector.select(lTimeout);
         }
         else
         {
            nKeyCount = m_selector.select();
         }

         if (s_logger.isDumpEnabled())
         {
            s_logger.dump("Selected " + nKeyCount + " keys");
         }

         synchronized (this)
         {
            if (m_bShutdown)
            {
               return;
            }
         }
      }
      catch (IOException e)
      {
         s_logger.debug("I/O error on select()", e);
      }
      catch (ClosedSelectorException cse)
      {
         s_logger.error("Selector is closed", cse);

         return;
      }

      lCurrentTime = System.currentTimeMillis();

      // Determine which channels to close due to inactivity
      while (lIdleTimeout > 0 && m_lastActiveTimeQueue.size() > 0)
      {
         long lTimeDiff = lCurrentTime - ((Long)m_lastActiveTimeQueue.firstValue()).longValue();

         if (lTimeDiff < lIdleTimeout)
         {
            break;
         }

         GenericSocketChannel channel = (GenericSocketChannel)m_lastActiveTimeQueue.firstKey();
         SelectionKey key = channel.getKey(m_selector);

         m_lastActiveTimeQueue.removeFirst();

         if (key != null && !key.isReadable())
         {
            if (s_logger.isDebugEnabled())
            {
               s_logger.debug("Closing inactive connection. (" + channel.getLocalSocketAddress()
                  + " <-> " + channel.getRemoteSocketAddress() + ")");
            }

            key.cancel();
            channel.setInactive(true);
            startConsumer(channel);
         }
      }

      if (nKeyCount <= 0)
      {
         return;
      }

      synchronized (m_selector)
      {
         Set keySet = m_selector.selectedKeys();
         Iterator itr = keySet.iterator();

         while (itr.hasNext())
         {
            SelectionKey key = (SelectionKey)itr.next();

            if (!key.isValid())
            {
               itr.remove();

               continue;
            }

            if (key.isReadable())
            {
               itr.remove();
               key.cancel();
               startConsumer((GenericSocketChannel)key.attachment());
            }
            else if (key.isAcceptable())
            {
               itr.remove();

               // sanity check
               assert m_listenerSocket.getChannel() == key.channel();

               try
               {
                  while (acceptNewClient() != null)
                  {
                  }
               }
               catch (Exception e)
               {
                  s_logger.error("Error while trying to accept new client in " + this, e);
               }
            }
         }
      }
   }

   /**
    * Helper function.
    * Calls accept() on the channel, and returns the resulting client's
    * socket channel.
    * @return The new client's socket channel.
    * @throws IOException if there is an I/O error.
    */
   protected GenericSocketChannel acceptNewClient() throws IOException
   {
      SocketChannel sc = m_listenerSocket.getChannel().accept();

      if (sc == null)
      {
         return null;
      }

      GenericSocketChannel clientSocketChannel;
      TCPConsumerConfig tcpConfig = (TCPConsumerConfig)m_config;

      if (tcpConfig.getSecure())
      {
         clientSocketChannel = new SecureSocketChannel(sc, tcpConfig.getTrustedCertificate(),
            tcpConfig.getCertificateStore(), tcpConfig.getPassword(), (byte)tcpConfig.getAuthentication());
      }
      else
      {
         clientSocketChannel = new PlainSocketChannel(sc);
      }

      if (clientSocketChannel != null)
      {
         // add client to list of active clients
         clientSocketChannel.configureBlocking(false);
         clientSocketChannel.setSoTimeout(tcpConfig.getReadTimeout());
         clientSocketChannel.register(m_selector, SelectionKey.OP_READ);
         clientSocketChannel.setKeepAlive(tcpConfig.isKeepAlive());
         clientSocketChannel.setNoDelay(tcpConfig.isNoDelay());

         if (tcpConfig.getSenderBufferSize() > 0)
         {
            clientSocketChannel.setSenderBufferSize(tcpConfig.getSenderBufferSize());
         }

         TCPResourceAdapter.addSocketChannel(clientSocketChannel);

         // initialize channel's last active time
         if (tcpConfig.getIdleTimeout() > 0)
         {
            m_lastActiveTimeQueue.putLast(clientSocketChannel, Primitive.createLong(System.currentTimeMillis()));
         }
      }

      if (s_logger.isDumpEnabled())
      {
         s_logger.dump("Accepted a " + ((clientSocketChannel == null) ? "NULL" : "NEW") + " client");
      }

      return clientSocketChannel;
   }

   /**
    * Start a TCP consumer with the given socket channel.
    * @param socketChannel The socket channel to start a TCP consumer for.
    */
   protected void startConsumer(GenericSocketChannel socketChannel)
   {
      TCPConsumer consumer = null;

      try
      {
         consumer = (TCPConsumer)getConsumer();
         consumer.start(socketChannel);
         consumer = null;
      }
      catch (Throwable t)
      {
         s_logger.error("Error in " + this, t);
      }
      finally
      {
         if (consumer != null)
         {
            consumer.dispose();
         }
      }
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumerPool#interrupt()
    */
   protected void interrupt()
   {
      close();
   }
}
