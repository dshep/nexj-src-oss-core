package nexj.core.rpc.udp;

import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.MulticastSocket;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.security.AccessController;
import java.security.PrivilegedActionException;
import java.security.PrivilegedExceptionAction;

import nexj.core.meta.integration.channel.udp.UDPChannel;
import nexj.core.rpc.pool.ChannelConsumerPool;
import nexj.core.util.NetUtil;
import nexj.core.util.pool.consumer.ConsumerAdapter;
import nexj.core.util.pool.consumer.GenericConsumer;

/**
 * UDP consumer pool.
 */
public class UDPConsumerPool extends ChannelConsumerPool
{
   // associations

   /**
    * The listener socket.
    */
   protected DatagramSocket m_socket;

   // constructors

   /**
    * Constructs the consumer pool.
    * @param channel The UDP channel.
    * @param adapter The consumer adapter.
    */
   public UDPConsumerPool(UDPChannel channel, ConsumerAdapter adapter)
   {
      super(channel, adapter);
   }

   // operations

   /**
    * @see nexj.core.util.pool.consumer.GenericConsumerPool#isSynchronized()
    */
   protected boolean isSynchronized()
   {
      return false;
   }

   /**
    * @see nexj.core.util.pool.consumer.GenericConsumerPool#connect()
    */
   protected void connect() throws Throwable
   {
      try
      {
         AccessController.doPrivileged(new PrivilegedExceptionAction()
         {
            public Object run() throws Exception
            {
               UDPChannel config = (UDPChannel)m_config;
               InetAddress host = null;
               InetAddress group = null;

               if (config.getLocalHost() != null)
               {
                  host = InetAddress.getByName(config.getLocalHost());
               }

               if (config.getGroup() != null)
               {
                  group = InetAddress.getByName(config.getGroup());
               }
               else if (host != null && host.isMulticastAddress())
               {
                  group = host;
                  host = null;
               }

               InetSocketAddress bindAddress = new InetSocketAddress(host,
                  (group == null) ? NetUtil.getBindPort(config.getLocalPort()) : config.getLocalPort());

               if (group != null)
               {
                  MulticastSocket socket = new MulticastSocket(bindAddress);

                  synchronized (this)
                  {
                     m_socket = socket;
                  }

                  socket.joinGroup(group);
               }
               else if (bindAddress.getPort() <= 0)
               {
                  DatagramChannel channel = DatagramChannel.open();

                  synchronized (this)
                  {
                     m_socket = channel.socket();
                  }

                  m_socket.bind(bindAddress);
               }
               else
               {
                  synchronized (this)
                  {
                     m_socket = UDPConnection.openSocket(bindAddress.getAddress(), bindAddress.getPort());
                  }
               }

               m_socket.setReceiveBufferSize(config.getMaxPacketSize());

               return null;
            }
         });
      }
      catch (PrivilegedActionException e)
      {
         throw e.getException();
      }
   }

   /**
    * @see nexj.core.util.pool.consumer.GenericConsumerPool#listen()
    */
   protected void listen()
   {
      DatagramSocket socket = null;
      UDPConsumer consumer = null;

      try
      {
         consumer = (UDPConsumer)getConsumer();

         synchronized (this)
         {
            socket = m_socket;
         }

         if (socket != null)
         {
            AccessController.doPrivileged(new DatagramSocketReceiveAction(socket, consumer.getPacket()));
            consumer.setTime((isMonitored()) ? System.currentTimeMillis() : 0);
            consumer.start();
            consumer = null;
         }
      }
      catch (Throwable t)
      {
         if (t instanceof PrivilegedActionException)
         {
            t = ((PrivilegedActionException)t).getException();
         }

         if (socket == null || !socket.isClosed())
         {
            handleException(t);
         }
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
    * @see nexj.core.util.pool.consumer.GenericConsumerPool#suspend()
    */
   protected void suspend()
   {
      close();
   }

   /**
    * @see nexj.core.util.pool.consumer.GenericConsumerPool#interrupt()
    */
   protected void interrupt()
   {
      close();
   }

   /**
    * @see nexj.core.util.pool.consumer.GenericConsumerPool#close()
    */
   protected void close()
   {
      if (m_socket != null)
      {
         if (m_bStop)
         {
            if (m_socket.getChannel() != null)
            {
               try
               {
                  m_socket.getChannel().close();
               }
               catch (Throwable t)
               {
               }
            }

            m_socket.close();
         }
         else
         {
            UDPConnection.closeSocket(m_socket);
         }

         m_socket = null;
      }
   }

   /**
    * @see nexj.core.util.pool.consumer.GenericConsumerPool#createConsumer()
    */
   protected GenericConsumer createConsumer() throws Throwable
   {
      return new UDPConsumer(this);
   }

   /**
    * @return The listener socket.
    */
   public synchronized DatagramSocket getSocket()
   {
      return m_socket;
   }

   // inner classes

   /**
    * Privileged action for packet receiving.
    */
   protected static class DatagramSocketReceiveAction implements PrivilegedExceptionAction
   {
      protected DatagramSocket m_socket;
      protected DatagramPacket m_packet;
      
      public DatagramSocketReceiveAction(DatagramSocket socket, DatagramPacket packet)
      {
         m_socket = socket;
         m_packet = packet;
      }

      public Object run() throws Exception
      {
         if (m_socket.getChannel() != null)
         {
            ByteBuffer buf = ByteBuffer.wrap(m_packet.getData());

            m_packet.setSocketAddress(m_socket.getChannel().receive(buf));
            m_packet.setLength(buf.position());
         }
         else
         {
            m_socket.receive(m_packet);
         }

         return null;
      }
   }
}
