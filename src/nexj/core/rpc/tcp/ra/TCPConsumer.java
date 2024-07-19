// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.tcp.ra;

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Method;
import java.net.InetSocketAddress;
import java.security.cert.Certificate;

import javax.resource.spi.endpoint.MessageEndpoint;

import nexj.core.rpc.ra.GenericConsumer;
import nexj.core.rpc.tcp.TCPListener;
import nexj.core.util.Logger;

/**
 * TCP consumer, corresponding to one active instance of an MDB.
 */
public class TCPConsumer extends GenericConsumer
{
   // associations

   /**
    * The socket channel.
    */
   protected GenericSocketChannel m_socketChannel;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(TCPConsumer.class);

   /**
    * The TCPListener.onMessage() method object.
    */
   protected final static Method TCPLISTENER_ONMESSAGE_METHOD = getMethod(TCPListener.class,
      "onMessage", new Class[]{InputStream.class, InetSocketAddress.class,
      InetSocketAddress.class, Certificate[].class});

   // constructors

   /**
    * Create a TCP consumer.
    * @param pool A reference to the TCP consumer pool.
    * @throws Throwable if the TCPConsumer cannot be created.
    */
   public TCPConsumer(TCPConsumerPool pool) throws Throwable
   {
      super(pool, s_logger);
   }

   // operations

   /**
    * Start the consumer.
    * @param socketChannel The socket channel that is waiting to be read from.
    * @throws Throwable if the consumer cannot be started.
    */
   public void start(GenericSocketChannel socketChannel) throws Throwable
   {
      m_socketChannel = socketChannel;
      activate();
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumer#close()
    */
   protected void close()
   {
      try
      {
         m_socketChannel.close();
      }
      catch (IOException e)
      {
      }
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumer#consume()
    */
   protected boolean consume() throws Throwable
   {
      ((TCPConsumerPool)m_pool).addActiveSocketChannel(m_socketChannel);
      consume(m_socketChannel);
      ((TCPConsumerPool)m_pool).removeActiveSocketChannel(m_socketChannel);

      return false;
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumer#deliver(java.lang.Object)
    */
   protected void deliver(Object message) throws Throwable
   {
      GenericSocketChannel socketChannel = (GenericSocketChannel)message;

      if (socketChannel == null)
      {
         return;
      }

      if (socketChannel.isInactive())
      {
         TCPResourceAdapter.removeSocketChannel(socketChannel);
         socketChannel.close();
      }
      else if (!socketChannel.isOpen() || !socketChannel.isConnected())
      {
         // end of stream
         TCPResourceAdapter.removeSocketChannel(socketChannel);
      }
      else
      {
         socketChannel.configureBlocking(true);

         try
         {
            // deliver the InputStream to the listener
            ((TCPListener)m_endpoint).onMessage(socketChannel.getInputStream(),
               socketChannel.getRemoteSocketAddress(),
               socketChannel.getLocalSocketAddress(),
               socketChannel.getPeerCertificates());
         }
         finally
         {
            if (!socketChannel.isOpen() || !socketChannel.isConnected())
            {
               // Finished with socket
               TCPResourceAdapter.removeSocketChannel(socketChannel);
            }
         }
      }
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumer#getMethod()
    */
   protected Method getMethod()
   {
      return TCPLISTENER_ONMESSAGE_METHOD;
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumer#init()
    */
   protected MessageEndpoint init() throws Throwable
   {
      return m_pool.getFactory().createEndpoint(null);
   }
}
