// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.tcp.ra;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Inet6Address;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.nio.channels.SocketChannel;

/**
 * A standard (non-SSL) socket channel.
 */
public class PlainSocketChannel extends GenericSocketChannel
{
   // constructors

   /**
    * Construct a standard client socket channel
    */
   public PlainSocketChannel()
   {
   }

   /**
    * Construct a standard server socket channel.
    * @param socketChannel The SocketChannel returned from a call to accept() by a listening socket.
    */
   public PlainSocketChannel(SocketChannel socketChannel)
   {
      m_socketChannel = socketChannel;
      m_socket = socketChannel.socket();
   }

   // operations

   /**
    * @see nexj.core.rpc.tcp.ra.GenericSocketChannel#connect(java.net.InetSocketAddress, java.net.InetSocketAddress, int, int, int)
    */
   public boolean connect(InetSocketAddress remoteAddress, InetSocketAddress localAddress,
      int nTimeout, int nReceiverBufferSize, int nSenderBufferSize) throws IOException
   {
      if (remoteAddress.getAddress() instanceof Inet6Address)
      {
         m_socket = new Socket();
      }
      else
      {
         m_socketChannel = SocketChannel.open();
         m_socket = m_socketChannel.socket();
      }

      if (nReceiverBufferSize > 0)
      {
         m_socket.setReceiveBufferSize(nReceiverBufferSize);
      }

      if (nSenderBufferSize > 0)
      {
         m_socket.setSendBufferSize(nSenderBufferSize);
      }

      m_socket.bind(localAddress);
      m_socket.connect(remoteAddress, nTimeout);

      return m_socket.isConnected();
   }

   /**
    * @see nexj.core.rpc.tcp.ra.GenericSocketChannel#getInputStream()
    */
   public InputStream getInputStream() throws IOException
   {
      return m_socket.getInputStream();
   }

   /**
    * @see nexj.core.rpc.tcp.ra.GenericSocketChannel#getOutputStream()
    */
   public OutputStream getOutputStream() throws IOException
   {
      return m_socket.getOutputStream();
   }

   /**
    * @see nexj.core.rpc.tcp.ra.GenericSocketChannel#close()
    */
   public void close() throws IOException
   {
      if (m_socketChannel != null)
      {
         m_socketChannel.close();

         return;
      }

      if (m_socket != null)
      {
         m_socket.close();
      }
   }
}
