// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.tcp.ra;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.SocketException;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.SocketChannel;
import java.security.cert.Certificate;

/**
 * Abstract base class for SSL and non-SSL socket channels.
 */
public abstract class GenericSocketChannel
{
   // attributes

   /**
    * True iff this channel is inactive.
    */
   protected boolean m_bInactive;

   // associations

   /**
    * The underlying socket channel.
    * Unused when we are a connecting to an IPv6 remote address.
    */
   protected SocketChannel m_socketChannel;

   /**
    * The underlying Java socket.
    * Used exclusively when we are a connecting to an IPv6 remote address.
    */
   protected Socket m_socket;

   // abstract methods

   /**
    * Bind to localAddress and connect to remoteAddress.
    * @param remoteAddress The remote address to connect to.
    * @param localAddress The local address to bind to. Can be null.
    * @param nTimeout The connection timeout in milliseconds (0 means infinite).
    * @param nReceiverBufferSize The SO_RCVBUF option for the socket, 0 for system default.
    * @param nSenderBufferSize The SO_SNDBUF option for the socket, 0 for system default.
    * @return True if the connection was successful. False otherwise.
    * @throws IOException if an I/O error occurs.
    */
   public abstract boolean connect(InetSocketAddress remoteAddress, InetSocketAddress localAddress,
      int nTimeout, int nReceiverBufferSize, int nSenderBufferSize) throws IOException;

   /**
    * Bind to localAddress and connect to remoteAddress.
    * @param remoteAddress The remote address to connect to.
    * @param localAddress The local address to bind to. Can be null.
    * @param nTimeout The connection timeout in milliseconds (0 means infinite).
    * @return True if the connection was successful. False otherwise.
    * @throws IOException if an I/O error occurs.
    */
   public boolean connect(InetSocketAddress remoteAddress, InetSocketAddress localAddress,
      int nTimeout) throws IOException
   {
      return connect(remoteAddress, localAddress, nTimeout, 0, 0);
   }

   /**
    * Bind to localAddress and connect to remoteAddress.
    * @param remoteAddress The remote address to connect to.
    * @param localAddress The local address to bind to. Can be null.
    * @return True if the connection was successful. False otherwise.
    * @throws IOException if an I/O error occurs.
    */
   public boolean connect(InetSocketAddress remoteAddress, InetSocketAddress localAddress) throws IOException
   {
      return connect(remoteAddress, localAddress, 0, 0, 0);
   }

   /**
    * Connect to remoteAddress.
    * @param remoteAddress The remote address to connect to.
    * @param nTimeout The connection timeout in milliseconds (0 means infinite).
    * @return True if the connection was successful. False otherwise.
    * @throws IOException if an I/O error occurs.
    */
   public boolean connect(InetSocketAddress remoteAddress, int nTimeout) throws IOException
   {
      return connect(remoteAddress, null, nTimeout, 0, 0);
   }

   /**
    * Connect to remoteAddress.
    * @param remoteAddress The remote address to connect to.
    * @return True if the connection was successful. False otherwise.
    * @throws IOException if an I/O error occurs.
    */
   public boolean connect(InetSocketAddress remoteAddress) throws IOException
   {
      return connect(remoteAddress, null, 0, 0, 0);
   }

   /**
    * Obtain an InputStream for this channel.
    * @return The input stream for the channel.
    * @throws IOException if an I/O error occurs.
    */
   public abstract InputStream getInputStream() throws IOException;

   /**
    * Obtain an OutputStream for this channel.
    * @return The output stream for the channel.
    * @throws IOException if an I/O error occurs.
    */
   public abstract OutputStream getOutputStream() throws IOException;

   /**
    * Close the channel and underlying socket connection.
    * @throws IOException if an I/O error occurs.
    */
   public abstract void close() throws IOException;

   // operations

   /**
    * Set the blocking mode for the channel.
    * @param bBlockingMode The desired blocking mode for the channel.
    * @throws IOException if an I/O error occurs.
    */
   public void configureBlocking(boolean bBlockingMode) throws IOException
   {
      m_socketChannel.configureBlocking(bBlockingMode);
   }

   /**
    * Set the SO_TIMEOUT value for the underlying socket.
    * @param nTimeout The desired SO_TIMEOUT value for the underlying socket.
    * @throws SocketException if a socket error occurs.
    */
   public void setSoTimeout(int nTimeout) throws SocketException
   {
      m_socket.setSoTimeout(nTimeout);
   }

   /**
    * Obtain the SO_TIMEOUT value for the underlying socket.
    * @return The SO_TIMEOUT value for the underlying socket.
    * @throws SocketException if a socket error occurs.
    */
   public int getSoTimeout() throws SocketException
   {
      return m_socket.getSoTimeout();
   }

   /**
    * Register this channel with a selector.
    * @param selector The selector to register with.
    * @param nOps The operation flags to select on.
    * @return The SelectionKey returned by the selector.
    * @throws ClosedChannelException if this channel is closed.
    */
   public SelectionKey register(Selector selector, int nOps) throws ClosedChannelException
   {
      return m_socketChannel.register(selector, nOps, this);
   }

   /**
    * Retrieves the key representing the channel's registration with the given selector.
    * @param selector The selector.
    * @return The key for the given selector.
    */
   public SelectionKey getKey(Selector selector)
   {
      return m_socketChannel.keyFor(selector);
   }

   /**
    * Obtain the local address of the underlying socket.
    * @return The local address of the underlying socket.
    */
   public InetSocketAddress getLocalSocketAddress()
   {
      return (InetSocketAddress)m_socket.getLocalSocketAddress();
   }

   /**
    * Obtain the remote address of the underlying socket.
    * @return The remote address of the underlying socket.
    */
   public InetSocketAddress getRemoteSocketAddress()
   {
      return (InetSocketAddress)m_socket.getRemoteSocketAddress();
   }

   /**
    * Check if this channel is open.
    * @return True iff the channel is open.
    */
   public boolean isOpen()
   {
      return !m_socket.isClosed();
   }

   /**
    * Check if this channel is connected.
    * @return True iff the channel is connected.
    */
   public boolean isConnected()
   {
      return m_socket.isConnected();
   }

   /**
    * Obtain the certificate chain presented by the remote host.
    * @return The certificate chain presented by the remote host. Can be null.
    */
   public Certificate[] getPeerCertificates()
   {
      return null;
   }

   /**
    * @param bInactive True iff this channel is inactive.
    */
   public void setInactive(boolean bInactive)
   {
      m_bInactive = bInactive;
   }

   /**
    * @return True iff this channel is inactive.
    */
   public boolean isInactive()
   {
      return m_bInactive;
   }

   /**
    * @param bKeepAlive True to set the TCP keep-alive property on the underlying socket.
    * @throws SocketException if a socket error occurs.
    */
   public void setKeepAlive(boolean bKeepAlive) throws SocketException
   {
      m_socket.setKeepAlive(bKeepAlive);
   }

   /**
    * @return True if the TCP keep-alive property is set on the underlying socket.
    * @throws SocketException if a socket error occurs.
    */
   public boolean isKeepAlive() throws SocketException
   {
      return m_socket.getKeepAlive();
   }

   /**
    * @param bNoDelay True if the TCP no-delay property should be set on the underlying socket.
    * @throws SocketException if a socket error occurs.
    */
   public void setNoDelay(boolean bNoDelay) throws SocketException
   {
      m_socket.setTcpNoDelay(bNoDelay);
   }

   /**
    * @return True if the TCP no-delay property is set on the underlying socket.
    * @throws SocketException if a socket error occurs.
    */
   public boolean isNoDelay() throws SocketException
   {
      return m_socket.getTcpNoDelay();
   }

   /**
    * Sets the send buffer size (SO_SNDBUF socket option).
    * @param nSenderBufferSize The SO_SNDBUF option for the socket, must be greater than 0.
    * @throws SocketException if a socket error occurs.
    */
   public void setSenderBufferSize(int nSenderBufferSize) throws SocketException
   {
      m_socket.setSendBufferSize(nSenderBufferSize);
   }

   /**
    * @return The SO_SNDBUF option for the socket.
    * @throws SocketException if a socket error occurs.
    */
   public int getSenderBufferSize() throws SocketException
   {
      return m_socket.getSendBufferSize();
   }

   /**
    * @param nTOS The RFC 1349 type-of-service value (sum of lowCost=2, reliability=4, throughput=8, lowDelay=16).
    * @throws SocketException if a socket error occurs.
    */
   public void setTOS(int nTOS) throws SocketException
   {
      m_socket.setTrafficClass(nTOS);
   }

   /**
    * @return The RFC 1349 type-of-service value (sum of lowCost=2, reliability=4, throughput=8, lowDelay=16).
    * @throws SocketException if a socket error occurs.
    */
   public int getTOS() throws SocketException
   {
      return m_socket.getTrafficClass();
   }
}
