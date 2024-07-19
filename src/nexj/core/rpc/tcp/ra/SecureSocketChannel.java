// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.tcp.ra;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.SocketChannel;
import java.security.AccessController;
import java.security.KeyStore;
import java.security.PrivilegedActionException;
import java.security.PrivilegedExceptionAction;
import java.security.cert.Certificate;

import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLEngine;
import javax.net.ssl.SSLEngineResult;
import javax.net.ssl.SSLException;
import javax.net.ssl.SSLPeerUnverifiedException;
import javax.net.ssl.SSLSession;
import javax.net.ssl.SSLEngineResult.HandshakeStatus;
import javax.net.ssl.SSLEngineResult.Status;

import nexj.core.util.CertificateUtil;
import nexj.core.util.Logger;
import nexj.core.util.RandUtil;

/**
 * A socket channel that implements SSL/TLS for both clients and servers.
 */
public class SecureSocketChannel extends GenericSocketChannel
{
   // constants

   protected final static int STATE_INITIAL_HANDSHAKE = 0;

   /**
    * The SSL engine is currently performing a handshake.
    */
   protected final static int STATE_HANDSHAKING = 1;

   /**
    * The SSL engine is not currently performing a handshake.
    */
   protected final static int STATE_NOT_HANDSHAKING = 2;

   /**
    * This channel is closed but may not be completely shutdown.
    */
   protected final static int STATE_CLOSED = 3;

   /**
    * The shutdown of this channel is complete.
    */
   protected final static int STATE_SHUTDOWN_COMPLETE = 4;

   // attributes

   /**
    * The current state of this secure channel.
    */
   protected int m_nState;

   /**
    * True iff this channel is functioning as a client.
    */
   protected boolean m_bClientMode;

   // associations

   /**
    * The SSL engine.
    */
   protected SSLEngine m_engine;

   /**
    * The SSL context object.
    */
   protected SSLContext m_sslContext;

   /**
    * Buffer for data ready to be written to the network.
    */
   protected ByteBuffer m_outNetData;

   /**
    * Buffer for data ready to be passed to the application.
    */
   protected ByteBuffer m_inAppData;

   /**
    * Buffer for data read in from the network.
    */
   protected ByteBuffer m_inNetData;

   /**
    * The input stream for this channel.
    */
   protected InputStream m_inputStream;

   /**
    * The output stream for this channel.
    */
   protected OutputStream m_outputStream;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(SecureSocketChannel.class);

   /**
    * The trusted certificate.
    * The remote system's certificate must be signed by this certificate, or a
    * trusted certificate chain must be presented that contains this certificate.
    * If null, this channel will trust certificates in the default trust store.
    */
   protected Certificate m_trustedCertificate;

   /**
    * The certificate and private key of this system, to be used by client certificate
    * authentication when logging on to a remote system or as the server certificate
    * when acting as a server.
    */
   protected KeyStore m_certificateStore;

   /**
    * The password protecting the private key in m_certificateStore.
    */
   protected char[] m_achPassword;

   // constructors

   /**
    * Constructs a client SSL socket channel.
    * @param trust The trusted certificate. If null, trusts certificates in the default trust store.
    * @param certificate The certificate and private key of this system. Can be null.
    * @param sPassword The password protecting the private key of this system.
    * @throws IOException if an I/O error occurs.
    */
   public SecureSocketChannel(Certificate trust, KeyStore certificate, String sPassword) throws IOException
   {
      m_nState = STATE_CLOSED;
      m_bClientMode = true;
      m_trustedCertificate = trust;
      m_certificateStore = certificate;
      m_achPassword = (sPassword == null ? null : sPassword.toCharArray());

      initSSLContext();
   }

   /**
    * Constructs a server SSL socket channel.
    * @param socketChannel The SocketChannel returned from a call to accept() by a listening socket.
    * @param trust The trusted certificate. If null, trusts certificates in the default trust store.
    * @param certificate The certificate and private key of this system. Cannot be null.
    * @param sPassword The password protecting the private key of this system.
    * @throws IOException if an I/O error occurs.
    */
   public SecureSocketChannel(SocketChannel socketChannel, Certificate trust, KeyStore certificate,
      String sPassword, byte nClientAuthMode) throws IOException
   {
      m_nState = STATE_INITIAL_HANDSHAKE;
      m_bClientMode = false;
      m_socketChannel = socketChannel;
      m_socket = m_socketChannel.socket();
      m_trustedCertificate = trust;
      m_certificateStore = certificate;
      m_achPassword = (sPassword == null ? null : sPassword.toCharArray());

      initSSLContext();

      m_engine = m_sslContext.createSSLEngine(null, 0);

      // Client authentication mode
      switch (nClientAuthMode)
      {
         case TCPConsumerConfig.CLIENT_AUTH_SUPPORTED:
         {
            m_engine.setWantClientAuth(true);

            break;
         }
         case TCPConsumerConfig.CLIENT_AUTH_REQUIRED:
         {
            m_engine.setNeedClientAuth(true);

            break;
         }
         default: // CLIENT_AUTH_NONE
         {
            break;
         }
      }

      m_engine.setUseClientMode(false);
      initBuffers(m_engine.getSession());
   }

   // operations

   /**
    * Initialize the ByteBuffers.
    * @param session The SSL session object.
    */
   protected void initBuffers(SSLSession session)
   {
      // allocate buffers
      m_outNetData = ByteBuffer.allocate(session.getPacketBufferSize());
      m_inAppData = ByteBuffer.allocate(session.getApplicationBufferSize() * 2);
      m_inNetData = ByteBuffer.allocate(session.getPacketBufferSize());
   }

   /**
    * Initialize the SSLContext.
    * @throws SSLException if the SSLContext object could not be initialized.
    */
   protected void initSSLContext() throws SSLException
   {
      try
      {
         AccessController.doPrivileged(new PrivilegedExceptionAction()
         {
            public Object run() throws Exception
            {
               m_sslContext = SSLContext.getInstance("TLS");
               m_sslContext.init(
                  CertificateUtil.getKeyManagers(m_certificateStore, m_achPassword),
                  CertificateUtil.getTrustManagers(m_trustedCertificate),
                  RandUtil.getSecureRandom());

               return null;
            }
         });
      }
      catch (PrivilegedActionException e)
      {
         throw new SSLException("Failure to initialize SSLContext", e.getCause());
      }
   }

   /**
    * @see nexj.core.rpc.tcp.ra.GenericSocketChannel#connect(java.net.InetSocketAddress, java.net.InetSocketAddress, int, int, int)
    */
   public boolean connect(InetSocketAddress remoteAddress, InetSocketAddress localAddress,
      int nTimeout, int nReceiverBufferSize, int nSenderBufferSize) throws IOException
   {
      if (m_bClientMode)
      {
         m_engine = m_sslContext.createSSLEngine(null, 0);
         m_engine.setUseClientMode(true);
         initBuffers(m_engine.getSession());

         m_socketChannel = SocketChannel.open();
         m_socket = m_socketChannel.socket();

         if (nReceiverBufferSize > 0)
         {
            m_socket.setReceiveBufferSize(nReceiverBufferSize);
         }

         if (nSenderBufferSize > 0)
         {
            m_socket.setSendBufferSize(nSenderBufferSize);
         }

         m_socket.bind(localAddress);
         m_socketChannel.configureBlocking(true);
         m_socket.connect(remoteAddress, nTimeout);

         if (m_socketChannel.isConnected())
         {
            m_nState = STATE_INITIAL_HANDSHAKE;

            return true;
         }
      }

      return false;
   }

   /**
    * @see nexj.core.rpc.tcp.ra.GenericSocketChannel#getPeerCertificates()
    */
   public Certificate[] getPeerCertificates()
   {
      if (m_engine.getNeedClientAuth() || m_engine.getWantClientAuth())
      {
         try
         {
            return m_engine.getSession().getPeerCertificates();
         }
         catch (SSLPeerUnverifiedException e)
         {
            return null;
         }
      }

      return null;
   }

   /**
    * Complete the SSL handshake process.
    * @throws IOException if an I/O error occurs.
    */
   protected void doInitialHandshake() throws IOException
   {
      m_engine.beginHandshake();

      while (m_nState == STATE_INITIAL_HANDSHAKE || m_nState == STATE_HANDSHAKING)
      {
         try
         {
            doHandshake();
         }
         catch (IOException e)
         {
            shutdown();

            throw e;
         }
      }

      if (isClosed())
      {
         shutdown();

         throw new ClosedChannelException();
      }
   }

   /**
    * Complete the SSL handshake process.
    * @throws IOException if an I/O error occurs.
    */
   protected void doHandshake() throws IOException
   {
      ByteBuffer dummy = ByteBuffer.allocate(0);
      SSLEngineResult engineResult;
      HandshakeStatus hsStatus = m_engine.getHandshakeStatus();
      boolean bBlocking = m_socketChannel.isBlocking();

      m_socketChannel.configureBlocking(false);

      for (;;)
      {
         if (hsStatus == HandshakeStatus.NEED_WRAP)
         {
            m_outNetData.clear();
            engineResult = m_engine.wrap(dummy, m_outNetData);
            s_logger.dump(engineResult);

            Status engineStatus = engineResult.getStatus();

            if (engineStatus == Status.OK)
            {
               // normal
               m_outNetData.flip();
               hsStatus = engineResult.getHandshakeStatus();
            }
            else if (engineStatus == Status.CLOSED)
            {
               m_nState = STATE_CLOSED;

               break;
            }
            else
            {
               // unexpected handshake status
               throw new IOException("Unexpected SSL engine status after call to wrap(). Expected "
                  + Status.OK + ", got " + engineStatus);
            }

            // write data to socket
            flushData();
         }
         else if (hsStatus == HandshakeStatus.NEED_UNWRAP)
         {
            int n = m_socketChannel.read(m_inNetData);

            if (n < 0)
            {
               // EOF, so set to closed
               m_nState = STATE_CLOSED;

               break;
            }
            else if (n == 0 && m_inNetData.position() == 0)
            {
               // No data to unwrap
               break;
            }

            ByteBuffer tmpAppData = ByteBuffer.allocate(m_engine.getSession().getApplicationBufferSize());
            Status engineStatus;

            m_inNetData.flip();
            tmpAppData.clear();

            do
            {
               engineResult = m_engine.unwrap(m_inNetData, tmpAppData);
               engineStatus = engineResult.getStatus();
               s_logger.dump(engineResult);

            }
            while (engineStatus == SSLEngineResult.Status.OK
                  && engineResult.getHandshakeStatus() == SSLEngineResult.HandshakeStatus.NEED_UNWRAP
                  && engineResult.bytesProduced() == 0);

            if (tmpAppData.position() == 0
                  && engineStatus == SSLEngineResult.Status.OK
                  && m_inNetData.hasRemaining())
            {
               engineResult = m_engine.unwrap(m_inNetData, tmpAppData);
               engineStatus = engineResult.getStatus();
               s_logger.dump(engineResult);
            }

            m_inNetData.compact();

            if (m_nState != STATE_INITIAL_HANDSHAKE)
            {
               tmpAppData.flip();
               m_inAppData.compact();
               m_inAppData.put(tmpAppData);
               m_inAppData.flip();
            }

            hsStatus = engineResult.getHandshakeStatus();

            if (engineStatus == Status.CLOSED)
            {
               // SSLEngine is closed... cannot complete handshake
               m_nState = STATE_CLOSED;

               break;
            }
            else if (engineStatus == Status.BUFFER_OVERFLOW)
            {
               // buffer size should've been set to session.getPacketBufferSize()
               throw new IOException("BUFFER_OVERFLOW result from call to unwrap()");
            }

            if (m_nState != STATE_INITIAL_HANDSHAKE && m_inAppData.hasRemaining())
            {
               break;
            }
         }
         else if (hsStatus == HandshakeStatus.NEED_TASK)
         {
            doTasks();
            hsStatus = m_engine.getHandshakeStatus();
         }
         else if (hsStatus == HandshakeStatus.FINISHED)
         {
            s_logger.debug("Completed Handshake");

            if (m_nState == STATE_INITIAL_HANDSHAKE)
            {
               m_inAppData.flip();
            }

            m_nState = STATE_NOT_HANDSHAKING;

            break;
         }
         else
         {
            throw new IOException("Unknown handshake status: " + hsStatus);
         }
      }

      m_socketChannel.configureBlocking(bBlocking);
   }

   /**
    * Check if another SSL handshake needs to take place,
    * and if so, set state to STATE_HANDSHAKING.
    * @throws IOException if an I/O error occurs.
    */
   protected void checkHandshake() throws IOException
   {
      doTasks();

      HandshakeStatus hsStatus = m_engine.getHandshakeStatus();

      if (hsStatus == HandshakeStatus.NOT_HANDSHAKING)
      {
         return;   // Not starting a re-handshake
      }

      if (hsStatus == HandshakeStatus.FINISHED)
      {
         // Should not happen!
         throw new IOException("FINISHED state obtained while checking for re-handshake");
      }

      // Start a re-handshake
      m_nState = STATE_HANDSHAKING;
   }

   /**
    * As part of the SSL handshake process, run any tasks given
    * to us by the SSL engine.
    */
   protected void doTasks()
   {
      Runnable task;

      while ((task = m_engine.getDelegatedTask()) != null)
      {
         task.run();
      }
   }

   /**
    * @see nexj.core.rpc.tcp.ra.GenericSocketChannel#close()
    */
   public void close() throws IOException
   {
      shutdown();
   }

   /**
    * Attempt to write data from m_outNetData buffer
    * to the socket.
    * @return True iff all data was written to the socket.
    * @throws IOException if an I/O error occurs.
    */
   protected boolean flushData() throws IOException
   {
      if (!m_outNetData.hasRemaining())
      {
         return true;
      }

      m_socketChannel.write(m_outNetData);

      return !m_outNetData.hasRemaining();
   }

   /**
    * @see nexj.core.rpc.tcp.ra.GenericSocketChannel#getInputStream()
    */
   public InputStream getInputStream() throws IOException
   {
      if (m_nState == STATE_INITIAL_HANDSHAKE)
      {
         doInitialHandshake();
      }

      if (m_inputStream == null)
      {
         m_inputStream = new InputStream()
         {
            public int available() throws IOException
            {
               return m_inAppData.remaining() + m_socket.getInputStream().available();
            }
   
            protected int readArray(byte nArray[], int nOffset, int nLength)
            {
               // copy from our buffer to wrapped buffer
               if (m_inAppData.remaining() > nLength)
               {
                  // buf is too small for all of the app data
                  m_inAppData.get(nArray, nOffset, nLength);
   
                  return nLength;
               }
   
               ByteBuffer buf = ByteBuffer.wrap(nArray, nOffset, nLength);
               int nStartPos = buf.position();
   
               buf.put(m_inAppData);
   
               return buf.position() - nStartPos;
            }
   
            public int read(byte nArray[], int nOffset, int nLength) throws IOException
            {
               // just return bytes that have already
               // been buffered
               if (m_inAppData.hasRemaining())
               {
                  return readArray(nArray, nOffset, nLength);
               }
   
               // possibly do re-handshake
               while (m_nState == STATE_HANDSHAKING && !m_inAppData.hasRemaining())
               {
                  doHandshake();
   
                  if (m_inAppData.hasRemaining())
                  {
                     return readArray(nArray, nOffset, nLength);
                  }
               }
   
               if (isClosed())
               {
                  shutdown();
   
                  return -1;
               }
   
               do
               {
                  if (readByteBuffer() < 0)
                  {
                     return -1;
                  }
               }
               while (!m_inAppData.hasRemaining() && !isClosed());
   
               if (m_inAppData.hasRemaining())
               {
                  return readArray(nArray, nOffset, nLength);
               }
   
               return -1;
            }
   
            public int read() throws IOException
            {
               // if there are still buffered bytes,
               // then return one
               if (m_inAppData.hasRemaining())
               {
                  return (int)m_inAppData.get() & 0xFF;
               }
   
               // possibly do re-handshake
               while (m_nState == STATE_HANDSHAKING && !m_inAppData.hasRemaining())
               {
                  doHandshake();
   
                  if (m_inAppData.hasRemaining())
                  {
                     return (int)m_inAppData.get() & 0xFF;
                  }
               }
   
               if (isClosed())
               {
                  shutdown();
   
                  return -1;
               }
   
               do
               {
                  if (readByteBuffer() < 0)
                  {
                     return -1;
                  }
               }
               while (!m_inAppData.hasRemaining() && !isClosed());
   
               if (m_inAppData.hasRemaining())
               {
                  return (int)m_inAppData.get() & 0xFF;
               }
   
               return -1;
            }
   
            public void close() throws IOException
            {
               if (!m_socket.isInputShutdown())
               {
                  m_socket.getInputStream().close();
               }
            }
         };
      }

      return m_inputStream;
   }

   /**
    * Read data from the socket into the m_inNetData buffer,
    * and unwrap it into the m_inAppData buffer.
    * If there are no bytes available on the socket, this
    * function will block.
    * @return The number of bytes read into application buffer.
    * @throws IOException if an I/O error occurs.
    */
   protected int readByteBuffer() throws IOException
   {
      boolean bBlocking = m_socketChannel.isBlocking();

      m_socketChannel.configureBlocking(false);

      try
      {
         if (m_nState == STATE_INITIAL_HANDSHAKE)
         {
            throw new IOException("Read called during initial handshake");
         }

         if (m_engine.isInboundDone())
         {
            // EOF
            return -1;
         }

         int n = m_socketChannel.read(m_inNetData);

         if (n == 0)
         {
            // Nothing was read, so block waiting for a single byte.
            ByteBuffer oneByte = ByteBuffer.allocate(1);

            m_socketChannel.configureBlocking(true);

            while (n == 0)
            {
               n = m_socketChannel.read(oneByte);
            }

            m_socketChannel.configureBlocking(false);

            if (n < 0)
            {
               m_nState = STATE_CLOSED;

               return -1;
            }

            if (n == 1)
            {
               oneByte.flip();
               m_inNetData.put(oneByte);
            }

            return 1;
         }

         if (n < 0)
         {
            m_nState = STATE_CLOSED;
         }

         m_inAppData.clear();

         for (;;)
         {
            m_inNetData.flip();

            SSLEngineResult engineResult = m_engine.unwrap(m_inNetData, m_inAppData);
            Status engineStatus = engineResult.getStatus();

            s_logger.dump(engineResult);
            m_inNetData.compact();

            if (engineStatus == Status.CLOSED)
            {
               // SSLEngine is closed... cannot complete.
               m_nState = STATE_CLOSED;

               break;
            }
            else if (engineStatus == Status.OK)
            {
               // expected
               checkHandshake();
            }
            else if (engineStatus == Status.BUFFER_UNDERFLOW)
            {
               // just means we need to read more data
               break;
            }
            else if (engineStatus == Status.BUFFER_OVERFLOW)
            {
               // buffer size should've been set to session.getPacketBufferSize()
               throw new IOException("BUFFER_OVERFLOW result from call to unwrap()");
            }

            if (m_inNetData.position() == 0)
            {
               break;
            }
         }

         m_inAppData.flip();

         return m_inAppData.remaining();
      }
      finally
      {
         m_socketChannel.configureBlocking(bBlocking);
      }
   }

   /**
    * @see nexj.core.rpc.tcp.ra.GenericSocketChannel#getOutputStream()
    */
   public OutputStream getOutputStream() throws IOException
   {
      if (m_nState == STATE_INITIAL_HANDSHAKE)
      {
         doInitialHandshake();
      }

      if (m_outputStream == null)
      {
         m_outputStream = new OutputStream()
         {
            protected byte[] m_nByteArray;
   
            public void write(byte nArray[], int nOffset, int nLength) throws IOException
            {
               int n = 0;
   
               while (n < nLength)
               {
                  // possibly do re-handshake
                  while (m_nState == STATE_HANDSHAKING)
                  {
                     doHandshake();
                  }
   
                  if (isClosed())
                  {
                     shutdown();
   
                     throw new ClosedChannelException();
                  }
   
                  n += writeByteBuffer(ByteBuffer.wrap(nArray, nOffset + n, nLength - n));
               }
            }
   
            public void write(int n) throws IOException
            {
               if (m_nByteArray == null)
               {
                  m_nByteArray = new byte[1];
               }
   
               m_nByteArray[0] = (byte)n;
               write(m_nByteArray, 0, 1);
            }
   
            public void close() throws IOException
            {
               if (!m_socket.isOutputShutdown())
               {
                  m_socket.getOutputStream().close();
               }
            }
         };
      }

      return m_outputStream;
   }

   /**
    * Wrap the data from the source buffer into the
    * m_outNetData buffer, and then write it to the
    * socket.
    * @param src The source buffer.
    * @return The number of bytes written from source buffer.
    * @throws IOException if an I/O error occurs.
    */
   protected int writeByteBuffer(ByteBuffer src) throws IOException
   {
      if (m_nState == STATE_INITIAL_HANDSHAKE)
      {
         throw new IOException("Cannot call write during initial handshake.");
      }

      if (!flushData())
      {
         return 0;
      }

      // all pending bytes have been written
      m_outNetData.clear();

      SSLEngineResult engineResult = m_engine.wrap(src, m_outNetData);
      Status engineStatus = engineResult.getStatus();

      s_logger.dump(engineResult);
      m_outNetData.flip();

      if (engineStatus == Status.OK)
      {
         // expected
         checkHandshake();
      }
      else
      {
         // CLOSED: SSLEngine is closed... cannot complete.
         // BUFFER_UNDERFLOW: shouldn't happen after call to wrap().
         // BUFFER_OVERFLOW: buffer size should've been set to session.getPacketBufferSize()
         throw new IOException("Unexpected SSL engine status after call to wrap(). Got "
            + engineStatus + ", expected " + Status.OK);
      }

      while (!flushData());

      return engineResult.bytesConsumed();
   }

   /**
    * Shutdown the SSL connection by writing out any
    * remaining data, and closing the socket.
    * @throws IOException if an I/O error occurs.
    */
   protected void shutdown() throws IOException
   {
      if (m_nState == STATE_SHUTDOWN_COMPLETE)
      {
         return;
      }

      try
      {
         s_logger.debug("Shutting down secure socket channel: " + m_socket);
         m_nState = STATE_CLOSED;

         ByteBuffer dummy = ByteBuffer.allocate(0);

         m_engine.closeOutbound();

         while (!m_engine.isOutboundDone())
         {
            // generate close message
            m_outNetData.clear();

            SSLEngineResult engineResult = m_engine.wrap(dummy, m_outNetData);
            Status engineStatus = engineResult.getStatus();

            s_logger.dump(engineResult);

            if (engineStatus != Status.CLOSED)
            {
               throw new IOException("Unexpected SSL engine status during shutdown procedure. Got "
                  + engineStatus + ", expected " + Status.CLOSED);
            }

            m_outNetData.flip();

            // flush data to socket
            try
            {
               while (m_outNetData.hasRemaining() && m_socketChannel.write(m_outNetData) >= 0);
            }
            catch (IOException e)
            {
               s_logger.debug(e);
            }
         }

         m_socketChannel.close();
      }
      finally
      {
         m_nState = STATE_SHUTDOWN_COMPLETE;
      }
   }

   /**
    * @return True iff the channel is in an un-open state.
    */
   protected boolean isClosed()
   {
      return m_nState == STATE_CLOSED || m_nState == STATE_SHUTDOWN_COMPLETE;
   }
}
