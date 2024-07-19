// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.tcp.ra;

import java.io.FilterInputStream;
import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.SocketException;
import java.net.SocketTimeoutException;
import java.security.AccessController;
import java.security.PrivilegedActionException;
import java.security.PrivilegedExceptionAction;

import javax.resource.ResourceException;
import javax.resource.spi.ConnectionEvent;
import javax.resource.spi.ConnectionRequestInfo;
import javax.security.auth.Subject;

import nexj.core.rpc.RPCException;
import nexj.core.rpc.ra.GenericConnection;
import nexj.core.rpc.ra.GenericConnectionRequestInfo;
import nexj.core.rpc.ra.UnsharedManagedConnection;
import nexj.core.util.ObjUtil;

/**
 * Managed TCP connection.
 */
public class TCPManagedConnection extends UnsharedManagedConnection
{
   // attributes

   /**
    * True iff this is a client socket that was returned from a call to
    * accept() in the TCPConsumerPool.  We will call this a server connection
    * since the framework is acting as the server.
    */
   protected boolean m_bServerConnection;

   /**
    * True iff no error has occurred on the connection and False if it shouldn't be used anymore.
    */
   protected boolean m_bValid;

   /**
    * True if this connection is using SSL/TLS.
    */
   protected boolean m_bSecure;

   /**
    * True if the underlying socket has returned end-of-stream.
    */
   protected boolean m_bEndOfStream;

   // associations

   /**
    * The TCP socket channel.
    */
   protected GenericSocketChannel m_socketChannel;

   /**
    * The remote address.
    */
   protected InetSocketAddress m_remoteAddress;

   /**
    * The local address.
    */
   protected InetSocketAddress m_localAddress;

   // constructors

   /**
    * Constructs a TCP managed connection.
    * @param tcpcri The connection request information.
    * @throws ResourceException if the managed connection cannot be created.
    */
   public TCPManagedConnection(final TCPConnectionRequestInfo tcpcri) throws ResourceException
   {
      m_remoteAddress = tcpcri.getRemoteAddress();

      if (m_remoteAddress.isUnresolved())
      {
         throw new RPCException("err.rpc.tcp.invalidRemoteHost", new Object[]{m_remoteAddress.getHostName()});
      }

      m_localAddress = tcpcri.getLocalAddress();

      if (m_localAddress.isUnresolved())
      {
         throw new RPCException("err.rpc.tcp.invalidLocalHost", new Object[]{m_localAddress.getHostName()});
      }

      m_bValid = true;
      m_bSecure = tcpcri.isSecure();

      m_bServerConnection = true;

      // Check the RA mapping for the case where we're acting as a server.
      m_socketChannel = TCPResourceAdapter.getSocketChannel(m_remoteAddress, m_localAddress);

      if (m_socketChannel == null)
      {
         try
         {
            m_socketChannel = (GenericSocketChannel)AccessController.doPrivileged(new PrivilegedExceptionAction()
            {
               public Object run() throws Exception
               {
                  // Create a new socket and connect to remote destination
                  GenericSocketChannel gsc;

                  if (m_bSecure)
                  {
                     gsc = new SecureSocketChannel(tcpcri.getTrustedCertificate(), tcpcri.getCertificateStore(), tcpcri.getPassword());
                  }
                  else
                  {
                     gsc = new PlainSocketChannel();
                  }

                  gsc.connect(m_remoteAddress, m_localAddress, tcpcri.getConnectionTimeout(),
                     tcpcri.getReceiverBufferSize(), tcpcri.getSenderBufferSize());
                  gsc.setNoDelay(tcpcri.isNoDelay());
                  gsc.setKeepAlive(tcpcri.isKeepAlive());

                  return gsc;
               }
            });
         }
         catch (PrivilegedActionException e)
         {
            invalidate();

            throw new ResourceException(ObjUtil.getMessage(e.getException()), e.getException());
         }
         catch (Throwable t)
         {
            invalidate();

            throw new ResourceException(ObjUtil.getMessage(t), t);
         }

         m_bServerConnection = false;
      }
   }

   // operations

   /**
    * @see nexj.core.rpc.ra.GenericManagedConnection#createConnection(javax.security.auth.Subject, javax.resource.spi.ConnectionRequestInfo)
    */
   protected synchronized GenericConnection createConnection(Subject subject, ConnectionRequestInfo cri) throws ResourceException
   {
      TCPConnectionRequestInfo tcpcri = (TCPConnectionRequestInfo)cri;

      if (!ObjUtil.equal(TCPConnectionRequestInfo.getHostString(tcpcri.getRemoteAddress()),
         TCPConnectionRequestInfo.getHostString(m_remoteAddress)) ||
         tcpcri.getRemoteAddress().getPort() != m_remoteAddress.getPort())
      {
         throw new IllegalStateException("Remote address mismatch");
      }

      if (!ObjUtil.equal(tcpcri.getLocalAddress().getAddress().getHostAddress(),
         m_localAddress.getAddress().getHostAddress()))
      {
         throw new IllegalStateException("Local address mismatch");
      }

      if (tcpcri.isSecure() != m_bSecure)
      {
         throw new IllegalStateException("Secure/Insecure connection mismatch");
      }

      return new TCPConnection();
   }

   /**
    * @see nexj.core.rpc.ra.GenericManagedConnection#matches(javax.security.auth.Subject, nexj.core.rpc.ra.GenericConnectionRequestInfo)
    */
   public synchronized boolean matches(Subject subject, GenericConnectionRequestInfo cri)
   {
      if (!m_bValid)
      {
         return false;
      }

      if (cri == null)
      {
         return m_remoteAddress == null;
      }

      if (!(cri instanceof TCPConnectionRequestInfo))
      {
         return false;
      }

      TCPConnectionRequestInfo tcpcri = (TCPConnectionRequestInfo)cri;

      return ObjUtil.equal(TCPConnectionRequestInfo.getHostString(tcpcri.getRemoteAddress()),
            TCPConnectionRequestInfo.getHostString(m_remoteAddress)) &&
         tcpcri.getRemoteAddress().getPort() == m_remoteAddress.getPort() &&
         ObjUtil.equal(tcpcri.getLocalAddress().getAddress().getHostAddress(),
            m_localAddress.getAddress().getHostAddress()) &&
         tcpcri.isSecure() == m_bSecure;
   }

   /**
    * @see nexj.core.rpc.ra.GenericManagedConnection#destroy()
    */
   public synchronized void destroy() throws ResourceException
   {
      super.destroy();

      if (m_socketChannel != null)
      {
         if (m_bServerConnection)
         {
            TCPResourceAdapter.removeSocketChannel(m_socketChannel);
         }
         else
         {
            try
            {
               m_socketChannel.close();
            }
            catch (IOException e)
            {
            }

            try
            {
               // Clean up any bytes on the input stream to avoid
               // "Address already in use: bind" errors when re-sending
               // from the same local port.
               while (m_socketChannel.getInputStream().read() != -1);
            }
            catch (IOException e)
            {
            }
         }

         m_socketChannel = null;
      }
   }

   /**
    * @return The local socket address.
    */
   public synchronized InetSocketAddress getLocalAddress()
   {
      return (InetSocketAddress)m_socketChannel.getLocalSocketAddress();
   }

   /**
    * @return The remote socket address.
    */
   public synchronized InetSocketAddress getRemoteAddress()
   {
      return (InetSocketAddress)m_socketChannel.getRemoteSocketAddress();
   }

   /**
    * @return The socket's associated output stream.
    * @throws IOException if there is an I/O error.
    */
   public synchronized OutputStream getOutputStream() throws IOException
   {
      return new FilterOutputStream(m_socketChannel.getOutputStream())
      {
         public void write(byte nArray[], int nOffset, int nLength) throws IOException
         {
            try
            {
               out.write(nArray, nOffset, nLength);
            }
            catch (IOException e)
            {
               invalidate();

               throw e;
            }
         }

         public void write(int nByte) throws IOException
         {
            try
            {
               out.write(nByte);
            }
            catch (IOException e)
            {
               invalidate();

               throw e;
            }
         }
      };
   }

   /**
    * @return The socket's associated input stream
    * @throws IOException if there is an I/O error.
    */
   public synchronized InputStream getInputStream() throws IOException
   {
      return new FilterInputStream(m_socketChannel.getInputStream())
      {
         public int read(byte nArray[], int nOffset, int nLength) throws IOException
         {
            try
            {
               int n = in.read(nArray, nOffset, nLength);

               if (n == -1)
               {
                  m_bEndOfStream = true;
               }

               return n;
            }
            catch (SocketTimeoutException e)
            {
               invalidate();

               throw new TCPTimeoutException(e);
            }
            catch (IOException e)
            {
               invalidate();

               throw e;
            }
         }

         public int read() throws IOException
         {
            try
            {
               int n = in.read();

               if (n == -1)
               {
                  m_bEndOfStream = true;
               }

               return n;
            }
            catch (SocketTimeoutException e)
            {
               invalidate();

               throw new TCPTimeoutException(e);
            }
            catch (IOException e)
            {
               invalidate();

               throw e;
            }
         }
      };
   }

   /**
    * @return True iff the connection is not in an error state.
    */
   public synchronized boolean isValid()
   {
      return m_bValid;
   }

   /**
    * @return True iff the underlying socket has returned end-of-stream.
    */
   public synchronized boolean isEndOfStream()
   {
      return m_bEndOfStream;
   }

   /**
    * Invalidates the connection.
    */
   public synchronized void invalidate()
   {
      if (m_bValid)
      {
         m_bValid = false;
         notify(ConnectionEvent.CONNECTION_ERROR_OCCURRED, this, null);
      }
   }

   /**
    * @see nexj.core.rpc.ra.GenericManagedConnection#cleanup()
    */
   public synchronized void cleanup() throws ResourceException
   {
      if (m_bValid)
      {
         if (!m_bServerConnection)
         {
            try
            {
               // reset the read timeout before re-entering the pool
               m_socketChannel.setSoTimeout(0);
            }
            catch (SocketException e)
            {
               ObjUtil.rethrow(e);
            }
         }

         super.cleanup();
      }
   }

   /**
    * @return The message read timeout (milliseconds).
    */
   public synchronized int getReadTimeout()
   {
      try
      {
         return m_socketChannel.getSoTimeout();
      }
      catch (SocketException e)
      {
         invalidate();

         throw ObjUtil.rethrow(e);
      }
   }

   /**
    * @param nTimeout The message read timeout (milliseconds).
    */
   public synchronized void setReadTimeout(int nTimeout)
   {
      if (m_socketChannel != null)
      {
         try
         {
            m_socketChannel.setSoTimeout(nTimeout);
         }
         catch (SocketException e)
         {
            invalidate();

            ObjUtil.rethrow(e);
         }
      }
   }

   /**
    * @return The RFC 1349 type-of-service value (sum of lowCost=2, reliability=4, throughput=8, lowDelay=16).
    */
   public synchronized int getTOS()
   {
      try
      {
         return m_socketChannel.getTOS();
      }
      catch (SocketException e)
      {
         invalidate();

         throw ObjUtil.rethrow(e);
      }
   }

   /**
    * @param nTOS The RFC 1349 type-of-service value (sum of lowCost=2, reliability=4, throughput=8, lowDelay=16).
    */
   public synchronized void setTOS(int nTOS)
   {
      if (m_socketChannel != null)
      {
         try
         {
            m_socketChannel.setTOS(nTOS);
         }
         catch (SocketException e)
         {
            invalidate();

            ObjUtil.rethrow(e);
         }
      }
   }
}
