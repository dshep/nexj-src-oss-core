// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.tcp.ra;

import java.net.Inet6Address;
import java.net.InetSocketAddress;
import java.security.AccessController;
import java.security.KeyStore;
import java.security.PrivilegedActionException;
import java.security.PrivilegedExceptionAction;
import java.security.cert.Certificate;

import javax.resource.ResourceException;

import nexj.core.rpc.ra.GenericConnectionRequestInfo;
import nexj.core.util.ObjUtil;

/**
 * TCP connection request info.
 */
public class TCPConnectionRequestInfo extends GenericConnectionRequestInfo
{
   // constants

   /**
    * The default IPv4 bind address.
    */
   public final static String DEFAULT_IP4_BIND = "0.0.0.0";

   /**
    * The default IPv6 bind address.
    */
   public final static String DEFAULT_IP6_BIND = "0:0:0:0:0:0:0:0";

   // attributes

   /**
    * The connection timeout in milliseconds (0 means infinite).
    */
   protected int m_nConnectionTimeout;

   /**
    * The SO_SNDBUF option for the underlying socket, 0 for system default.
    */
   protected int m_nSenderBufferSize;

   /**
    * The SO_RCVBUF option for the underlying socket, 0 for system default.
    */
   protected int m_nReceiverBufferSize;

   /**
    * True if the TCP no-delay property should be set on the underlying socket.
    */
   protected boolean m_bNoDelay;

   /**
    * True if the TCP keep-alive property should be set on the underlying socket.
    */
   protected boolean m_bKeepAlive;

   /**
    * True for SSL/TLS connections.
    */
   protected boolean m_bSecure;

   // associations

   /**
    * The remote address.
    */
   protected InetSocketAddress m_remoteAddress;

   /**
    * The local address.
    */
   protected InetSocketAddress m_localAddress;

   /**
    * The certificate password.
    */
   protected String m_sPassword;

   /**
    * The trusted certificate.
    * The remote system's certificate must be signed by this certificate, or a
    * trusted certificate chain must be presented that contains this certificate.
    * If null, this channel will trust certificates in the default trust store.
    */
   protected Certificate m_trustedCertificate;

   /**
    * The certificate and private key of this, to use for outgoing
    * connections (optional) or for functioning as a server (required).
    */
   protected KeyStore m_certificateStore;

   // constructors

   /**
    * Constructs the TCP connection request info.
    * @param remoteAddress The remote address.
    * @param localAddress The local address.
    * @param nTimeout The connection timeout in milliseconds (0 means infinite).
    * @param bNoDelay True if the TCP no-delay property should be set on the underlying socket.
    * @param bKeepAlive True if the TCP keep-alive property should be set on the underlying socket.
    * @param nSenderBufferSize The SO_SNDBUF option for the underlying socket, 0 for system default.
    * @param nReceiverBufferSize The SO_RCVBUF option for the underlying socket, 0 for system default.
    * @throws ResourceException if the connection request object could not be created.
    */
   public TCPConnectionRequestInfo(InetSocketAddress remoteAddress, InetSocketAddress localAddress,
      int nTimeout, boolean bNoDelay, boolean bKeepAlive, int nSenderBufferSize, int nReceiverBufferSize) throws ResourceException
   {
      this(remoteAddress,
         localAddress,
         false,
         null,
         null,
         null,
         nTimeout,
         bNoDelay,
         bKeepAlive,
         nSenderBufferSize,
         nReceiverBufferSize);
   }

   /**
    * Constructs the TCP connection request info.
    * @param remoteAddress The remote socket address.
    * @param localAddress The local socket address.
    * @param bSecure True if this is an SSL connection.
    * @param trustedCertificate The trusted certificate. If null, use the default trust store.
    * @param certificateStore The client's certificate (KeyStore) for SSL client certificate
    *    authentication. Can be null if not required.
    * @param sPassword The password for the client's key store.
    * @param nTimeout The connection timeout in milliseconds (0 means infinite).
    * @param bNoDelay True if the TCP no-delay property should be set on the underlying socket.
    * @param bKeepAlive True if the TCP keep-alive property should be set on the underlying socket.
    * @param nSenderBufferSize The SO_SNDBUF option for the underlying socket, 0 for system default.
    * @param nReceiverBufferSize The SO_RCVBUF option for the underlying socket, 0 for system default.
    * @throws ResourceException if the connection request object could not be created.
    */
   public TCPConnectionRequestInfo(final InetSocketAddress remoteAddress, InetSocketAddress localAddress,
      boolean bSecure, Certificate trustedCertificate, KeyStore certificateStore, String sPassword,
      int nTimeout, boolean bNoDelay, boolean bKeepAlive, int nSenderBufferSize, int nReceiverBufferSize) throws ResourceException
   {
      assert remoteAddress != null;

      if (localAddress == null)
      {
         try
         {
            m_localAddress = (InetSocketAddress)AccessController.doPrivileged(new PrivilegedExceptionAction()
            {
               public Object run() throws Exception
               {
                  if (remoteAddress.getAddress() instanceof Inet6Address)
                  {
                     return new InetSocketAddress(DEFAULT_IP6_BIND, 0);
                  }

                  return new InetSocketAddress(DEFAULT_IP4_BIND, 0);
               }
            });
         }
         catch (PrivilegedActionException e)
         {
            throw new ResourceException(ObjUtil.getMessage(e.getException()), e.getException());
         }
      }
      else
      {
         m_localAddress = localAddress;
      }

      m_remoteAddress = remoteAddress;
      m_bSecure = bSecure;
      m_trustedCertificate = trustedCertificate;
      m_certificateStore = certificateStore;
      m_sPassword = sPassword;
      m_nConnectionTimeout = nTimeout;
      m_bNoDelay = bNoDelay;
      m_bKeepAlive = bKeepAlive;
      m_nSenderBufferSize = nSenderBufferSize;
      m_nReceiverBufferSize = nReceiverBufferSize;
   }

   // operations

   /**
    * @see java.lang.Object#hashCode()
    */
   public int hashCode()
   {
      return (getHostString(m_remoteAddress).hashCode() + m_remoteAddress.getPort())
         << 16 ^ m_localAddress.getAddress().getHostAddress().hashCode()
         ^ (m_certificateStore == null ? 0 : m_certificateStore.hashCode())
            ^ (m_sPassword == null ? 0 : m_sPassword.hashCode())
            ^ (m_trustedCertificate == null ? 0 : m_trustedCertificate.hashCode())
            ^ (m_bSecure ? 1 : 0);
   }

   /**
    * @see java.lang.Object#equals(java.lang.Object)
    */
   public boolean equals(Object obj)
   {
      if (!(obj instanceof TCPConnectionRequestInfo))
      {
         return false;
      }

      TCPConnectionRequestInfo cri = (TCPConnectionRequestInfo)obj;

      return getHostString(cri.getRemoteAddress()).equals(getHostString(m_remoteAddress))
         && cri.getRemoteAddress().getPort() == m_remoteAddress.getPort() &&
         cri.getLocalAddress().getAddress().getHostAddress().equals(m_localAddress.getAddress().getHostAddress())
         && cri.isSecure() == m_bSecure
         && ObjUtil.equal(cri.getCertificateStore(), m_certificateStore)
         && ObjUtil.equal(cri.getPassword(), m_sPassword)
         && ObjUtil.equal(cri.getTrustedCertificate(), m_trustedCertificate);
   }

   /**
    * Get host string.
    * @param addr An InetSocketAddress object. Can be null.
    * @return The host string represented by addr. If addr is null, returns the empty string.
    */
   public static String getHostString(InetSocketAddress addr)
   {
      if (addr == null)
      {
         return "";
      }

      String str = addr.toString();
      String retStr;
      int indexSlash = str.indexOf('/');
      int indexColon = str.lastIndexOf(':');

      if (indexSlash == -1)
      {
         retStr = str.substring(0, indexColon);
      }
      else if (str.startsWith("/"))
      {
         retStr = str.substring(1, indexColon);
      }
      else
      {
         retStr = str.substring(0, indexSlash);
      }

      return retStr;
   }

   /**
    * @return The remote address.
    */
   public InetSocketAddress getRemoteAddress()
   {
      return m_remoteAddress;
   }

   /**
    * @return The local address.
    */
   public InetSocketAddress getLocalAddress()
   {
      return m_localAddress;
   }

   /**
    * @return True if this connection is using SSL/TLS.
    */
   public boolean isSecure()
   {
      return m_bSecure;
   }

   /**
    * Gets the certificate to trust when establishing an SSL connection.
    * @return The certificate that will be trusted; null to trust certificates in the default
    * trust store.
    */
   public Certificate getTrustedCertificate()
   {
      return m_trustedCertificate;
   }

   /**
    * @return The certificate and private key of this, to use for outgoing
    * connections (optional) or for functioning as a server (required).
    */
   public KeyStore getCertificateStore()
   {
      return m_certificateStore;
   }

   /**
    * @return The certificate password.
    */
   public String getPassword()
   {
      return m_sPassword;
   }

   /**
    * @return The connection timeout in milliseconds (0 means infinite).
    */
   public int getConnectionTimeout()
   {
      return m_nConnectionTimeout;
   }

   /**
    * @return True if the TCP no-delay property should be set on the underlying socket.
    */
   public boolean isNoDelay()
   {
      return m_bNoDelay;
   }

   /**
    * @return True if the TCP keep-alive property should be set on the underlying socket.
    */
   public boolean isKeepAlive()
   {
      return m_bKeepAlive;
   }

   /**
    * @return The SO_SNDBUF option for the underlying socket, 0 for system default.
    */
   public int getSenderBufferSize()
   {
      return m_nSenderBufferSize;
   }

   /**
    * @return The SO_RCVBUF option for the underlying socket, 0 for system default.
    */
   public int getReceiverBufferSize()
   {
      return m_nReceiverBufferSize;
   }

   /**
    * @see nexj.core.rpc.ra.GenericConnectionRequestInfo#getManagedConnectionPartition()
    */
   public Object getManagedConnectionPartition()
   {
      return getHostString(m_remoteAddress) + ':' + m_remoteAddress.getPort() + ':'
         + getHostString(m_localAddress) + ':' + (m_bSecure ? '1' : '0');
   }
}
