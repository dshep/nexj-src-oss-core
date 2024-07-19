// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.tcp.ra;

import java.security.KeyStore;
import java.security.cert.Certificate;

import javax.resource.spi.InvalidPropertyException;

import nexj.core.rpc.ra.GenericConsumerConfig;
import nexj.core.util.CertificateUtil;
import nexj.core.util.SysUtil;
import nexj.core.util.cipher.CharacterStreamCipherDispatcher;

/**
 * TCP consumer endpoint configuration, corresponding to one MDB metadata spec.
 */
public class TCPConsumerConfig extends GenericConsumerConfig
{
   // constants

   /**
    * No client certificate authentication.
    */
   public final static byte CLIENT_AUTH_NONE = 0;

   /**
    * Client certificate authentication supported.
    */
   public final static byte CLIENT_AUTH_SUPPORTED = 1;

   /**
    * Client certificate authentication required.
    */
   public final static byte CLIENT_AUTH_REQUIRED = 2;

   // attributes

   /**
    * The local host name for binding.
    */
   protected String m_sLocalHost;

   /**
    * The certificate password.
    */
   protected String m_sPassword;

   /**
    * The base64 encoding of m_certificateStore.
    */
   protected String m_sCertificateStore;

   /**
    * The server backlog.
    */
   protected int m_nBacklog;

   /**
    * The local port for binding.
    */
   protected int m_nLocalPort;

   /**
    * The message read timeout (milliseconds).
    */
   protected int m_nReadTimeout;

   /**
    * The idle timeout (minutes).
    */
   protected int m_nIdleTimeout;

   /**
    * The SO_SNDBUF option for the underlying socket, 0 for system default.
    */
   protected int m_nSenderBufferSize;

   /**
    * The SO_RCVBUF option for the underlying socket, 0 for system default.
    */
   protected int m_nReceiverBufferSize;

   /**
    * True if the TCP keep-alive property should be set on the underlying socket.
    */
   protected boolean m_bKeepAlive;

   /**
    * True if the TCP no-delay property should be set on the underlying socket.
    */
   protected boolean m_bNoDelay;

   /**
    * True if this connection uses SSL/TLS.
    */
   protected boolean m_bSecure;

   /**
    * The client certificate authentication mode, one of the CLIENT_AUTH_* constants.
    */
   protected byte m_nClientAuthMode = CLIENT_AUTH_NONE;

   // associations

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

   // operations

   /**
    * Sets the local host for binding.
    * @param sHost The local hostname or IP address to bind to ("*" or null means bind to all local interfaces).
    */
   public void setLocalHost(String sHost)
   {
      if ("*".equals(sHost))
      {
         sHost = null;
      }

      m_sLocalHost = sHost;
   }

   /**
    * @return The local host name for binding.
    */
   public String getLocalHost()
   {
      return m_sLocalHost;
   }

   /**
    * Sets the local port for binding.
    * @param nPort The local port to bind to.
    */
   public void setLocalPort(int nPort)
   {
      m_nLocalPort = nPort;
   }

   /**
    * @return The local port for binding.
    */
   public int getLocalPort()
   {
      return m_nLocalPort;
   }

   /**
    * @param nTimeout The message read timeout (milliseconds).
    */
   public void setReadTimeout(int nTimeout)
   {
      m_nReadTimeout = nTimeout;
   }

   /**
    * @return The message read timeout (milliseconds).
    */
   public int getReadTimeout()
   {
      return m_nReadTimeout;
   }

   /**
    * @param nTimeout The idle timeout (minutes).
    */
   public void setIdleTimeout(int nTimeout)
   {
      m_nIdleTimeout = nTimeout;
   }

   /**
    * @return The idle timeout (minutes).
    */
   public int getIdleTimeout()
   {
      return m_nIdleTimeout;
   }

   /**
    * Sets the send buffer size (SO_SNDBUF socket option).
    * @param nSenderBufferSize The SO_SNDBUF option for the underlying socket, 0 for system default.
    */
   public void setSenderBufferSize(int nSenderBufferSize)
   {
      m_nSenderBufferSize = nSenderBufferSize;
   }

   /**
    * @return The SO_SNDBUF option for the underlying socket, 0 for system default.
    */
   public int getSenderBufferSize()
   {
      return m_nSenderBufferSize;
   }

   /**
    * Sets the receive buffer size (SO_RCVBUF socket option).
    * @param nReceiverBufferSize The SO_RCVBUF option for the underlying socket, 0 for system default.
    */
   public void setReceiverBufferSize(int nReceiverBufferSize)
   {
      m_nReceiverBufferSize = nReceiverBufferSize;
   }

   /**
    * @return The SO_RCVBUF option for the underlying socket, 0 for system default.
    */
   public int getReceiverBufferSize()
   {
      return m_nReceiverBufferSize;
   }

   /**
    * @param bKeepAlive True to set the TCP keep-alive property on the underlying socket.
    */
   public void setKeepAlive(boolean bKeepAlive)
   {
      m_bKeepAlive = bKeepAlive;
   }

   /**
    * @return True if the TCP keep-alive property is set on the underlying socket.
    */
   public boolean isKeepAlive()
   {
      return m_bKeepAlive;
   }

   /**
    * @param bNoDelay True to set the TCP no-delay property on the underlying socket.
    */
   public void setNoDelay(boolean bNoDelay)
   {
      m_bNoDelay = bNoDelay;
   }

   /**
    * @return True if the TCP no-delay property should be set on the underlying socket.
    */
   public boolean isNoDelay()
   {
      return m_bNoDelay;
   }

   /**
    * Sets the server backlog.
    * @param nBacklog The server backlog.
    */
   public void setBacklog(int nBacklog)
   {
      m_nBacklog = nBacklog;
   }

   /**
    * @return The server backlog.
    */
   public int getBacklog()
   {
      return m_nBacklog;
   }

   /**
    * @param bSecure True if this connection uses SSL/TLS.
    */
   public void setSecure(boolean bSecure)
   {
      m_bSecure = bSecure;
   }

   /**
    * @return True if this connection uses SSL/TLS.
    */
   public boolean getSecure()
   {
      return m_bSecure;
   }

   /**
    * @param nClientAuthMode The client certificate authentication mode, one of the CLIENT_AUTH_* constants.
    */
   public void setAuthentication(int nClientAuthMode)
   {
      m_nClientAuthMode = (byte)nClientAuthMode;
   }

   /**
    * @return The client certificate authentication mode, one of the CLIENT_AUTH_* constants.
    */
   public int getAuthentication()
   {
      return (int)m_nClientAuthMode & 0xFF;
   }

   /**
    * Set the key store's password.
    * @param sPassword The key store's password.
    */
   public void setPassword(String sPassword)
   {
      CharacterStreamCipherDispatcher dec = new CharacterStreamCipherDispatcher();

      dec.init(SysUtil.getConfigProperties());
      m_sPassword = dec.decrypt(sPassword);
   }

   /**
    * @return The key store's password.
    */
   public String getPassword()
   {
      return m_sPassword;
   }

   /**
    * Sets the remote machine public certificate to trust when establishing an SSL connection.
    * @param sTrust The trusted X.509 certificate, base64-encoded.
    * @throws InvalidPropertyException if the certificate cannot be parsed.
    */
   public void setTrust(String sTrust) throws InvalidPropertyException
   {
      // Create the trust certificate
      try
      {
         m_trustedCertificate = CertificateUtil.parseCertificate(sTrust);
      }
      catch (Exception e)
      {
         throw new InvalidPropertyException("Invalid trust certificate: \"" + sTrust + "\"", e);
      }
   }
   
   /**
    * Sets the remote machine public certificate to trust when establishing an SSL connection.
    * @param trust The trusted certificate.
    */
   public void setTrustedCertificate(Certificate trust) 
   {
      m_trustedCertificate = trust;
   }

   /**
    * Must be implemented so setTrust(String) will be called by the application server.
    * @return The trusted X.509 certificate, base64-encoded.
    */
   public String getTrust()
   {
      return CertificateUtil.formatCertificate(m_trustedCertificate);
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
    * Sets the certificate and private key to use to identify this machine when
    * establishing an outbound connection using client certificate authentication,
    * or for incoming connections when functioning as a server.
    * @param sCertificateStore The PKCS #12 file containing the certificate and private key
    *                         that identify this machine, in base64 encoding.
    */
   public void setCertificate(String sCertificateStore)
   {
      m_sCertificateStore = sCertificateStore;
   }
   
   /**
    * Sets the certificate and private key to use to identify this machine when
    * establishing an outbound connection using client certificate authentication,
    * or for incoming connections when functioning as a server.
    * @param certificateStore The store containing the certificate and private key.
    */
   public void setCertificate(KeyStore certificateStore)
   {
      m_certificateStore = certificateStore;
   }

   /**
    * Must be implemented so setCertificate(String) will be called by the application server.
    * @return null
    */
   public String getCertificate()
   {
      return m_sCertificateStore;
   }

   /**
    * Gets the certificate and private key to use to identify this machine when
    * establishing an outbound connection using client certificate authentication,
    * or for incoming connections when functioning as a server.
    * @return The certificate and private key, bound together in a KeyStore.
    */
   public KeyStore getCertificateStore()
   {
      return m_certificateStore;
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumerConfig#validate()
    */
   public void validate() throws InvalidPropertyException
   {
      super.validate();

      if (m_nLocalPort < TCPResourceAdapter.MIN_PORT || m_nLocalPort > TCPResourceAdapter.MAX_PORT)
      {
         throw new InvalidPropertyException("Port not within the range "
            + TCPResourceAdapter.MIN_PORT + " - " + TCPResourceAdapter.MAX_PORT + ".");
      }

      if (m_nSenderBufferSize < 0)
      {
         throw new InvalidPropertyException("Send buffer size cannot be negative.");
      }

      if (m_nReceiverBufferSize < 0)
      {
         throw new InvalidPropertyException("Receive buffer size cannot be negative.");
      }

      // create the key store
      if (m_sCertificateStore != null)
      {
         try
         {
            m_certificateStore = CertificateUtil.parseKeyStore(m_sCertificateStore, m_sPassword);
         }
         catch (Exception ex)
         {
            throw new InvalidPropertyException("Invalid key store", ex);
         }
       }
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumerConfig#toString()
    */
   public String toString()
   {
      StringBuilder buf = new StringBuilder(64);

      buf.append(super.toString());
      buf.append("(bind=");
      buf.append((m_sLocalHost == null) ? "*" : m_sLocalHost);
      buf.append(':');
      buf.append(m_nLocalPort);
      buf.append(')');

      return buf.toString();
   }
}
