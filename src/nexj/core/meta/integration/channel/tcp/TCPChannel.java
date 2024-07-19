// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.channel.tcp;

import java.nio.charset.Charset;
import java.security.KeyStore;
import java.security.cert.Certificate;

import nexj.core.integration.MessageStreamFactory;
import nexj.core.meta.Component;
import nexj.core.meta.MetadataException;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.channel.jms.MessageQueue;
import nexj.core.rpc.tcp.DefaultMessageStreamFactory;
import nexj.core.runtime.InvocationContext;

/**
 * TCP channel metadata
 */
public class TCPChannel extends Channel
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
    * The remote host.
    */
   protected String m_sRemoteHost;

   /**
    * The local host for binding.
    */
   protected String m_sLocalHost;

   /**
    * The string message encoding.
    */
   protected String m_sEncoding;

   /**
    * The default user account for processing the messages.
    */
   protected String m_sDefaultUser;

   /**
    * The SSL certificate password.
    */
   protected String m_sPassword;

   /**
    * The remote port.
    */
   protected int m_nRemotePort;

   /**
    * The local port for binding (0 is ephemeral).
    */
   protected int m_nLocalPort;

   /**
    * The server backlog.
    */
   protected int m_nBacklog;

   /**
    * The message read timeout (milliseconds).
    */
   protected int m_nTimeout;

   /**
    * The idle timeout (minutes).
    */
   protected int m_nIdleTimeout;

   /**
    * The connection timeout in milliseconds (0 means infinite).
    */
   protected int m_nConnectionTimeout = 60000;

   /**
    * True to resolve IP addresses to hostnames.
    */
   protected boolean m_bResolvingEnabled;

   /**
    * True if this channel uses SSL/TLS.
    */
   protected boolean m_bSecure;

   /**
    * True if the TCP keep-alive property should be set on the underlying socket.
    */
   protected boolean m_bKeepAlive;

   /**
    * True if the TCP no-delay property should be set on the underlying socket.
    */
   protected boolean m_bNoDelay;

   /**
    * The client certificate authentication mode, one of the CLIENT_AUTH_* constants.
    */
   protected byte m_nClientAuthMode = CLIENT_AUTH_NONE;

   /**
    * The maximum connection pool size.
    */
   protected int m_nMaxSenders = 16;

   /**
    * The maximum number of consumer threads.
    */
   protected int m_nMaxReceivers = 4;

   /**
    * The SO_SNDBUF option for the underlying socket, 0 for system default.
    */
   protected int m_nSenderBufferSize;

   /**
    * The SO_RCVBUF option for the underlying socket, 0 for system default.
    */
   protected int m_nReceiverBufferSize;

   /**
    * The RFC 1349 type-of-service value (sum of lowCost=2, reliability=4, throughput=8, lowDelay=16).
    */
   protected int m_nTOS;

   // associations

   /**
    * The trusted certificate.
    * The remote system's certificate must be signed by this certificate, or a
    * trusted certificate chain must be presented that contains this certificate.
    * If null, this channel will trust certificates in the default trust store.
    */
   protected Certificate m_trustedCertificate;

   /**
    * The certificate and private key to use for outgoing
    * connections (optional) or for functioning as a server (required).
    */
   protected KeyStore m_certificateStore;

   /**
    * The optional queue for forwarding the received messages.
    */
   protected MessageQueue m_queue;

   /**
    * The message splitter component.
    */
   protected Component m_splitter;

   /**
    * The user mapping component.
    */
   protected Component m_mapper;

   /**
    * The default message stream factory.
    */
   protected static MessageStreamFactory s_defaultFactory = new DefaultMessageStreamFactory();

   // constructors

   /**
    * Constructs the metadata object.
    * @param sName The name of the channel.
    */
   public TCPChannel(String sName)
   {
      super(sName);
   }

   // operations

   /**
    * @see nexj.core.meta.integration.Channel#isSynchronous()
    */
   public boolean isSynchronous()
   {
      return true;
   }

   /**
    * @see nexj.core.meta.integration.Channel#isTransactional()
    */
   public boolean isTransactional()
   {
      return false;
   }

   /**
    * Sets the remote host.
    * @param sHost The hostname or IP address of the remote host.
    */
   public void setRemoteHost(String sHost)
   {
      verifyNotReadOnly();
      m_sRemoteHost = sHost;
   }

   /**
    * @return The remote host.
    */
   public String getRemoteHost()
   {
      return m_sRemoteHost;
   }

   /**
    * Sets the remote port.
    * @param nPort The remote port.
    */
   public void setRemotePort(int nPort)
   {
      verifyNotReadOnly();
      m_nRemotePort = nPort;
   }

   /**
    * @return The remote port.
    */
   public int getRemotePort()
   {
      return m_nRemotePort;
   }

   /**
    * Sets the local host for binding.
    * @param sLocalHost The hostname or IP address of the local host.
    */
   public void setLocalHost(String sLocalHost)
   {
      verifyNotReadOnly();
      m_sLocalHost = sLocalHost;
   }

   /**
    * @return The local host for binding.
    */
   public String getLocalHost()
   {
      return m_sLocalHost;
   }

   /**
    * Sets the local port for binding (0 is ephemeral).
    * @param nLocalPort The local port.
    */
   public void setLocalPort(int nLocalPort)
   {
      verifyNotReadOnly();
      m_nLocalPort = nLocalPort;
   }

   /**
    * @return The local port for binding (0 is ephemeral).
    */
   public int getLocalPort()
   {
      return m_nLocalPort;
   }

   /**
    * Sets the string message encoding.
    * @param sEncoding The string encoding for messages on the channel.
    */
   public void setEncoding(String sEncoding)
   {
      verifyNotReadOnly();

      if (sEncoding != null)
      {
         try
         {
            Charset.forName(sEncoding);
         }
         catch (IllegalArgumentException e)
         {
            throw new MetadataException("err.meta.encoding", new Object[]{sEncoding});
         }
      }

      m_sEncoding = sEncoding;
   }

   /**
    * @return The string message encoding.
    */
   public String getEncoding()
   {
      return m_sEncoding;
   }

   /**
    * Sets the default user account for processing the messages.
    * @param sDefaultUser The default user account for processing messages.
    */
   public void setDefaultUser(String sDefaultUser)
   {
      verifyNotReadOnly();
      m_sDefaultUser = sDefaultUser;
   }

   /**
    * @return The default user account for processing the messages.
    */
   public String getDefaultUser()
   {
      return m_sDefaultUser;
   }

   /**
    * Sets the optional queue for forwarding the received messages.
    * @param queue A message queue.
    */
   public void setQueue(Channel queue)
   {
      verifyNotReadOnly();

      if (queue != null && !(queue instanceof MessageQueue))
      {
         throw new MetadataException("err.meta.integration.tcp.queueChannel", new Object[]{getName()});
      }

      setQueue((MessageQueue)queue);
   }

   /**
    * Sets the optional queue for forwarding the received messages.
    * @param queue A message queue.
    */
   public void setQueue(MessageQueue queue)
   {
      verifyNotReadOnly();

      if (queue != null)
      {
         if (!m_bReceivable)
         {
            throw new MetadataException("err.meta.integration.tcp.nonReceivableChannel",
               new Object[]{getName(), queue.getName()});
         }

         if (!queue.isSendable())
         {
            throw new MetadataException("err.meta.integration.tcp.nonSenderQueue",
               new Object[]{queue.getName(), getName()});
         }
      }

      m_queue = queue;
   }

   /**
    * @return The optional queue for forwarding the received messages.
    */
   public MessageQueue getQueue()
   {
      return m_queue;
   }

   /**
    * Sets the message splitter component.
    * @param component The message splitter component.
    */
   public void setSplitter(Component component)
   {
      verifyNotReadOnly();
      m_splitter = component;
   }

   /**
    * Gets the message splitter component.
    * @return The message splitter component.
    */
   public Component getSplitter()
   {
      return m_splitter;
   }

   /**
    * Gets the message stream factory.
    */
   public MessageStreamFactory getMessageStreamFactory(InvocationContext context)
   {
      if (m_splitter == null)
      {
         return s_defaultFactory;
      }

      return (MessageStreamFactory)m_splitter.getInstance(context);
   }

   /**
    * Sets whether to resolve hostnames in received messages.
    * @param bResolve True if hostnames should be resolved.
    */
   public void setResolvingEnabled(boolean bResolve)
   {
      verifyNotReadOnly();
      m_bResolvingEnabled = bResolve;
   }

   /**
    * @return True to resolve hostnames; false otherwise.
    */
   public boolean isResolvingEnabled()
   {
      return m_bResolvingEnabled;
   }

   /**
    * Sets the server backlog.
    * @param nBacklog The server backlog.
    */
   public void setBacklog(int nBacklog)
   {
      verifyNotReadOnly();
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
    * @param nTimeout The message read timeout (milliseconds).
    */
   public void setReadTimeout(int nTimeout)
   {
      verifyNotReadOnly();
      m_nTimeout = nTimeout;
   }

   /**
    * @return The message read timeout (milliseconds).
    */
   public int getReadTimeout()
   {
      return m_nTimeout;
   }

   /**
    * @param nTimeout The idle timeout (minutes).
    */
   public void setIdleTimeout(int nTimeout)
   {
      verifyNotReadOnly();
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
    * Sets the connection timeout in milliseconds.
    * @param nTimeout The connection timeout in milliseconds to set (0 means infinite).
    */
   public void setConnectionTimeout(int nTimeout)
   {
      verifyNotReadOnly();
      m_nConnectionTimeout = nTimeout;
   }

   /**
    * @return The connection timeout in milliseconds (0 means infinite).
    */
   public int getConnectionTimeout()
   {
      return m_nConnectionTimeout;
   }

   /**
    * @param bKeepAlive True to set the TCP keep-alive property on the underlying socket.
    */
   public void setKeepAlive(boolean bKeepAlive)
   {
      verifyNotReadOnly();
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
      verifyNotReadOnly();
      m_bNoDelay = bNoDelay;
   }

   /**
    * @return True if the TCP no-delay property is set on the underlying socket.
    */
   public boolean isNoDelay()
   {
      return m_bNoDelay;
   }

   /**
    * @param bSecure True if this channel will use SSL/TLS.
    */
   public void setSecure(boolean bSecure)
   {
      verifyNotReadOnly();
      m_bSecure = bSecure;
   }

   /**
    * @return True iff this channel is using SSL/TLS.
    */
   public boolean isSecure()
   {
      return m_bSecure;
   }

   /**
    * @param nClientAuthMode The client certificate authentication mode, one of the CLIENT_AUTH_* constants.
    */
   public void setClientAuthMode(byte nClientAuthMode)
   {
      verifyNotReadOnly();
      m_nClientAuthMode = nClientAuthMode;
   }

   /**
    * @return The client certificate authentication mode, one of the CLIENT_AUTH_* constants.
    */
   public byte getClientAuthMode()
   {
      return m_nClientAuthMode;
   }

   /**
    * Sets the password.
    * @param sPassword The password for this channel's SSL key store.
    */
   public void setPassword(String sPassword)
   {
      verifyNotReadOnly();
      m_sPassword = sPassword;
   }

   /**
    * Gets the password.
    * @return The password for this channel's SSL key store.
    */
   public String getPassword()
   {
      return m_sPassword;
   }

   /**
    * Sets the certificate to trust when establishing an SSL connection.
    * @param trustedCertificate The certificate that will be trusted; null to trust certificates in the default
    * trust store.
    */
   public void setTrustedCertificate(Certificate trustedCertificate)
   {
      verifyNotReadOnly();
      m_trustedCertificate = trustedCertificate;
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
    * @param certificateStore A PKCS #12 key store containing the certificate and private key.
    */
   public void setCertificateStore(KeyStore certificateStore)
   {
      verifyNotReadOnly();
      m_certificateStore = certificateStore;
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
    * Sets the user mapping component.
    * @param component The user mapping component.
    */
   public void setMapper(Component component)
   {
      verifyNotReadOnly();
      m_mapper = component;
   }

   /**
    * Gets the user mapping component.
    * @return The user mapping component.
    */
   public Component getMapper()
   {
      return m_mapper;
   }

   /**
    * Sets the maximum connection pool size.
    * @param nMaxSenders The maximum connection pool size to set.
    */
   public void setMaxSenders(int nMaxSenders)
   {
      verifyNotReadOnly();
      m_nMaxSenders = nMaxSenders;
   }

   /**
    * @return The maximum connection pool size.
    */
   public int getMaxSenders()
   {
      return m_nMaxSenders;
   }

   /**
    * Sets the maximum number of consumer threads.
    * @param nMaxReceivers The maximum number of consumer threads to set.
    */
   public void setMaxReceivers(int nMaxReceivers)
   {
      verifyNotReadOnly();
      m_nMaxReceivers = nMaxReceivers;
   }

   /**
    * @return The maximum number of consumer threads.
    */
   public int getMaxReceivers()
   {
      return m_nMaxReceivers;
   }

   /**
    * Sets the send buffer size (SO_SNDBUF socket option).
    * @param nSenderBufferSize The SO_SNDBUF option for the underlying socket, 0 for system default.
    */
   public void setSenderBufferSize(int nSenderBufferSize)
   {
      verifyNotReadOnly();
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
      verifyNotReadOnly();
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
    * Sets the RFC 1349 type-of-service.
    * @param nTOS The RFC 1349 type-of-service value (sum of lowCost=2, reliability=4, throughput=8, lowDelay=16).
    */
   public void setTOS(int nTOS)
   {
      verifyNotReadOnly();

      if (nTOS < 0 || nTOS > 255)
      {
         throw new MetadataException("err.meta.integration.tcp.invalidTOS",
            new Object[]{getName()});
      }

      m_nTOS = nTOS;
   }

   /**
    * @return The RFC 1349 type-of-service value (sum of lowCost=2, reliability=4, throughput=8, lowDelay=16).
    */
   public int getTOS()
   {
      return m_nTOS;
   }
}
