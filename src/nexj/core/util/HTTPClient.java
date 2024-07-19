// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InterruptedIOException;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.PasswordAuthentication;
import java.net.Proxy;
import java.net.ProxySelector;
import java.net.Socket;
import java.net.SocketAddress;
import java.net.SocketException;
import java.net.URI;
import java.net.URL;
import java.security.AccessController;
import java.security.GeneralSecurityException;
import java.security.KeyStore;
import java.security.PrivilegedAction;
import java.security.cert.Certificate;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import java.util.zip.GZIPInputStream;
import java.util.zip.InflaterInputStream;

import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.KeyManager;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;

import nexj.core.util.auth.AuthenticationException;
import nexj.core.util.auth.Authenticator;
import nexj.core.util.auth.AuthenticatorFactory;
import nexj.core.util.auth.LoginException;
import nexj.core.util.auth.PasswordAuthenticationProvider;
import nexj.core.util.pool.resource.GenericResource;
import nexj.core.util.pool.resource.GenericResourcePool;
import nexj.core.util.pool.resource.Resource;
import nexj.core.util.pool.resource.ResourceFactoryException;
import nexj.core.util.pool.resource.ResourcePool;
import nexj.core.version.Version;

/**
 * HTTP client implementation.
 */
public class HTTPClient
{
   // constants

   /**
    * Default buffer size.
    */
   public final static int BUF_SIZE = 8192;

   /**
    * Maximum number of authentication/redirection attempts.
    */
   public final static int MAX_REDIR_ATTEMPTS = 16;

   /**
    * Maximum number of basic authentication requests.
    */
   public final static int MAX_BASIC_ATTEMPTS = 3;

   /**
    * SPNEGO with cached credentials.
    */
   public final static int SPNEGO_SILENT = 0;

   /**
    * SPNEGO with explicit credentials.
    */
   public final static int SPNEGO_CRED = 1;

   /**
    * SPNEGO does not work.
    */
   public final static int SPNEGO_NONE = 2;

   /**
    * No authentication has been attempted.
    */
   protected final static int AUTH_NONE = 0;

   /**
    * Basic authentication.
    */
   protected final static int AUTH_BASIC = 1;

   /**
    * SPNEGO authentication.
    */
   protected final static int AUTH_SPNEGO = 2;

   /**
    * HTTPS tunnel not required.
    */
   protected final static int TUNNEL_NONE = 0;

   /**
    * HTTPS tunnel required or being created.
    */
   protected final static int TUNNEL_CREATE = 1;

   /**
    * HTTPS tunnel ready at proxy, but SSL not yet established.
    */
   protected final static int TUNNEL_READY = 2;

   /**
    * HTTP tunnel in-use.
    */
   protected final static int TUNNEL_ESTABLISHED = 3;

   /**
    * Cookie comparator.
    */
   protected final static Comparator COOKIE_COMPARATOR = new Comparator()
   {
      public int compare(Object left, Object right)
      {
         return ((Cookie)right).getPath().length() - ((Cookie)left).getPath().length();
      }
   };

   // attributes

   /**
    * The host URI part.
    */
   protected String m_sHost;

   /**
    * The host:port URI part.
    */
   protected String m_sHostPort;

   /**
    * The HTTP method.
    */
   protected String m_sMethod;

   /**
    * The HTTP protocol.
    */
   protected String m_sProtocol = HTTP.HTTP_1_1;

   /**
    * The HTTP response protocol.
    */
   protected String m_sResponseProtocol;

   /**
    * The HTTP response message.
    */
   protected String m_sResponseMessage;

   /**
    * The HTTP response status code.
    */
   protected int m_nResponseStatus;

   /**
    * The HTTPS tunnel status (one of the TUNNEL_* constants).
    */
   protected int m_nTunnelStatus;

   /**
    * Basic authentication attempt count.
    */
   protected int m_nBasicCount;

   /**
    * The strict SPNEGO flag.
    */
   protected boolean m_bSPNEGOStrict;

   /**
    * True if the authentication handshake has been completed on the client.
    * This does not mean that the authentication is complete,
    * as the server has to send the final response.
    */
   protected boolean m_bAuthDone;

   /**
    * The chunked transfer encoding mode.
    * Chunked transfer encoding has to be enabled explicitly,
    * either through this flag or through the Transfer-Encoding: chunked
    * header, as IIS 6.0 does not understand such requests.
    */
   protected boolean m_bChunked;

   /**
    * True to use a preset proxy.
    */
   protected boolean m_bPresetProxy;

   /**
    * True if m_connection has been retrieved from the cache.
    */
   protected boolean m_bCachedConnection;

   /**
    * The connection timeout in milliseconds (0 means infinite).
    */
   protected int m_nConnectionTimeout = 60000;

   /**
    * The read timeout in milliseconds (0 means infinite).
    */
   protected int m_nReadTimeout;

   /**
    * The token passed in the authentication response header.
    */
   protected String m_sToken;

   /**
    * The connection timestamp.
    */
   protected long m_lTime;

   // associations

   /**
    * The HTTP proxy.
    */
   protected Proxy m_proxy;

   /**
    * The HTTP server URI.
    */
   protected URI m_uri;

   /**
    * The request header map.
    */
   protected MIMEHeaderMap m_requestHeaderMap = new MIMEHeaderMap();

   /**
    * The response header map.
    */
   protected MIMEHeaderMap m_responseHeaderMap = new MIMEHeaderMap();

   /**
    * The HTTP connection pool.
    */
   protected HTTPConnectionPool m_pool;

   /**
    * The HTTP connection.
    */
   protected HTTPConnection m_connection;

   /**
    * The address of the server.
    */
   protected InetSocketAddress m_serverAddress;

   /**
    * Server authentication strategy.
    */
   protected AuthenticationStrategy m_serverAuthentication = new AuthenticationStrategy(false);

   /**
    * Proxy authentication strategy.
    */
   protected AuthenticationStrategy m_proxyAuthentication = new AuthenticationStrategy(true);

   /**
    * Authentication strategy for current service.
    */
   protected AuthenticationStrategy m_currentAuthentication = m_serverAuthentication;

   /**
    * The active authenticator.
    */
   protected Authenticator m_authenticator;

   /**
    * Cookie map: Cookie[sDomainKey].
    */
   protected Lookup m_cookieMap;

   /**
    * The cached authentication header.
    */
   protected MIMEHeader m_authHeader;

   /**
    * The cached proxy authentication header.
    */
   protected MIMEHeader m_proxyAuthHeader;

   /**
    * The public certificate of the remote system, when using client certificate
    * authentication. If null, trusts certificates in the default trust store.
    */
   protected Certificate m_trustedCertificate;

   /**
    * The certificate and private key of this system, to be used by client certificate
    * authentication when logging on to a remote system.
    */
   protected KeyStore m_clientCertificateStore;

   /**
    * The password protecting the private key in m_clientCertificateStore.
    */
   protected char[] m_achClientCertificatePassword;

   /**
    * The default connection pool.
    */
   protected static GenericHTTPConnectionPool s_connectionPool;

   /**
    * A cache of trust managers for each trusted certificate: (TrustManager[])[Certificate]
    */
   protected static Lookup s_trustedCertificateMap = new SoftHashTab();

   /**
    * A cache of key managers for each client certificate: (KeyManager[])[KeyStore]
    */
   protected static Lookup s_clientCertificateMap = new SoftHashTab();

   /**
    * Top-level domain set.
    */
   protected final static Holder s_topDomainSet = new HashHolder();

   static
   {
      String[] domains = new String[]{"biz", "com", "edu", "net", "org", "gov", "mil", "int"};

      for (int i = 0; i < domains.length; ++i)
      {
         s_topDomainSet.add(domains[i]);
      }
   }

   /**
    * Set of headers that can be sent to the proxy. The rest of headers will be filtered out.
    */
   protected final static Holder s_proxyHeaderSet = new HashHolder(9); // of type String

   static
   {
      s_proxyHeaderSet.add(HTTP.HEADER_ACCEPT);
      s_proxyHeaderSet.add(HTTP.HEADER_ACCEPT_ENCODING);
      s_proxyHeaderSet.add(HTTP.HEADER_CONTENT_LENGTH);
      s_proxyHeaderSet.add(HTTP.HEADER_COOKIE);
      s_proxyHeaderSet.add(HTTP.HEADER_HOST);
      s_proxyHeaderSet.add(HTTP.HEADER_PROXY_CONNECTION);
      s_proxyHeaderSet.add(HTTP.HEADER_TRANSFER_ENCODING);
      s_proxyHeaderSet.add(HTTP.HEADER_USER_AGENT);
      s_proxyHeaderSet.add(HTTP.PROXY_AUTH_REQUEST_HEADER);
   }

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(HTTPClient.class);

   // operations

   /**
    * Sets the HTTP connection pool.
    * @param pool The connection pool to set. Null means the default pool.
    */
   public void setConnectionPool(HTTPConnectionPool pool)
   {
      m_pool = pool;
   }

   /**
    * @return The HTTP connection pool.
    */
   public HTTPConnectionPool getConnectionPool()
   {
      if (m_pool == null)
      {
         m_pool = getDefaultConnectionPool();
      }

      return m_pool;
   }

   /**
    * Sets the connection timeout in milliseconds.
    * @param nTimeout The connection timeout in milliseconds to set (0 means infinite).
    */
   public void setConnectionTimeout(int nTimeout)
   {
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
    * Sets the read timeout in milliseconds (0 means infinite).
    * @param nReadTimeout The read timeout in milliseconds (0 means infinite) to set.
    */
   public void setReadTimeout(int nReadTimeout)
   {
      m_nReadTimeout = nReadTimeout;
   }

   /**
    * @return The read timeout in milliseconds (0 means infinite).
    */
   public int getReadTimeout()
   {
      return m_nReadTimeout;
   }

   /**
    * Sets the HTTP proxy.
    * @param proxy The HTTP proxy to set.
    */
   public void setProxy(Proxy proxy)
   {
      if (!ObjUtil.equal(proxy, m_proxy))
      {
         m_proxyAuthHeader = null;
         m_proxyAuthentication.clearServiceName();
      }

      m_proxy = proxy;
      m_bPresetProxy = (proxy != null);
   }

   /**
    * @return The HTTP proxy.
    */
   public Proxy getProxy()
   {
      return m_proxy;
   }

   /**
    * @return The server URI.
    */
   public URI getURI()
   {
      return m_uri;
   }

   /**
    * @return The host URI part.
    */
   public String getHost()
   {
      return m_sHost;
   }

   /**
    * @return The host:port URI part.
    */
   public String getHostPort()
   {
      return m_sHostPort;
   }

   /**
    * @return The HTTP method.
    */
   public String getMethod()
   {
      return m_sMethod;
   }

   /**
    * Sets the HTTP protocol.
    * @param sProtocol The HTTP protocol, one of the HTTP.HTTP_* constants.
    */
   public void setProtocol(String sProtocol)
   {
      assert sProtocol != null;

      m_sProtocol = sProtocol;
   }

   /**
    * Gets the HTTP protocol.
    * @return The HTTP protocol, one of the HTTP.HTTP_* constants.
    */
   public String getProtocol()
   {
      return m_sProtocol;
   }

   /**
    * Sets the password provider.
    * @param passwordProvider The password provider to set.
    */
   public void setPasswordProvider(PasswordAuthenticationProvider passwordProvider)
   {
      m_serverAuthentication.setProvider(passwordProvider);
   }

   /**
    * @return The password provider.
    */
   public PasswordAuthenticationProvider getPasswordProvider()
   {
      return m_serverAuthentication.getProvider();
   }

   /**
    * Sets the password provider for authenticating to the proxy.
    * @param passwordProvider The password provider to set.
    */
   public void setProxyPasswordProvider(PasswordAuthenticationProvider passwordProvider)
   {
      if (!ObjUtil.equal(passwordProvider, m_proxyAuthentication.getProvider()))
      {
         m_proxyAuthHeader = null;
      }

      m_proxyAuthentication.setProvider(passwordProvider);
   }

   /**
    * Gets the password provider for authenticating to the proxy.
    * @return The password provider.
    */
   public PasswordAuthenticationProvider getProxyPasswordProvider()
   {
      return m_proxyAuthentication.getProvider();
   }

   /**
    * Sets the SPNEGO protocol mode (one of the SPNEGO_* constants).
    * @param nSPNEGO The SPNEGO protocol mode (one of the SPNEGO_* constants) to set.
    */
   public void setSPNEGOMode(int nSPNEGO)
   {
      m_serverAuthentication.setSPNEGO(nSPNEGO);
   }

   /**
    * @return The SPNEGO protocol mode (one of the SPNEGO_* constants).
    */
   public int getSPNEGOMode()
   {
      return m_serverAuthentication.getSPNEGO();
   }

   /**
    * Sets the SPNEGO protocol mode for proxy authentication.
    * @param nSPNEGO The SPNEGO protocol mode (one of the SPNEGO_* constants) to set.
    */
   public void setProxySPNEGOMode(int nSPNEGO)
   {
      if (nSPNEGO != m_proxyAuthentication.getSPNEGO())
      {
         m_proxyAuthHeader = null;
      }

      m_proxyAuthentication.setSPNEGO(nSPNEGO);
   }

   /**
    * @return The SPNEGO protocol mode (one of the SPNEGO_* constants).
    */
   public int getProxySPNEGOMode()
   {
      return m_proxyAuthentication.getSPNEGO();
   }

   /**
    * Sets the strict SPNEGO flag.
    * @param bSPNEGOStrict The strict SPNEGO flag to set.
    */
   public void setSPNEGOStrict(boolean bSPNEGOStrict)
   {
      m_bSPNEGOStrict = bSPNEGOStrict;
   }

   /**
    * @return The strict SPNEGO flag.
    */
   public boolean isSPNEGOStrict()
   {
      return m_bSPNEGOStrict;
   }

   /**
    * Sets the chunked transfer encoding mode.
    * @param bChunked The chunked transfer encoding mode to set.
    */
   public void setChunked(boolean bChunked)
   {
      m_bChunked = bChunked;
   }

   /**
    * @return The chunked transfer encoding mode.
    */
   public boolean isChunked()
   {
      return m_bChunked;
   }

   /**
    * @return The request header map.
    */
   public MIMEHeaderMap getRequestHeaders()
   {
      return m_requestHeaderMap;
   }

   /**
    * @return The HTTP response status, typically one of the HTTP.STATUS_* constants.
    */
   public int getResponseStatus()
   {
      return m_nResponseStatus;
   }

   /**
    * @return The response protocol, typically one of the HTTP.HTTP_* constants.
    */
   public String getResponseProtocol()
   {
      return m_sResponseProtocol;
   }

   /**
    * @return The response message.
    */
   public String getResponseMessage()
   {
      return m_sResponseMessage;
   }

   /**
    * @return The response header map.
    */
   public MIMEHeaderMap getResponseHeaders()
   {
      return m_responseHeaderMap;
   }

   /**
    * Sets the remote system certificate to trust when using client certificate authentication.
    *
    * @param trustedCertificate The public certificate of the remote system; null to trust
    * certificates in the default trust store.
    */
   public void setTrustedCertificate(Certificate trustedCertificate)
   {
      m_trustedCertificate = trustedCertificate;
   }

   /**
    * Sets the certificate and private key for this system when logging in to a remote system
    * using client certificate authentication.
    *
    * @param clientCertificateStore The certificate and private key.
    * @param sKeyPass The password of the KeyStore.
    */
   public void setClientCertificate(KeyStore clientCertificateStore, String sKeyPass)
   {
      m_clientCertificateStore = clientCertificateStore;
      m_achClientCertificatePassword = sKeyPass.toCharArray();
   }

   /**
    * Sets proactive authentication.
    * @param bAuthProactive The proactive authenticaion.
    */
   public void setAuthProactive(boolean bAuthProactive)
   {
      m_serverAuthentication.setAuthProactive(bAuthProactive);
   }

   /**
    * @return True, if authentication is proactive.
    */
   public boolean isAuthProactive()
   {
      return m_serverAuthentication.isAuthProactive();
   }

   /**
    * Sets proactive authentication for proxy.
    * @param bAuthProactive The proactive authenticaion for proxy.
    */
   public void setProxyAuthProactive(boolean bProxyAuthProactive)
   {
      m_proxyAuthentication.setAuthProactive(bProxyAuthProactive);
   }

   /**
    * @return True, if authentication is proactive for proxy.
    */
   public boolean isProxyAuthProactive()
   {
      return m_proxyAuthentication.isAuthProactive();
   }

   /**
    * Invokes an HTTP method on a server.
    * @param uri The HTTP server URL.
    * @param sMethod The method name.
    * @param requestHandler The request handler. Can be null.
    * @param responseHandler The response handler. Can be null.
    * @return The response handler return value.
    * @throws IOException if an I/O error occurs.
    */
   public Object invoke(URI uri, String sMethod, RequestHandler requestHandler, ResponseHandler responseHandler)
      throws IOException
   {
      assert sMethod != null;

      m_serverAuthentication.clearServiceName();

      if (m_authHeader != null)
      {
         if (uri.equals(m_uri))
         {
            if (m_requestHeaderMap.find(m_authHeader.getName()) == null)
            {
               m_requestHeaderMap.add(m_authHeader);
            }
         }
         else
         {
            m_authHeader = null;
         }
      }

      if (m_proxyAuthHeader != null)
      {
         if (m_requestHeaderMap.find(m_proxyAuthHeader.getName()) == null)
         {
            m_requestHeaderMap.add(m_proxyAuthHeader);
         }
      }

      if (!m_bPresetProxy)
      {
         m_proxy = (Proxy)ProxySelector.getDefault().select(uri).get(0);
      }

      m_nBasicCount = -1;
      m_uri = uri;
      m_sMethod = sMethod;

      m_nTunnelStatus = (isHTTPProxy() && HTTP.SCHEME_SSL.equalsIgnoreCase(uri.getScheme())) ? TUNNEL_CREATE : TUNNEL_NONE;

      ByteArrayOutputStream requestStream = null;
      boolean bRequestStream = false;
      boolean bRequestEmpty = false;
      boolean bRequestLength = false;
      long lRequestLength = -1;

      try
      {
      attempt:
         for (int nAttempt = 0; nAttempt < MAX_REDIR_ATTEMPTS; ++nAttempt)
         {
            // Write the request

            connect();

            if (!bRequestStream)
            {
               bRequestEmpty = isRequestEmpty() || m_nTunnelStatus == TUNNEL_CREATE;  // "CONNECT" request should be empty

               if (!bRequestEmpty)
               {
                  bRequestLength = isLengthRequired();

                  if (bRequestLength)
                  {
                     if (requestHandler != null)
                     {
                        requestStream = new ByteArrayOutputStream(1024);
                        requestHandler.handleRequest(this, requestStream);
                     }

                     bRequestStream = true;
                  }
               }
            }

            m_requestHeaderMap.set(HTTP.HEADER_HOST, m_sHostPort);
            m_requestHeaderMap.setDefault(HTTP.HEADER_ACCEPT, "*/*");
            m_requestHeaderMap.setDefault(HTTP.HEADER_ACCEPT_ENCODING, "gzip, deflate");
            m_requestHeaderMap.setDefault(HTTP.HEADER_USER_AGENT, SysUtil.CAPTION + '/' + Version.RELEASE);

            // Avoid blind relay problem with persistent conns. Supports persistent conn to proxy in HTTP/1.1, where persistent is default.
            if (!isHTTPProxy() || m_nTunnelStatus == TUNNEL_ESTABLISHED)
            {
               if (m_requestHeaderMap.setDefault(HTTP.HEADER_CONNECTION, "keep-alive").findValue("keep-alive") != null)
               {
                  m_requestHeaderMap.setDefault(HTTP.HEADER_KEEP_ALIVE, "300");
               }

               m_requestHeaderMap.remove(HTTP.HEADER_PROXY_CONNECTION);
            }
            else
            {
               // Needed for Squid, not necessary on Microsoft ISA
               // Use non-standard Proxy-Connection header to prevent connection close during Negotiate authentication.
               m_requestHeaderMap.setDefault(HTTP.HEADER_PROXY_CONNECTION, "keep-alive");

               // Avoid blind relay problem
               m_requestHeaderMap.remove(HTTP.HEADER_CONNECTION);
            }

            addCookies(m_requestHeaderMap);

            if (bRequestLength)
            {
               lRequestLength = (requestStream == null) ? 0 : requestStream.size();
               m_requestHeaderMap.set(HTTP.HEADER_CONTENT_LENGTH, Long.toString(lRequestLength));
            }
            else if (bRequestEmpty)
            {
               m_requestHeaderMap.remove(HTTP.HEADER_CONTENT_LENGTH);
            }
            else
            {
               MIMEHeader header = m_requestHeaderMap.find(HTTP.HEADER_CONTENT_LENGTH);

               if (header != null && header.getValue() != null)
               {
                  lRequestLength = Long.parseLong(header.getValue());
               }
            }

            // use proactive authentication if this is not a subsequent attempt
            if (m_currentAuthentication.isAuthProactive()
               && m_currentAuthentication.parseAuthProtocol() == AUTH_NONE)
            {
               updateBasicAuthCount();
               startBasicAuth();
            }

            HTTPOutputStream ostream = new HTTPOutputStream(m_connection.getSocket().getOutputStream());
            boolean bChunked = false;

            if (bRequestEmpty || lRequestLength >= 0)
            {
               m_requestHeaderMap.remove(HTTP.HEADER_TRANSFER_ENCODING);
            }
            else if (isProtocolCurrent(m_sProtocol))
            {
               m_requestHeaderMap.set(HTTP.HEADER_TRANSFER_ENCODING, "chunked");
               bChunked = true;
            }

            if (s_logger.isDebugEnabled())
            {
               s_logger.debug(
                  (
                     (m_nTunnelStatus == TUNNEL_CREATE) ?
                     ("HTTP request: CONNECT " + m_sHostPort) : ("HTTP request: " + m_sMethod + ' ' + m_uri)
                  ) + ' ' + ((m_sProtocol == null) ? "" : m_sProtocol) +
                  ((s_logger.isDumpEnabled()) ? SysUtil.LINE_SEP + m_requestHeaderMap : "")
               );
            }

            try
            {
               if (m_nTunnelStatus == TUNNEL_CREATE)
               {
                  startRequest(ostream, true);
                  ostream.flush();
               }
               else
               {
                  startRequest(ostream, false);

                  ostream.setChunked(bChunked);

                  if (bRequestStream)
                  {
                     if (requestStream != null)
                     {
                        requestStream.writeTo(ostream);
                     }
                  }
                  else
                  {
                     if (requestHandler != null)
                     {
                        if (bRequestEmpty)
                        {
                           requestHandler.handleRequest(this, null);
                        }
                        else
                        {
                           requestStream = new ByteArrayOutputStream(1024);

                           BackupOutputStream bstream = new BackupOutputStream(ostream, requestStream);

                           requestHandler.handleRequest(this, bstream);
                        }
                     }

                     bRequestStream = true;
                  }

                  ostream.close();
               }
            }
            catch (SocketException e)
            {
               if (s_logger.isDebugEnabled())
               {
                  if (s_logger.isDumpEnabled())
                  {
                     s_logger.dump("Output error; retrying.", e);
                  }
                  else
                  {
                     s_logger.debug("Output error; retrying: " + e.getClass().getName() + ": " + e.getMessage());
                  }
               }

               if (m_bCachedConnection)
               {
                  --nAttempt;
               }

               disconnect(false);

               continue;
            }

            // Close the socket output stream, if necessary

            if (m_sProtocol == null)
            {
               m_connection.getSocket().shutdownOutput();
            }

            // Read the response

            HTTPInputStream istream = new HTTPInputStream(new BufferedInputStream(m_connection.getSocket().getInputStream(), BUF_SIZE));

            for (;;)
            {
               if (!istream.start())
               {
                  s_logger.debug("Server has disconnected; retrying.");

                  if (m_bCachedConnection)
                  {
                     --nAttempt;
                  }

                  disconnect(false);
                  m_nTunnelStatus = (isHTTPProxy() && HTTP.SCHEME_SSL.equalsIgnoreCase(uri.getScheme())) ? TUNNEL_CREATE : TUNNEL_NONE;
                  continue attempt;
               }

               if (s_logger.isDebugEnabled())
               {
                  s_logger.debug("HTTP response: " + ((m_sResponseProtocol == null) ? "" :
                     m_sResponseProtocol + ' ' + m_nResponseStatus + ' ' + m_sResponseMessage) +
                     ((s_logger.isDumpEnabled()) ? SysUtil.LINE_SEP + m_responseHeaderMap : ""));
               }

               MIMEHeader header = m_responseHeaderMap.find(HTTP.HEADER_SET_COOKIE);

               if (header != null)
               {
                  setCookies(header);
               }

               if (skip())
               {
                  istream.close();
               }
               else
               {
                  break;
               }
            }

            boolean bAuthComplete = authenticate();

            if (bAuthComplete && !redirect() && m_nTunnelStatus != TUNNEL_CREATE)
            {
               Object result = null;

               if (responseHandler != null)
               {
                  InputStream responseStream = istream;
                  MIMEHeader header = m_responseHeaderMap.find(HTTP.HEADER_CONTENT_ENCODING);

                  if (header != null)
                  {
                     int nLast = header.getValueCount() - 1;

                     if (nLast >= 0)
                     {
                        String sCoding = header.getValue(nLast).getName();

                        if (sCoding.equalsIgnoreCase("gzip") || sCoding.equalsIgnoreCase("x-gzip"))
                        {
                           responseStream = new GZIPInputStream(istream);
                        }
                        else if (sCoding.equalsIgnoreCase("deflate"))
                        {
                           responseStream = new InflaterInputStream(istream);
                        }

                        if (responseStream != istream)
                        {
                           if (nLast == 0)
                           {
                              m_responseHeaderMap.remove(HTTP.HEADER_CONTENT_ENCODING);
                           }
                           else
                           {
                              header.removeValue(nLast);
                           }
                        }
                     }
                  }

                  result = responseHandler.handleResponse(this, responseStream);
               }

               complete(istream);

               return result;
            }
            else if (m_nTunnelStatus != TUNNEL_CREATE)
            {
               complete(istream);
            }

            if (m_nTunnelStatus == TUNNEL_CREATE)
            {
               if (bAuthComplete)
               {
                  if (m_nResponseStatus / 100 != 2)
                  {
                     disconnect(false);

                     throw new HTTPClientException("Unable to establish SSL tunnel");
                  }

                  resetAuth();
                  m_nTunnelStatus = TUNNEL_READY;
                  m_proxyAuthHeader = null;
               }
               else
               {
                  complete(istream);
               }
            }
         }

         throw new HTTPClientException("Too many redirects");
      }
      catch (IOException e)
      {
         disconnect(false);

         throw e;
      }
      catch (RuntimeException e)
      {
         disconnect(false);

         throw e;
      }
      finally
      {
         resetAuth();
         m_currentAuthentication.clearUserPassword();

         if (m_authenticator != null)
         {
            m_authenticator.dispose();
            m_authenticator = null;
         }

         m_requestHeaderMap.clear();
         m_responseHeaderMap.clear();

         disconnect(true);
      }
   }

   /**
    * Resets the internal state.
    */
   public void reset()
   {
      m_nResponseStatus = 0;
      m_sResponseMessage = null;
      m_sResponseProtocol = null;
      m_cookieMap = null;
      m_authHeader = null;
      m_proxyAuthHeader = null;
      m_serverAuthentication.clearUserPasswordSaved();
      m_proxyAuthentication.clearUserPasswordSaved();
   }

   /**
    * Set the client cookies from a response header.
    * @param The Set-Cookie header.
    */
   protected void setCookies(MIMEHeader header)
   {
      long lTime = 0;

      for (int i = 0, n = header.getValueCount(); i < n; ++i)
      {
         MIMEHeader.Value value = header.getValue(i);
         String sName = value.getName();
         int k = sName.indexOf('=');

         if (k <= 0)
         {
            continue;
         }

         String sValue = sName.substring(k + 1);

         sName = sName.substring(0, k);

         Cookie cookie = new Cookie(sName, sValue);

         try
         {
            sValue = value.findArg("expires");

            if (sValue != null)
            {
               cookie.setExpiration(HTTP.parseCookieDateTime(sValue).getTime());
            }
            else
            {
               sValue = value.findArg("max-age");

               if (sValue != null)
               {
                  long lMaxAge = Long.parseLong(sValue);

                  if (lMaxAge > 0)
                  {
                     if (lTime == 0)
                     {
                        lTime = System.currentTimeMillis();
                     }

                     if (lMaxAge >= (Long.MAX_VALUE - lTime) / 1000)
                     {
                        lMaxAge = Long.MAX_VALUE;
                     }
                     else
                     {
                        lMaxAge = lMaxAge * 1000 + lTime;
                     }
                  }

                  cookie.setExpiration(lMaxAge);
               }
               else
               {
                  cookie.setExpiration(Long.MIN_VALUE);
               }
            }
         }
         catch (Exception e)
         {
            continue;
         }

         String sDomain = value.findArg("domain");

         if (sDomain == null)
         {
            sDomain = m_sHost.toLowerCase(Locale.ENGLISH);
         }
         else
         {
            sDomain = sDomain.toLowerCase(Locale.ENGLISH);

            if (sDomain.length() == 0)
            {
               continue;
            }

            if (sDomain.charAt(0) != '.')
            {
               sDomain = '.' + sDomain;
            }

            k = sDomain.lastIndexOf('.');

            int m = sDomain.lastIndexOf('.', k - 1);

            if (m < 0)
            {
               continue;
            }

            if (sDomain.lastIndexOf('.', m - 1) < 0 &&
               !s_topDomainSet.contains(sDomain.substring(k + 1)))
            {
               continue;
            }
         }

         cookie.setDomain(sDomain);

         String sPath = value.findArg("path");

         if (sPath == null)
         {
            sPath = m_uri.getRawPath();

            if (sPath == null)
            {
               sPath = "/";
            }
         }

         while (sPath.length() > 1 && sPath.charAt(sPath.length() - 1) == '/')
         {
            sPath = sPath.substring(0, sPath.length() - 1);
         }

         cookie.setPath(sPath);
         cookie.setSecure(value.findArg("secure") != null);

         if (!cookie.matches(m_sHost, m_uri))
         {
            if (s_logger.isDebugEnabled())
            {
               s_logger.debug("Rejecting an invalid cookie: " + cookie);
            }

            continue;
         }

         if (m_cookieMap == null)
         {
            m_cookieMap = new HashTab(4);
         }

         sName = Cookie.getDomainKey(sDomain);

         Cookie lastCookie = (Cookie)m_cookieMap.get(sName);

         if (lastCookie != null)
         {
            Cookie match = lastCookie.find(cookie);

            if (match != null)
            {
               if (match.remove())
               {
                  m_cookieMap.remove(sName);
                  lastCookie = null;
               }
               else if (match == lastCookie)
               {
                  lastCookie = match.getNext();
                  m_cookieMap.put(sName, lastCookie);
               }
            }
         }

         if (cookie.getExpiration() != Long.MIN_VALUE)
         {
            if (lTime == 0)
            {
               lTime = System.currentTimeMillis();
            }

            if (cookie.getExpiration() <= lTime)
            {
               if (s_logger.isDebugEnabled())
               {
                  s_logger.debug("Removing an expired cookie: " + cookie);
               }

               continue;
            }
         }

         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Adding a cookie: " + cookie);
         }

         m_cookieMap.put(sName, cookie);

         if (lastCookie != null)
         {
            lastCookie.add(cookie);
         }
      }
   }

   /**
    * Adds the client cookies to the request header map.
    * @param headerMap The header map.
    */
   protected void addCookies(MIMEHeaderMap headerMap)
   {
      if (m_cookieMap != null && m_cookieMap.size() != 0)
      {
         String sKey = Cookie.getDomainKey(m_sHost);
         Cookie start = (Cookie)m_cookieMap.get(sKey);

         if (start != null)
         {
            List cookieList = new ArrayList(4);
            Cookie cookie = start;

            do
            {
               if (cookie.getExpiration() != Long.MIN_VALUE &&
                  cookie.getExpiration() <= m_lTime)
               {
                  if (s_logger.isDebugEnabled())
                  {
                     s_logger.debug("Removing an expired cookie: " + cookie);
                  }

                  if (cookie.remove())
                  {
                     m_cookieMap.remove(sKey);

                     break;
                  }

                  if (cookie == start)
                  {
                     start = cookie.getNext();
                     m_cookieMap.put(sKey, start);

                     continue;
                  }
               }
               else
               {
                  if (cookie.matches(m_sHost, m_uri))
                  {
                     cookieList.add(cookie);
                  }
               }

               cookie = cookie.getNext();
            }
            while (cookie != start);

            if (cookieList.size() != 0)
            {
               Collections.sort(cookieList, COOKIE_COMPARATOR);

               StringBuilder buf = new StringBuilder(64);

               for (int i = 0, n = cookieList.size(); i != n; ++i)
               {
                  if (i != 0)
                  {
                     buf.append("; ");
                  }

                  cookie = (Cookie)cookieList.get(i);

                  buf.append(cookie.getName());
                  buf.append('=');
                  buf.append(cookie.getValue());
               }

               headerMap.set(new MIMEHeader(HTTP.HEADER_COOKIE, buf.toString()));
            }
         }
      }
   }

   /**
    * Determines if the protocol is at least HTTP/1.1.
    * @param sProtocol The HTTP protocol.
    * @return True if the protocol is at least HTTP/1.1.
    */
   protected boolean isProtocolCurrent(String sProtocol)
   {
      return sProtocol != null &&
         sProtocol.regionMatches(0, HTTP.HTTP_1_1, 0, HTTP.HTTP_LENGTH) &&
         sProtocol.compareTo(HTTP.HTTP_1_1) >= 0;
   }

   /**
    * @return True if a request has only headers.
    */
   protected boolean isRequestEmpty()
   {
      return m_sMethod.equals(HTTP.METHOD_GET) ||
         m_sMethod.equals(HTTP.METHOD_HEAD) ||
         m_sMethod.equals(HTTP.METHOD_DELETE);
   }

   /**
    * @return True if the content length has to be computed.
    */
   protected boolean isLengthRequired()
   {
      if (m_sProtocol == null)
      {
         return false;
      }

      MIMEHeader header = m_requestHeaderMap.find(HTTP.HEADER_CONTENT_LENGTH);

      if (header != null)
      {
         String sLength = header.getValue();

         if (sLength == null || sLength.length() == 0 || sLength.charAt(0) == '-')
         {
            return true;
         }
      }

      if (!isProtocolCurrent(m_sProtocol))
      {
         return true;
      }

      if (m_bChunked)
      {
         return false;
      }

      header = m_requestHeaderMap.find(HTTP.HEADER_TRANSFER_ENCODING);

      return header == null || header.getValueCount() == 0 ||
         !header.getValue(header.getValueCount() - 1).getName().equalsIgnoreCase("chunked");
   }

   /**
    * Writes the request to the output stream.
    * @param ostream The output stream.
    * @param bTunnelRequest True to establish an HTTP tunnel through the proxy server; false otherwise;
    */
   protected void startRequest(HTTPOutputStream ostream, boolean bTunnelRequest) throws IOException
   {
      ostream.write((bTunnelRequest) ? HTTP.METHOD_CONNECT : m_sMethod);
      ostream.write(' ');

      if (bTunnelRequest)
      {
         ostream.write(m_sHostPort);

         if (m_sHostPort.indexOf(':') < 0)
         {
            ostream.write(":443");
         }
      }
      else
      {
         if (isHTTPProxy() && m_nTunnelStatus == TUNNEL_NONE)
         {
            ostream.write(m_uri.getScheme());
            ostream.write("://");
            ostream.write(m_sHostPort);
         }

         String sPath = m_uri.getRawPath();

         if (sPath == null || sPath.length() == 0)
         {
            sPath = "/";
         }

         ostream.write(sPath);

         String sQuery = m_uri.getRawQuery();

         if (sQuery != null)
         {
            ostream.write('?');
            ostream.write(sQuery);
         }

         String sFragment = m_uri.getRawFragment();

         if (sFragment != null)
         {
            ostream.write('#');
            ostream.write(sFragment);
         }
      }

      if (m_sProtocol != null)
      {
         ostream.write(' ');
         ostream.write(m_sProtocol);
         ostream.write("\r\n");

         for (int i = 0, n = m_requestHeaderMap.size(); i < n; ++i)
         {
            MIMEHeader header = m_requestHeaderMap.get(i);
            String sHeaderName = header.getName();

            if (bTunnelRequest && !s_proxyHeaderSet.contains(sHeaderName))
            {
               continue;
            }

            ostream.write(sHeaderName);
            ostream.write(": ");
            ostream.write(header.getValue());
            ostream.write("\r\n");
         }
      }

      ostream.write("\r\n");
   }

   /**
    * Resolves the address if it is unresolved.
    * @param address The address to resolve.
    * @return The resolved address.
    */
   protected static InetSocketAddress resolve(final InetSocketAddress address)
   {
      if (address.isUnresolved())
      {
         return (InetSocketAddress)AccessController.doPrivileged(
            new PrivilegedAction()
            {
               public Object run()
               {
                  return new InetSocketAddress(((InetSocketAddress)address).getHostName(),
                     ((InetSocketAddress)address).getPort());
               }
            }
         );
      }

      return address;
   }

   /**
    * Connects to the server.
    */
   protected void connect() throws IOException
   {
      int nPort = m_uri.getPort();

      if (m_connection != null)
      {
         if (isHTTPProxy() && m_nTunnelStatus == TUNNEL_READY)
         {
            if (s_logger.isDebugEnabled())
            {
               s_logger.debug("Layering SSL socket to " + m_sHost + ":" + nPort +
                  " over tunnel socket: " + m_connection.getSocket());
            }

            m_connection.startSSL();
            m_nTunnelStatus = TUNNEL_ESTABLISHED;
         }

         m_lTime = System.currentTimeMillis();

         return;
      }

      m_bCachedConnection = false;
      m_sHostPort = m_sHost = (m_uri.getHost() != null) ? m_uri.getHost() : InetAddress.getByName(null).getHostName();

      if (nPort >= 0)
      {
         m_sHostPort += ":" + nPort;
      }

      boolean bSSL = false;

      if (HTTP.SCHEME_HTTP.equalsIgnoreCase(m_uri.getScheme()))
      {
         if (nPort < 0)
         {
            nPort = 80;
         }
      }
      else if (HTTP.SCHEME_SSL.equalsIgnoreCase(m_uri.getScheme()))
      {
         if (nPort < 0)
         {
            nPort = 443;
         }

         bSSL = true;
      }
      else
      {
         throw new HTTPClientException("Unknown scheme \"" + m_uri.getScheme() + "\"");
      }

      final int nSocketPort = nPort;

      m_serverAddress = (InetSocketAddress)AccessController.doPrivileged(
         new PrivilegedAction()
         {
            public Object run()
            {
               return new InetSocketAddress(m_sHost, nSocketPort);
            }
         });

      boolean bHTTPProxy = isHTTPProxy();
      HTTPConfig config = new HTTPConfig(
         (bHTTPProxy) ? resolve((InetSocketAddress)m_proxy.address()) : m_serverAddress,
         m_nConnectionTimeout, m_nReadTimeout, bSSL, m_trustedCertificate, m_clientCertificateStore,
         m_achClientCertificatePassword, (bHTTPProxy && bSSL) ? m_serverAddress : null, m_proxy, m_proxyAuthentication);

      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Connecting to " + config.getAddress());
      }

      try
      {
         m_connection = (HTTPConnection)getConnectionPool().get(config);
      }
      catch (ResourceFactoryException ex)
      {
         if (ex.getCause() instanceof IOException)
         {
            throw (IOException)ex.getCause();
         }

         throw ex;
      }

      m_lTime = System.currentTimeMillis();

      if (m_connection.isCached())
      {
         m_nTunnelStatus = (m_nTunnelStatus == TUNNEL_CREATE) ? TUNNEL_ESTABLISHED : TUNNEL_NONE;
         m_bCachedConnection = true;
      }
   }

   /**
    * Disconnects the client.
    * @param bCache True to cache the socket.
    */
   protected void disconnect(boolean bCache) throws IOException
   {
      if (m_connection != null)
      {
         if (bCache)
         {
            m_connection.release();
         }
         else
         {
            m_connection.dispose();
         }

         m_connection = null;
         m_serverAddress = null;
         m_bCachedConnection = false;
      }
   }

   /**
    * Completes the request/response cycle.
    * @param istream The HTTP input stream.
    */
   protected void complete(HTTPInputStream istream) throws IOException
   {
      istream.close();

      // Close the socket, if necessary

      if (m_sProtocol == null || !istream.isChunked() && istream.getMaxCount() < 0)
      {
         disconnect(false);
      }
      else
      {
         MIMEHeader header = m_responseHeaderMap.find(HTTP.HEADER_CONNECTION);

         if (header == null)
         {
            if (!isProtocolCurrent(m_sResponseProtocol))
            {
               disconnect(false);
            }
         }
         else if (m_sProtocol.equals(HTTP.HTTP_1_0) && header.findValue("keep-alive") == null ||
            header.findValue("close") != null)
         {
            disconnect(false);
         }
      }
   }

   /**
    * Skips the response, if needed.
    * @return True if the response must be skipped.
    */
   protected boolean skip() throws IOException
   {
      return m_nResponseStatus / 100 == 1;
   }

   /**
    * Redirects the client.
    * @return True if redirection is required.
    */
   protected boolean redirect() throws IOException
   {
      if (m_nResponseStatus == HTTP.STATUS_REQUEST_TIMEOUT)
      {
         return true;
      }

      return false;
   }

   /**
    * Sets the request authentication token.
    * @param nProtocol The authentication protocol, one of the AUTH_* constants.
    * @param token The authentication token.
    */
   protected void setAuthToken(int nProtocol, byte[] token) throws IOException
   {
      if (m_currentAuthentication.isProxy())
      {
         m_proxyAuthHeader = m_requestHeaderMap.set(HTTP.PROXY_AUTH_REQUEST_HEADER,
            ((nProtocol == AUTH_BASIC) ? HTTP.BASIC_PROTOCOL : HTTP.SPNEGO_PROTOCOL) + " " + Base64Util.encode(token));

         if (nProtocol != AUTH_BASIC)
         {
            m_proxyAuthHeader = null;
         }

         // Maintain server authentication (for Squid)
         if (m_authHeader != null)
         {
            m_requestHeaderMap.set(m_authHeader);
         }
      }
      else
      {
         m_authHeader = m_requestHeaderMap.set(HTTP.AUTH_REQUEST_HEADER,
            ((nProtocol == AUTH_BASIC) ? HTTP.BASIC_PROTOCOL : HTTP.SPNEGO_PROTOCOL) + " " + Base64Util.encode(token));

         if (nProtocol != AUTH_BASIC)
         {
            m_authHeader = null;
         }

         // Maintain proxy authentication (for Squid)
         if (m_proxyAuthHeader != null)
         {
            m_requestHeaderMap.set(m_proxyAuthHeader);
         }
      }
   }

   /**
    * @return True if the proxy is an HTTP proxy; false otherwise (or if no proxy set).
    */
   protected boolean isHTTPProxy()
   {
      return m_proxy != null && m_proxy.type() == Proxy.Type.HTTP;
   }

   /**
    * Retrieves the user name and the password.
    * @return True if they are different object from the previous ones.
    */
   protected boolean retrievePassword()
   {
      if (m_currentAuthentication.getUser() == null)
      {
         PasswordAuthenticationProvider provider = m_currentAuthentication.getProvider();

         if (provider == null)
         {
            return false;
         }

         if (provider instanceof Prompt)
         {
            ((Prompt)provider).setPrompt((m_currentAuthentication.getUserSaved() == null) ? null : "Invalid password");
         }

         PasswordAuthentication pwa = provider.getPasswordAuthentication();

         m_currentAuthentication.setUser((pwa == null) ? null : pwa.getUserName());
         m_currentAuthentication.setPassword((pwa == null) ? null : pwa.getPassword());

         if (m_currentAuthentication.isCurrentMatchedBySaved())
         {
            return false;
         }

         m_currentAuthentication.copyToSaved();
      }

      return true;
   }

   /**
    * Resets the authentication state.
    */
   protected void resetAuth()
   {
      m_sToken = null;
      m_bAuthDone = false;
      m_requestHeaderMap.remove(HTTP.AUTH_REQUEST_HEADER);
      m_requestHeaderMap.remove(HTTP.PROXY_AUTH_REQUEST_HEADER);
   }

   /**
    * Authenticates the client, if necessary.
    * @return True if the handshake is complete or not needed.
    * @throws IOException if an I/O error occurs.
    */
   protected boolean authenticate() throws IOException
   {
      switch (m_nResponseStatus)
      {
         case HTTP.STATUS_UNAUTHORIZED:

            if (m_currentAuthentication.isProxy())
            {
               completeAuthentication();
               m_bAuthDone = false;
               m_nBasicCount = -1;
            }

            m_currentAuthentication = m_serverAuthentication;

            break;

         case HTTP.STATUS_PROXY_AUTHENTICATION:

            // Disallow proxy auth if no proxy or through a tunnel
            if (!isHTTPProxy() || m_nTunnelStatus == TUNNEL_ESTABLISHED)
            {
               throw new LoginException("Proxy authentication disallowed to non-proxy");
            }

            m_currentAuthentication = m_proxyAuthentication;

            break;

         case HTTP.STATUS_FORBIDDEN:
            if (s_logger.isDebugEnabled())
            {
               s_logger.debug("Authentication error " +
                  m_nResponseStatus + ": " + m_sResponseMessage);
            }

            m_bAuthDone = false;

            if (m_currentAuthentication.getProvider() == null || m_currentAuthentication.getProvider().isAuthenticationDeterministic())
            {
               return true;
            }

            if (m_currentAuthentication.getSPNEGO() == SPNEGO_SILENT)
            {
               m_currentAuthentication.setSPNEGO(SPNEGO_CRED);
               m_nBasicCount = 0;
               m_sToken = null;

               if (m_authenticator != null)
               {
                  m_authenticator.dispose();
                  m_authenticator = null;
               }

               resetAuth();

               return false;
            }

            break;

         default:
            completeAuthentication();

            return true;
      }

      updateBasicAuthCount();

      int nProtocol = m_currentAuthentication.parseAuthProtocol();

      while (nProtocol == AUTH_SPNEGO)
      {
         if (m_authenticator == null)
         {
            try
            {
               m_authenticator = AuthenticatorFactory.create();
            }
            catch (Exception e)
            {
               m_serverAuthentication.setSPNEGO(SPNEGO_NONE);
               m_proxyAuthentication.setSPNEGO(SPNEGO_NONE);
               nProtocol = m_currentAuthentication.parseAuthProtocol();
            }
         }

         if (m_authenticator != null)
         {
            try
            {
               byte[] token;

               if (m_sToken == null)
               {
                  if (m_currentAuthentication.getSPNEGO() == SPNEGO_SILENT && !m_bAuthDone)
                  {
                     // Use mutual authentication due to bug #6733095 in Sun's JRE: http://bugs.sun.com/view_bug.do?bug_id=6733095
                     m_authenticator.init(Authenticator.PROTOCOL_SPNEGO, Authenticator.MODE_MUTUAL, m_currentAuthentication.getServiceName(), null, null, null);
                  }
                  else
                  {
                     m_currentAuthentication.setSPNEGO(SPNEGO_CRED);
                     m_bAuthDone = false;

                     if (m_nBasicCount < 0)
                     {
                        m_currentAuthentication.copyFromSaved();
                     }
                     else
                     {
                        m_currentAuthentication.clearUserPassword();
                     }

                     if (m_nBasicCount++ == MAX_BASIC_ATTEMPTS || !retrievePassword())
                     {
                        throw new IOException("Too many authentication attempts");
                     }

                     // Use mutual authentication due to bug #6733095 in Sun's JRE: http://bugs.sun.com/view_bug.do?bug_id=6733095
                     m_authenticator.init(Authenticator.PROTOCOL_SPNEGO, Authenticator.MODE_MUTUAL, m_currentAuthentication.getServiceName(), null, m_currentAuthentication.getUser(), m_currentAuthentication.getPassword());
                  }

                  if (s_logger.isDebugEnabled())
                  {
                     s_logger.debug("Starting authentication sequence with service \"" + m_currentAuthentication.getServiceName() +
                        ((m_currentAuthentication.getUser() == null) ? "\"" : "\", user \"" + m_currentAuthentication.getUser() + "\""));
                  }

                  token = m_authenticator.nextToken(null);
               }
               else
               {
                  token = Base64Util.decode(m_sToken);

                  if (s_logger.isDebugEnabled())
                  {
                     s_logger.debug("Authentication response token " + new Binary(token));
                  }

                  token = m_authenticator.nextToken(token);
               }

               resetAuth();

               if (token != null)
               {
                  if (s_logger.isDebugEnabled())
                  {
                     s_logger.debug("Authentication request token " + new Binary(token));
                  }

                  setAuthToken(AUTH_SPNEGO, token);
               }

               if (m_authenticator.isDone())
               {
                  m_authenticator.dispose();
                  m_authenticator = null;
                  m_bAuthDone = true;
               }

               return false;
            }
            catch (CancellationException e)
            {
               throw e;
            }
            catch (Exception e)
            {
               s_logger.debug("Authentication error", e);

               if (m_currentAuthentication.getSPNEGO() == SPNEGO_SILENT &&
                  e instanceof LoginException)
               {
                  m_currentAuthentication.setSPNEGO(SPNEGO_CRED);
               }
               else
               {
                  m_currentAuthentication.setSPNEGO(SPNEGO_NONE);
                  nProtocol = m_currentAuthentication.parseAuthProtocol();
                  m_authenticator.dispose();
                  m_authenticator = null;
               }

               m_nBasicCount = 0;
               m_sToken = null;
            }
         }
      }

      if (nProtocol == AUTH_BASIC)
      {
         startBasicAuth();

         return false;
      }

      throw new LoginException();
   }

   /**
    * Updates basic authentication attempt count.
    */
   private void updateBasicAuthCount()
   {
      if (m_nBasicCount < 0 && m_currentAuthentication.getUserSaved() == null)
      {
         m_nBasicCount = 0;
      }

      if (m_bAuthDone)
      {
         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Authentication error " + m_nResponseStatus + ": " + m_sResponseMessage);
         }
      }
   }

   /**
    * Append basic authentication header to the request. 
    */
   private void startBasicAuth() throws IOException
   {
      if (m_nBasicCount < 0)
      {
         m_currentAuthentication.copyFromSaved();
      }

      if (m_nBasicCount++ == MAX_BASIC_ATTEMPTS || !retrievePassword())
      {
         m_authHeader = null;

         throw new LoginException();
      }

      StringBuffer buf = new StringBuffer(32);

      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Attempting basic authentication for user \"" + m_currentAuthentication.getUser() + "\"");
      }

      if (m_currentAuthentication.getUser() != null)
      {
         buf.append(m_currentAuthentication.getUser());
      }

      buf.append(':');

      if (m_currentAuthentication.getPassword() != null)
      {
         buf.append(m_currentAuthentication.getPassword());
      }

      byte[] token = buf.toString().getBytes("UTF-8");

      for (int i = 0; i < buf.length(); ++i)
      {
         buf.setCharAt(i, ' ');
      }

      m_currentAuthentication.setUser(null);

      resetAuth();
      setAuthToken(AUTH_BASIC, token);
      Arrays.fill(token, (byte)0);
   }

   /**
    * Completes authentication. If using an authenticator then it is disposed.
    *
    * @throws IOException If an I/O error occurs.
    */
   protected void completeAuthentication() throws IOException
   {
      if (m_authenticator != null)
      {
         if (m_currentAuthentication.parseAuthProtocol() == AUTH_SPNEGO && m_sToken != null)
         {
            try
            {
               byte[] token = Base64Util.decode(m_sToken);

               if (s_logger.isDebugEnabled())
               {
                  s_logger.debug("Authentication response token " + new Binary(token));
               }

               if (!m_bSPNEGOStrict ||
                  m_authenticator.nextToken(token) == null &&
                  m_authenticator.isDone())
               {
                  m_authenticator.dispose();
                  m_authenticator = null;

                  s_logger.debug("Authentication complete");

                  return;
               }
            }
            catch (Exception e)
            {
               s_logger.debug("Authentication error", e);
            }
         }
         else if (m_bSPNEGOStrict)
         {
            throw new AuthenticationException("err.auth.server");
         }

         m_authenticator.dispose();
         m_authenticator = null;
      }
      else if (m_bAuthDone)
      {
         s_logger.debug("Authentication complete");
      }
   }

   /**
    * @return The default HTTP connection pool.
    */
   public static synchronized GenericHTTPConnectionPool getDefaultConnectionPool()
   {
      if (s_connectionPool == null)
      {
         s_connectionPool = new GenericHTTPConnectionPool();
      }

      return s_connectionPool;
   }

   // inner classes

   /**
    * HTTP request handler.
    */
   public interface RequestHandler
   {
      /**
       * Outputs the request.
       * @param client The HTTP client.
       * @param ostream The output stream. Null, if the method does not support a request body.
       * @throws IOException if an I/O error occurs.
       */
      void handleRequest(HTTPClient client, OutputStream ostream) throws IOException;
   }

   /**
    * HTTP response handler.
    */
   public interface ResponseHandler
   {
      /**
       * Inputs the response.
       * @param client The HTTP client.
       * @param istream The input stream. Null, if the method or the error code does not support a response body.
       * @return The HTTPClient.invoke() result.
       * @throws IOException if an I/O error occurs.
       */
      Object handleResponse(HTTPClient client, InputStream istream) throws IOException;
   }

   /**
    * Output stream for generating an HTTP request.
    */
   protected class HTTPOutputStream extends MIMEOutputStream
   {
      // constants

      /**
       * The buffer offset for chunked encoding.
       */
      protected final static int BUF_OFS = 8;

      // attributes

      /**
       * The buffer byte count.
       */
      protected int m_nCount;

      /**
       * The chunked segment offset in the buffer.
       */
      protected int m_nOffset;

      /**
       * True if the stream has been closed.
       */
      protected boolean m_bClosed;

      /**
       * The chunked transfer encoding flag.
       */
      protected boolean m_bChunked;

      // associations

      /**
       * The stream butter.
       */
      protected byte[] m_buf = new byte[BUF_OFS + BUF_SIZE + BUF_OFS + 8];

      // constructors

      /**
       * Constructs an HTTP output stream.
       * @param ostream The socket output stream.
       */
      public HTTPOutputStream(OutputStream ostream)
      {
         super(ostream);
         m_nCount = m_nOffset = BUF_OFS;
      }

      // operations

      /**
       * Sets the chunked transfer encoding flag.
       * @param bChunked The chunked transfer encodign flag to set.
       */
      public void setChunked(boolean bChunked)
      {
         m_bChunked = bChunked;
         m_nOffset = m_nCount;
      }

      /**
       * @return The chunked transfer encoding flag.
       */
      public boolean isChunked()
      {
         return m_bChunked;
      }

      /**
       * @see nexj.core.util.MIMEOutputStream#write(int)
       */
      public void write(int ch) throws IOException
      {
         if (m_nCount == BUF_OFS + BUF_SIZE)
         {
            flush();
         }

         m_buf[m_nCount++] = (byte)ch;
      }

      /**
       * @see nexj.core.util.MIMEOutputStream#flush()
       */
      public void flush() throws IOException
      {
         if (m_nCount != BUF_OFS)
         {
            flush(m_nCount);
         }
      }

      /**
       * Flushes the buffer.
       * @param nCount The chunk byte count.
       */
      protected void flush(int nCount) throws IOException
      {
         int nOfs = BUF_OFS;

         if (m_bChunked && nCount != m_nOffset)
         {
            m_buf[--nOfs] = '\n';
            m_buf[--nOfs] = '\r';
            nCount -= m_nOffset;

            do
            {
               int nDigit = nCount & 0x0f;

               m_buf[--nOfs] = (byte)(nDigit + ((nDigit < 10) ? '0' : 'A' - 10));
               nCount >>>= 4;
            }
            while (nCount != 0);

            if (m_nOffset != BUF_OFS)
            {
               System.arraycopy(m_buf, nOfs, m_buf, m_buf.length - BUF_OFS, BUF_OFS - nOfs);
               System.arraycopy(m_buf, BUF_OFS, m_buf, nOfs, m_nOffset - BUF_OFS);
               System.arraycopy(m_buf, m_buf.length - BUF_OFS, m_buf, m_nOffset - BUF_OFS + nOfs, BUF_OFS - nOfs);
            }

            m_buf[m_nCount++] = '\r';
            m_buf[m_nCount++] = '\n';
         }

         m_ostream.write(m_buf, nOfs, m_nCount - nOfs);
         m_nCount = m_nOffset = BUF_OFS;
         m_ostream.flush();
      }

      /**
       * @see java.io.OutputStream#close()
       */
      public void close() throws IOException
      {
         if (!m_bClosed)
         {
            int nCount = m_nCount;

            if (m_bChunked && nCount != m_nOffset)
            {
               m_buf[m_nCount++] = '\r';
               m_buf[m_nCount++] = '\n';
               m_buf[m_nCount++] = '0';
               m_buf[m_nCount++] = '\r';
               m_buf[m_nCount++] = '\n';
            }

            if (m_bChunked || nCount != BUF_OFS)
            {
               flush(nCount);
            }

            m_bClosed = true;


         }
      }
   }

   /**
    * Input stream for parsing an HTTP response.
    */
   protected class HTTPInputStream extends MIMEInputStream
   {
      // attributes

      /**
       * Current stream byte count.
       */
      protected long m_lCount;

      /**
       * Maximum stream byte count.
       */
      protected long m_lMaxCount = -1;

      /**
       * The chunked transfer encoding flag.
       */
      protected boolean m_bChunked;

      /**
       * End-of-File flag.
       */
      protected boolean m_bEOF;

      // constructors

      /**
       * Constructs an HTTP input stream.
       * @param istream The socket input stream.
       */
      protected HTTPInputStream(InputStream istream)
      {
         super(istream, ENCODING);
      }

      // operations

      /**
       * @return The maximum stream byte count.
       */
      public long getMaxCount()
      {
         return m_lMaxCount;
      }

      /**
       * @return The chunked transfer encoding flag.
       */
      public boolean isChunked()
      {
         return m_bChunked;
      }

      /**
       * @see nexj.core.util.MIMEInputStream#read()
       */
      public int read() throws IOException
      {
         for (;;)
         {
            if (m_lCount < m_lMaxCount)
            {
               int ch = m_istream.read();

               if (ch < 0)
               {
                  throw new HTTPClientException("Premature end of stream");
               }

               ++m_lCount;

               return ch;
            }

            if (m_lMaxCount < 0)
            {
               return m_istream.read();
            }

            if (m_bChunked && !m_bEOF)
            {
               long lCount = 0;
               boolean bSize = false;
               int ch;

               if (m_lMaxCount != 0)
               {
                  ch = m_istream.read();

                  if (ch == CHAR_CR)
                  {
                     ch = m_istream.read();
                  }

                  if (ch != CHAR_LF)
                  {
                     throw new HTTPClientException("Invalid chunk delimiter");
                  }
               }

               for (;;)
               {
                  ch = m_istream.read();

                  if (ch < 0)
                  {
                     break;
                  }

                  int nDigit = Character.digit((char)ch, 16);

                  if (nDigit < 0 || (lCount & ~(~0L >>> 4)) != 0)
                  {
                     break;
                  }

                  lCount = (lCount << 4) | nDigit;
                  bSize = true;
               }

               if (!bSize || ch < 0 || !Character.isWhitespace((char)ch) && ch != ';')
               {
                  throw new HTTPClientException("Invalid chunk size");
               }

               while (ch != CHAR_LF)
               {
                  if (ch < 0)
                  {
                     throw new HTTPClientException("Invalid chunk");
                  }

                  ch = m_istream.read();
               }

               m_lCount = 0;

               if (lCount != 0)
               {
                  m_lMaxCount = lCount;

                  continue;
               }

               m_lMaxCount = -1;
               parseHeaders(m_responseHeaderMap);
               m_lMaxCount = 0;
               m_bEOF = true;
            }

            return -1;
         }
      }

      /**
       * @see java.io.InputStream#read(byte[], int, int)
       */
      public int read(byte[] buf, int nOfs, int nCount) throws IOException
      {
         if (nCount <= 0 || m_bEOF)
         {
            return -1;
         }

         if (m_lCount < m_lMaxCount)
         {
            int n = m_istream.read(buf, nOfs, (int)Math.min(nCount, m_lMaxCount - m_lCount));

            if (n <= 0)
            {
               throw new HTTPClientException("Premature end of stream");
            }

            m_lCount += n;

            return n;
         }

         if (m_lMaxCount < 0)
         {
            return m_istream.read(buf, nOfs, nCount);
         }

         if (m_bChunked)
         {
            int ch = read();

            if (ch >= 0)
            {
               buf[nOfs] = (byte)ch;

               return 1;
            }
         }

         return -1;
      }

      /**
       * Starts processing the response.
       * @return True if the response header has been processed, false if reconnection is required.
       */
      public boolean start() throws IOException
      {
         m_lCount = 0;
         m_lMaxCount = -1;
         m_responseHeaderMap.clear();

         if (!parseStatusLine())
         {
            return false;
         }

         parseHeaders(m_responseHeaderMap);

         if (m_nResponseStatus / 100 == 1 || m_nResponseStatus == HTTP.STATUS_NO_CONTENT ||
            m_nResponseStatus == HTTP.STATUS_NOT_MODIFIED || m_sMethod.equals(HTTP.METHOD_HEAD))
         {
            m_lMaxCount = 0;
         }
         else
         {
            MIMEHeader header = m_responseHeaderMap.find(HTTP.HEADER_TRANSFER_ENCODING);

            if (header != null && header.findValue("chunked") != null && header.getValueCount() == 1)
            {
               m_responseHeaderMap.remove(HTTP.HEADER_CONTENT_LENGTH);
               m_lMaxCount = 0;
               m_bChunked = true;
            }
            else
            {
               header = m_responseHeaderMap.find(HTTP.HEADER_CONTENT_LENGTH);

               if (header != null)
               {
                  try
                  {
                     m_lMaxCount = Long.parseLong(header.getValue());

                     if (m_lMaxCount < 0)
                     {
                        m_lMaxCount = 0;
                     }
                  }
                  catch (NumberFormatException e)
                  {
                     throw new HTTPClientException("Invalid content length", e);
                  }
               }
            }
         }

         m_lCount = 0;

         return true;
      }

      /**
       * Parses the HTTP status line.
       * @return True if the status line has been parsed, false to reconnect.
       */
      protected boolean parseStatusLine() throws IOException
      {
         StringBuffer buf = new StringBuffer(32);

         m_sResponseProtocol = null;
         m_sResponseMessage = "";
         m_nResponseStatus = 0;

         // Check if the response starts with "HTTP/"
         m_istream.mark(HTTP.HTTP_LENGTH);

         for (int i = 0; i < HTTP.HTTP_LENGTH; ++i)
         {
            int ch;

            if (i == 0)
            {
               try
               {
                  ch = m_istream.read();
               }
               catch (InterruptedIOException e)
               {
                  throw e;
               }
               catch (IOException e)
               {
                  return false;
               }
            }
            else
            {
               ch = m_istream.read();
            }

            if (ch < 0 || ch != HTTP.HTTP_1_1.charAt(i))
            {
               m_istream.reset();

               if (i == 0)
               {
                  return false;
               }

               buf.setLength(0);

               break;
            }

            buf.append((char)ch);
         }

         m_istream.mark(0);

         if (buf.length() == 0)
         {
            m_nResponseStatus = 200;
         }
         else
         {
            for (int ch; (ch = m_istream.read()) >= 0;)
            {
               if (ch == CHAR_LF)
               {
                  int n = buf.length();

                  if (n != 0 && buf.charAt(n - 1) == CHAR_CR)
                  {
                     buf.setLength(n - 1);
                  }

                  break;
               }

               buf.append((char)ch);
            }

            int i = HTTP.HTTP_LENGTH;
            int n = buf.length();

            while (i < n && buf.charAt(i) != ' ')
            {
               ++i;
            }

            m_sResponseProtocol = buf.substring(0, i);
            ++i;

            int k = i;

            while (i < n && buf.charAt(i) != ' ')
            {
               ++i;
            }

            if (k < i)
            {
               try
               {
                  m_nResponseStatus = Integer.parseInt(buf.substring(k, i));
               }
               catch (NumberFormatException e)
               {
                  throw new HTTPClientException("Invalid HTTP status code", e);
               }
            }
            else
            {
               throw new HTTPClientException("Missing HTTP status code");
            }

            ++i;

            if (i < n)
            {
               m_sResponseMessage = buf.substring(i);
            }
         }

         return true;
      }

      /**
       * @see nexj.core.util.MIMEInputStream#close()
       */
      public void close() throws IOException
      {
         while (read() >= 0) ;

         if (m_lMaxCount >= 0 && m_lCount != m_lMaxCount)
         {
            throw new HTTPClientException("Server has disconnected prematurely");
         }
      }
   }

   /**
    * HTTP connection pool.
    */
   public static abstract class HTTPConnectionPool extends GenericResourcePool
   {
      /**
       * @see nexj.core.util.pool.resource.GenericResourcePool#getLogger()
       */
      protected Logger getLogger()
      {
         return s_logger;
      }

      /**
       * @see nexj.core.util.pool.resource.GenericResourcePool#create(java.lang.Object)
       */
      protected Resource create(Object config) throws Exception
      {
         return new HTTPConnection((HTTPConfig)config);
      }
   }

   /**
    * Generic implementation of the connection pool.
    */
   public static class GenericHTTPConnectionPool extends HTTPConnectionPool
   {
      // attributes

      /**
       * The maximum connection count.
       */
      protected int m_nMaxSize = 256;

      /**
       * The busy timeout (ms).
       */
      protected long m_lBusyTimeout = 10000;

      /**
       * The idle timeout (ms).
       */
      protected long m_lIdleTimeout = 5 * 60 * 1000;

      // operations

      /**
       * Sets the maximum connection count.
       * @param nMaxSize The maximum connection count to set.
       */
      public void setMaxSize(int nMaxSize)
      {
         lock();

         try
         {
            m_nMaxSize = nMaxSize;
            wake();
         }
         finally
         {
            unlock();
         }
      }

      /**
       * @return The maximum connection count.
       * @see ResourcePool#getMaxSize()
       */
      public int getMaxSize()
      {
         lock();

         try
         {
            return m_nMaxSize;
         }
         finally
         {
            unlock();
         }
      }

      /**
       * Sets the busy timeout (ms).
       * @param lBusyTimeout The busy timeout (ms) to set.
       */
      public void setBusyTimeout(long lBusyTimeout)
      {
         lock();

         try
         {
            m_lBusyTimeout = lBusyTimeout;
            wake();
         }
         finally
         {
            unlock();
         }
      }

      /**
       * @return The busy timeout (ms).
       * @see ResourcePool#getBusyTimeout()
       */
      public long getBusyTimeout()
      {
         lock();

         try
         {
            return m_lBusyTimeout;
         }
         finally
         {
            unlock();
         }
      }

      /**
       * Sets the idle timeout (ms).
       * @param lIdleTimeout The idle timeout (ms) to set.
       */
      public void setIdleTimeout(long lIdleTimeout)
      {
         lock();

         try
         {
            m_lIdleTimeout = lIdleTimeout;
         }
         finally
         {
            unlock();
         }
      }

      /**
       * @return The idle timeout (ms).
       * @see ResourcePool#getIdleTimeout()
       */
      public long getIdleTimeout()
      {
         lock();

         try
         {
            return m_lIdleTimeout;
         }
         finally
         {
            unlock();
         }
      }
   }

   /**
    * Persistent connection.
    */
   public static class HTTPConnection extends GenericResource
   {
      // attributes

      /**
       * True if the connection is cached, false if new.
       */
      protected boolean m_bCached;

      // associations

      /**
       * The cached socket.
       */
      protected Socket m_socket;

      /**
       * The connection configuration.
       */
      protected HTTPConfig m_config;

      /**
       * The SSL socket factory.
       */
      protected static SSLSocketFactory s_sslSocketFactory;

      // constructors

      /**
       * Constructs the persistent connection.
       * @param config The connection configuration.
       */
      public HTTPConnection(final HTTPConfig config) throws IOException
      {
         m_config = config;

         try
         {
            m_socket = (Socket)AccessController.doPrivileged(
               new PrivilegedAction()
               {
                  public Object run()
                  {
                     Proxy proxy = config.getProxy();
   
                     return (proxy == null || config.isHTTPProxy()) ? new Socket() : new Socket(proxy);
                  }
               });
   
            m_socket.bind((SocketAddress)AccessController.doPrivileged(
               new PrivilegedAction()
               {
                  public Object run()
                  {
                     return new InetSocketAddress(0);
                  }
               }));
   
            m_socket.setKeepAlive(true);
            m_socket.setSoTimeout(m_config.getReadTimeout());
            m_socket.connect(m_config.getAddress(), m_config.getConnectionTimeout());
   
            if (m_config.isSSL() && !m_config.isHTTPProxy())
            {
               startSSL();
            }
         }
         catch (IOException e)
         {
            dispose();

            throw e;
         }
      }

      // operations

      /**
       * Starts an SSL connection.
       */
      public void startSSL() throws IOException
      {
         InetSocketAddress address = m_config.getSSLAddress();

         m_socket = getSocketFactory().createSocket(m_socket,
            address.getHostName(), address.getPort(), true);
      }

      /**
       * @return The cached socket.
       */
      public Socket getSocket()
      {
         return m_socket;
      }

      /**
       * @return True if the connection is cached, false if new.
       */
      public boolean isCached()
      {
         return m_bCached;
      }

      /**
       * @see nexj.core.util.pool.resource.Resource#reset()
       */
      public void reset()
      {
         m_bCached = true;
      }

      /**
       * Gets the connection factory for SSL sockets. Uses keystores if specified,
       * otherwise uses the default Java keystore.
       * @return The connection factory for SSL sockets.
       */
      protected SSLSocketFactory getSocketFactory()
      {
         if (m_config.isKeyStoreDefault())
         {
            synchronized (HTTPConnection.class)
            {
               if (s_sslSocketFactory == null)
               {
                  AccessController.doPrivileged(
                     new PrivilegedAction()
                     {
                        public Object run()
                        {
                           try
                           {
                              s_sslSocketFactory = ((HttpsURLConnection)new URL("https:///").openConnection()).getSSLSocketFactory();
                           }
                           catch (Exception e)
                           {
                              getLogger().debug("Unable to obtain the HTTPS URL connection socket factory; using the default", e);
                              s_sslSocketFactory = (SSLSocketFactory)SSLSocketFactory.getDefault();
                           }

                           return null;
                        }
                     });
               }

               return s_sslSocketFactory;
            }
         }

         return (SSLSocketFactory)AccessController.doPrivileged(
            new PrivilegedAction()
            {
               public Object run()
               {
                  try
                  {
                     SSLContext context = SSLContext.getInstance("TLS");

                     context.init(
                        m_config.getKeyManagers(),
                        m_config.getTrustManagers(),
                        RandUtil.getSecureRandom());

                     return context.getSocketFactory();
                  }
                  catch (GeneralSecurityException e)
                  {
                     throw ObjUtil.rethrow(e);
                  }
               }
            });
      }

      /**
       * @see nexj.core.util.pool.resource.GenericResource#getLogger()
       */
      protected Logger getLogger()
      {
         return s_logger;
      }

      /**
       * @see nexj.core.util.pool.resource.GenericResource#drop()
       */
      protected void drop() throws Throwable
      {
         Logger logger = getLogger();

         if (logger.isDebugEnabled())
         {
            logger.debug("Closing socket " + m_socket);
         }

         try
         {
            m_socket.close();
         }
         catch (Throwable t)
         {
            s_logger.debug("Socket closing error", t);
         }
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         StringBuffer buf = new StringBuffer(128);

         buf.append("HTTPConnection(socket=");
         buf.append(m_socket);

         if (m_lTime != 0)
         {
            buf.append(", time=");
            StringUtil.appendUTC(buf, new Timestamp(m_lTime), false);
         }

         buf.append(')');

         return buf.toString();
      }
   };

   /**
    * Persistent connection configuration.
    *
    * The connection pool is divided along these attributes:
    * 1) SSL / not SSL: These are different types of connections; one may not be exchanged for the other.
    * 2) address (next-hop): The address of the network resource being connected. One cannot exchange a
    *    connection on one address for a connection on another.
    * 3) trusted cert and client cert: To prevent an unauthenticated SSL connection from using the credentials
    *    of an already-authenticated SSL connection.
    * 4) proxy credentials: SPNEGO authentication to a proxy is sticky to the connection through the proxy; only
    *    clients using the same proxy credentials should be allowed to share pooled connections.
    * 5) endpointAddress: An SSL tunnel through a proxy, once established, may only be used for additional
    *    communications with that endpoint.
    */
   public static class HTTPConfig
   {
      // attributes

      /**
       * The client certificate password.
       */
      protected char[] m_achClientCertificatePassword;

      /**
       * The proxy password.
       */
      protected char[] m_achProxyPassword;

      /**
       * The connection timeout in milliseconds (0 means infinite).
       */
      protected int m_nConnectionTimeout;

      /**
       * The read timeout in milliseconds (0 means infinite).
       */
      protected int m_nReadTimeout;

      /**
       * The cached hash code.
       */
      protected int m_nHashCode;

      /**
       * The SSL flag.
       */
      protected boolean m_bSSL;

      // associations

      /**
       * The socket address.
       */
      protected InetSocketAddress m_address;

      /**
       * The address of the end point of the socket, if connection is a tunnel. For SSL tunnel, this
       * address is the address of the remote web server, and m_address is the proxy server address.
       */
      protected InetSocketAddress m_endpointAddress;

      /**
       * The trust manager array when using client certificate authentication.
       */
      protected Certificate m_trustedCertificate;

      /**
       * They key manager array when using client certificate authentication.
       */
      protected KeyStore m_clientCertificateStore;

      /**
       * The connection proxy.
       */
      protected Proxy m_proxy;

      /**
       * The proxy user.
       */
      protected Object m_proxyUser;

      // constructors

      /**
       * Constructs the connection configuration.
       * @param address The socket address.
       * @param nConnectionTimeout The connection timeout in milliseconds (0 means infinite).
       * @param nReadTimeout The read timeout in milliseconds (0 means infinite).
       * @param bSSL True if the connection is encrypted.
       * @param trustedCertificate The trusted certificate; null if not using client certificate authentication.
       * @param clientCertificateStore The client certificate key store; null if not using client certificate authentication.
       * @param achClientCertificatePassword The password protecting the private key in clientCertificateStore.
       * @param endpointAddress The address of the tunnel end point; null if not tunneled.
       * @param proxy The connection proxy. Can be null.
       * @param strategy The proxy authentication strategy. Can be null.
       */
      public HTTPConfig(InetSocketAddress address, int nConnectionTimeout, int nReadTimeout, boolean bSSL,
         Certificate trustedCertificate, KeyStore clientCertificateStore, char[] achClientCertificatePassword,
         InetSocketAddress endpointAddress, Proxy proxy, AuthenticationStrategy strategy)
      {
         m_address = address;
         m_nConnectionTimeout = nConnectionTimeout;
         m_nReadTimeout = nReadTimeout;
         m_bSSL = bSSL;
         m_trustedCertificate = trustedCertificate;
         m_clientCertificateStore = clientCertificateStore;
         m_achClientCertificatePassword = achClientCertificatePassword;
         m_endpointAddress = endpointAddress;
         m_proxy = proxy;

         if (strategy != null && isHTTPProxy())
         {
            if (strategy.getSPNEGO() == SPNEGO_SILENT)
            {
               m_proxyUser = Undefined.VALUE;
            }
            else
            {
               PasswordAuthenticationProvider provider = strategy.getProvider();

               if (provider != null)
               {
                  PasswordAuthentication auth = provider.getPasswordAuthentication();

                  if (auth != null)
                  {
                     m_proxyUser = auth.getUserName();
                     m_achProxyPassword = auth.getPassword();
                  }
               }
            }
         }
      }

      // operations

      /**
       * @return The server or the proxy address.
       */
      public InetSocketAddress getAddress()
      {
         return m_address;
      }

      /**
       * @return The connection timeout in milliseconds (0 means infinite).
       */
      public int getConnectionTimeout()
      {
         return m_nConnectionTimeout;
      }

      /**
       * @return The read timeout in milliseconds (0 means infinite).
       */
      public int getReadTimeout()
      {
         return m_nReadTimeout;
      }

      /**
       * @return The proxy. Can be null.
       */
      public Proxy getProxy()
      {
         return m_proxy;
      }

      /**
       * @return True if the proxy is an HTTP proxy.
       */
      public boolean isHTTPProxy()
      {
         return m_proxy != null && m_proxy.type() == Proxy.Type.HTTP;
      }

      /**
       * @return True if the connection is encrypted.
       */
      public boolean isSSL()
      {
         return m_bSSL;
      }

      /**
       * @return True if the default key stores are used.
       */
      public boolean isKeyStoreDefault()
      {
         return m_clientCertificateStore == null && m_trustedCertificate == null;
      }

      /**
       * @return The SSL server address.
       */
      public InetSocketAddress getSSLAddress()
      {
         return (m_endpointAddress != null && isHTTPProxy()) ? m_endpointAddress : m_address;
      }

      /**
       * @return The key managers for the client certificate store. 
       */
      public KeyManager[] getKeyManagers()
      {
         return CertificateUtil.getKeyManagers(m_clientCertificateStore, m_achClientCertificatePassword);
      }

      /**
       * @return The trust managers for the client certificate store.
       */
      public TrustManager[] getTrustManagers()
      {
         return CertificateUtil.getTrustManagers(m_trustedCertificate);
      }

      /**
       * @see java.lang.Object#equals(java.lang.Object)
       */
      public boolean equals(Object obj)
      {
         if (!(obj instanceof HTTPConfig))
         {
            return false;
         }

         HTTPConfig config = (HTTPConfig)obj;

         boolean bEqual = m_bSSL == config.m_bSSL &&
            m_address.equals(config.m_address) &&
            m_trustedCertificate == config.m_trustedCertificate &&
            m_clientCertificateStore == config.m_clientCertificateStore &&
            ObjUtil.equal(m_endpointAddress, config.m_endpointAddress) &&
            ObjUtil.equal(m_proxy, config.m_proxy) &&
            m_nConnectionTimeout == config.m_nConnectionTimeout &&
            m_nReadTimeout == config.m_nReadTimeout;

         if (bEqual && m_proxyUser != null)
         {
            bEqual &= m_proxyUser.equals(config.m_proxyUser) && Arrays.equals(m_achProxyPassword, config.m_achProxyPassword);
         }

         return bEqual;
      }

      /**
       * @see java.lang.Object#hashCode()
       */
      public int hashCode()
      {
         if (m_nHashCode != 0)
         {
            return m_nHashCode;
         }

         int nHashCode = m_address.hashCode() ^ ((m_bSSL) ? 33 : 0);

         if (m_clientCertificateStore != null)
         {
            nHashCode ^= m_clientCertificateStore.hashCode();
         }

         if (m_trustedCertificate != null)
         {
            nHashCode ^= m_trustedCertificate.hashCode();
         }

         if (m_endpointAddress != null)
         {
            nHashCode ^= m_endpointAddress.hashCode();
         }

         if (m_proxy != null)
         {
            nHashCode ^= m_proxy.hashCode();
         }

         if (m_proxyUser != null)
         {
            nHashCode ^= m_proxyUser.hashCode();
            nHashCode ^= Arrays.hashCode(m_achProxyPassword);
         }

         nHashCode ^= m_nConnectionTimeout;
         nHashCode ^= m_nReadTimeout;

         return (m_nHashCode = (nHashCode == 0) ? 1 : nHashCode);
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         StringBuilder buf = new StringBuilder(128);

         buf.append((m_bSSL) ? "https://" : "http://");
         buf.append(m_address);
         buf.append(m_trustedCertificate);
         buf.append(m_clientCertificateStore);

         return buf.toString();
      }
   }

   /**
    * Represents a cookie.
    */
   protected static class Cookie
   {
      // attributes

      /**
       * The cookie name.
       */
      protected String m_sName;

      /**
       * The cookie value.
       */
      protected String m_sValue;

      /**
       * The cookie domain.
       */
      protected String m_sDomain;

      /**
       * The cookie path.
       */
      protected String m_sPath;

      /**
       * The expiration timestamp.
       */
      protected long m_lExpiration;

      /**
       * The secure flag.
       */
      protected boolean m_bSecure;

      // associations

      /**
       * The previous cookie in a circular list.
       */
      private Cookie m_prev;

      /**
       * The next cookie in a circular list.
       */
      private Cookie m_next;

      // constructors

      /**
       * Constructs a cookie.
       * @param sName The cookie name.
       * @param sValue The cookie value.
       */
      public Cookie(String sName, String sValue)
      {
         m_sName = sName;
         m_sValue = sValue;
         m_prev = m_next = this;
      }

      // operations

      /**
       * @return The cookie name.
       */
      public String getName()
      {
         return m_sName;
      }

      /**
       * @return The cookie value.
       */
      public String getValue()
      {
         return m_sValue;
      }

      /**
       * Sets the cookie domain.
       * @param sDomain The cookie domain to set.
       */
      public void setDomain(String sDomain)
      {
         m_sDomain = sDomain;
      }

      /**
       * @return The cookie domain.
       */
      public String getDomain()
      {
         return m_sDomain;
      }

      /**
       * Sets the cookie path.
       * @param sPath The cookie path to set.
       */
      public void setPath(String sPath)
      {
         m_sPath = sPath;
      }

      /**
       * @return The cookie path.
       */
      public String getPath()
      {
         return m_sPath;
      }

      /**
       * Sets the expiration timestamp.
       * @param lExpiration The expiration timestamp to set.
       */
      public void setExpiration(long lExpiration)
      {
         m_lExpiration = lExpiration;
      }

      /**
       * @return The expiration timestamp.
       */
      public long getExpiration()
      {
         return m_lExpiration;
      }

      /**
       * Sets the secure flag.
       * @param bSecure The secure flag to set.
       */
      public void setSecure(boolean bSecure)
      {
         m_bSecure = bSecure;
      }

      /**
       * @return The secure flag.
       */
      public boolean isSecure()
      {
         return m_bSecure;
      }

      /**
       * @return The next cookie.
       */
      public Cookie getNext()
      {
         return m_next;
      }

      /**
       * Adds a cookie to the circular list.
       * @param cookie The cookie to add.
       */
      public void add(Cookie cookie)
      {
         cookie.m_next = this;
         cookie.m_prev = m_prev;
         m_prev.m_next = cookie;
         m_prev = cookie;
      }

      /**
       * Removes itself from the circular list.
       * @return True if the list is empty after the operation.
       */
      public boolean remove()
      {
         m_next.m_prev = m_prev;
         m_prev.m_next = m_next;

         return m_next == this;
      }

      /**
       * Finds a cookie that is equal to a given cookie.
       * @param cookie The cookie to look for.
       * @return The found cookie, or null if not found.
       */
      public Cookie find(Cookie cookie)
      {
         Cookie next = this;

         do
         {
            if (cookie.equals(next))
            {
               return next;
            }

            next = next.getNext();
         }
         while (next != this);

         return null;
      }

      /**
       * Checks if a cookie matches a URI.
       * @param sHost The host name (overrides the URI host).
       * @param uri The URI to match.
       */
      public boolean matches(String sHost, URI uri)
      {
         if (m_bSecure && !uri.getScheme().equalsIgnoreCase(HTTP.SCHEME_SSL))
         {
            return false;
         }

         if (m_sDomain.length() > sHost.length())
         {
            return false;
         }

         int i = sHost.length() - m_sDomain.length();

         if (!sHost.regionMatches(true, i, m_sDomain, 0, m_sDomain.length()) ||
            i > 0 && m_sDomain.charAt(0) != '.')
         {
            return false;
         }

         String sPath = uri.getRawPath();

         if (!sPath.startsWith(m_sPath) ||
            !m_sPath.equals("/") &&  sPath.length() > m_sPath.length() && sPath.charAt(m_sPath.length()) != '/')
         {
            return false;
         }

         return true;
      }

      /**
       * Gets the domain key for a given host name.
       * @param sHost The host name.
       * @return The domain key.
       */
      public static String getDomainKey(String sHost)
      {
         sHost = sHost.toLowerCase(Locale.ENGLISH);

         int i = sHost.lastIndexOf('.');

         if (i > 0)
         {
            boolean bTop = s_topDomainSet.contains(sHost.substring(i + 1));

            i = sHost.lastIndexOf('.', i - 1);

            if (i > 0 && !bTop)
            {
               i = sHost.lastIndexOf('.', i - 1);
            }

            if (i > 0)
            {
               return sHost.substring(i);
            }
         }

         return sHost;
      }

      /**
       * @see java.lang.Object#equals(java.lang.Object)
       */
      public boolean equals(Object obj)
      {
         if (!(obj instanceof Cookie))
         {
            return false;
         }

         Cookie cookie = (Cookie)obj;

         return m_sName.equalsIgnoreCase(cookie.getName()) &&
            m_sDomain.equals(cookie.getDomain()) &&
            m_sPath.equals(cookie.getPath());
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         StringBuilder buf = new StringBuilder(64);

         buf.append(m_sName);
         buf.append('=');
         buf.append(m_sValue);
         buf.append("; domain=");
         buf.append(m_sDomain);
         buf.append("; path=");
         buf.append(m_sPath);

         if (m_lExpiration != Long.MIN_VALUE)
         {
            buf.append("; expires=");
            buf.append(HTTP.formatCookieDateTime(new Timestamp(m_lExpiration)));
         }

         if (m_bSecure)
         {
            buf.append("; secure");
         }

         return buf.toString();
      }
   }

   /**
    * Used for password authentication to a resource.
    */
   protected class AuthenticationStrategy
   {
      /**
       * The current user name.
       */
      protected String m_sUser;

      /**
       * The previous user name.
       */
      protected String m_sUserSaved;

      /**
       * The current user password.
       */
      protected char[] m_achPassword;

      /**
       * The previous user password.
       */
      protected char[] m_achPasswordSaved;

      /**
       * The SPNEGO protocol mode (one of the SPNEGO_* constants).
       */
      protected int m_nSPNEGO;

      /**
       * The Kerberos service name: HTTP/host.domain.ext.
       */
      protected String m_sServiceName;

      /**
       * True if the strategy is for proxy authentication.
       */
      protected boolean m_bProxy;

      /**
       * True, if authentication is proactive.
       */
       protected boolean m_bAuthProactive;

      // associations

      /**
       * The password provider.
       */
      protected PasswordAuthenticationProvider m_provider;

      // constructors

      /**
       * Constructs the strategy.
       * @param bProxy True if the strategy is for proxy authentication.
       */
      public AuthenticationStrategy(boolean bProxy)
      {
         m_bProxy = bProxy;
      }

      // operations

      /**
       * Sets the current user.
       *
       * @param sUser The user to set.
       */
      public void setUser(String sUser)
      {
         m_sUser = sUser;
      }

      /**
       * Gets the current user.
       *
       * @return The current user.
       */
      public String getUser()
      {
         return m_sUser;
      }

      /**
       * Gets the saved user.
       *
       * @return The saved user.
       */
      public String getUserSaved()
      {
         return m_sUserSaved;
      }

      /**
       * Sets the current password. Copies the argument.
       *
       * @param achPassword The password to set.
       */
      public void setPassword(char[] achPassword)
      {
         if (m_achPassword != null)
         {
            Arrays.fill(m_achPassword, ' ');
         }

         m_achPassword = (achPassword == null) ? null : (char[])achPassword.clone();
      }

      /**
       * Gets the current password.
       *
       * @return The current password.
       */
      public char[] getPassword()
      {
         return m_achPassword;
      }

      /**
       * Sets the password provider.
       *
       * @param provider The password provider to set.
       */
      public void setProvider(PasswordAuthenticationProvider provider)
      {
         m_provider = provider;
      }

      /**
       * Gets the password provider.
       *
       * @return The password provider.
       */
      public PasswordAuthenticationProvider getProvider()
      {
         return m_provider;
      }

      /**
       * Securely clears the current user/password.
       */
      public void clearUserPassword()
      {
         m_sUser = null;

         if (m_achPassword != null)
         {
            Arrays.fill(m_achPassword, ' ');
            m_achPassword = null;
         }
      }

      /**
       * Securely clears the saved user/password.
       */
      public void clearUserPasswordSaved()
      {
         m_sUserSaved = null;

         if (m_achPasswordSaved != null)
         {
            Arrays.fill(m_achPasswordSaved, ' ');
            m_achPasswordSaved = null;
         }
      }

      /**
       * Copies the saved user/password to the current user/password.
       */
      public void copyFromSaved()
      {
         m_sUser = m_sUserSaved;

         if (m_achPassword != null)
         {
            Arrays.fill(m_achPassword, ' ');
         }

         m_achPassword = (m_achPasswordSaved == null) ? null : (char [])m_achPasswordSaved.clone();
      }

      /**
       * Copies the current user/password to the saved user/password.
       */
      public void copyToSaved()
      {
         m_sUserSaved = m_sUser;

         if (m_achPasswordSaved != null)
         {
            Arrays.fill(m_achPasswordSaved, ' ');
         }

         m_achPasswordSaved = (m_achPassword == null) ? null : (char [])m_achPassword.clone();
      }

      /**
       * Determines whether the current user and password match the saved user and password.
       *
       * @return True if the current user and password match the saved user and password; false otherwise.
       */
      public boolean isCurrentMatchedBySaved()
      {
         return m_sUser == m_sUserSaved && Arrays.equals(m_achPassword, m_achPasswordSaved);
      }

      /**
       * Gets the SPNEGO protocol mode.
       *
       * @return One of the SPNEGO_* constants.
       */
      public int getSPNEGO()
      {
         return m_nSPNEGO;
      }

      /**
       * Sets the SPNEGO protocol mode.
       *
       * @param nSPNEGO One of the SPNEGO_* constants.
       */
      public void setSPNEGO(int nSPNEGO)
      {
         if (nSPNEGO != SPNEGO_SILENT && nSPNEGO != SPNEGO_CRED && nSPNEGO != SPNEGO_NONE)
         {
            throw new IllegalArgumentException();
         }

         m_nSPNEGO = nSPNEGO;
      }

      /**
       * Gets flag indicating that this strategy is for authenticating to proxies.
       *
       * @return True if this strategy authenticates to proxies; false otherwise.
       */
      public boolean isProxy()
      {
         return m_bProxy;
      }

      /**
       * Gets the Kerberos service principal name.
       *
       * @return The kerberos service name for the current URL.
       */
      public String getServiceName()
      {
         if (m_sServiceName == null)
         {
            InetSocketAddress address = (isProxy()) ?
               resolve((InetSocketAddress)m_proxy.address()) :
               (InetSocketAddress)m_serverAddress;

            m_sServiceName = "HTTP/" + address.getAddress().getCanonicalHostName();
         }

         return m_sServiceName;
      }

      /**
       * Clears the Kerberos service principal name, forcing it to be recomputed on the next
       * authentication attempt.
       */
      public void clearServiceName()
      {
         m_sServiceName = null;
      }

      /**
       * Parses the response headers for the authentication protocol and
       * sets the authentication information in m_sToken.
       * @return The authentication protocol, one of the AUTH_* constants.
       */
      protected int parseAuthProtocol() throws IOException
      {
         MIMEHeader header = m_responseHeaderMap.find(isProxy() ? HTTP.PROXY_AUTH_RESPONSE_HEADER : HTTP.AUTH_RESPONSE_HEADER);
         int nProtocol = AUTH_NONE;

         if (header != null)
         {
            for (int i = 0; i < header.getValueCount(); ++i)
            {
               String s = header.getValue(i).getName();

               if (nProtocol == AUTH_NONE && s.regionMatches(true, 0, HTTP.BASIC_PROTOCOL, 0, HTTP.BASIC_LENGTH))
               {
                   if (s.length() == HTTP.BASIC_LENGTH)
                   {
                      nProtocol = AUTH_BASIC;
                      m_sToken = null;
                   }
                   else if (s.charAt(HTTP.BASIC_LENGTH) == ' ')
                   {
                      nProtocol = AUTH_BASIC;
                      m_sToken = s.substring(HTTP.BASIC_LENGTH + 1);
                   }
               }
               else if (m_nSPNEGO != SPNEGO_NONE && s.regionMatches(true, 0, HTTP.SPNEGO_PROTOCOL, 0, HTTP.SPNEGO_LENGTH))
               {
                  /*
                   * Verify that the proxy supports connection pinning.
                   * (See RFC4559: http://www.ietf.org/rfc/rfc4559.txt)
                   */
                  if (!isProxy() && isHTTPProxy() && !HTTP.SCHEME_SSL.equalsIgnoreCase(m_uri.getScheme()))
                  {
                     MIMEHeader supportHeader = m_responseHeaderMap.find(HTTP.PROXY_SUPPORT_RESPONSE_HEADER);

                     if (supportHeader == null || !supportHeader.getValue().equals(HTTP.PROXY_SUPPORT_SESSION_BASED_AUTH))
                     {
                        continue;
                     }
                  }

                  if (s.length() == HTTP.SPNEGO_LENGTH)
                  {
                     nProtocol = AUTH_SPNEGO;
                     m_sToken = null;

                     break;
                  }

                  if (s.charAt(HTTP.SPNEGO_LENGTH) == ' ')
                  {
                     nProtocol = AUTH_SPNEGO;
                     m_sToken = s.substring(HTTP.SPNEGO_LENGTH + 1);

                     break;
                  }
               }
            }
         }

         return nProtocol;
      }

      /**
       * Sets proactive authentication.
       * @param bAuthProactive The proactive authenticaion.
       */
     public void setAuthProactive(boolean bAuthProactive)
     {
        m_bAuthProactive = bAuthProactive;
     }

     /**
      * @return True, if authentication is proactive.
      */
     public boolean isAuthProactive()
     {
        return m_bAuthProactive;
     }
   }
}
