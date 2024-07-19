// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.channel.http;

import java.net.MalformedURLException;
import java.net.Proxy;
import java.net.URL;
import java.security.KeyStore;
import java.security.cert.Certificate;

import nexj.core.meta.MetadataException;
import nexj.core.meta.Primitive;
import nexj.core.meta.PrimitivePrivilege;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.ChannelType;
import nexj.core.scripting.Compiler;
import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.CertificateUtil;

/**
 * HTTP channel.
 */
public class HTTPChannel extends Channel
{
   // constants

   /**
    * Use anonymous login.
    */
   public final static byte AUTH_NONE = 0;

   /**
    * Use basic authentication.
    */
   public final static byte AUTH_BASIC = 1;

   /**
    * Use proactive authentication.
    */
   public final static byte AUTH_PROACTIVE = 2;

   /**
    * Use HTTP channel credentials for SPNEGO/SSO.
    */
   public final static byte AUTH_CRED = 3;

   /**
    * Use the server process token for SPNEGO/SSO.
    */
   public final static byte AUTH_SERVER = 4;

   /**
    * Propagate the authenticated user token for SSO.
    */
   public final static byte AUTH_CLIENT = 5;

   /**
    * Client certificate authentication.
    */
   public final static byte AUTH_CERT = 6;

   // attributes

   /**
    * The maximum request size.
    */
   protected long m_lMaxRequestSize;

   /**
    * The secure transport flag.
    */
   protected boolean m_bSecure;

   /**
    * The DELETE method implementation flag.
    */
   protected boolean m_bDeleteImplemented;

   /**
    * The GET method implementation flag.
    */
   protected boolean m_bGetImplemented;

   /**
    * The HEAD method implementation flag.
    */
   protected boolean m_bHeadImplemented;

   /**
    * The OPTIONS method implementation flag.
    */
   protected boolean m_bOptionsImplemented;

   /**
    * The POST method implementation flag.
    */
   protected boolean m_bPostImplemented = true;

   /**
    * The PUT method implementation flag..
    */
   protected boolean m_bPutImplemented;

   /**
    * The TRACE method implementation flag.
    */
   protected boolean m_bTraceImplemented;

   /**
    * The client authentication mode, one of the AUTH_* constants.
    */
   protected byte m_nAuthMode = AUTH_BASIC;

   /**
    * The client proxy authentication mode, one of the AUTH_* constants.
    */
   protected byte m_nProxyAuthMode = AUTH_BASIC;

   /**
    * The user name.
    */
   protected String m_sUser;

   /**
    * The password.
    */
   protected String m_sPassword;

   /**
    * The HTTP server URL.
    */
   protected String m_sURL;

   /**
    * The default content type.
    */
   protected String m_sContentType;

   /**
    * The User-Agent header.
    */
   protected String m_sAgent;

   /**
    * The user name for authenticating to a proxy.
    */
   protected String m_sProxyUser;

   /**
    * The password for authenticating to a proxy.
    */
   protected String m_sProxyPassword;

   // associations

   /**
    * The privilege for HTTP server authorization.
    */
   protected PrimitivePrivilege m_privilege;

   /**
    * The boolean expression identifying an error.
    * The raw HTTP message is passed as this.
    */
   protected Object m_errorExpression;

   /**
    * The error function.
    */
   protected Function m_errorFunction;

   /**
    * The public certificate of a trusted remote party.
    */
   protected Certificate m_trust;

   /**
    * The certificate and private key of this, to use for outgoing
    * connections (used for logging in to the remote system).
    */
   protected KeyStore m_certificate;

   /**
    * The proxy server to use for outgoing connections.
    */
   protected Proxy m_proxy;

   /**
    * The body data type. Null means that is it determined based on HTTP headers.
    */
   protected Primitive m_dataType;

   /**
    * The read timeout in milliseconds.
    */
   protected int m_nReadTimeout;

   /**
    * The connection timeout in milliseconds.
    */
   protected int m_nConnectionTimeout = 60000;

   // constructors

   /**
    * @see nexj.core.meta.integration.Channel#setType(nexj.core.meta.integration.ChannelType)
    */
   public void setType(ChannelType type)
   {
      super.setType(type);

      if (type != null)
      {
         m_bSecure = type.getMetadata().isSecureTransport();
      }
   }

   /**
    * Constructs the channel.
    * @param sName The channel name.
    */
   public HTTPChannel(String sName)
   {
      super(sName);
   }

   // operations

   /**
    * Sets the maximum request size.
    * @param lMaxRequestSize The maximum request size to set.
    */
   public void setMaxRequestSize(long lMaxRequestSize)
   {
      verifyNotReadOnly();
      m_lMaxRequestSize = lMaxRequestSize;
   }

   /**
    * @return The maximum request size.
    */
   public long getMaxRequestSize()
   {
      return m_lMaxRequestSize;
   }

   /**
    * @see nexj.core.meta.integration.Channel#isTransactional()
    */
   public boolean isTransactional()
   {
      return false;
   }

   /**
    * @see nexj.core.meta.integration.Channel#isSynchronous()
    */
   public boolean isSynchronous()
   {
      return true;
   }

   /**
    * Sets the secure transport flag.
    * @param bSecure The secure transport flag to set.
    */
   public void setSecure(boolean bSecure)
   {
      verifyNotReadOnly();
      m_bSecure = bSecure;
   }

   /**
    * @return The secure transport flag.
    */
   public boolean isSecure()
   {
      return m_bSecure;
   }

   /**
    * Sets the DELETE method implementation flag.
    * @param bDeleteImplemented The DELETE method implementation flag to set.
    */
   public void setDeleteImplemented(boolean bDeleteImplemented)
   {
      verifyNotReadOnly();
      m_bDeleteImplemented = bDeleteImplemented;
   }

   /**
    * @return The DELETE method implementation flag.
    */
   public boolean isDeleteImplemented()
   {
      return m_bDeleteImplemented;
   }

   /**
    * Sets the GET method implementation flag.
    * @param bGetImplemented The GET method implementation flag to set.
    */
   public void setGetImplemented(boolean bGetImplemented)
   {
      verifyNotReadOnly();
      m_bGetImplemented = bGetImplemented;
   }

   /**
    * @return The GET method implementation flag.
    */
   public boolean isGetImplemented()
   {
      return m_bGetImplemented;
   }

   /**
    * Sets the HEAD method implementation flag.
    * @param bHeadImplemented The HEAD method implementation flag to set.
    */
   public void setHeadImplemented(boolean bHeadImplemented)
   {
      verifyNotReadOnly();
      m_bHeadImplemented = bHeadImplemented;
   }

   /**
    * @return The HEAD method implementation flag.
    */
   public boolean isHeadImplemented()
   {
      return m_bHeadImplemented;
   }

   /**
    * Sets the OPTIONS method implementation flag.
    * @param bOptionsImplemented The OPTIONS method implementation flag to set.
    */
   public void setOptionsImplemented(boolean bOptionsImplemented)
   {
      verifyNotReadOnly();
      m_bOptionsImplemented = bOptionsImplemented;
   }

   /**
    * @return The OPTIONS method implementation flag.
    */
   public boolean isOptionsImplemented()
   {
      return m_bOptionsImplemented;
   }

   /**
    * Sets the POST method implementation flag.
    * @param bPostImplemented The POST method implementation flag to set.
    */
   public void setPostImplemented(boolean bPostImplemented)
   {
      verifyNotReadOnly();
      m_bPostImplemented = bPostImplemented;
   }

   /**
    * @return The POST method implementation flag.
    */
   public boolean isPostImplemented()
   {
      return m_bPostImplemented;
   }

   /**
    * Sets the PUT method implementation flag..
    * @param bPutImplemented The PUT method implementation flag. to set.
    */
   public void setPutImplemented(boolean bPutImplemented)
   {
      verifyNotReadOnly();
      m_bPutImplemented = bPutImplemented;
   }

   /**
    * @return The PUT method implementation flag..
    */
   public boolean isPutImplemented()
   {
      return m_bPutImplemented;
   }

   /**
    * Sets the TRACE method implementation flag.
    * @param bTraceImplemented The TRACE method implementation flag to set.
    */
   public void setTraceImplemented(boolean bTraceImplemented)
   {
      verifyNotReadOnly();
      m_bTraceImplemented = bTraceImplemented;
   }

   /**
    * @return The TRACE method implementation flag.
    */
   public boolean isTraceImplemented()
   {
      return m_bTraceImplemented;
   }

   /**
    * Sets the privilege for HTTP server authorization.
    * @param privilege The privilege for HTTP server authorization to set.
    */
   public void setPrivilege(PrimitivePrivilege privilege)
   {
      verifyNotReadOnly();
      m_privilege = privilege;
   }

   /**
    * @return The privilege for HTTP server authorization.
    */
   public PrimitivePrivilege getPrivilege()
   {
      return m_privilege;
   }

   /**
    * Sets the client authentication mode, one of the AUTH_* constants.
    * @param nAuthMode The authentication mode, one of the AUTH_* constants to set.
    */
   public void setAuthMode(byte nAuthMode)
   {
      verifyNotReadOnly();
      m_nAuthMode = nAuthMode;
   }

   /**
    * @return The client authentication mode, one of the AUTH_* constants.
    */
   public byte getAuthMode()
   {
      return m_nAuthMode;
   }

   /**
    * Sets the user name.
    * @param sUser The user name to set.
    */
   public void setUser(String sUser)
   {
      verifyNotReadOnly();
      m_sUser = sUser;
   }

   /**
    * @return The user name.
    */
   public String getUser()
   {
      return m_sUser;
   }

   /**
    * Sets the password.
    * @param sPassword The password to set.
    */
   public void setPassword(String sPassword)
   {
      verifyNotReadOnly();
      m_sPassword = sPassword;
   }

   /**
    * @return The password.
    */
   public String getPassword()
   {
      return m_sPassword;
   }

   /**
    * Sets the read timeout in milliseconds.
    * @param nReadTimeout The read timeout in milliseconds to set.
    */
   public void setReadTimeout(int nReadTimeout)
   {
      verifyNotReadOnly();
      m_nReadTimeout = nReadTimeout;
   }

   /**
    * Sets the connection timeout in milliseconds.
    * @param nConnectionTimeout The connection timeout in milliseconds to set.
    */
   public void setConnectionTimeout(int nConnectionTimeout)
   {
      verifyNotReadOnly();
      m_nConnectionTimeout = nConnectionTimeout;
   }

   /**
    * @return The read timeout in milliseconds.
    */
   public int getReadTimeout()
   {
      return m_nReadTimeout;
   }

   /**
    * @return The read timeout in milliseconds.
    */
   public int getConnectionTimeout()
   {
      return m_nConnectionTimeout;
   }

   /**
    * Sets the HTTP server URL.
    * @param sURL The HTTP server URL to set.
    * @throws MetadataException if a malformed URL is set.
    */
   public void setURL(String sURL) throws MetadataException
   {
      verifyNotReadOnly();
      m_sURL = sURL;

      if (sURL != null)
      {
         try
         {
            new URL(sURL);
         }
         catch (MalformedURLException e)
         {
            throw new MetadataException("err.meta.integration.url", new Object[]{sURL, getName()});
         }
      }
   }

   /**
    * @return The HTTP server URL.
    */
   public String getURL()
   {
      return m_sURL;
   }

   /**
    * Sets the default content type.
    * @param sContentType The default content type to set.
    */
   public void setContentType(String sContentType)
   {
      verifyNotReadOnly();
      m_sContentType = sContentType;
   }

   /**
    * @return The default content type.
    */
   public String getContentType()
   {
      return m_sContentType;
   }

   /**
    * Sets the User-Agent header.
    * @param sAgent The User-Agent header to set.
    */
   public void setAgent(String sAgent)
   {
      verifyNotReadOnly();
      m_sAgent = sAgent;
   }

   /**
    * @return The User-Agent header.
    */
   public String getAgent()
   {
      return m_sAgent;
   }

   /**
    * Sets the remote machine public certificate to trust when establishing an SSL connection.
    *
    * @param sTrust The remote machine's X.509 certificate, base64-encoded.
    */
   public void setTrustedCertificate(String sTrust)
   {
      verifyNotReadOnly();

      try
      {
         m_trust = CertificateUtil.parseCertificate(sTrust);
      }
      catch (Exception ex)
      {
         throw new MetadataException("err.meta.integration.http.invalidTrustCertificate",
            new Object[] {getName(), sTrust}, ex);
      }
   }

   /**
    * Sets the remote machine public certificate to trust when establishing an SSL connection.
    *
    * @param trustedCertificate The remote machine's X.509 certificate; null to trust certificates
    * in the default trust store.
    */
   public void setTrustedCertificate(Certificate trustedCertificate)
   {
      verifyNotReadOnly();
      m_trust = trustedCertificate;
   }

   /**
    * Gets the remote machine public certificate to trust when establishing an SSL connection.
    *
    * @return The certificate that will be trusted; null to trust certificates in the default
    * trust store.
    */
   public Certificate getTrustedCertificate()
   {
      return m_trust;
   }

   /**
    * Sets the certificate and private key to use to identify this machine when
    * establishing an outbound connection using client certificate authentication.
    *
    * @param clientCertificate The KeyStore containing the public certificate and
    *                          private key that identify this machine.
    */
   public void setClientCertificate(KeyStore clientCertificate)
   {
      verifyNotReadOnly();
      m_certificate = clientCertificate;
   }

   /**
    * Gets the certificate and private key to use to identify this machine when
    * establishing an outbound connection using client certificate authentication.
    *
    * @return The certificate and private key, bound together in a KeyStore.
    */
   public KeyStore getClientCertificate()
   {
      return m_certificate;
   }

   /**
    * Sets and compiles the error expression returning non-#f
    * if an HTTP message should be treated as an error.
    * @param expr The error expression. Can be null.
    * @param machine The VM for compilation.
    */
   public void setErrorExpression(Object expr, Machine machine)
   {
      m_errorExpression = expr;

      if (expr == null)
      {
         m_errorFunction = null;
      }
      else
      {
         m_errorFunction = new Compiler().compile(
            Pair.list(Symbol.LAMBDA, Pair.list(Symbol.THIS), expr),
            null, "channel:" + m_sName, machine, false);
      }
   }

   /**
    * @return The error expression. Can be null.
    */
   public Object getErrorExpression()
   {
      return m_errorExpression;
   }

   /**
    * @return The error function. Can be null.
    */
   public Function getErrorFunction()
   {
      return m_errorFunction;
   }

   /**
    * Sets the proxy server to use for outgoing connections.
    *
    * @param proxy The proxy server to use for outgoing connections; null to use JRE default.
    */
   public void setProxy(Proxy proxy)
   {
      verifyNotReadOnly();
      m_proxy = proxy;
   }

   /**
    * Gets the proxy server to use for outgoing connections.
    *
    * @return The proxy server to use for outgoing connections; null to use JRE default.
    */
   public Proxy getProxy()
   {
      return m_proxy;
   }

   /**
    * Sets the client proxy authentication mode, one of the AUTH_* constants.
    * @param nAuthMode The proxy authentication mode, one of the AUTH_* constants to set.
    */
   public void setProxyAuthMode(byte nAuthMode)
   {
      verifyNotReadOnly();
      m_nProxyAuthMode = nAuthMode;
   }

   /**
    * @return The client proxy authentication mode, one of the AUTH_* constants.
    */
   public byte getProxyAuthMode()
   {
      return m_nProxyAuthMode;
   }

   /**
    * Sets the user name for authenticating to a proxy.
    * @param sUser The user name to set.
    */
   public void setProxyUser(String sUser)
   {
      verifyNotReadOnly();
      m_sProxyUser = sUser;
   }

   /**
    * Gets the user name for authenticating to a proxy.
    * @return The user name.
    */
   public String getProxyUser()
   {
      return m_sProxyUser;
   }

   /**
    * Sets the password for authenticating to a proxy.
    * @param sPassword The password to set.
    */
   public void setProxyPassword(String sPassword)
   {
      verifyNotReadOnly();
      m_sProxyPassword = sPassword;
   }

   /**
    * Gets the password for authenticating to a proxy.
    * @return The password.
    */
   public String getProxyPassword()
   {
      return m_sProxyPassword;
   }

   /**
    * Sets the body data type.
    * @param dataType The data type to set. Can be null (auto).
    */
   public void setDataType(Primitive dataType)
   {
      verifyNotReadOnly();
      m_dataType = dataType;
   }

   /**
    * @return The body data type. Can be null (auto).
    */
   public Primitive getDataType()
   {
      return m_dataType;
   }
}
