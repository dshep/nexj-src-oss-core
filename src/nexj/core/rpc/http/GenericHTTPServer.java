// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.http;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.Reader;
import java.security.Principal;
import java.security.cert.Certificate;
import java.security.cert.X509Certificate;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import nexj.core.meta.Metadata;
import nexj.core.meta.Repository;
import nexj.core.persistence.QueryTimeoutException;
import nexj.core.rpc.RPCSizeException;
import nexj.core.rpc.RPCUtil;
import nexj.core.rpc.RequestException;
import nexj.core.runtime.ContextUtil;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.runtime.SecurityViolationException;
import nexj.core.runtime.ThreadContextHolder;
import nexj.core.util.BrowserCapabilities;
import nexj.core.util.CertificateUtil;
import nexj.core.util.CompactXMLWriter;
import nexj.core.util.HTTP;
import nexj.core.util.HashTab;
import nexj.core.util.LimitInputStream;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.MIMEHeader;
import nexj.core.util.MultipartDataException;
import nexj.core.util.XMLWriter;
import nexj.core.util.lock.Lock;
import nexj.core.util.lock.LockMap;
import nexj.core.util.lock.TimeoutException;
import nexj.core.util.lock.WeakKeyLockMap;

/**
 * Generic HTTP server implementation.
 * Creates the invocation context and invokes a template method to do the work.
 */
public abstract class GenericHTTPServer implements HTTPServer, WebServer
{
   // constants

   /**
    * Content type of a form with file input fields.
    */
   public final static String MULTIPART_FORM_DATA = "multipart/form-data";
   
   /**
    * Default encoding.
    */
   public final static String ENCODING = "UTF-8";
   
   // attributes

   /**
    * The request type name.
    */
   protected String m_sType = "http";

   /**
    * The HTTP address.
    */
   protected String m_sAddress;

   /**
    * The root URL.
    */
   protected String m_sRoot;
   
   /**
    * The anonymous HTTP address.
    */
   protected String m_sAnonymousAddress;

   /**
    * The anonymous root URL.
    */
   protected String m_sAnonymousRoot;

   /**
    * The map of parameters from a request.
    */
   private Map m_paramMap;

   /**
    * The buffer size.
    */
   protected int m_nBufferSize;

   /**
    * The cache cookie.
    */
   protected String m_sCacheCookie;

   /**
    * The maximum request size in bytes.
    */
   protected long m_lMaxRequestSize;

   /**
    * True to allow request processing.
    */
   protected boolean m_bEnabled = true;

   /**
    * True to allow processing of anonymous requests.
    */
   protected boolean m_bAnonEnabled;

   /**
    * True if only headers are required.
    */
   protected boolean m_bHeadersOnly;

   /**
    * True to allow requests to RPC servers; false to disable RPC requests. RPC will
    * still be allowed if a trusted certificate is specified, unless
    * authProtocol=="certificate".
    */
   protected boolean m_bRPCEnabled;

   /**
    * True if this is a server for RPC; false otherwise.
    */
   protected boolean m_bRPCServer;

   /**
    * Session lock timeout in milliseconds.
    */
   protected long m_lLockTimeout;

   /**
    * Time in seconds after which session lock acquisition should be re-attempted automatically.
    */
   protected int m_nLockRetryDelay;

   /**
    * Maximum number of times that session lock acquisition should be automatically attempted.
    */
   protected int m_nLockRetryCount;

   /**
    * The cached browser version for the request as an array from major to minor version.
    */
   protected int m_nAgentType = BrowserCapabilities.BROWSER_DEFAULT;

   /**
    * The cached browser version for the request as an array from major to minor version.
    */
   protected int[] m_nAgentVersion = null;

   // associations

   /**
    * The web request interceptor.
    */
   protected WebInterceptor m_interceptor;
   
   /**
    * The HTTP servlet.
    */
   protected HttpServlet m_servlet;
   
   /**
    * The HTTP request.
    */
   protected HttpServletRequest m_request;
   
   /**
    * The HTTP response.
    */
   protected HttpServletResponse m_response;

   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   /**
    * The public certificate of a trusted remote party.
    */
   protected Certificate m_trust;

   /**
    * The server logger.
    */
   protected Logger m_logger = Logger.getLogger(getClass());

   /**
    * The session lock map.
    */
   protected final static LockMap s_lockMap = new WeakKeyLockMap();

   // operations

   /**
    * Sets the web request interceptor.
    * @param interceptor The web request interceptor to set.
    */
   public void setInterceptor(WebInterceptor interceptor)
   {
      m_interceptor = interceptor;
   }

   /**
    * @see WebServer#getCacheCookie()
    */
   public String getCacheCookie()
   {
      if (m_sCacheCookie == null)
      {
         m_sCacheCookie = HTTPUtil.getCacheCookie(getInvocationContext().getMetadata());
      }
      
      return m_sCacheCookie;
   }
   
   /**
    * @see WebServer#setCacheCookie(String)
    */
   public void setCacheCookie(String sCacheCookie)
   {
      m_sCacheCookie = sCacheCookie;
   }

   /**
    * @return The invocation context.
    */
   public InvocationContext getInvocationContext()
   {
      return m_context;
   }

   /**
    * Sets the request type name.
    * @param sType The request type name to set.
    */
   public void setType(String sType)
   {
      m_sType = sType;
   }

   /**
    * @return The request type name.
    */
   public String getType()
   {
      return m_sType;
   }

   /**
    * Sets the HTTP address.
    * @param sAddress The HTTP address to set.
    */
   public void setAddress(String sAddress)
   {
      m_sAddress = HTTPUtil.getHostURI(sAddress);
      m_sRoot = null;
   }

   /**
    * @return The HTTP address.
    */
   public String getAddress()
   {
      return m_sAddress;
   }

   /**
    * @see nexj.core.rpc.http.WebServer#getAgentType()
    */
   public int getAgentType()
   {
      if (m_nAgentType == BrowserCapabilities.BROWSER_DEFAULT)
      {
         m_nAgentType = BrowserCapabilities.getBrowserType(getHeader(HTTP.HEADER_USER_AGENT)); 
      }

      return m_nAgentType; 
   }

   /**
    * @see nexj.core.rpc.http.WebServer#getAgentVersion()
    */
   public int[] getAgentVersion()
   {
      if (m_nAgentVersion == null)
      {
         m_nAgentVersion = BrowserCapabilities.getBrowserVersion(getHeader(HTTP.HEADER_USER_AGENT)); 
      }

      return m_nAgentVersion;
   }

   /**
    * Sets the anonymous HTTP address.
    * @param sAnonymousAddress The anonymous HTTP address to set.
    */
   public void setAnonymousAddress(String sAnonymousAddress)
   {
      m_sAnonymousAddress = HTTPUtil.getHostURI(sAnonymousAddress);
   }

   /**
    * @return The anonymous HTTP address.
    */
   public String getAnonymousAddress()
   {
      return m_sAnonymousAddress;
   }
   
   /**
    * @return The request root.
    */
   public String getRoot()
   {
      if (m_sRoot == null)
      {
         m_sRoot = HTTPUtil.computeRoot(m_sAddress, m_request.getContextPath(),
            m_request.getScheme(), m_request.getServerPort(), m_request.getServerName(),
            m_context.getMetadata().isSecureTransport() && !m_request.isSecure());
      }

      return m_sRoot;
   }
   
   /**
    * @see nexj.core.rpc.http.WebServer#getRelativeRoot()
    */
   public String getRelativeRoot()
   {
      return HTTPUtil.computeRelativeRoot(m_request.getServletPath());
   }
   
   /**
    * @return The anonymous server root.
    */
   public String getAnonymousRoot()
   {
      if (m_sAnonymousRoot == null)
      {
         m_sAnonymousRoot = HTTPUtil.computeAnonymousRoot(m_sAnonymousAddress, m_request.getContextPath(), 
            m_request.getScheme(), m_request.getServerPort(), m_request.getServerName(), m_request.isSecure());
      }
      
      return m_sAnonymousRoot;
   }
   
   /**
    * Sets the buffer size.
    * @param nBufferSize The buffer size to set.
    */
   public void setBufferSize(int nBufferSize)
   {
      m_nBufferSize = nBufferSize;
   }

   /**
    * @return The buffer size.
    */
   public int getBufferSize()
   {
      return m_nBufferSize;
   }

   /**
    * Sets the maximum request size in bytes.
    * @param lMaxRequestSize The maximum request size in bytes to set.
    */
   public void setMaxRequestSize(long lMaxRequestSize)
   {
      m_lMaxRequestSize = lMaxRequestSize;
   }

   /**
    * @return The maximum request size in bytes.
    */
   public long getMaxRequestSize()
   {
      return m_lMaxRequestSize;
   }

   /**
    * Sets the session lock timeout in seconds.
    * @param nSeconds The seconds to set (0 for infinite).
    */
   public void setLockTimeout(int nSeconds)
   {
      m_lLockTimeout = nSeconds * 1000L;
   }

   /**
    * @return The session lock timeout in seconds. 
    */
   public int getLockTimeout()
   {
      return (int)(m_lLockTimeout / 1000);
   }

   /**
    * Sets the time in seconds to wait before automatically re-attempting session 
    * lock acquisition after timing out.
    * @param nSeconds The seconds to set (0 for never auto retry).
    */
   public void setLockRetryDelay(int nSeconds)
   {
      m_nLockRetryDelay = nSeconds;
   }

   /**
    * @return The amount of time in seconds to wait before re-attempting session lock 
    * acquisition after timing out. 
    */
   public int getLockRetryDelay()
   {
      return m_nLockRetryDelay;
   }

   /**
    * Sets the maximum number of times the framework should re-attempt to acquire a session 
    * lock after timing out.
    * @param nMax The number of times to automatically retry (0 for never).
    */
   public void setLockRetryCount(int nMax)
   {
      m_nLockRetryCount = nMax;
   }

   /**
    * @return The maximum number of times to automatically re-attempt to acquire a session lock. 
    */
   public int getLockRetryCount()
   {
      return m_nLockRetryCount;
   }

   /**
    * Sets the enablement flag.
    * @param bEnabled The enablement flag to set.
    */
   public void setEnabled(boolean bEnabled)
   {
      m_bEnabled = bEnabled;
   }

   /**
    * @return The enablement flag.
    */
   public boolean isEnabled()
   {
      return m_bEnabled;
   }

   /**
    * Sets the enablement flag for anonymous requests.
    * @param bEnabled True to enable processing of anonymous requests.
    */
   public void setAnonEnabled(boolean bEnabled)
   {
      m_bAnonEnabled = bEnabled;
   }

   /**
    * Gets the enablement flag for anonymous requests.
    * @return True to enable processing of anonymous requests; false otherwise.
    */
   public boolean isAnonEnabled()
   {
      return m_bAnonEnabled;
   }

   /**
    * Sets the remote machine public certificate to trust when establishing an SSL connection.
    * 
    * @param sTrust The remote machine's X.509 certificate, base64-encoded.
    */
   public void setTrustedCertificate(String sTrust)
   {
      m_trust = CertificateUtil.parseCertificate(sTrust);
   }

   /**
    * Sets the allow basic authentication in RPC flag.
    * 
    * @param bRPCEnabled True to allow RPC requests that do not use
    * client certificate authentication; false to deny them.
    */
   public void setRPCEnabled(boolean bRPCEnabled)
   {
      m_bRPCEnabled = bRPCEnabled;
   }

   /**
    * Gets the allow basic authentication in RPC flag.
    * 
    * @return True to allow RPC requests that do not use client certificate
    * authentication; false to deny them.
    */
   public boolean isRPCEnabled()
   {
      return m_bRPCEnabled;
   }

   /**
    * Sets the RPC server flag to indicate whether or not this server is for
    * handling RPC requests.
    * 
    * @param bRPCServer True if this is a server for RPC; false otherwise.
    */
   public void setRPCServer(boolean bRPCServer)
   {
      m_bRPCServer = bRPCServer;
   }

   /**
    * Gets the RPC server flag, indicating whether or not this server is for
    * handling RPC requests.
    * 
    * @return True if this is a server for RPC; false otherwise.
    */
   public boolean isRPCServer()
   {
      return m_bRPCServer;
   }
   
   /**
    * @see nexj.core.rpc.http.HTTPServer#invoke(javax.servlet.http.HttpServlet, javax.servlet.http.HttpServletRequest, javax.servlet.http.HttpServletResponse)
    */
   public final void invoke(HttpServlet servlet, HttpServletRequest request, HttpServletResponse response)
      throws ServletException, IOException
   {
      int nCookie = -1;
      boolean bIntercepted = false;

      /**
       * Set Character Encoding on request if it is null. This condition occurs
       * because browsers have a bug in that they don't send encoding in their form submissions
       * even when set in their HTML headers.
       */
      if (request.getCharacterEncoding() == null)
      {
         request.setCharacterEncoding(ENCODING);
      }

      if (!m_bEnabled)
      {
         response.sendError(HttpServletResponse.SC_NOT_FOUND, request.getRequestURI());

         return;
      }

      if (m_interceptor instanceof HTTPServer)
      {
         ((HTTPServer)m_interceptor).invoke(servlet, request, response);
      }

      try
      {
         m_servlet = servlet;
         m_request = request;
         m_response = response;
         verifyAuthentication();
         m_context = (InvocationContext)Repository.getMetadata().getComponent("System.InvocationContext").getInstance(null);
         m_context.setClientAddress("http:" + request.getRemoteAddr() + ':' + request.getRemotePort());
         nCookie = Logger.pushContext(m_context.getClientAddress());

         if (request.getHeader("X-Stealth") != null)
         {
            m_context.setStealth(true);
         }

         String sLocale = getLocale();

         if (sLocale != null)
         {
            m_context.setLocale(sLocale);
         }

         m_context.initialize(getPrincipal());
         Logger.pushContext(m_context.getPrincipal().getName());

         if (m_interceptor != null)
         {
            if (m_interceptor instanceof InvocationContextAware)
            {
               ((InvocationContextAware)m_interceptor).setInvocationContext(m_context);
            }

            if (m_interceptor.beginWebRequest(this))
            {
               return;
            }

            bIntercepted = true;
         }

         if (m_logger.isDebugEnabled() && !m_context.isStealth())
         {
            m_logger.debug("Received a " + m_request.getMethod() + " " + m_sType + " request from " +
               m_context.getPrincipal().getName() + " @ " + request.getRemoteHost() +
               " (" + m_context.getClientAddress() + ") at " + request.getRequestURL() +
               ((request.getQueryString() == null ? "" : "?" + request.getQueryString())));
         }

         m_bHeadersOnly = request.getMethod().equals("HEAD");

         if (!m_bHeadersOnly && m_nBufferSize > 0)
         {
            if (response.getBufferSize() < m_nBufferSize)
            {
               response.setBufferSize(m_nBufferSize);
            }
         }

         if (m_lMaxRequestSize > 0 && request.getHeader("X-Max-Content-Length") != null)
         {
            response.setHeader("X-Max-Content-Length", String.valueOf(m_lMaxRequestSize));
         }

         invoke();

         if (m_logger.isDebugEnabled() && !m_context.isStealth())
         {
            m_logger.debug("Completed the " + m_request.getMethod() + " " + m_sType + " request");
         }
      }
      catch (Throwable t)
      {
         handleException(t);
      }
      finally
      {
         if (bIntercepted)
         {
            m_interceptor.endWebRequest(this);
         }

         if (m_context != null)
         {
            try
            {
               m_context.complete(false);
            }
            catch (Throwable t)
            {
               handleException(t);
            }
         }

         if (nCookie != -1)
         {
            Logger.resetContext(nCookie);
         }

         ThreadContextHolder.setContext(null);
      }
   }

   /**
    * @return The HTTP request locale or null if unknown.
    */
   protected String getLocale()
   {
      String sLocale = m_request.getParameter("locale");

      if (sLocale != null)
      {
         return sLocale;
      }

      Locale locale = m_request.getLocale();

      if (locale != null)
      {
         return locale.toString();
      }

      return null;
   }

   /**
    * Handles an exception occurring during the processing.
    * @param t The exception to handle.
    */
   protected void handleException(Throwable t) throws ServletException, IOException
   {
      t = RPCUtil.handleException(t, isSystem(t), m_sType, m_logger);

      if (!m_response.isCommitted())
      {
         m_response.resetBuffer();

         if (t instanceof RPCSizeException && m_lMaxRequestSize > 0)
         {
            m_response.setHeader("X-Max-Content-Length", String.valueOf(m_lMaxRequestSize));
         }

         m_response.sendError(getStatus(t), getMessage(t));
      }
   }

   /**
    * Determines whether a given exception is a system error,
    * which should not be shown directly to end-users. 
    * @param t The exception to test.
    * @return True if the exception is an error.
    */
   protected boolean isSystem(Throwable t)
   {
      if (t instanceof FileNotFoundException)
      {
         return false;
      }

      return RPCUtil.isSystem(t);
   }
   
   /**
    * Gets a displayable message from an exception.
    * @param t The exception form which to get the message.
    * @return The message. 
    */
   protected String getMessage(Throwable t)
   {
      String sMsg  = ContextUtil.getMessage(t, m_context);

      if (sMsg.length() > 0 && sMsg.charAt(sMsg.length() - 1) == '.')
      {
         sMsg = sMsg.substring(0, sMsg.length() - 1);
      }

      return sMsg;
   }

   /**
    * Gets an HttpServletResponse.SC_* status code from an exception.
    * @param t The exception.
    * @return The HTTP status code.
    */
   protected int getStatus(Throwable t)
   {
      if (t instanceof SecurityViolationException)
      {
         return HttpServletResponse.SC_FORBIDDEN;
      }

      if (t instanceof RPCSizeException)
      {
         return HttpServletResponse.SC_REQUEST_ENTITY_TOO_LARGE;
      }

      if (t instanceof RequestException)
      {
         return HttpServletResponse.SC_BAD_REQUEST;
      }

      if (t instanceof FileNotFoundException)
      {
         return HttpServletResponse.SC_NOT_FOUND;
      }

      if (t instanceof TimeoutException ||
         t instanceof QueryTimeoutException)
      {
         return HttpServletResponse.SC_REQUEST_TIMEOUT;
      }

      return HttpServletResponse.SC_INTERNAL_SERVER_ERROR;
   }
   
   /**
    * @return The request input stream.
    */
   protected InputStream getInputStream() throws IOException
   {
      if (m_lMaxRequestSize > 0)
      {
         int nSize = m_request.getContentLength();
         
         if (nSize < 0)
         {
            return new LimitInputStream(m_request.getInputStream(), m_lMaxRequestSize, true);
         }

         if (nSize > m_lMaxRequestSize)
         {
            throw new RPCSizeException("err.rpc.requestSize",
               new Object[]{new Long(m_lMaxRequestSize)});
         }
      }

      return m_request.getInputStream();
   }
   
   /**
    * @return The request reader.
    */
   protected Reader getReader() throws IOException
   {
      String sEncoding = m_request.getCharacterEncoding();

      if (sEncoding == null)
      {
         sEncoding = ENCODING;
      }

      return new BufferedReader(new InputStreamReader(getInputStream(), sEncoding));
   }
   
   /**
    * @return True if the request contains multipart form data.
    */
   protected boolean isMultipart()
   {
      if (!m_request.getMethod().equalsIgnoreCase("POST"))
      {
         return false;
      }
      
      String sContentType = m_request.getHeader("Content-Type");
      
      return sContentType != null && sContentType.regionMatches(true,
         0, MULTIPART_FORM_DATA, 0, MULTIPART_FORM_DATA.length());
   }

   /**
    * Gets the multipart request parameters.
    * @param sizeMap The map of parameter name to maximum data size. Can be null.
    * @param lDefMaxSize The default maximum data size for a parameter.
    * @return Map of parameter name to data value.
    */
   protected Lookup getMultipartParameters(Lookup sizeMap, long lDefMaxSize) throws IOException, RequestException
   {
      try
      {
         Lookup paramMap = new HashTab();
         MIMEHeader header = new MIMEHeader(null, m_request.getHeader("Content-Type"));

         if (header.getFirstValue() == null || !header.getFirstValue().getName().equals(MULTIPART_FORM_DATA))
         {
            throw new MultipartDataException("Unexpected content type");
         }

         String sSeparator = header.getFirstValue().findArg("boundary");

         if (sSeparator == null || sSeparator.length() == 0)
         {
            throw new MultipartDataException("Missing multipart boundary in content-type");
         }
            
         String sEncoding = m_request.getCharacterEncoding();
         
         if (sEncoding == null)
         {
            sEncoding = ENCODING;
         }

         HTTPUtil.addMultiPartParameters(getInputStream(), sEncoding, sSeparator, paramMap, sizeMap, lDefMaxSize);
         
         return paramMap;
      }
      catch (IOException e)
      {
         throw new RequestException("err.rpc.attachment", e);
      }
   }
   
   /**
    * Template method to do the server work.
    */
   protected abstract void invoke() throws ServletException, IOException;

   /**
    * @see nexj.core.rpc.http.WebServer#getContextPath()
    */
   public String getContextPath()
   {
      return m_request.getContextPath();
   }

   /**
    * @see nexj.core.rpc.http.WebServer#setHeader(java.lang.String, java.lang.String)
    */
   public void setHeader(String sName, String value)
   {
      m_response.setHeader(sName, value);
   }

   /**
    * @see nexj.core.rpc.http.WebServer#getHeader(java.lang.String)
    */
   public String getHeader(String sName)
   {
      return m_request.getHeader(sName);
   }

   /**
    * @see nexj.core.rpc.http.WebServer#getHeaderNames()
    */
   public Enumeration getHeaderNames()
   {
      return m_request.getHeaderNames();
   }

   /**
    * @see nexj.core.rpc.http.WebServer#getHeaders(java.lang.String)
    */
   public Enumeration getHeaders(String sName)
   {
      return m_request.getHeaders(sName);
   }

   /**
    * @see nexj.core.rpc.http.WebServer#getMethod()
    */
   public String getMethod()
   {
      return m_request.getMethod();
   }

   /**
    * @see nexj.core.rpc.http.WebServer#getParameter(java.lang.String)
    */
   public String getParameter(String sName)
   {
      Map paramMap = getParameterMap();
      String[] sValue = (String[])paramMap.get(sName);

      return (sValue == null) ? m_request.getParameter(sName) : sValue[0];
   }

   /**
    * @see nexj.core.rpc.http.WebServer#getParameterMap()
    */
   public Map getParameterMap()
   {
      if (m_paramMap == null)
      {
         Map oldMap = m_request.getParameterMap();
         String sQuery = m_request.getQueryString();
         
         m_paramMap = (Map)(Object)HTTPUtil.parseQuery(sQuery, null);
         
         // We must add all other parameters that are not in the query string.
         if (m_paramMap != null)
         {
            // Process the POST data reserved parameter, added during a POST load balancer redirect, in the query string.
            // The data is decrypted and added as parameter key/value pairs to the paramMap.
            if (m_paramMap.containsKey(WebServer.POST_BODY_PARAMETER))
            {
               Object oPostData = m_request.getAttribute(WebServer.POST_BODY_PARAMETER);

               if (oPostData instanceof String[] && ((String[])oPostData)[0] != null)
               {
                  Lookup postDataMap = HTTPUtil.parseQuery(((String[])oPostData)[0], null);

                  for (Lookup.Iterator iter = postDataMap.iterator(); iter.hasNext();)
                  {
                     iter.next();

                     m_paramMap.put(iter.getKey(), iter.getValue());
                  }
               }
            }

            for (Iterator iter = oldMap.entrySet().iterator(); iter.hasNext();)
            {
               Map.Entry entry = (Map.Entry)iter.next();
               Object key = entry.getKey();
               Object oldValue = m_paramMap.put(key, entry.getValue());

               if (oldValue != null)
               {
                  m_paramMap.put(key, oldValue);
               }
            }
         }
         else
         {
            m_paramMap = oldMap;
         }
      }

      return m_paramMap;
   }

   /**
    * @see nexj.core.rpc.http.WebServer#getPrincipal()
    */
   public Principal getPrincipal()
   {
      Principal principal = m_request.getUserPrincipal();

      if (principal == null)
      {
         Metadata metadata = m_context.getMetadata();

         if (HTTPUtil.isAnonymousRequest(m_request, metadata))
         {
            principal = metadata.getAnonymousUser();
         }
      }

      return principal;
   }

   /**
    * @see nexj.core.rpc.http.WebServer#getQueryString()
    */
   public String getQueryString()
   {
      return m_request.getQueryString();
   }

   /**
    * @see nexj.core.rpc.http.WebServer#getRequestURI()
    */
   public String getRequestURI()
   {
      return m_request.getRequestURI();
   }
   
   /**
   * @see nexj.core.rpc.http.getRequestedSessionId()
   */
   public String getRequestedSessionId()
   {
      return m_request.getRequestedSessionId();
   }

   /**
    * @see nexj.core.rpc.http.WebServer#getScheme()
    */
   public String getScheme()
   {
      return m_request.getScheme();
   }

   /**
    * @see nexj.core.rpc.http.WebServer#getServerName()
    */
   public String getServerName()
   {
      return m_request.getServerName();
   }

   /**
    * @see nexj.core.rpc.http.WebServer#getServerPort()
    */
   public int getServerPort()
   {
      return m_request.getServerPort();
   }

   /**
    * @see nexj.core.rpc.http.WebServer#getServletPath()
    */
   public String getServletPath()
   {
      return m_request.getServletPath();
   }

   /**
    * @see nexj.core.rpc.http.WebServer#getResponseOutputStream()
    */
   public OutputStream getResponseOutputStream() throws IOException
   {
      return m_response.getOutputStream();
   }

   /**
    * @see nexj.core.rpc.http.WebServer#getSessionLock()
    */
   public Lock getSessionLock() throws TimeoutException
   {
      Lock lock = s_lockMap.get(m_request.getSession(), m_lLockTimeout);

      lock.lock();

      return lock;
   }

   /**
    * @see nexj.core.rpc.http.WebServer#setSessionAttribute(java.lang.String, java.lang.Object)
    */
   public void setSessionAttribute(String sName, Object value)
   {
      m_request.getSession().setAttribute(sName, value);
   }

   /**
    * @see nexj.core.rpc.http.WebServer#getSessionAttribute(java.lang.String)
    */
   public Object getSessionAttribute(String sName)
   {
      HttpSession session = m_request.getSession(false);

      return (session == null) ? null : session.getAttribute(sName);
   }

   /**
    * @see nexj.core.rpc.http.WebServer#removeSessionAttribute(java.lang.String)
    */
   public void removeSessionAttribute(String sName)
   {
      HttpSession session = m_request.getSession(false);

      if (session != null)
      {
         session.removeAttribute(sName);
      }
   }

   /**
    * @see nexj.core.rpc.http.WebServer#getAttributeNames()
    */
   public Enumeration getAttributeNames()
   {
      HttpSession session = m_request.getSession(false);

      return (session == null) ? null : session.getAttributeNames();
   }

   /**
    * @see nexj.core.rpc.http.WebServer#invalidateSession()
    */
   public void invalidateSession()
   {
      HttpSession session = m_request.getSession(false);

      if (session != null)
      {
         session.invalidate();
      }
   }

   /**
    * @see nexj.core.rpc.http.WebServer#getSessionId()
    */
   public String getSessionId()
   {
      HttpSession session = m_request.getSession(false);

      return (session == null) ? null : session.getId();
   }

   /**
    * @see nexj.core.rpc.http.WebServer#getXMLWriter()
    */
   public XMLWriter getXMLWriter() throws IOException
   {
      return new CompactXMLWriter(m_response.getWriter());
   }

   /**
    * @see nexj.core.rpc.http.WebServer#isSecure()
    */
   public boolean isSecure()
   {
      return m_request.isSecure();
   }

   /**
    * @see nexj.core.rpc.http.WebServer#isSessionExpired()
    */
   public boolean isSessionExpired()
   {
      Object sessionExpired = getSessionAttribute(INVALID_ATTRIBUTE);

      if (sessionExpired instanceof Boolean)
      {
         return ((Boolean)sessionExpired).booleanValue();
      }

      return m_request.getRequestedSessionId() != null && !m_request.isRequestedSessionIdValid();
   }

   /**
    * @see nexj.core.rpc.http.WebServer#sendRedirect(java.lang.String)
    */
   public void sendRedirect(String sLocation) throws IOException
   {
      m_response.sendRedirect(sLocation);
   }

   /**
    * @see nexj.core.rpc.http.WebServer#setResponseContentType(java.lang.String)
    */
   public void setResponseContentType(String sContentType)
   {
      m_response.setContentType(sContentType);
   }

   /**
    * @see nexj.core.rpc.http.WebServer#setResponseContentLength(int)
    */
   public void setResponseContentLength(int nLength)
   {
      m_response.setContentLength(nLength);
   }

   /**
    * @see nexj.core.rpc.http.WebServer#isResponseCommitted()
    */
   public boolean isResponseCommitted()
   {
      return m_response.isCommitted();
   }

   /**
    * @see nexj.core.rpc.http.WebServer#resetResponseBuffer()
    */
   public void resetResponseBuffer()
   {
      m_response.resetBuffer();
   }

   /**
    * Verifies that the request uses the correct authentication. Override in
    * subclasses to provide custom security checks.
    */
   protected void verifyAuthentication()
   {
      Metadata metadata = Repository.getMetadata();

      if (m_bRPCServer)
      {
         // Allows client certificates unless authProtocol=="certificate" and authRPC==false
         verifyCertificateCredentials(
            (metadata.getAuthenticationProtocol() == Metadata.AUTH_PROTOCOL_CERTIFICATE)
            ? ((m_bRPCEnabled) ? m_trust : null) : m_trust,
            m_bRPCEnabled
         );
      }
      else
      {
         // Allows client certificate only if authProtocol=="certificate"
         verifyCertificateCredentials(
            (metadata.getAuthenticationProtocol() == Metadata.AUTH_PROTOCOL_CERTIFICATE) ? m_trust : null,
            (metadata.getAuthenticationProtocol() != Metadata.AUTH_PROTOCOL_CERTIFICATE)
         );
      }

      if (!isAnonEnabled() && HTTPUtil.isAnonymousRequest(m_request, metadata))
      {
         throw new SecurityViolationException("err.rpc.anonymous");
      }
   }

   /**
    * Verifies that the request's authentication data matches the given parameters.
    * 
    * @param trustedCertificate The certificate to trust if the request uses client
    * certificate authentication.
    * @param bAllowBasic A flag indicating whether or not requests made with HTTP
    * basic authentication shall be allowed.
    */
   protected void verifyCertificateCredentials(Certificate trustedCertificate, boolean bAllowBasic)
   {
      boolean bRequestUsesCertificateAuth = HTTPUtil.isUsingClientCertificateAuthentication(m_request);

      if (bRequestUsesCertificateAuth)
      {
         if (trustedCertificate == null)
         {
            throw new SecurityViolationException("err.integration.unauthorized", new Object[]{m_servlet.getServletName()});
         }

         X509Certificate[] certs = (X509Certificate[])m_request.getAttribute(HTTPUtil.CLIENT_CERTIFICATE_ATTRIBUTE_NAME);

         if (certs == null)
         {
            throw new SecurityViolationException("err.integration.missingCertificate", new Object[]{m_servlet.getServletName()});
         }

         // The certificate should now be validated against allowed certificates for this channel.
         if (!HTTPUtil.isCertificateMatched(trustedCertificate, certs))
         {
            throw new SecurityViolationException("err.integration.unauthorized", new Object[]{m_servlet.getServletName()});
         }
      }
      else if (HttpServletRequest.BASIC_AUTH.equals(m_request.getAuthType()))
      {
         if (!bAllowBasic)
         {
            throw new SecurityViolationException("err.integration.unauthorized", new Object[]{m_servlet.getServletName()});
         }
      }
   }
}
