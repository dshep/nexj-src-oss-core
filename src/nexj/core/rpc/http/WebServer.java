// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.http;

import java.io.IOException;
import java.io.OutputStream;
import java.security.Principal;
import java.util.Enumeration;
import java.util.Map;

import nexj.core.util.SysUtil;
import nexj.core.util.XMLWriter;
import nexj.core.util.lock.Lock;
import nexj.core.util.lock.TimeoutException;

public interface WebServer
{
   // constants

   /**
    * Session attribute indicating the session is invalid.
    */
   public final static String INVALID_ATTRIBUTE = "$invalid";

   /**
    * Last access time attribute.
    */
   public final static String ATIME_ATTRIBUTE = "$atime";

   /**
    * Parameter that includes all the post data being submitted on the URL of the GET redirect.
    * Only used when the load balancer issues a redirect for a POST request.
    */
   public final static String POST_BODY_PARAMETER = ".";

   public final static String CLIENT_CONTEXT = SysUtil.NAMESPACE + ".clientContext.";

   // operations

   /**
    * Gets agent type.
    * @return One of BROWSER_IE, BROWSER_MOBILE, BROWSER_HTML4.
    */
   public int getAgentType();

   /**
    * @return The browser version as an array from major to minor version.
    */
   public int[] getAgentVersion();

   /**
    * Sets the anonymous HTTP address.
    * @param sAnonymousAddress The anonymous HTTP address to set.
    */
   public void setAnonymousAddress(String sAnonymousAddress);

   /**
    * @return The anonymous HTTP address.
    */
   public String getAnonymousAddress();

   String getContextPath();

   String getScheme();

   int getServerPort();

   boolean isSecure();

   /**
    * Determines if the current session is an expired one.
    */
   boolean isSessionExpired();

   String getServerName();

   Principal getPrincipal();

   String getHeader(String sName);

   Enumeration getHeaders(String sName);

   void setResponseContentType(String sContentType);

   void setResponseContentLength(int nLength);

   /**
    * @return True if data has been already sent to the client
    * (does not count buffered but unsent data).
    */
   boolean isResponseCommitted();

   /**
    * Clears the response buffer, which contains unsent data.
    */
   void resetResponseBuffer();

   OutputStream getResponseOutputStream() throws IOException;

   /**
    * Establishes and locks a session.
    * @return An object for serializing session access.
    * @throws TimeoutException if the lock has timed out.
    */
   Lock getSessionLock() throws TimeoutException;

   /**
    * @return Amount of time to wait before re-attempting to acquire a session lock (in seconds).
    */
   int getLockRetryDelay();

   /**
    * @return Maximum number of time to automatically re-attempt session lock acquisition.
    */
   int getLockRetryCount();

   Object getSessionAttribute(String sName);

   /**
    * @return The enumeration object which allows iterating over session attribute names.
    */
   Enumeration getAttributeNames();

   String getRequestedSessionId();

   /**
    * Removes the specified attribute from the current session.
    */
   void removeSessionAttribute(String sName);

   void setSessionAttribute(String sName, Object value);

   /**
    * Invalidate the session - remove all session attributes and prevent the session from being used again
    */
   void invalidateSession();

   XMLWriter getXMLWriter() throws IOException;

   String getParameter(String sName);

   Map getParameterMap();

   String getServletPath();

   void setHeader(String sName, String sValue);

   Enumeration getHeaderNames();

   String getMethod();

   /**
    * Returns a relative path from the current servlet path up to the context path.
    */
   String getRelativeRoot();

   String getRequestURI();

   String getQueryString();

   String getSessionId();

   /**
    * Sets the cache cookie.
    * @param sCacheCookie The cache cookie to set.
    */
   void setCacheCookie(String sCacheCookie);

   /**
    * @return The cache cookie.
    */
   String getCacheCookie();

   /**
    * Returns a redirection HTTP response.
    * @param sLocation The redirection URL.
    */
   void sendRedirect(String sLocation) throws IOException;
}
