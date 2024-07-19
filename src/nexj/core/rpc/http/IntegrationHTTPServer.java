// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.http;

import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;
import java.security.Principal;
import java.security.cert.X509Certificate;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import nexj.core.integration.Input;
import nexj.core.integration.IntegrationException;
import nexj.core.integration.io.ObjectInput;
import nexj.core.integration.io.ReaderInput;
import nexj.core.integration.io.StreamInput;
import nexj.core.meta.Metadata;
import nexj.core.meta.Primitive;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.Format;
import nexj.core.meta.integration.MessageTable;
import nexj.core.meta.integration.SchemaExporter;
import nexj.core.meta.integration.channel.http.HTTPChannel;
import nexj.core.rpc.RPCException;
import nexj.core.rpc.RequestException;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.SecurityViolationException;
import nexj.core.runtime.UnitOfWork;
import nexj.core.util.HTTP;
import nexj.core.util.HashTab;
import nexj.core.util.IOUtil;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.MIMEUtil;
import nexj.core.util.ObjUtil;
import nexj.core.util.PropertyIterator;
import nexj.core.util.StringUtil;
import nexj.core.util.lock.Lock;

/**
 * Integration HTTP channel server.
 */
public class IntegrationHTTPServer extends GenericHTTPServer
{
   // constants

   /**
    * The full HTTP GET Query String to pass to the server to trigger WSDL generation.
    */
   protected final static String GENERATE_WSDL_QUERY = "wsdl";

   // attributes

   /**
    * The authentication path.
    */
   protected String m_sAuthenticationPath = "/authentication";

   // associations

   /**
    * The authentication channel map: AuthenticationChannel[String].
    */
   protected Lookup m_authenticationChannelMap;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(IntegrationHTTPServer.class);
   
   /**
    * The session lock.
    */
   protected Lock m_sessionLock;

   // operations

   /**
    * Verification will occur in the invoke() method.
    * @see nexj.core.rpc.http.GenericHTTPServer#verifyAuthentication()
    */
   protected void verifyAuthentication()
   {
   }

   /**
    * @see nexj.core.rpc.http.GenericHTTPServer#getLocale()
    */
   protected String getLocale()
   {
      Locale locale = m_request.getLocale();

      if (locale != null)
      {
         return locale.toString();
      }

      return null;
   }

   /**
    * Sets the authentication path.
    * @param sAuthenticationPath The authentication path to set.
    */
   public void setAuthenticationPath(String sAuthenticationPath)
   {
      m_sAuthenticationPath = sAuthenticationPath;
   }

   /**
    * @return The authentication path.
    */
   public String getAuthenticationPath()
   {
      return m_sAuthenticationPath;
   }

   /**
    * Determines if the request is for an authentication channel.
    * @return True if the request is for an authentication channel.
    */
   protected boolean isAuthenticationRequested()
   {
      String sServletPath = m_request.getServletPath();

      return sServletPath.length() >= m_sAuthenticationPath.length() &&
         StringUtil.equalIgnoreCase(m_sAuthenticationPath, sServletPath.substring(0, m_sAuthenticationPath.length()));
   }

   /**
    * Tests a channel to determine if it belongs to the set of authentication channels for this
    * server.
    * @param sChannel The name of the channel to test.
    * @return True if the channel is an authentication channel.
    */
   protected boolean isAuthenticationChannel(String sChannel)
   {
      return m_authenticationChannelMap != null && m_authenticationChannelMap.contains(sChannel);
   }

   /**
    * Tests a channel to determine if it is enabled for authentication in this server.
    * @param sChannel The name of the channel to test.
    * @return True if the channel is an authentication channel.
    */
   protected boolean isAuthenticationChannelEnabled(String sChannel)
   {
      if (m_authenticationChannelMap == null)
      {
         return false;
      }

      AuthenticationChannel authChannel = (AuthenticationChannel)m_authenticationChannelMap.get(sChannel);

      return authChannel != null && authChannel.isEnabled();
   }

   /**
    * Adds an authentication channel.
    * @param authChannel The channel to add.
    */
   public void addAuthenticationChannel(AuthenticationChannel authChannel)
   {
      if (m_authenticationChannelMap == null)
      {
         m_authenticationChannelMap = new HashTab(1);
      }

      m_authenticationChannelMap.put(authChannel.getChannel(), authChannel);
   }

   /**
    * @see nexj.core.rpc.http.GenericHTTPServer#invoke()
    */
   protected void invoke() throws ServletException, IOException
   {
      String sRequestPath = m_request.getPathInfo();

      if (sRequestPath == null || sRequestPath.length() <= 1)
      {
         throw new RequestException("err.rpc.http.channel");
      }

      int i = sRequestPath.indexOf('/', 1);
      String sChannel = null;
      String sPath = "";

      if (i < 0)
      {
         sChannel = sRequestPath.substring(1);
      }
      else
      {
         sChannel = sRequestPath.substring(1, i);
         sPath = sRequestPath.substring(i);
      }

      boolean bAuthentication = isAuthenticationRequested();
      Metadata metadata = m_context.getMetadata();
      Channel channel = metadata.getChannel(sChannel);

      if (!(channel instanceof HTTPChannel) || !channel.isReceivable())
      {
         throw new RPCException("err.rpc.http.notReceiver", new Object[]{sChannel});
      }

      HTTPChannel http = (HTTPChannel)channel;

      if (!http.isReceivable())
      {
         throw new RPCException("err.rpc.http.notReceiver", new Object[]{sChannel});
      }

      if (http.isSecure() && !m_request.isSecure())
      {
         throw new RequestException("err.rpc.http.insecure", new Object[]{sChannel});
      }

      if (bAuthentication)
      {
         // Accept requests only on enabled authentication channels
         if (!isAuthenticationChannelEnabled(http.getName()))
         {
            throw new SecurityViolationException("err.rpc.http.authenticationNotEnabled", new Object[]{http.getName()});
         }
      }
      else if (isAuthenticationChannel(http.getName()))
      {
         // Don't accept requests on authentication channels
         throw new SecurityViolationException("err.rpc.http.authenticationOnly", new Object[]{http.getName()});
      }

      // Deny anonymous access to non-anonymous channels, and vice-versa
      if (HTTPUtil.isAnonymousRequest(m_request, metadata) || bAuthentication)
      {
         if (http.getAuthMode() != HTTPChannel.AUTH_NONE)
         {
            throw new SecurityViolationException("err.rpc.anonymous");
         }
      }
      else if (http.getAuthMode() == HTTPChannel.AUTH_NONE)
      {
         throw new SecurityViolationException("err.rpc.notAnonymous", new Object[]{http.getName()});
      }

      boolean bRequestUsesCertificateAuth = HTTPUtil.isUsingClientCertificateAuthentication(m_request);

      // Deny access to client certificate channels if no certificate present
      if (http.getAuthMode() == HTTPChannel.AUTH_CERT)
      {
         if (!bRequestUsesCertificateAuth)
         {
            throw new SecurityViolationException("err.rpc.http.certificateRequired", new Object[]{http.getName()});
         }

         X509Certificate[] certs = (X509Certificate[])m_request.getAttribute(HTTPUtil.CLIENT_CERTIFICATE_ATTRIBUTE_NAME);

         if (certs == null)
         {
            throw new SecurityViolationException("err.integration.missingCertificate", new Object[]{http.getName()});
         }

         // The certificate should now be validated against allowed certificates for this channel.
         if (!HTTPUtil.isCertificateMatched(http.getTrustedCertificate(), certs))
         {
            throw new SecurityViolationException("err.integration.unauthorized", new Object[]{http.getName()});
         }
      }
      else if (bRequestUsesCertificateAuth)
      {
         // Deny access to non-certificate-auth channels through certificate authentication.
         throw new SecurityViolationException("err.integration.unauthorized", new Object[]{http.getName()});
      }

      if (http.getPrivilege() != null && !m_context.getPrivilegeSet().contains(http.getPrivilege()))
      {
         throw new SecurityViolationException("err.integration.unauthorized", new Object[]{sChannel});
      }

      m_lMaxRequestSize = http.getMaxRequestSize();

      HTTPAdapter adapter = (HTTPAdapter)channel.getReceiver().getInstance(m_context);

      adapter.setServer(this);

      String sMethod = m_request.getMethod();
      boolean bImplemented;

      if (sMethod.equals("POST"))
      {
         bImplemented = http.isPostImplemented();
      }
      else if (sMethod.equals("GET"))
      {
         bImplemented = http.isGetImplemented();

         if (channel.getBindingCount() > 0 &&
            GENERATE_WSDL_QUERY.equalsIgnoreCase(m_request.getQueryString()))
         {
            SchemaExporter exporter = null;
            MessageTable msgTable = channel.getMessageTable();

            if (msgTable != null)
            {
               Format format = msgTable.getFormat();

               if (format != null)
               {
                  Class exporterClass = format.getExporter();

                  if (exporterClass != null)
                  {
                     Object obj = m_context.getClassInstance(exporterClass);

                     if (obj instanceof SchemaExporter)
                     {
                        exporter = (SchemaExporter)obj;
                     }
                  }
               }
            }

            if (exporter != null)
            {
               Writer writer;

               m_response.setStatus(HttpServletResponse.SC_OK);
               m_response.setContentType("text/xml; charset=UTF-8");
               writer = m_response.getWriter();
               exporter.exportSchema(channel, getRoot() + "/", writer);
               writer.close();
               return;
            }
         }
      }
      else if (sMethod.equals("HEAD"))
      {
         if (http.isHeadImplemented())
         {
            bImplemented = true;
         }
         else if (http.isGetImplemented())
         {
            sMethod = "GET";
            bImplemented = true;
         }
         else
         {
            bImplemented = false;
         }
      }
      else if (sMethod.equals("PUT"))
      {
         bImplemented = http.isPutImplemented();
      }
      else if (sMethod.equals("DELETE"))
      {
         bImplemented = http.isDeleteImplemented();
      }
      else if (sMethod.equals("TRACE"))
      {
         if (!http.isTraceImplemented())
         {
            doTrace();
            return;
         }

         bImplemented = true;
      }
      else if (sMethod.equals("OPTIONS"))
      {
         if (!http.isOptionsImplemented())
         {
            doOptions(http);
            return;
         }

         bImplemented = true;
      }
      else
      {
         bImplemented = false;
      }

      if (!bImplemented)
      {
         if (m_request.getProtocol().endsWith("1.1"))
         {
            m_response.setHeader("Allowed", getAllowedMethods(http));
            m_response.sendError(HttpServletResponse.SC_METHOD_NOT_ALLOWED);
         }
         else
         {
            m_response.sendError(HttpServletResponse.SC_BAD_REQUEST);
         }
      }
      else if (adapter.isBound(channel, m_context))
      {
         TransferObject tobj = new TransferObject(6);
         String sEncoding = m_request.getCharacterEncoding();

         if (sEncoding == null)
         {
            sEncoding = HTTPAdapter.getEncoding(http.getContentType(), HTTPAdapter.DEFAULT_ENCODING);
            m_request.setCharacterEncoding(sEncoding);
         }

         tobj.setValue(HTTPAdapter.CHANNEL, http.getName());
         tobj.setValue(HTTPAdapter.METHOD, sMethod);
         tobj.setValue(HTTPAdapter.PATH, sPath);

         TransferObject headers = new TransferObject();

         for (Enumeration enm = m_request.getHeaderNames(); enm.hasMoreElements();)
         {
            String sName = (String)enm.nextElement();
            String sNameLower = sName.toLowerCase(Locale.ENGLISH);
            Object value;

            switch (HTTP.getHeaderType(sNameLower))
            {
               case HTTP.TYPE_INTEGER:
                  value = Primitive.createInteger(m_request.getIntHeader(sName));
                  break;

               case HTTP.TYPE_DATE:
                  long lDate = m_request.getDateHeader(sName);
                  value = (lDate == -1) ? null : new Timestamp(lDate);
                  break;

               default:
                  value = m_request.getHeader(sName);
                  break;
            }

            headers.setValue(sNameLower, value);
         }

         tobj.setValue(HTTPAdapter.HEADERS, headers);

         // get session state
         HttpSession session = m_request.getSession(false);

         try
         {
            if (session != null)
            {
               m_sessionLock = getSessionLock();

               TransferObject state = new TransferObject();

               for (Enumeration enm = session.getAttributeNames(); enm.hasMoreElements();)
               {
                  String sName = (String)enm.nextElement();

                  if (isStateAccessible(sName, bAuthentication))
                  {
                     state.setValue(sName, session.getAttribute(sName));
                  }
               }

               tobj.setValue(HTTPAdapter.STATE, state);
            }

            Map parameterMap = getParameterMap();
            TransferObject parameters = null;

            if (parameterMap != null && parameterMap.size() > 0)
            {
               parameters = new TransferObject(parameterMap.size());

               for (Iterator itr = parameterMap.entrySet().iterator(); itr.hasNext(); )
               {
                  Map.Entry entry = (Map.Entry)itr.next();
                  String[] sValueArray = (String[])entry.getValue();
                  Object value;

                  if (sValueArray == null)
                  {
                     value = null;
                  }
                  else if (sValueArray.length == 1)
                  {
                     value = sValueArray[0];
                  }
                  else
                  {
                     List valueList = new ArrayList(sValueArray.length);

                     for (int k = 0; k < sValueArray.length; k++)
                     {
                        valueList.add(sValueArray[k]);
                     }

                     value = valueList;
                  }

                  parameters.setValue((String)entry.getKey(), value);
               }
            }

            if (isMultipart())
            {
               Lookup paramMap = getMultipartParameters(null, 0);

               if (parameters == null && paramMap.size() > 0)
               {
                  parameters = new TransferObject(paramMap.size());
               }

               for (Lookup.Iterator itr = paramMap.iterator(); itr.hasNext();)
               {
                  itr.next();
                  parameters.setValue((String)itr.getKey(), itr.getValue());
               }
            }

            if (parameters != null)
            {
               tobj.setValue(HTTPAdapter.PARAMETERS, parameters);
            }

            tobj.setValue(HTTPAdapter.BODY,
               (http.getDataType() == Primitive.BINARY ||
                  http.getDataType() == null &&
                  MIMEUtil.isBinaryMIMEType(m_request.getHeader(HTTP.HEADER_CONTENT_TYPE))) ?
                  new StreamInput(getInputStream(), sEncoding) :
                     (Input)new ReaderInput(getReader()));

            try
            {
               UnitOfWork uow = m_context.initUnitOfWork();

               uow.checkLicense();
               m_context.beginTransaction();
               adapter.receive(tobj, channel, m_context);
               m_context.complete(true);
            }
            catch (Throwable e)
            {
               m_context.complete(false);
               ObjUtil.rethrow(e);
            }
         }
         finally
         {
            if (m_sessionLock != null)
            {
               m_sessionLock.unlock();
            }
         }
      }
   }

   /**
    * Implements the HTTP TRACE method.
    */
   protected void doTrace() throws IOException
   {
      StringBuffer buf = new StringBuffer(128);

      buf.append("TRACE ");
      buf.append(m_request.getRequestURI());
      buf.append(' ');
      buf.append(m_request.getProtocol());
      buf.append("\r\n");

      for (Enumeration enm = m_request.getHeaderNames(); enm.hasMoreElements();)
      {
         String sName = (String)enm.nextElement();

         buf.append(sName);
         buf.append(": ");
         buf.append(m_request.getHeader(sName));
         buf.append("\r\n");
      }

      byte[] response = buf.toString().getBytes(m_response.getCharacterEncoding());
      buf = null;

      m_response.setContentType("message/http");
      m_response.setContentLength(response.length);

      ServletOutputStream ostream = m_response.getOutputStream();

      ostream.write(response);
      ostream.close();
   }

   /**
    * Implements the HTTP OPTIONS method.
    * @param channel The HTTP channel.
    */
   protected void doOptions(HTTPChannel channel)
   {
      m_response.addHeader("Allowed", getAllowedMethods(channel));
      m_response.setContentLength(0);
   }

   /**
    * Returns a comma-separated list of allowed HTTP methods.
    * @param channel The HTTP channel.
    * @return The comma-separated list of HTTP methods.
    */
   protected String getAllowedMethods(HTTPChannel channel)
   {
      StringBuffer buf = new StringBuffer(64);

      if (channel.isDeleteImplemented())
      {
         buf.append("DELETE, ");
      }

      if (channel.isGetImplemented())
      {
         buf.append("GET, ");
      }

      if (channel.isHeadImplemented() || channel.isGetImplemented())
      {
         buf.append("HEAD, ");
      }

      buf.append("OPTIONS, ");

      if (channel.isPostImplemented())
      {
         buf.append("POST, ");
      }

      if (channel.isPutImplemented())
      {
         buf.append("PUT, ");
      }

      buf.append("TRACE");

      return buf.toString();
   }

   /**
    * Adds a header to the response with type conversion.
    * @param sName The header name.
    * @param value The header value.
    */
   protected void setHeader(String sName, Object value)
   {
      if (value != null)
      {
         if (value instanceof Timestamp)
         {
            m_response.setDateHeader(sName, ((Timestamp)value).getTime());
         }
         else
         {
            m_response.setHeader(sName, Primitive.toString(value));
         }
      }
   }

   /**
    * Determines whether a state attribute is a system attribute.
    * @param sAttributeName The name of the attribute.
    * @return True if the attribute is a system attribute.
    */
   protected boolean isSystemAttribute(String sAttributeName)
   {
      return (sAttributeName.length() != 0 && sAttributeName.charAt(0) == '$');
   }

   /**
    * Determines whether a state attribute is accessible for read and write.
    * @param sAttributeName Then name of the attribute.
    * @param bAuthentication True if the request is an authentication request.
    * @return True if the attribute is accessible.
    */
   protected boolean isStateAccessible(String sAttributeName, boolean bAuthentication)
   {
      return bAuthentication || !isSystemAttribute(sAttributeName);
   }

   /**
    * Sends a response to the client.
    * @param tobj The response message.
    * @param channel The HTTP channel.
    */
   public void reply(TransferObject tobj, HTTPChannel channel) throws IntegrationException
   {
      try
      {
         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Sending a response on channel \"" + channel.getName() + "\"");
            s_logger.dump(tobj);
         }

         boolean bAuthentication = isAuthenticationRequested();
         TransferObject state = (TransferObject)tobj.findValue(HTTPAdapter.STATE);

         // Check for session changes
         if (state != null)
         {
            // get session
            HttpSession session = m_request.getSession(false);

            if (isStateAccessible(HTTPAdapter.AUTH_PRINCIPAL, bAuthentication))
            {
               if (state.hasValue(HTTPAdapter.AUTH_PRINCIPAL))
               {
                  if (session != null)
                  {
                     // principal has changed, clear out old session
                     for (Enumeration attrEnm = session.getAttributeNames(); attrEnm.hasMoreElements();)
                     {
                        String sName = (String)attrEnm.nextElement();

                        if (!isSystemAttribute(sName))
                        {
                           session.removeAttribute(sName);
                        }
                     }
                  }

                  if (state.getValue(HTTPAdapter.AUTH_PRINCIPAL) != null)
                  {
                     session = m_request.getSession(true); // if a new principle is set, create a new session.
                     
                     if (m_sessionLock == null)
                     {
                        m_sessionLock = getSessionLock();
                     }
                  }
               }
            }

            if (session != null)
            {
               for (PropertyIterator attrIter = state.getIterator(); attrIter.hasNext();)
               {
                  String sName = (String)attrIter.next();
                  Object value = attrIter.getValue();

                  if (isStateAccessible(sName, bAuthentication) && !ObjUtil.equal(value, session.getAttribute(sName)))
                  {
                     if (value == null)
                     {
                        session.removeAttribute(sName);
                     }
                     else
                     {
                        session.setAttribute(sName, value);
                     }
                  }
               }
            }
         }

         TransferObject headers = (TransferObject)tobj.findValue(HTTPAdapter.HEADERS);

         if (headers != null)
         {
            for (PropertyIterator itr = headers.getIterator(); itr.hasNext();)
            {
               itr.next();

               Object value = itr.getValue();

               if (value instanceof Collection)
               {
                  for (Iterator citr = ((Collection)value).iterator(); citr.hasNext();)
                  {
                     setHeader(itr.getName(), citr.next());
                  }
               }
               else
               {
                  setHeader(itr.getName(), value);
               }
            }
         }

         Number status = (Number)tobj.findValue(HTTPAdapter.STATUS);
         int nStatus = (status == null) ? HttpServletResponse.SC_OK : status.intValue();
         int nSeverity = (nStatus / 100);

         if (nSeverity == 2)
         {
            m_response.setStatus(nStatus);
         }
         else if (nSeverity == 3)
         {
            m_response.setStatus(nStatus);

            switch (nStatus)
            {
               case HttpServletResponse.SC_MOVED_PERMANENTLY:
               case HttpServletResponse.SC_MOVED_TEMPORARILY:
               case HttpServletResponse.SC_SEE_OTHER:
               case HttpServletResponse.SC_USE_PROXY:
               case HttpServletResponse.SC_TEMPORARY_REDIRECT:
                  if (!m_response.containsHeader("Location"))
                  {
                     String sURL = (String)tobj.findValue(HTTPAdapter.URL);

                     if (sURL != null)
                     {
                        m_response.setHeader("Location", sURL);
                     }
                  }

                  break;
            }

            m_bHeadersOnly = true;
         }
         else
         {
            String sMessage = (String)tobj.findValue(HTTPAdapter.MESSAGE);

            if (sMessage != null)
            {
               m_response.sendError(nStatus, sMessage);
            }
            else
            {
               m_response.sendError(nStatus);
            }

            tobj.removeValue(HTTPAdapter.BODY);

            if (!m_response.isCommitted())
            {
               m_response.resetBuffer();
            }
         }

         if (m_bHeadersOnly)
         {
            m_response.setContentLength(0);
         }
         else
         {
            Object body = tobj.findValue(HTTPAdapter.BODY);

            if (body != null)
            {
               if (m_response.getContentType() == null && channel.getContentType() != null)
               {
                  m_response.setContentType(channel.getContentType());
               }

               ObjectInput input = new ObjectInput(body);

               if (body instanceof String)
               {
                  if (m_response.getContentType() == null)
                  {
                     m_response.setContentType("text/plain");
                  }

                  input.setEncoding(m_response.getCharacterEncoding());

                  Writer writer = m_response.getWriter();

                  IOUtil.copy(writer, input.getReader());
                  writer.close();
               }
               else
               {
                  OutputStream ostream = m_response.getOutputStream();

                  IOUtil.copy(ostream, input.getInputStream());
                  ostream.close();
               }
            }
         }
      }
      catch (IOException e)
      {
         throw new IntegrationException("err.integration.io", e);
      }
   }

   /**
    * @see nexj.core.rpc.http.GenericHTTPServer#getPrincipal()
    */
   public Principal getPrincipal()
   {
      if (isAuthenticationRequested())
      {
         return m_context.getMetadata().getAnonymousUser();
      }

      return super.getPrincipal();
   }
}
