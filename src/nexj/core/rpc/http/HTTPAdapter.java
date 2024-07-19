// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.http;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.StringReader;
import java.io.Writer;
import java.net.InetSocketAddress;
import java.net.PasswordAuthentication;
import java.net.Proxy;
import java.net.URISyntaxException;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import nexj.core.integration.IntegrationException;
import nexj.core.integration.Receiver;
import nexj.core.integration.Responder;
import nexj.core.integration.Sender;
import nexj.core.integration.StateException;
import nexj.core.integration.io.ObjectInput;
import nexj.core.integration.io.ObjectOutput;
import nexj.core.integration.io.ReaderInput;
import nexj.core.integration.io.StreamInput;
import nexj.core.meta.Primitive;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.channel.http.HTTPChannel;
import nexj.core.meta.integration.format.xml.RootXMLMessagePartMapping;
import nexj.core.monitoring.Counter;
import nexj.core.monitoring.jmx.StatUtil;
import nexj.core.rpc.RPCException;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.Initializable;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.scripting.Intrinsic;
import nexj.core.scripting.Pair;
import nexj.core.util.CancellationException;
import nexj.core.util.HTTP;
import nexj.core.util.HTTPClient;
import nexj.core.util.HashHolder;
import nexj.core.util.IOUtil;
import nexj.core.util.Logger;
import nexj.core.util.MIMEHeader;
import nexj.core.util.MIMEHeaderMap;
import nexj.core.util.MIMEUtil;
import nexj.core.util.PropertyIterator;
import nexj.core.util.URIUtil;
import nexj.core.util.UTF8BOMIgnoreInputStream;
import nexj.core.util.Undefined;
import nexj.core.util.auth.AuthenticationException;
import nexj.core.util.auth.LoginException;
import nexj.core.util.auth.PasswordAuthenticationProvider;

/**
 * HTTP channel sender.
 */
public class HTTPAdapter extends Receiver implements Sender, Responder,
   PasswordAuthenticationProvider, InvocationContextAware, Initializable
{
   // constants

   /**
    * Default HTTP content encoding.
    * It is ISO-8859-1, not UTF-8 for better integration.
    */
   public final static String DEFAULT_ENCODING = "ISO-8859-1";

   /**
    * The URL message part: String.
    */
   public final static String URL = "url";

   /**
    * The method message part: String.
    */
   public final static String METHOD = "method";

   /**
    * The headers message part: TransferObject.
    */
   public final static String HEADERS = "headers";

   /**
    * The parameters request part: TransferObject.
    */
   public final static String PARAMETERS = "parameters";
   
   /**
    * The state message part: TransferObject.
    */
   public final static String STATE = "state";
   
   /**
    * The state principal part: String.
    */
   public final static String AUTH_PRINCIPAL = "$principal";

   /**
    * The path request part: String.
    * This is the path after the channel name in the request URL.
    */
   public final static String PATH = "path";

   /**
    * The user request part: String.
    */
   public final static String USER = "principal";

   /**
    * The password request part: String.
    */
   public final static String PASSWORD = "password";

   /**
    * The status response part: String.
    */
   public final static String STATUS = "status";

   /**
    * The message response part: String.
    */
   public final static String MESSAGE = "message";

   /**
    * The proxy host name request part: String.
    */
   public final static String PROXY_HOST = "proxyHost";

   /**
    * The proxy port number request part: Integer.
    */
   public final static String PROXY_PORT = "proxyPort";

   /**
    * The proxy user request part: String.
    */
   public final static String PROXY_USER = "proxyUser";

   /**
    * The proxy password request part: String.
    */
   public final static String PROXY_PASSWORD = "proxyPassword";

   /**
    * Pattern for determining a charset from a content type heaver value.
    */
   protected final static Pattern CHARSET_PATTERN = Pattern.compile(
      ".*?\\bcharset\\s*=\\s*\"?([^\\s>/\"]+).*?", Pattern.CASE_INSENSITIVE | Pattern.DOTALL);

   // attributes

   /**
    * True if a send/receive sequence is in progress.
    */
   protected boolean m_bInProgress;

   /**
    * Counter of messages sent since the creation of this component
    */
   protected Counter m_sentCounter = new Counter();

   // associations

   /**
    * The HTTP channel.
    */
   protected HTTPChannel m_channel;

   /**
    * The HTTP integration server, if the adapter is processing
    * the request in server mode.
    */
   protected IntegrationHTTPServer m_server;

   /**
    * The HTTP connection credentials.
    */
   protected PasswordAuthentication m_credentials;

   /**
    * The HTTP client.
    */
   protected HTTPClient m_client;

   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   /**
    * The adapter logger.
    */
   protected Logger m_logger;

   // operations

   /**
    * Sets the HTTP channel.
    * @param channel The HTTP channel to set.
    */
   public void setChannel(HTTPChannel channel)
   {
      m_channel = channel;
   }

   /**
    * @return The HTTP channel.
    */
   public HTTPChannel getChannel()
   {
      return m_channel;
   }

   /**
    * Sets the HTTP integration server.
    * @param server The HTTP integration server to set.
    */
   public void setServer(IntegrationHTTPServer server)
   {
      m_server = server;

      if (server != null)
      {
         m_bInProgress = true;
      }
   }

   /**
    * @return The HTTP integration server.
    */
   public IntegrationHTTPServer getServer()
   {
      return m_server;
   }

   /**
    * @see nexj.core.runtime.InvocationContextAware#setInvocationContext(nexj.core.runtime.InvocationContext)
    */
   public void setInvocationContext(InvocationContext context)
   {
      m_context = context;
   }

   /**
    * @see nexj.core.runtime.Initializable#initialize()
    */
   public void initialize() throws Exception
   {
      m_logger = m_channel.getLogger();
   }

   /**
    * @see nexj.core.integration.Sender#createOutput()
    */
   public ObjectOutput createOutput()
   {
      return new ObjectOutput();
   }

   /**
    * @see nexj.core.integration.Sender#prepare(nexj.core.rpc.TransferObject, nexj.core.rpc.TransferObject, nexj.core.meta.integration.Message)
    */
   public void prepare(TransferObject raw, TransferObject tobj, Message message) throws IntegrationException
   {
      if (message.getRoot().getMapping() instanceof RootXMLMessagePartMapping)
      {
         RootXMLMessagePartMapping mapping = (RootXMLMessagePartMapping)message.getRoot().getMapping();

         if (mapping.getEnvelope() != RootXMLMessagePartMapping.ENVELOPE_NONE)
         {
            TransferObject headers = (TransferObject)raw.findValue(HEADERS);

            if (headers == null)
            {
               headers = new TransferObject(2);
               raw.setValue(HEADERS, headers);
            }

            if (headers.findValue("content-type") == null)
            {
               switch (mapping.getEnvelope())
               {
                  case RootXMLMessagePartMapping.ENVELOPE_SOAP:
                     headers.setValue("content-type", "text/xml; charset=UTF-8");
                     break;

                  case RootXMLMessagePartMapping.ENVELOPE_SOAP12:
                     headers.setValue("content-type", "application/soap+xml; charset=UTF-8");
                     break;
               }
            }

            if (headers.findValue("soapaction") == null)
            {
               String sAction = (mapping.getAction() == null) ? "" : mapping.getAction();
               StringBuilder buf = new StringBuilder(sAction.length() + 2);

               buf.append('"');
               buf.append(sAction);
               buf.append('"');

               headers.setValue("soapaction", buf.toString());
            }
         }
      }
   }

   /**
    * @see nexj.core.integration.Sender#send(nexj.core.rpc.TransferObject)
    */
   public void send(TransferObject tobj) throws IntegrationException
   {
      if (m_server != null)
      {
         if (!m_bInProgress)
         {
            throw new StateException("err.integration.state.sendDup", new Object[]{m_channel.getName()});
         }

         long lStartTime = System.nanoTime();

         try
         {
            incrementCounters();
            m_server.reply(tobj, m_channel);
            m_bInProgress = false;
         }
         finally
         {
            StatUtil.updateAverage(m_context, m_channel.getSenderStatPath(), Channel.STAT_AVERAGE_SEND_TIME,
                  (double)(System.nanoTime() - lStartTime) / 1000000);
         }
      }
      else
      {
         respond(tobj, false);
      }
   }

   /**
    * Increment sent counters.
    */
   private void incrementCounters()
   {
      m_sentCounter.add(1);
      StatUtil.incrCounter(m_context, m_channel.getSenderStatPath(), Channel.STAT_TOTAL_COUNT, 1);
   }

   /**
    * @see nexj.core.integration.Sender#send(java.util.Collection)
    */
   public void send(Collection col) throws IntegrationException
   {
      for (Iterator itr = col.iterator(); itr.hasNext();)
      {
         send((TransferObject)itr.next());
      }
   }

   /**
    * @see nexj.core.integration.Responder#respond(nexj.core.rpc.TransferObject)
    */
   public TransferObject respond(TransferObject tobj) throws IntegrationException
   {
      return respond(tobj, true);
   }

   /**
    * Invokes the HTTP server.
    * @param tobj The request message.
    * @param bResponse True to return the response,
    * false to discard the response and return null.
    * @return The response message, or null if bResponse is false.
    */
   protected TransferObject respond(TransferObject tobj, final boolean bResponse) throws IntegrationException
   {
      if (m_server != null)
      {
         throw new StateException("err.integration.state.respond", new Object[]{m_channel.getName()});
      }

      long lStartTime = System.nanoTime();

      if (m_logger.isDebugEnabled())
      {
         m_logger.debug("Sending a " + ((bResponse) ? "message" : "request") +
            " on channel \"" + m_channel.getName() + "\"");
         m_logger.dump(tobj);
      }

      // TODO: Implement cookies, session state, multipart posts and AUTH_CLIENT.

      String sURL = (String)tobj.findValue(URL);

      if (sURL == null)
      {
         sURL = m_channel.getURL();

         if (sURL == null)
         {
            throw new IntegrationException("err.integration.http.noURL");
         }
      }

      try
      {
         if (m_client == null)
         {
            m_client = new HTTPClient();
         }

         m_client.setTrustedCertificate(m_channel.getTrustedCertificate());
         m_client.setReadTimeout(m_channel.getReadTimeout());
         m_client.setConnectionTimeout(m_channel.getConnectionTimeout());

         byte nAuthMode = m_channel.getAuthMode(); 

         switch (nAuthMode)
         {
            case HTTPChannel.AUTH_BASIC:
            case HTTPChannel.AUTH_NONE:
               m_client.setSPNEGOMode(HTTPClient.SPNEGO_NONE);
               break;

            case HTTPChannel.AUTH_PROACTIVE:
               m_client.setSPNEGOMode(HTTPClient.SPNEGO_NONE);
               m_client.setAuthProactive(true);
               break;

            case HTTPChannel.AUTH_CRED:
               m_client.setSPNEGOMode(HTTPClient.SPNEGO_CRED);
               break;

            case HTTPChannel.AUTH_SERVER:
               m_client.setSPNEGOMode(HTTPClient.SPNEGO_SILENT);
               break;

            case HTTPChannel.AUTH_CLIENT:
               // TODO: Implement credential propagation
               break;

            case HTTPChannel.AUTH_CERT:
               m_client.setClientCertificate(m_channel.getClientCertificate(), m_channel.getPassword());
               break;
         }

         if (nAuthMode == HTTPChannel.AUTH_BASIC
            || nAuthMode == HTTPChannel.AUTH_PROACTIVE
            || nAuthMode == HTTPChannel.AUTH_CRED)
         {
            String sUser = (String)tobj.findValue(USER);

            if (sUser == null)
            {
               if (m_credentials == null && m_channel.getUser() != null)
               {
                  m_credentials = new PasswordAuthentication(m_channel.getUser(),
                     ((m_channel.getPassword() == null) ? "" : m_channel.getPassword()).toCharArray());
               }

               m_client.setPasswordProvider(this);
            }
            else
            {
               String sPassword = (String)tobj.findValue(PASSWORD);
               final PasswordAuthentication credentials = new PasswordAuthentication(sUser,
                  ((sPassword == null) ? "" : sPassword).toCharArray());

               m_client.setPasswordProvider(new PasswordAuthenticationProvider()
               {
                  public PasswordAuthentication getPasswordAuthentication()
                  {
                     return credentials;
                  }

                  public boolean isAuthenticationDeterministic()
                  {
                     return true;
                  }
               });
            }
         }
         else
         {
            m_client.setPasswordProvider(this);
         }

         final String sProxyHost = (String)tobj.findValue(PROXY_HOST);
         Proxy proxy = m_channel.getProxy();

         if (sProxyHost != null)
         {
            final Integer proxyPort = (Integer)tobj.findValue(PROXY_PORT);

            if (proxyPort != null)
            {
               proxy = new Proxy(Proxy.Type.HTTP, (InetSocketAddress)AccessController.doPrivileged(
                  new PrivilegedAction()
                  {
                     public Object run()
                     {
                        return InetSocketAddress.createUnresolved(sProxyHost, proxyPort.intValue());
                     }
                  }
               ));
            }
         }

         m_client.setProxy(proxy);

         switch (m_channel.getProxyAuthMode())
         {
            case HTTPChannel.AUTH_BASIC:
               m_client.setProxySPNEGOMode(HTTPClient.SPNEGO_NONE);
               break;

            case HTTPChannel.AUTH_PROACTIVE:
               m_client.setProxySPNEGOMode(HTTPClient.SPNEGO_NONE);
               m_client.setProxyAuthProactive(true);
               break;

            case HTTPChannel.AUTH_CRED:
               m_client.setProxySPNEGOMode(HTTPClient.SPNEGO_CRED);
               break;

            case HTTPChannel.AUTH_SERVER:
               m_client.setProxySPNEGOMode(HTTPClient.SPNEGO_SILENT);
               break;
         }

         String sProxyUser = (String)tobj.findValue(PROXY_USER);
         String sProxyPassword;

         if (sProxyUser == null)
         {
            sProxyPassword = m_channel.getProxyPassword();
            sProxyUser = m_channel.getUser();
         }
         else
         {
            sProxyPassword = (String)tobj.findValue(PROXY_PASSWORD);
         }

         if (sProxyUser != null)
         {
            final PasswordAuthentication proxyCredentials = new PasswordAuthentication(sProxyUser,
               ((sProxyPassword == null) ? "" : sProxyPassword).toCharArray());

            m_client.setProxyPasswordProvider(new PasswordAuthenticationProvider()
            {
               public PasswordAuthentication getPasswordAuthentication()
               {
                  return proxyCredentials;
               }

               public boolean isAuthenticationDeterministic()
               {
                  return true;
               }
            });
         }

         MIMEHeaderMap headerMap = m_client.getRequestHeaders();

         headerMap.clear();
         setHeaders(headerMap, (TransferObject)tobj.findValue(HEADERS));

         if (m_channel.getAgent() != null)
         {
            headerMap.setDefault(HTTP.HEADER_USER_AGENT, m_channel.getAgent());
         }

         if (m_channel.getContentType() != null)
         {
            headerMap.setDefault(HTTP.HEADER_CONTENT_TYPE, m_channel.getContentType());
         }

         Pair req = parametrize(sURL, tobj.findValue(BODY),
            (TransferObject)tobj.findValue(PARAMETERS), headerMap);

         final Object body = req.getTail();
         String sMethod = (String)tobj.findValue(METHOD);

         if (sMethod == null)
         {
            if (body != null)
            {
               sMethod = HTTP.METHOD_POST;
            }
            else
            {
               sMethod = HTTP.METHOD_GET;
            }
         }
         else
         {
            sMethod = sMethod.toUpperCase(Locale.ENGLISH);
         }

         sURL = (String)req.getHead();
         m_context.addRPCCount(1);
         incrementCounters();

         return (TransferObject)m_client.invoke(URIUtil.parse(sURL), sMethod,
            new HTTPClient.RequestHandler()
            {
               public void handleRequest(HTTPClient client, OutputStream ostream) throws IOException
               {
                  writeBody(client, ostream, body);
               }
            },
            new HTTPClient.ResponseHandler()
            {
               public Object handleResponse(HTTPClient client, InputStream istream) throws IOException
               {
                  TransferObject tobj = null;

                  if (bResponse || m_channel.getErrorFunction() != null)
                  {
                     tobj = new TransferObject(5);
                     tobj.setClassName("HTTP");
                     tobj.setValue(STATUS, Primitive.createInteger(m_client.getResponseStatus()));

                     String sMessage = m_client.getResponseMessage();

                     if (sMessage != null && sMessage.length() != 0)
                     {
                        tobj.setValue(MESSAGE, sMessage);
                     }

                     MIMEHeaderMap headerMap = m_client.getResponseHeaders();
                     String sEncoding = getEncoding(headerMap.find(HTTP.HEADER_CONTENT_TYPE), null);

                     if (sEncoding == null)
                     {
                        sEncoding = getEncoding(m_channel.getContentType(), DEFAULT_ENCODING);
                     }

                     if (istream == null)
                     {
                        tobj.setValue(BODY, null);
                     }
                     else
                     {
                        MIMEHeader header;

                        if (m_channel.getDataType() == Primitive.BINARY ||
                           m_channel.getDataType() == null && MIMEUtil.isBinaryMIMEType(
                              ((header = headerMap.find(HTTP.HEADER_CONTENT_TYPE)) == null) ? null : header.getValue()))
                        {
                           tobj.setValue(BODY, new StreamInput(istream, sEncoding).getBinary());
                        }
                        else
                        {
                           istream = UTF8BOMIgnoreInputStream.wrap(istream, sEncoding);
                           tobj.setValue(BODY, new ReaderInput(new InputStreamReader(istream, sEncoding)).getString());
                        }
                     }

                     tobj.setValue(CHANNEL, m_channel.getName());
                     tobj.setValue(HEADERS, getHeaders(m_client.getResponseHeaders()));
                  }

                  if (m_logger.isDebugEnabled())
                  {
                     m_logger.debug("Received a response on channel \"" + m_channel.getName() + "\"");
                     m_logger.dump(tobj);
                  }

                  if (m_channel.getErrorFunction() != null)
                  {
                     if (Intrinsic.isTrue(m_context.getMachine().invoke(m_channel.getErrorFunction(), tobj, (Pair)null)))
                     {
                        fail(m_client);
                     }
                  }
                  else if (isError(m_client.getResponseStatus()))
                  {
                     fail(m_client);
                  }

                  return tobj;
               }
            });
      }
      catch (URISyntaxException e)
      {
         throw new IntegrationException("err.integration.uri", new Object[]{sURL}, e);
      }
      catch (IOException e)
      {
         throw new IntegrationException("err.integration.io", e);
      }
      catch (RuntimeException e)
      {
         if (m_client != null)
         {
            if (e instanceof AuthenticationException ||
               e instanceof CancellationException)
            {
               m_client.reset();
            }
         }

         throw new IntegrationException("err.integration.io", e);
      }
      finally
      {
         StatUtil.updateAverage(m_context, m_channel.getSenderStatPath(), Channel.STAT_AVERAGE_SEND_TIME,
               (double)(System.nanoTime() - lStartTime) / 1000000);
      }
   }

   /**
    * @see nexj.core.util.auth.PasswordAuthenticationProvider#getPasswordAuthentication()
    */
   public PasswordAuthentication getPasswordAuthentication()
   {
      return m_credentials;
   }

   /**
    * @see nexj.core.util.auth.PasswordAuthenticationProvider#isAuthenticationDeterministic()
    */
   public boolean isAuthenticationDeterministic()
   {
      return true;
   }

   /**
    * Gets the encoding from a content type header.
    * @param header The Content-Type MIME header.
    * @param sDefaultEncoding The default encoding to return.
    * @return The encoding name.
    */
   public static String getEncoding(MIMEHeader header, String sDefaultEncoding)
   {
      if (header != null && header.getFirstValue() != null)
      {
         String sEncoding = header.getFirstValue().findArg("charset");

         if (sEncoding != null)
         {
            return sEncoding;
         }
      }

      return sDefaultEncoding;
   }

   /**
    * Gets the encoding from a content type.
    * @param sContentType The content type. Can be null.
    * @param sDefaultEncoding The default encoding to return.
    * @return The encoding name.
    */
   public static String getEncoding(String sContentType, String sDefaultEncoding)
   {
      if (sContentType != null)
      {
         Matcher matcher = CHARSET_PATTERN.matcher(sContentType);

         if (matcher.matches())
         {
            return matcher.group(1);
         }
      }

      return sDefaultEncoding;
   }

   /**
    * Formats a value as a string.
    * @param value The value to format.
    * @return The formatted value.
    */
   public static String toString(Object value)
   {
      if (value instanceof Timestamp)
      {
         return HTTP.formatDateTime((Timestamp)value);
      }

      return Primitive.toString(value);
   }

   /**
    * Sets the request headers on a header map from a transfer object.
    * @param headerMap The MIME header map.
    * @param headers The transfer object with the headers. Can be null.
    */
   public static void setHeaders(MIMEHeaderMap headerMap, TransferObject headers)
   {
      if (headers != null)
      {
         for (PropertyIterator itr = headers.getIterator(); itr.hasNext();)
         {
            itr.next();
            headerMap.add(itr.getName(), toString(itr.getValue()));
         }
      }
   }

   /**
    * Gets the response headers from a header map as a transfer object.
    * @param headerMap The MIME header map.
    * @return The transfer object with the headers.
    */
   public static TransferObject getHeaders(MIMEHeaderMap headerMap)
   {
      TransferObject headers = new TransferObject(headerMap.size());

      for (int i = 0, n = headerMap.size(); i < n; ++i)
      {
         MIMEHeader header = headerMap.get(i);
         String sName = header.getName().toLowerCase(Locale.ENGLISH);
         String sValue = header.getValue();
         Object value;

         switch (HTTP.getHeaderType(sName))
         {
            case HTTP.TYPE_INTEGER:
               value = Primitive.toInteger(sValue);
               break;

            case HTTP.TYPE_DATE:
               if (sValue.length() == 0 || sValue.equals("-1"))
               {
                  value = null;
               }
               else
               {
                  value = HTTP.parseDateTime(sValue);
               }

               break;

            default:
               value = sValue;
               break;
         }

         headers.setValue(sName, value);
      }

      return headers;
   }

   /**
    * Parameterizes the HTTP URL and body.
    * @param sURL The HTTP request URL.
    * @param body The HTTP request body.
    * @param params The parameters.
    * @param headerMap The header map.
    * @return Pair (sURL . body).
    */
   public static Pair parametrize(String sURL, Object body, TransferObject params, MIMEHeaderMap headerMap) throws IOException
   {
      if (params != null)
      {
         int nQueryIndex = sURL.indexOf('?');
         int nQueryEndIndex = sURL.indexOf('#', nQueryIndex + 1);
         StringBuilder qryBuf = new StringBuilder(Math.max(sURL.length(), 64));
         int nDelimiterIndex;
         String sQuery;

         nQueryEndIndex = (nQueryEndIndex >= 0) ? nQueryEndIndex : sURL.length();

         if (nQueryIndex >= 0)
         {
            qryBuf.append(sURL, 0, nQueryIndex + 1);  // include delimiter
            sQuery = sURL.substring(nQueryIndex + 1, nQueryEndIndex);
            nDelimiterIndex = Integer.MAX_VALUE;
         }
         else
         {
            qryBuf.append(sURL, 0, nQueryEndIndex);
            sQuery = null;
            nDelimiterIndex = qryBuf.length();
         }

         StringBuilder bodyBuf = null;
         MIMEHeader contentTypeHdr = headerMap.find(HTTP.HEADER_CONTENT_TYPE);

         if (contentTypeHdr != null)
         {
            MIMEHeader.Value contentType = contentTypeHdr.getFirstValue();

            if (contentType != null && contentType.getName().equals(HTTP.ENCODING_FORM))
            {
               bodyBuf = new StringBuilder(128);
            }
         }

         appendParameters(qryBuf, bodyBuf, params, sQuery);

         // If URL parameters added, ensure URL has proper delimiter
         if (qryBuf.length() > nDelimiterIndex)
         {
            qryBuf.insert(nDelimiterIndex, '?');
         }

         // Add the fragment, if any
         qryBuf.append(sURL, nQueryEndIndex, sURL.length());

         sURL = qryBuf.toString();

         if (body == null && bodyBuf != null)
         {
            body = bodyBuf.toString();
         }
      }

      return new Pair(sURL, body);
   }

   /**
    * Appends the URL-encoded parameters to string buffers.
    * @param qryBuf The query buffer.
    * @param bodybuf The body buffer. Can be null.
    * @param params The transfer object containing the parameters.
    * @param sQuery The query string containing the parameters that should end up in the query buffer.
    *    The rest of them will be appended to the body buffer. Can be null.
    * @throws IOException if an I/O error occurs.
    */
   public static void appendParameters(StringBuilder qryBuf, StringBuilder bodyBuf,
      TransferObject params, String sQuery) throws IOException
   {
      if (sQuery == null)
      {
         sQuery = "";
      }

      if (params == null)
      {
         qryBuf.append(sQuery);
      }
      else
      {
         boolean bFirst = true;
         Set paramSet = null;
         int n = sQuery.length();

         for (int i = 0; i < n;)
         {
            int k = sQuery.indexOf('&', i);

            if (k < 0 || k > n)
            {
               k = n;
            }

            int j = sQuery.indexOf('=', i);

            if (j < 0 || j > k)
            {
               j = k;
            }

            String sName = URLDecoder.decode(sQuery.substring(i, j), "UTF-8");

            if (paramSet == null || !paramSet.contains(sName))
            {
               Object value = params.findValue(sName, Undefined.VALUE);

               if (value == Undefined.VALUE)
               {
                  if (bFirst)
                  {
                     bFirst = false;
                  }
                  else
                  {
                     qryBuf.append('&');
                  }

                  qryBuf.append(sQuery, i, k);
               }
               else
               {
                  appendParameter(qryBuf, sName, value, bFirst);
                  bFirst = false;

                  if (paramSet == null)
                  {
                     paramSet = new HashHolder(4);
                  }

                  paramSet.add(sName);
               }
            }

            i = k + 1;
         }

         List paramList = null;

         for (Iterator itr = params.getIterator(); itr.hasNext();)
         {
            String sName = (String)itr.next();

            if (paramSet == null || !paramSet.contains(sName))
            {
               if (paramList == null)
               {
                  paramList = new ArrayList(Math.max(params.getValueCount() - ((paramSet == null) ? 0 : paramSet.size()), 1));
               }

               paramList.add(sName);
            }
         }

         if (paramList != null)
         {
            if (bodyBuf == null)
            {
               bodyBuf = qryBuf;
            }
            else
            {
               bFirst = true;
            }

            Collections.sort(paramList);

            for (int i = 0, k = paramList.size(); i < k; ++i)
            {
               String sName = (String)paramList.get(i);

               appendParameter(bodyBuf, sName, params.findValue(sName), bFirst);
               bFirst = false;
            }
         }
      }
   }

   /**
    * Writes a URL-encoded parameter to a string buffer.
    * @param buf The destination string buffer.
    * @param sName The parameter name.
    * @param value The parameter value.
    * @param bFirst True if this is the first parameter.
    * @throws IOException if an I/O error occurs.
    */
   public static void appendParameter(StringBuilder buf, String sName, Object value, boolean bFirst) throws IOException
   {
      if (value instanceof List)
      {
         List list = (List)value;

         for (int i = 0, n = list.size(); i < n; ++i)
         {
            appendParameter(buf, sName, list.get(i), bFirst);
            bFirst = false;
         }
      }
      else
      {
         if (!bFirst)
         {
            buf.append('&');
         }

         buf.append(URLEncoder.encode(sName, "UTF-8"));

         if (value != null)
         {
            buf.append('=');
            buf.append(URLEncoder.encode(toString(value), "UTF-8"));
         }
      }
   }

   /**
    * Transmits the HTTP request body over an HTTP URL connection.
    * @param connection The HTTP client.
    * @param ostream The output stream.
    * @param The body value. Can be null.
    * @throws IOException if an I/O error occurs.
    */
   public static void writeBody(HTTPClient client, OutputStream ostream, Object body) throws IOException
   {
      if (body != null && ostream != null)
      {
         String sEncoding = getEncoding(client.getRequestHeaders().find(HTTP.HEADER_CONTENT_TYPE), DEFAULT_ENCODING);

         if (body instanceof String)
         {
            String sBody = (String)body;

            if (sBody.length() != 0)
            {
               Writer writer = new OutputStreamWriter(ostream, sEncoding);

               try
               {
                  IOUtil.copy(writer, new StringReader(sBody));
               }
               finally
               {
                  if (writer != null)
                  {
                     writer.close();
                  }
               }
            }
         }
         else
         {
            ObjectInput input = new ObjectInput(body);

            if (!input.isEmpty())
            {
               input.setEncoding(sEncoding);

               InputStream istream = input.getInputStream();

               try
               {
                  IOUtil.copy(ostream, istream);
               }
               finally
               {
                  IOUtil.close(istream);
               }
            }
         }
      }
   }

   /**
    * Throws an exception based on the HTTP response status.
    * @param client The HTTP client.
    */
   public static void fail(HTTPClient client) throws LoginException, RPCException
   {
      int nStatus = client.getResponseStatus();

      RuntimeException e = null;

      if (nStatus == HTTP.STATUS_UNAUTHORIZED ||
         nStatus == HTTP.STATUS_FORBIDDEN)
      {
         e = new LoginException("err.auth.login");
      }

      if (e == null || e.getCause() == null)
      {
         RPCException x = new HTTPException(nStatus, client.getResponseMessage());

         if (e == null)
         {
            e = x;
         }
         else
         {
            e.initCause(x);
         }
      }

      throw e;
   }

   /**
    * Determines if an HTTP status code is an error, from the point of view of the HTTP adapter.
    * @param nStatus The HTTP status code.
    * @return True if it is an error.
    */
   public static boolean isError(int nStatus)
   {
      return nStatus / 100 > 2 && nStatus != HTTP.STATUS_NOT_MODIFIED;
   }

   /**
    * @see nexj.core.integration.Sender#getSentCount()
    */
   public long getSentCount()
   {
      return m_sentCounter.get();
   }
}
