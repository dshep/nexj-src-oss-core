// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.http;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLEncoder;
import java.util.Locale;

import javax.servlet.ServletException;

import nexj.core.runtime.Initializable;
import nexj.core.util.HashHolder;
import nexj.core.util.Holder;
import nexj.core.util.IOUtil;
import nexj.core.util.J2EEUtil;
import nexj.core.util.NetUtil;
import nexj.core.util.StringUtil;
import nexj.core.util.SubstReader;
import nexj.core.util.SysUtil;
import nexj.core.util.URLUtil;

/**
 * HTTP server that performs template substitution.
 */
public class TemplateHTTPServer extends GenericHTTPServer implements Initializable
{
   // attributes

   /**
    * The output stream encoding.
    */
   protected String m_sEncoding = ENCODING;

   /**
    * The output stream MIME type.
    */
   protected String m_sMIMEType;

   /**
    * The request URL.
    */
   protected String m_sURL;

   /**
    * The RPC protocol.
    */
   protected String m_sProtocol = "text";

   /**
    * The template path, relative to the context path.
    */
   protected String m_sTemplate;

   /**
    * The host name.
    */
   protected String m_sHost;

   /**
    * The servlet URL.
    */
   protected String m_sServlet;

   /**
    * The application name.
    */
   protected String m_sApplication;

   /**
    * The application caption.
    */
   protected String m_sCaption;

   /**
    * The application icon.
    */
   protected String m_sIcon;

   // associations

   /**
    * Set of paths to ignore.
    */
   protected Holder m_ignoreSet;

   // operations
   
   /**
    * Sets the output stream encoding.
    * @param sEncoding The output stream encoding to set.
    */
   public void setEncoding(String sEncoding)
   {
      m_sEncoding = StringUtil.trimToNull(sEncoding);
   }

   /**
    * @return The output stream encoding.
    */
   public String getEncoding()
   {
      return m_sEncoding;
   }
   
   /**
    * Sets the output stream MIME type.
    * @param sMIMEType The output stream MIME type to set.
    */
   public void setMIMEType(String sMIMEType)
   {
      m_sMIMEType = StringUtil.trimToNull(sMIMEType);
   }

   /**
    * @return The output stream MIME type.
    */
   public String getMIMEType()
   {
      return m_sMIMEType;
   }
   
   /**
    * Sets the request URL.
    * @param sURL The request URL to set.
    */
   public void setURL(String sURL)
   {
      m_sURL = sURL;
   }

   /**
    * @return The request URL.
    */
   public String getURL()
   {
      return m_sURL;
   }
   
   /**
    * Sets the RPC protocol.
    * @param sProtocol The RPC protocol to set.
    */
   public void setProtocol(String sProtocol)
   {
      m_sProtocol = StringUtil.trimToNull(sProtocol);
   }

   /**
    * @return The RPC protocol.
    */
   public String getProtocol()
   {
      return m_sProtocol;
   }

   /**
    * Sets the template path, relative to the context path.
    * @param sTemplate The template path, relative to the context path to set.
    */
   public void setTemplate(String sTemplate)
   {
      m_sTemplate = StringUtil.trimToNull(sTemplate);
   }

   /**
    * @return The template path, relative to the context path.
    */
   public String getTemplate()
   {
      return m_sTemplate;
   }

   /**
    * @return Returns the servlet.
    */
   public String getServlet()
   {
      return m_sServlet;
   }

   /**
    * @param servlet The servlet to set.
    */
   public void setServlet(String servlet)
   {
      m_sServlet = servlet;
   }
   
   /**
    * Adds a path to the ignore set.
    */
   public void addIgnore(String sPath)
   {
      if (m_ignoreSet == null)
      {
         m_ignoreSet = new HashHolder();
      }
      
      m_ignoreSet.add(sPath);
   }

   /**
    * @see nexj.core.runtime.Initializable#initialize()
    */
   public void initialize() throws Exception
   {
      if (getAddress() != null)
      {
         try
         {
            m_sHost = new URI(getAddress()).getHost();
         }
         catch (URISyntaxException e)
         {
         }
      }
   }

   /**
    * @see nexj.core.rpc.http.GenericHTTPServer#invoke()
    */
   protected void invoke() throws ServletException, IOException
   {
      String sServletPath = m_request.getServletPath();
      InputStream istream = null;
      Reader reader = null;
      boolean bIgnored = (m_ignoreSet != null && m_ignoreSet.contains(sServletPath));

      if (m_sTemplate != null && !bIgnored)
      {
         int i = m_sTemplate.indexOf("${platform}");

         if (i >= 0)
         {
            m_sTemplate = m_sTemplate.substring(0, i) +
               J2EEUtil.getPlatformName().toLowerCase(Locale.ENGLISH) +
               m_sTemplate.substring(i + 11);
         }
      }
      else
      {
         m_sTemplate = sServletPath;
      }

      m_sURL = m_request.getRequestURL().toString();
      m_sApplication = (bIgnored) ? getParameter("application") : sServletPath;

      if (m_sApplication != null)
      {
         if (m_sApplication.startsWith("/"))
         {
            m_sApplication = m_sApplication.substring(1);
         }

         int i = m_sApplication.indexOf('/');
         
         if (i >= 0)
         {
            m_sApplication = m_sApplication.substring(0, i);
         }
         else
         {
            i = m_sApplication.lastIndexOf('.');

            if (i >= 0)
            {
               m_sApplication = m_sApplication.substring(0, i);
            }
         }
      }

      m_sCaption = (bIgnored) ? getParameter("caption") : null;
      m_sIcon = (bIgnored) ? getParameter("icon") : null;
      m_sServlet = sServletPath.substring(1);

      if (m_sHost == null)
      {
         m_sHost = m_request.getServerName();
      }
      
      try
      {
         if (!m_bHeadersOnly)
         {
            if (m_sTemplate.startsWith("/"))
            {
               URL url = m_servlet.getServletContext().getContext(m_context.getMetadata().getHTTPContextRoot()).getResource(m_sTemplate);

               if (url == null)
               {
                  throw new IOException("Resource \"" + m_sTemplate + "\" not found");
               }

               istream = URLUtil.openStream(url);
            }
            else
            {
               istream = URLUtil.openResource(getClass(), m_sTemplate);
            }

            reader = getTemplateReader(istream);
         }
   
         if (m_sMIMEType == null)
         {
            m_sMIMEType = m_servlet.getServletContext().getMimeType(m_sTemplate);
   
            if (m_sMIMEType == null)
            {
               m_sMIMEType = "text/html";
            }
         }

         setHeaders();

         if (!m_bHeadersOnly)
         {
            IOUtil.copy(m_response.getWriter(), reader);
         }
      }
      finally
      {
         IOUtil.close(reader);
         IOUtil.close(istream);
      }
   }

   /**
    * Sets the response headers.
    */
   protected void setHeaders()
   {
      m_response.setContentType(m_sMIMEType + "; charset=" + m_sEncoding);
      m_response.setDateHeader("Last-Modified", SysUtil.BUILD_TIME);
      HTTPUtil.setCached(m_response, true);
   }

   /**
    * Gets a reader for the given URL connection.
    * @param istream The input stream.
    */
   protected final Reader getReader(InputStream istream) throws IOException
   {
      return new InputStreamReader(new BufferedInputStream(istream), m_sEncoding);
   }

   /**
    * Gets a reader for a given template URL connection.
    * @param istream The input stream.
    */
   protected Reader getTemplateReader(InputStream istream) throws IOException
   {
      return new TemplateReader(getReader(istream));
   }

   /**
    * Substitutes the variables in a string.
    * @param sText The string in which to substitute the variables.
    * @return The expanded string.
    */
   protected String substitute(String sText) throws IOException
   {
      StringWriter writer = new StringWriter(sText.length());
      
      IOUtil.copy(writer, new TemplateReader(new StringReader(sText)));
      
      return writer.toString();
   }
   
   /**
    * Generate a unique application instance ID.
    * @return A new application ID.
    */
   public static String generateAppInstanceId()
   {
      return Long.toString(System.currentTimeMillis());
   }
   
   // inner classes

   protected class TemplateReader extends SubstReader
   {
      public TemplateReader(Reader reader)
      {
         super(reader);
      }

      protected String getValue(String sName) throws IOException
      {
         if (sName.length() > 0 && sName.charAt(0) == '*')
         {
            String value = expand(sName.substring(1));

            if (value != null)
            {
               value = URLEncoder.encode(value, m_sEncoding);
            }

            return value;
         }

         return expand(sName);
      }

      private String expand(String sName) throws IOException
      {
         if (sName.length() != 0)
         {
            switch (sName.charAt(0))
            {
               case 'a':
                  if (sName.equals("anonroot"))
                  {
                     return getAnonymousRoot();
                  }
                  else if (sName.equals("application"))
                  {
                     return m_sApplication;
                  }
                  else if (sName.equals("appInstanceId"))
                  {
                     return generateAppInstanceId(); 
                  }

                  break;
                  
               case 'c':
                  if (sName.equals("cacheCookie"))
                  {
                     return getCacheCookie();
                  }
                  else if (sName.equals("caption"))
                  {
                     return m_sCaption;
                  }

                  break;

               case 'e':
                  if (sName.equals("encoding"))
                  {
                     return m_sEncoding;
                  }

                  break;

               case 'h':
                  if (sName.equals("host"))
                  {
                     return m_sHost;
                  }

                  break;
                  
               case 'i':
                  if (sName.startsWith("if:"))
                  {
                     int i = sName.indexOf(':', 3);

                     if (i > 0 && isTrue(sName.substring(3, i)))
                     {
                        return substitute(sName.substring(i + 1));
                     }
                  }
                  else if (sName.startsWith("ifnot:"))
                  {
                     int i = sName.indexOf(':', 6);

                     if (i > 0 && !isTrue(sName.substring(6, i)))
                     {
                        return substitute(sName.substring(i + 1));
                     }
                  }
                  else if (sName.equals("icon"))
                  {
                     return m_sIcon;
                  }

                  break;

               case 'n':
                  if (sName.equals("node"))
                  {
                     return NetUtil.getNodeName();
                  }

                  break;
                  
               case 'p':
                  if (sName.startsWith("property:"))
                  {
                     int i = sName.indexOf(':', 9);

                     if (i < 0)
                     {
                        return SysUtil.getConfigProperties().getProperty(sName.substring(9));
                     }

                     return SysUtil.getConfigProperties().getProperty(sName.substring(9, i), substitute(sName.substring(i + 1)));
                  }

                  if (sName.equals("protocol"))
                  {
                     return m_sProtocol;
                  }

                  break;

               case 'q':
                  if (sName.startsWith("quote:"))
                  {
                     return sName.substring(6);
                  }

                  if (sName.equals("query"))
                  {
                     return m_request.getQueryString();
                  }

                  break;

               case 'r':
                  if (sName.equals("root"))
                  {
                     return getRoot();
                  }

                  break;

               case 's':
                  if (sName.equals("servlet"))
                  {
                     return m_sServlet;
                  }

                  if (sName.equals("server"))
                  {
                     return getRoot();
                  }

                  break;

               case 'u':
                  if (sName.equals("url"))
                  {
                     return m_sURL;
                  }

                  break;
            }
         }

         return null;
      }
      
      /**
       * Returns the boolean value of a string expression of the form "system.boolean.property" or of the
       * form "system.property=string".
       * @param String sExpr The expression.
       * @return boolean The value of the expression.
       */
      protected boolean isTrue(String sExpr)
      {
         int i = sExpr.indexOf('=');

         if (i < 0)
         {
            return StringUtil.parseBoolean(SysUtil.getConfigProperties().getProperty(sExpr, "false"));
         }

         return SysUtil.getConfigProperties().getProperty(sExpr.substring(0, i), "").equals(sExpr.substring(i + 1));
      }
   }
}
