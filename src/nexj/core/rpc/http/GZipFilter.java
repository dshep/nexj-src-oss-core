// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.http;

import java.io.IOException;
import java.util.Enumeration;
import java.util.Set;
import java.util.StringTokenizer;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import nexj.core.util.HTTP;
import nexj.core.util.HashHolder;
import nexj.core.util.StringUtil;
import nexj.core.util.SysUtil;

/**
 * GZip compression filter.
 */
public class GZipFilter implements Filter
{

   // constants
   /**
    * Header to force GZip compression.
    */
   public final static String HEADER_COMPRESSION = "X-Compress";

   
   // attributes

   /**
    * Flag to enable the filter.
    */
   protected boolean m_bEnabled;

   // associations

   /**
    * Set of pre-gzipped paths.
    */
   protected Set m_gzippedSet;

   // operations

   /**
    * @return True if the HTTP compression is enabled.
    */
   public static boolean isCompressionEnabled()
   {
      return StringUtil.parseBoolean(SysUtil.getConfigProperties().getProperty("httpGZip", "false"));
   }
   
   /**
    * @see javax.servlet.Filter#init(javax.servlet.FilterConfig)
    */
   public void init(FilterConfig config) throws ServletException
   {
      m_bEnabled = isCompressionEnabled();
      
      String sPaths = config.getInitParameter("gzipped");
      
      if (sPaths != null)
      {
         m_gzippedSet = new HashHolder();
         
         for (StringTokenizer tokenizer = new StringTokenizer(sPaths, ","); tokenizer.hasMoreTokens();)
         {
            m_gzippedSet.add(tokenizer.nextToken().trim());
         }
      }
   }

   /**
    * @see javax.servlet.Filter#destroy()
    */
   public void destroy()
   {
   }

   /**
    * @see javax.servlet.Filter#doFilter(javax.servlet.ServletRequest, javax.servlet.ServletResponse, javax.servlet.FilterChain)
    */
   public void doFilter(ServletRequest servletRequest, ServletResponse servletResponse, FilterChain chain) throws IOException, ServletException
   {
      HttpServletRequest request = (HttpServletRequest)servletRequest;
      HttpServletResponse response = (HttpServletResponse)servletResponse;

      if (m_gzippedSet != null &&
         (m_gzippedSet.contains(request.getServletPath()) ||
          request.getServletPath().length() == 0 && request.getPathInfo() != null &&
          m_gzippedSet.contains(request.getPathInfo())))
      {
         response.addHeader(HTTP.HEADER_CONTENT_ENCODING, "gzip");
         chain.doFilter(request, response);
      }
      else if (m_bEnabled)
      {
         doFilter(request, response, chain);
      }
      else
      {
         String sUserAgent = request.getHeader(HTTP.HEADER_USER_AGENT);

         if (sUserAgent != null && sUserAgent.endsWith(" micro"))
         {
            doFilter(request, response, chain);
         }
         else if (request.getHeader(HEADER_COMPRESSION) != null)
         {
            doFilter(request, response, chain);
         }
         else
         {
            chain.doFilter(request, response);
         }
      }
   }

   /**
    * HTTP version of the above method.
    * @see GZipFilter#doFilter(ServletRequest, ServletResponse, FilterChain)
    */
   protected void doFilter(HttpServletRequest request, HttpServletResponse response, FilterChain chain)  throws IOException, ServletException
   {
      Enumeration e = request.getHeaders(HTTP.HEADER_ACCEPT_ENCODING);

      if (e != null)
      {
         while (e.hasMoreElements())
         {
            if (((String)e.nextElement()).indexOf("gzip") >= 0)
            {
               GZipHttpServletResponseWrapper wrapper = new GZipHttpServletResponseWrapper(request, response);

               chain.doFilter(request, wrapper);
               wrapper.finish();

               return;
            }
         }
      }

      chain.doFilter(request, response);
   }
}
