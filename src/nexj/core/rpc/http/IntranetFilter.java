// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.http;

import java.io.IOException;
import java.net.InetAddress;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletResponse;

import nexj.core.util.StringUtil;
import nexj.core.util.SysUtil;

/**
 * Filter for restricting the traffic to the intranet.
 */
public class IntranetFilter implements Filter
{
   // attributes
   
   /**
    * Flag to enable the filter.
    */
   protected boolean m_bEnabled;
   
   // operations

   /**
    * @see javax.servlet.Filter#init(javax.servlet.FilterConfig)
    */
   public void init(FilterConfig config) throws ServletException
   {
      m_bEnabled = StringUtil.parseBoolean(SysUtil.getConfigProperties().getProperty("jnlpPrivate", "true"));
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
   public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException
   {
      if (m_bEnabled)
      { 
         InetAddress addr = InetAddress.getByName(request.getRemoteAddr());
         
         if (addr.isSiteLocalAddress() || addr.isLoopbackAddress() || addr.isLinkLocalAddress())
         {
            chain.doFilter(request, response);
         }
         else
         {
            ((HttpServletResponse)response).sendError(HttpServletResponse.SC_NOT_FOUND);
         }
      }
      else
      {
         chain.doFilter(request, response);
      }
   }
}
