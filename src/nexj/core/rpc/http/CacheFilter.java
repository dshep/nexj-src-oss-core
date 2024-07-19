// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.http;

import java.io.IOException;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletResponse;

/**
 * Filter adding caching headers to the responses.
 */
public class CacheFilter implements Filter
{
   /**
    * @see javax.servlet.Filter#init(javax.servlet.FilterConfig)
    */
   public void init(FilterConfig config) throws ServletException
   {
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
   public void doFilter(ServletRequest request, ServletResponse servletResponse, FilterChain chain) throws IOException, ServletException
   {
      HttpServletResponse response = (HttpServletResponse)servletResponse;

      if (request.getParameter("-") != null)
      {
         // Expires after 10 years
         HTTPUtil.setCached(response, true);
         response.setDateHeader("Expires", System.currentTimeMillis() + 10 * 365 * 24 * 60 * 60 * 1000L);
      }

      chain.doFilter(request, response);
   }
}
