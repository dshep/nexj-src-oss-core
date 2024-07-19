// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.http;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import nexj.core.meta.Metadata;
import nexj.core.meta.Repository;
import nexj.core.rpc.pool.PoolManager;
import nexj.core.runtime.LifecycleManager;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;
import nexj.core.util.StringUtil;

/**
 * HTTP servlet that instantiates HTTPServer to process the requests.
 */
public final class GenericHTTPServlet extends HttpServlet
{
   // constants

   /**
    * Serialization version.
    */
   private final static long serialVersionUID = 7547851653345925941L;

   // attributes

   /**
    * Default server component name.
    */
   protected String m_sDefaultServerName;

   // associations

   /**
    * Method name to server component name map: String[String].
    */
   protected Lookup m_methodMap;

   // operations

   /**
    * @see javax.servlet.GenericServlet#init()
    */
   public void init() throws ServletException
   {
      try
      {
         LifecycleManager.getLifecycle().startup();
      }
      catch (Exception e)
      {
         throw new ServletException(ObjUtil.getMessage(e), e);
      }

      Metadata metadata = Repository.getMetadata();

      for (Enumeration enm = getInitParameterNames(); enm.hasMoreElements();)
      {
         String sName = (String)enm.nextElement();
         String sComponentName = getInitParameter(sName);

         if (!StringUtil.isEmpty(sComponentName))
         {
            // Fail init() if component does not exist
           metadata.getComponent(sComponentName);

            if (sName.equals("*"))
            {
               m_sDefaultServerName = sComponentName;
            }
            else
            {
               if (m_methodMap == null)
               {
                  m_methodMap = new HashTab(2);
               }

               sName = sName.toUpperCase(Locale.ENGLISH);
               m_methodMap.put(sName, sComponentName);

               if (sName.equals("GET") && !m_methodMap.contains("HEAD"))
               {
                  m_methodMap.put("HEAD", sComponentName);
               }
            }
         }
      }
   }

   /**
    * @see javax.servlet.GenericServlet#destroy()
    */
   public void destroy()
   {
      m_methodMap = null;
      m_sDefaultServerName = null;
   }

   /**
    * @see javax.servlet.http.HttpServlet#doOptions(javax.servlet.http.HttpServletRequest, javax.servlet.http.HttpServletResponse)
    */
   protected void doOptions(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
   {
      if (!StringUtil.isEmpty(m_sDefaultServerName))
      {
         response.setHeader("Allow", "DELETE, GET, HEAD, OPTIONS, POST, PUT, TRACE");
      }
      else if (m_methodMap != null)
      {
         List methodList = new ArrayList(m_methodMap.size() + 2);

         for (Iterator itr = m_methodMap.iterator(); itr.hasNext();)
         {
            methodList.add(itr.next());
         }

         if (!m_methodMap.contains("OPTIONS"))
         {
            methodList.add("OPTIONS");
         }

         if (!m_methodMap.contains("TRACE"))
         {
            methodList.add("TRACE");
         }

         Collections.sort(methodList);

         StringBuffer buf = new StringBuffer(methodList.size() * 8);

         for (int i = 0, n = methodList.size(); i != n; ++i)
         {
            if (i != 0)
            {
               buf.append(", ");
            }

            buf.append(methodList.get(i));
         }

         response.setHeader("Allow", buf.toString());
      }

      response.setContentLength(0);
   }

   /**
    * @see javax.servlet.http.HttpServlet#service(javax.servlet.http.HttpServletRequest, javax.servlet.http.HttpServletResponse)
    */
   protected void service(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
   {
      String sComponentName;

      if (m_methodMap != null)
      {
         sComponentName = (String)m_methodMap.get(request.getMethod());

         if (sComponentName == null)
         {
            sComponentName = m_sDefaultServerName;
         }
      }
      else
      {
         sComponentName = m_sDefaultServerName;
      }

      if (!StringUtil.isEmpty(sComponentName))
      {
         Metadata metadata;
         PoolManager poolManager;

         synchronized (Repository.class)
         {
            metadata = Repository.getMetadata();
            poolManager = PoolManager.getInstance(metadata);
            poolManager.reference();
         }

         try
         {
            ((HTTPServer)metadata.getComponent(sComponentName).getInstance(null)).invoke(this, request, response);
         }
         finally
         {
            poolManager.release();
         }
      }
      else
      {
         super.service(request, response);
      }
   }
}
