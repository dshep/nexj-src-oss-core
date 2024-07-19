// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.http;

import java.io.IOException;
import java.io.StringReader;
import java.io.Writer;
import java.net.URLDecoder;

import javax.servlet.ServletException;

import nexj.core.meta.Component;
import nexj.core.rpc.CharacterStreamServer;
import nexj.core.rpc.Preprocessor;
import nexj.core.rpc.RPCUtil;
import nexj.core.rpc.Request;
import nexj.core.rpc.RequestException;
import nexj.core.rpc.TransferObject;
import nexj.core.util.Lookup;
import nexj.core.util.URLWriter;

/**
 * HTTP server for processing character streams.
 */
public class CharacterStreamHTTPServer extends GenericHTTPServer
{
   // attributes

   /**
    * The content MIME type and encoding.
    */
   protected String m_sContentType = "text/xml; charset=UTF-8";

   /**
    * True if the response should be URL-encoded.
    */
   protected boolean m_bURLEncoded;

   // associations

   /**
    * The character stream server.
    */
   protected Component m_server;

   // operations

   /**
    * Sets the content MIME type and encoding.
    * @param sContentType The content MIME type and encoding to set.
    */
   public void setContentType(String sContentType)
   {
      m_sContentType = sContentType;
   }

   /**
    * @return The content MIME type and encoding.
    */
   public String getContentType()
   {
      return m_sContentType;
   }

   /**
    * Sets the character stream server component.
    * @param server The character stream server component to set.
    */
   public void setServer(Component server)
   {
      m_server = server;
   }

   /**
    * @return The character stream server component.
    */
   public Component getServer()
   {
      return m_server;
   }

   /**
    * @see nexj.core.rpc.http.GenericHTTPServer#invoke()
    */
   protected void invoke() throws ServletException, IOException
   {
      RPCUtil.checkPrivilege(m_context);

      final Lookup paramMap = (isMultipart()) ? getMultipartParameters(null, 0) : null;

      m_bURLEncoded = (getParameter("url-encoded") != null || paramMap != null && paramMap.get("url-encoded") != null);
      m_response.setContentType((m_bURLEncoded) ? "text/plain; charset=UTF-8" : m_sContentType);

      CharacterStreamServer server = (CharacterStreamServer)m_server.getInstance(m_context);

      if (server instanceof HTTPServer)
      {
         ((HTTPServer)server).invoke(m_servlet, m_request, m_response);
      }

      if (paramMap != null)
      {
         Object req = paramMap.get("request");

         if (!(req instanceof String[]) || ((String[])req).length != 1)
         {
            throw new RequestException("err.rpc.attachment");
         }

         req = ((String[])req)[0];

         if (m_bURLEncoded)
         {
            req = URLDecoder.decode((String)req, m_request.getCharacterEncoding());
         }

         // Enhance the request with data from parameters "file-<argIndex>-<dataAttr>-<fileNameAttr>"
         server.invoke(new StringReader((String)req), getWriter(), new Preprocessor()
         {
            public void preprocess(Request request)
            {
               addMultipartData(request, paramMap);
            }
         });
      }
      else
      {
         server.invoke(getReader(), getWriter(), null);
      }
   }

   /**
    * @return A response writer.
    */
   protected Writer getWriter() throws IOException
   {
      Writer writer = m_response.getWriter();

      if (m_bURLEncoded)
      {
         writer = new URLWriter(writer);
      }

      return writer;
   }

   /**
    * Enhances the request with multipart data.
    * @param request The request to enhance.
    * @param paramMap The multipart parameter map.
    */
   public static void addMultipartData(Request request, Lookup paramMap)
   {
      for(Lookup.Iterator itr = paramMap.iterator(); itr.hasNext();)
      {
         String sName = (String)itr.next();

         if (sName.startsWith("file-") && sName.indexOf('.') < 0)
         {
            int i = sName.indexOf('-', 5);

            if (i > 0)
            {
               int j = sName.indexOf('-', i + 1);

               if (j < 0)
               {
                  j = sName.length();
               }

               TransferObject tobj = request.getObject(Integer.parseInt(sName.substring(5, i)));

               tobj.setValue(sName.substring(i + 1, j), itr.getValue());

               if (j < sName.length())
               {
                  tobj.setValue(sName.substring(j + 1), paramMap.get(sName + ".filename"));
               }
            }
         }
      }
   }
}
