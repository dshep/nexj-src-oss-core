// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.soap;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import nexj.core.rpc.CharacterStreamServer;
import nexj.core.rpc.Preprocessor;
import nexj.core.rpc.RPCException;
import nexj.core.rpc.RPCUtil;
import nexj.core.rpc.Request;
import nexj.core.rpc.RequestException;
import nexj.core.rpc.Response;
import nexj.core.rpc.Server;
import nexj.core.rpc.ServerException;
import nexj.core.rpc.http.HTTPServer;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.util.Logger;

/**
 * The SOAP request server. 
 */
public class SOAPServer implements HTTPServer, CharacterStreamServer, InvocationContextAware
{
   // associations

   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   /**
    * The generic server.
    */
   protected Server m_server;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(SOAPServer.class);

   // operations

   /**
    * @see nexj.core.rpc.http.HTTPServer#invoke(javax.servlet.http.HttpServlet, javax.servlet.http.HttpServletRequest, javax.servlet.http.HttpServletResponse)
    */
   public void invoke(HttpServlet servlet, HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
   {
      String sAction = request.getHeader("SOAPAction");

      if (sAction == null)
      {
         throw new RequestException("err.rpc.soap.missingActionHeader");
      }

      int i = 0;
      int k = sAction.length();

      if (i < k && sAction.charAt(i) == '"')
      {
         ++i;
      }

      if (k > i && sAction.charAt(k - 1) == '"')
      {
         --k;
      }
      
      sAction = sAction.substring(i, k);

      if (!sAction.equals("Server#invoke"))
      {
         throw new RequestException("err.rpc.soap.action", new Object[]{sAction});
      }
   }

   /**
    * @see nexj.core.rpc.CharacterStreamServer#invoke(java.io.Reader, java.io.Writer, nexj.core.rpc.Preprocessor)
    */
   public void invoke(Reader reader, Writer writer, Preprocessor preprocessor) throws RPCException
   {
      try
      {
         Request request = ((SOAPRequest)new SOAPUnmarshaller(m_context).deserialize(reader)).getRequest();

         if (preprocessor != null)
         {
            preprocessor.preprocess(request);
         }

         validateRequest(request);

         Response response = m_server.invoke(request);

         new SOAPMarshaller(m_context).serialize(new SOAPResponse(response), writer);
      }
      catch (Throwable t)
      {
         try
         {
            new SOAPMarshaller(m_context).serialize(new SOAPFault(
               RPCUtil.handleException(t, RPCUtil.isSystem(t), "SOAP", s_logger)), writer);
         }
         catch (RPCException e)
         {
            throw e;
         }
         catch (Exception e)
         {
            throw new ServerException("err.rpc.soap.serialize", e);
         }
      }
   }

   /**
    * Template method to validate a request.
    * @param request The request object.
    * @throws RPCException if the request is invalid.
    */
   protected void validateRequest(Request request) throws RPCException
   {
   }

   /**
    * Sets the invocation context.
    * @param context The invocation context to set.
    */
   public void setInvocationContext(InvocationContext context)
   {
      m_context = context;
   }

   /**
    * @return The invocation context.
    */
   public InvocationContext getInvocationContext()
   {
      return m_context;
   }

   /**
    * Sets the generic server.
    * @param server The generic server to set.
    */
   public void setServer(Server server)
   {
      m_server = server;
   }

   /**
    * @return The generic server.
    */
   public Server getServer()
   {
      return m_server;
   }
}
