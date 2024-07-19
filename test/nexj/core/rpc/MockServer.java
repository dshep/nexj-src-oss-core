// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc;

import nexj.core.rpc.Request;
import nexj.core.rpc.Response;
import nexj.core.rpc.Server;

/**
 * Mock server interface.
 */
public class MockServer implements Server
{
   /**
    * The request.
    */
   protected Request m_request;

   /**
    * The response.
    */
   protected Response m_response;

   // constructors

   /**
    * Constructs the server with an empty response. 
    */
   public MockServer()
   {
      m_response = new Response();
   }

   /**
    * Constructs the server.
    * @param response The response. 
    */
   public MockServer(Response response)
   {
      m_response = response;
   }

   /**
    * Constructs the server.
    * @param results The response results.
    */
   public MockServer(Object[] results)
   {
      setResults(results);
   }

   // operations

   /**
    * @return The request.
    */
   public Request getRequest()
   {
      return m_request;
   }

   /**
    * Sets the response.
    * @param response The response to set.
    */
   public void setResponse(Response response)
   {
      m_response = response;
   }

   /**
    * Sets a response with the specified results.
    * @param results The response results. 
    */
   public void setResults(Object[] results)
   {
      m_response = new Response();

      for (int i = 0; i < results.length; ++i)
      {
         m_response.addResult(results[i]);
      }
   }
   
   /**
    * Resets the server.
    */
   public void reset()
   {
      m_request = null;
   }
   
   /**
    * @see nexj.core.rpc.Server#invoke(nexj.core.rpc.Request)
    */
   public Response invoke(Request request)
   {
      m_request = request;

      return m_response;
   }
}
