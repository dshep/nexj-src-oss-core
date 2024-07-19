// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.soap;

import nexj.core.rpc.Response;

/**
 * SOAP Response Server.invoke(Request) return structure
 */
public class SOAPResponse
{
   // response
   
   /**
    * The server response.
    */
   protected Response m_response;

   // constructors

   /**
    * Constructs the SOAP response.
    * @param response The server response. 
    */
   public SOAPResponse(Response response)
   {
      setResponse(response);
   }

   // operations
   
   /**
    * Sets the server response.
    * @param response The server response to set.
    */
   public void setResponse(Response response)
   {
      m_response = response;
   }

   /**
    * @return The server response.
    */
   public Response getResponse()
   {
      return m_response;
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "SOAPResponse(response=" + m_response + ")";
   }
}
