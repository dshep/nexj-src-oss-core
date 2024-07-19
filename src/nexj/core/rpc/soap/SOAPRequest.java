// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.soap;

import nexj.core.rpc.Request;

/**
 * SOAP Server.invoke(Request) structure.
 */
public class SOAPRequest
{
   // associations
   
   /**
    * The server invocation request.
    */
   protected Request m_request;

   // constructors
   
   /**
    * Constructs the SOAP  request.
    * @param request The server request.
    */
   public SOAPRequest(Request request)
   {
      setRequest(request);
   }

   // operations
   
   /**
    * Sets the server invocation request.
    * @param request The server invocation request to set.
    */
   public void setRequest(Request request)
   {
      m_request = request;
   }

   /**
    * @return The server invocation request.
    */
   public Request getRequest()
   {
      return m_request;
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "SOAPRequest(request=" + m_request + ")";
   }
}
