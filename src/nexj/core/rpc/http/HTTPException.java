// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.http;

import nexj.core.meta.Primitive;
import nexj.core.rpc.RPCException;

/**
 * Exception indicating an HTTP error.
 */
public class HTTPException extends RPCException
{
   // constants
   
   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = 4209487337729135324L;

   // attributes

   /**
    * The HTTP response code.
    */
   protected int m_nCode;

   /**
    * The HTTP server message.
    */
   protected String m_sMessage;

   // constructors
   
   /**
    * Constructs the exception.
    * @param nCode The HTTP response code.
    * @param sMessage The HTTP server message.
    */
   public HTTPException(int nCode, String sMessage)
   {
      super("err.rpc.http.code", new Object[]{Primitive.createInteger(nCode), sMessage});
      m_nCode = nCode;
      m_sMessage = sMessage;
   }

   // operations

   /**
    * Sets the HTTP response code.
    * @param nStatus The HTTP response code to set.
    */
   public void setCode(int nStatus)
   {
      m_nCode = nStatus;
   }

   /**
    * @return The HTTP response code.
    */
   public int getCode()
   {
      return m_nCode;
   }
   
   /**
    * Sets the HTTP server message.
    * @param sMessage The HTTP server message to set.
    */
   public void setServerMessage(String sMessage)
   {
      m_sMessage = sMessage;
   }

   /**
    * @return The HTTP server message.
    */
   public String getServerMessage()
   {
      return m_sMessage;
   }
}
