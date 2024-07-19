// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.soap;

import nexj.core.rpc.RequestException;
import nexj.core.util.ObjUtil;

/**
 * SOAP Fault structure.
 */
public class SOAPFault
{
   // attributes

   /**
    * The namespace URI.
    */
   protected String m_sURI;

   /**
    * The fault code.
    */
   protected String m_sCode;

   /**
    * The fault message.
    */
   protected String m_sMessage;

   // associations

   /**
    * The fault throwable.
    */
   protected Throwable m_exception;

   // constructors
   
   /**
    * Constructs the fault object.
    * @param sCode The fault code.
    * @param sMessage The fault message.
    * @param t The exception.
    */
   public SOAPFault(String sCode, String sMessage, Throwable t)
   {
      m_sCode = sCode;
      m_sMessage = sMessage;
      m_exception = t;
   }

   /**
    * Constructs the fault object.
    * @param t The exception.
    */
   public SOAPFault(Throwable t)
   {
      setException(t);
   }

   // operations

   /**
    * Sets the fault throwable.
    * @param exception The fault throwable to set.
    */
   public void setException(Throwable exception)
   {
      m_exception = exception;

      if (exception != null)
      {
         if (m_sURI == null && m_sCode == null)
         {
            m_sURI = SOAP.ENV_URI;
            m_sCode = ((exception instanceof SOAPUnmarshallerException ||
               exception instanceof RequestException) ? "Client" : "Server") +
               "." + exception.getClass().getName();
         }

         if (m_sMessage == null)
         {
            m_sMessage = ObjUtil.getMessage(exception);
         }
      }
   }

   /**
    * @return The fault throwable.
    */
   public Throwable getException()
   {
      return m_exception;
   }

   /**
    * Sets the namespace URI.
    * @param sURI The namespace URI to set.
    */
   public void setURI(String sURI)
   {
      m_sURI = sURI;
   }

   /**
    * @return The namespace URI.
    */
   public String getURI()
   {
      return m_sURI;
   }
   
   /**
    * Sets the fault code.
    * @param sCode The fault code to set.
    */
   public void setCode(String sCode)
   {
      m_sCode = sCode;
   }

   /**
    * @return The fault code.
    */
   public String getCode()
   {
      return m_sCode;
   }
   
   /**
    * Sets the fault message.
    * @param sMessage The fault message to set.
    */
   public void setMessage(String sMessage)
   {
      m_sMessage = sMessage;
   }

   /**
    * @return The fault message.
    */
   public String getMessage()
   {
      return m_sMessage;
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "SOAPFault(uri=" + m_sURI + ", code=" + m_sCode +
         ", message=" + m_sMessage + ", exception=" + m_exception + ")";
   }
}
