// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.xml;

import nexj.core.integration.IntegrationException;

/**
 * SOAP Fault exception.
 */
public class SOAPFaultException extends IntegrationException
{
   // constants

   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = -3060796667745100678L;

   // attributes

   /**
    * The fault code.
    */
   protected String m_sFaultCode;

   /**
    * The fault string.
    */
   protected String m_sFaultString;

   /**
    * The fault actor.
    */
   protected String m_sFaultActor;

   // constructors

   public SOAPFaultException()
   {
      super("err.integration.xml.soapFault", new Object[3]);
   }

   protected SOAPFaultException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   // operations

   /**
    * Sets the fault code.
    * @param sFaultCode The fault code to set.
    */
   public void setFaultCode(String sFaultCode)
   {
      m_sFaultCode = sFaultCode;
      setArgArray();
   }

   /**
    * @return The fault code.
    */
   public String getFaultCode()
   {
      return m_sFaultCode;
   }

   /**
    * Sets the fault string.
    * @param sFaultString The fault string to set.
    */
   public void setFaultString(String sFaultString)
   {
      m_sFaultString = sFaultString;
      setArgArray();
   }

   /**
    * @return The fault string.
    */
   public String getFaultString()
   {
      return m_sFaultString;
   }
   
   /**
    * Sets the fault actor.
    * @param sFaultActor The fault actor to set.
    */
   public void setFaultActor(String sFaultActor)
   {
      m_sFaultActor = sFaultActor;
      setArgArray();
   }

   /**
    * @return The fault actor.
    */
   public String getFaultActor()
   {
      return m_sFaultActor;
   }

   /**
    * Sets the error arg array from the SOAP fault information fields.
    */
   protected void setArgArray()
   {
      if (m_argArray != null && m_argArray.length == 3)
      {
         m_argArray[0] = m_sFaultCode;
         m_argArray[1] = m_sFaultString;
         m_argArray[2] = m_sFaultActor;
      }
   }
}
