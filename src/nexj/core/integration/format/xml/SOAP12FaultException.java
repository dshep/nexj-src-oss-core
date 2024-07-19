package nexj.core.integration.format.xml;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import nexj.core.util.EmptyIterator;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;

/**
 * SOAP 1.2 Fault exception.
 */
public class SOAP12FaultException extends SOAPFaultException
{
   // constants

   /**
    * Serialization UID.
    */
   private static final long serialVersionUID = -1806670671836041987L;

   // attributes

   /**
    * The role of the node that reported the exception.
    */
   protected String m_sRole;

   // associations

   /**
    * Human-readable error message for the fault. Map of language name to fault reason.
    */
   protected Lookup m_langReasonMap; // of type String[String]

   /**
    * The fault sub-codes.
    */
   protected List m_subcodeList; // of type String[]

   // constructors

   public SOAP12FaultException()
   {
      super("err.integration.xml.soap12Fault", new Object[4]);
   }

   // operations

   /**
    * @return The list of fault sub-codes.
    */
   public List getFaultSubcodeList()
   {
      return (m_subcodeList == null) ? Collections.EMPTY_LIST : m_subcodeList;
   }

   /**
    * Adds a new fault sub-code.
    * @param sSubcode The sub-code to add.
    */
   public void appendFaultSubcode(String sSubcode)
   {
      if (m_subcodeList == null)
      {
         m_subcodeList = new ArrayList();
      }

      m_subcodeList.add(sSubcode);
   }

   /**
    * Sets the fault reason for a specific language.
    * @param sLang The language name.
    * @param sReason The fault reason.
    */
   public void setReason(String sLang, String sReason)
   {
      if (m_langReasonMap == null)
      {
         m_langReasonMap = new HashTab();
         m_sFaultString = sReason;
         setArgArray();
         setValue("reasonMap", m_langReasonMap);
      }

      m_langReasonMap.put(sLang, sReason);
   }

   /**
    * Gets the fault reason in the given language.
    * @param sLang The language name.
    * @return The fault reason; null if not found.
    */
   public String getReason(String sLang)
   {
      if (m_langReasonMap == null)
      {
         return null;
      }

      return (String)m_langReasonMap.get(sLang);
   }

   /**
    * @return An iterator over the languages for which a fault reason has been specified.
    */
   public Iterator getReasonLangIterator()
   {
      if (m_langReasonMap == null)
      {
         return EmptyIterator.getInstance();
      }

      return m_langReasonMap.iterator();
   }

   /**
    * Sets the source of the fault.
    * @param sNode A URI.
    */
   public void setNode(String sNode)
   {
      m_sFaultActor = sNode;
      setArgArray();
   }

   /**
    * Gets the source of the fault.
    * @return A URI.
    */
   public String getNode()
   {
      return m_sFaultActor;
   }

   /**
    * Sets the role of the node that reported the exception.
    * @param sRole A URI.
    */
   public void setRole(String sRole)
   {
      m_sRole = sRole;
      setArgArray();
   }

   /**
    * Gets the role of the node that reported the exception.
    * @return A URI.
    */
   public String getRole()
   {
      return m_sRole;
   }

   /**
    * @see nexj.core.integration.format.xml.SOAPFaultException#setArgArray()
    */
   protected void setArgArray()
   {
      if (m_argArray != null && m_argArray.length == 4)
      {
         m_argArray[0] = m_sFaultCode;
         m_argArray[1] = m_sFaultString;
         m_argArray[2] = m_sFaultActor;
         m_argArray[3] = m_sRole;
      }
   }

   /**
    * @see nexj.core.integration.format.xml.SOAPFaultException#setFaultActor(java.lang.String)
    */
   public void setFaultActor(String sFaultActor)
   {
      throw new IllegalStateException("Cannot set fault actor on a SOAP 1.2 exception, use setNode instead");
   }

   /**
    * @see nexj.core.integration.format.xml.SOAPFaultException#setFaultString(java.lang.String)
    */
   public void setFaultString(String sFaultString)
   {
      throw new IllegalStateException("Cannot set fault string on a SOAP 1.2 exception, use addReason instead");
   }
}
