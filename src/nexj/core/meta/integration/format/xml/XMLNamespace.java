// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.xml;

import nexj.core.meta.NamedMetadataObject;
import nexj.core.rpc.xml.XML;
import nexj.core.util.ObjUtil;

/**
 * Describes an XML namespace.
 */
public class XMLNamespace extends NamedMetadataObject
{
   // constants

   /**
    * The SOAP 1.2 namespace.
    */
   public final static String SOAP12 = "http://www.w3.org/2003/05/soap-envelope";

   /**
    * The SOAP 1.1 namespace.
    */
   public final static String SOAP = "http://schemas.xmlsoap.org/soap/envelope/";

   /**
    * The WSDL namespace.
    */
   public final static String WSDL = "http://schemas.xmlsoap.org/wsdl/";

   /**
    * The namespace for WSDL SOAP bindings.
    */
   public final static String WSDL_SOAP = "http://schemas.xmlsoap.org/wsdl/soap/";

   /**
    * The XML Schema instance namespace.
    */
   public final static String XSI = XML.XSI_URI;

   /**
    * The XML Schema namespace.
    */
   public final static String XSD = XML.XSD_URI;

   /**
    * Reserved name for the SOAP 1.2 namespace.
    */
   public final static String SOAP12_NAME = "soap12";

   /**
    * Reserved name for the SOAP 1.1 namespace.
    */
   public final static String SOAP_NAME = "soap";

   /**
    * Reserved name for the WSDL namespace.
    */
   public final static String WSDL_NAME = "wsdl";

   /**
    * The WS-I Basic Profile namespace name for the WSDL SOAP bindings.
    */
   public final static String WSDL_SOAP_NAME = "soapbind";

   /**
    * Reserved name for the XML Schema instance namespace.
    */
   public final static String XSI_NAME = "xsi";

   /**
    * Reserved name for the XML Schema namespace.
    */
   public final static String XSD_NAME = "xsd";


   // attributes

   /**
    * The namespace URI.
    */
   protected String m_sURI;

   /**
    * The XML schema document location.
    */
   protected String m_sSchema;

   /**
    * The is declared by envelope flag.
    */
   protected boolean m_bDeclaredByEnvelope;

   // constructors

   /**
    * Constructs the metadata object.
    * @param sName The namespace name.
    * @param sURL The URL name.
    * @param sSchema The XML schema document location, if any.
    * @param bDeclaredByEnvelope True iff this namespace is declared by the message's envelope.
    */
   public XMLNamespace(String sName, String sURL, String sSchema, boolean bDeclaredByEnvelope)
   {
      super(sName);
      m_sURI = sURL;
      m_sSchema = sSchema;
      m_bDeclaredByEnvelope = bDeclaredByEnvelope;
   }

   /**
    * Constructs the metadata object.
    * @param sName The namespace name.
    * @param sURL The URL name.
    */
   public XMLNamespace(String sName, String sURL)
   {
      super(sName);
      m_sURI = sURL;
      m_bDeclaredByEnvelope = false;
   }

   /**
    * Constructs the metadata object.
    */
   public XMLNamespace()
   {
      m_bDeclaredByEnvelope = false;
   }

   // operations
   
   /**
    * Sets the namespace URI.
    * @param sURL The namespace URI to set.
    */
   public void setURI(String sURL)
   {
      verifyNotReadOnly();
      m_sURI = sURL;
   }

   /**
    * @return The namespace URI.
    */
   public String getURI()
   {
      return m_sURI;
   }

   /**
    * Sets the XML schema document location.
    * @param sSchema The XML schema document location; null for none.
    */
   public void setSchema(String sSchema)
   {
      verifyNotReadOnly();
      m_sSchema = sSchema;
   }

   /**
    * Gets the XML schema document location.
    * @return The XML schema document location; null for none.
    */
   public String getSchema()
   {
      return m_sSchema;
   }

   /**
    * Sets the is declared by envelope flag.
    * @param bAttrib The is declared by envelope flag to set.
    */
   public void setDeclaredByEnvelope(boolean bDeclaredByEnvelope)
   {
      verifyNotReadOnly();
      m_bDeclaredByEnvelope = bDeclaredByEnvelope;
   }

   /**
    * @return The is declared by envelope flag.
    */
   public boolean isDeclaredByEnvelope()
   {
      return m_bDeclaredByEnvelope;
   }

   /**
    * @see java.lang.Object#equals(java.lang.Object)
    */
   public boolean equals(Object obj)
   {
      if (obj instanceof XMLNamespace)
      {
         XMLNamespace other = (XMLNamespace)obj;

         return ObjUtil.equal(m_sURI, other.m_sURI);
      }

      return false;
   }

   /**
    * @see java.lang.Object#hashCode()
    */
   public int hashCode()
   {
      return m_sURI.hashCode();
   }
}
