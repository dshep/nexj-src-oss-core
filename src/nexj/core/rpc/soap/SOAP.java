// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.soap;

import javax.xml.XMLConstants;

/**
 * SOAP constants.
 */
public class SOAP
{
   // constants

   /**
    * Namespace names.
    */
   public final static String XML_NS = XMLConstants.XMLNS_ATTRIBUTE;
   public final static String XSD_NS = "xs";
   public final static String XSI_NS = "xi";
   public final static String ENV_NS = "nv";
   public final static String ENC_NS = "nc";
   public final static String TNS_NS = "ns";
   
   /**
    * Namespace URIs.
    */
   public final static String XSD_URI = "http://www.w3.org/2001/XMLSchema";
   public final static String XSI_URI = "http://www.w3.org/2001/XMLSchema-instance";
   public final static String ENV_URI = "http://schemas.xmlsoap.org/soap/envelope/";
   public final static String ENC_URI = "http://schemas.xmlsoap.org/soap/encoding/";
   public final static String TNS_URI = "http://www.nexjsystems.com/xml-soap";

   // constructors
   
   /**
    * Prevents construction.
    */
   protected SOAP()
   {
   }
}
