// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.xml;

import javax.xml.XMLConstants;
import javax.xml.namespace.QName;

import nexj.core.meta.Primitive;
import nexj.core.meta.Type;
import nexj.core.rpc.soap.SOAP;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;

public class XML extends SOAP
{
   // constants for use in XML generation
   
   /**
    * Namespace prefixes.
    */
   public static final String NS_TAG_WADL      = "wadl";
   public static final String NS_TAG_WSDL      = "wsdl";
   public static final String NS_TAG_WSDL_SOAP = "wsdlsoap";
   public static final String NS_TAG_XLINK     = "xl";
   public static final String NS_TAG_XML = XMLConstants.XML_NS_PREFIX; // hardcoded as per w3c spec

   /**
    * Namespace URIs.
    * for common values see @see javax.xml.XMLConstants
    */
   public static final String NS_URI_SOAP_HTTP = "http://schemas.xmlsoap.org/soap/http";
   public static final String NS_URI_WADL      = "http://wadl.dev.java.net/2009/02";
   public static final String NS_URI_WSDL      = "http://schemas.xmlsoap.org/wsdl/";
   public static final String NS_URI_WSDL_SOAP = "http://schemas.xmlsoap.org/wsdl/soap/";
   public static final String NS_URI_TNS       = "http://www.nexj.com/xml";
   public static final String NS_URI_XLINK     = "http://www.w3.org/1999/xlink";
   public static final String NS_URI_XML       = XMLConstants.XML_NS_URI;
   /**
    * The suffix to use for array types.
    */
   public static final String ARRAY_SUFFIX = "-array";

   /**
    * Prefix used for base type and attribute names.
    */
   public static final String BASE_PREFIX = "_";

   /**
    * The XML file header.
    */
   public static final String HEADER = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";

   /**
    * List of type mappings that should be used for Attribute types.
    * All other types will use Attribute type in NS_URI_NEXJ_XML namespace.
    */
   protected final static Lookup/*<Type, QName>*/ s_typeMap = new HashTab/*<Type, QName>*/(10);

   static
   {
      s_typeMap.put(Primitive.ANY,       new QName(XML.XSD_URI, "anyType",      XML.XSD_NS));
      s_typeMap.put(Primitive.BINARY,    new QName(XML.XSD_URI, "base64Binary", XML.XSD_NS));
      s_typeMap.put(Primitive.BOOLEAN,   new QName(XML.XSD_URI, "boolean",      XML.XSD_NS));
      s_typeMap.put(Primitive.DECIMAL,   new QName(XML.XSD_URI, "decimal",      XML.XSD_NS));
      s_typeMap.put(Primitive.DOUBLE,    new QName(XML.XSD_URI, "double",       XML.XSD_NS));
      s_typeMap.put(Primitive.FLOAT,     new QName(XML.XSD_URI, "float",        XML.XSD_NS));
      s_typeMap.put(Primitive.INTEGER,   new QName(XML.XSD_URI, "int",          XML.XSD_NS));
      s_typeMap.put(Primitive.LONG,      new QName(XML.XSD_URI, "long",         XML.XSD_NS));
      s_typeMap.put(Primitive.STRING,    new QName(XML.XSD_URI, "string",       XML.XSD_NS));
      s_typeMap.put(Primitive.TIMESTAMP, new QName(XML.XSD_URI, "dateTime",     XML.XSD_NS));      
   }

   // constructors

   /**
    * Prevents instantiation.
    */
   private XML()
   {
   }

   // operations

   /**
    * Convenience function.
    * @see XML#getQualifiedType(Type, boolean)
    * @param bArray = false
    */
   public static QName getQualifiedType(Type type)
   {
      return getQualifiedType(type, false);
   }

   /**
    * Return the proper namespace qualified name of type.
    * This function contains any fixups/corrections/translations of internal types to XML types.
    * @param type The type to analyze.
    * @param bArray This is an array type.
    * @return Qualified type (or null if type == null).
    */
   public static QName getQualifiedType(Type type, boolean bArray)
   {
      QName qType = (QName)s_typeMap.get(type);

      if (qType == null)
      {
         return getTNSType(type.getName(), bArray);
      }

      return (bArray) ? getTNSType(qType.getLocalPart(), true) : qType;
   }

   /**
    * Convenience function.
    * @see XML#getTNSType(String, boolean)
    * @param bArray = false
    */
   public static QName getTNSType(String sType)
   {
      return getTNSType(sType, false);
   }

   /**
    * Return the proper TNS namespace qualified name of the requested type.
    * @param sType The type name to qualify with TNS (not null).
    * @param bArray This is an array type.
    * @return Qualified type in TNS namespace.
    */
   public static QName getTNSType(String sType, boolean bArray)
   {
      assert sType != null;

      return new QName(NS_URI_TNS, (bArray) ? (sType + ARRAY_SUFFIX) : sType, XML.TNS_NS);
   }
}