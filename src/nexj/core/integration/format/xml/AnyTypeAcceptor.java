// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.xml;

import java.io.IOException;
import java.io.StringWriter;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import nexj.core.meta.Primitive;
import nexj.core.rpc.TransferObject;
import nexj.core.util.Binary;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;
import nexj.core.util.SOAPUtil;
import nexj.core.util.StringUtil;
import nexj.core.util.XMLWriter;

/**
 * Processes an any-type element as a string.
 */
public class AnyTypeAcceptor implements ParseEventAcceptor
{
   // attributes

   /**
    * The name of the XML schema type being parsed; null to parse literal XML.
    */
   protected String m_sType;

   /**
    * The current level of element nesting--used to determine when the any-type element is closed.
    */
   protected int m_nLevel;

   // associations

   /**
    * The writer for processing xsd:any as a string.
    */
   protected XMLWriter m_writer;

   /**
    * The writer containing the result string.
    */
   protected StringWriter m_writerData;

   /**
    * Map of XML schema primitive names to their framework equivalents.
    */
   protected final static Lookup s_typePrimitiveMap = new HashTab(12);  // of type Primitive[String]

   static
   {
      s_typePrimitiveMap.put("string", Primitive.STRING);
      s_typePrimitiveMap.put("base64Binary", Primitive.BINARY);
      s_typePrimitiveMap.put("hexBinary", Primitive.BINARY);
      s_typePrimitiveMap.put("integer", Primitive.INTEGER);
      s_typePrimitiveMap.put("long", Primitive.LONG);
      s_typePrimitiveMap.put("decimal", Primitive.DECIMAL);
      s_typePrimitiveMap.put("float", Primitive.FLOAT);
      s_typePrimitiveMap.put("double", Primitive.DOUBLE);
      s_typePrimitiveMap.put("dateTime", Primitive.TIMESTAMP);
      s_typePrimitiveMap.put("date", Primitive.TIMESTAMP);
      s_typePrimitiveMap.put("time", Primitive.TIMESTAMP);
      s_typePrimitiveMap.put("boolean", Primitive.BOOLEAN);
   }

   // constructors

   /**
    * Creates a new XSD any element parser.
    */
   public AnyTypeAcceptor(String sType)
   {
      m_writerData = new StringWriter();
      m_writer = new XMLWriter(m_writerData);
      m_sType = sType;
   }

   // operations

   /**
    * @see nexj.core.integration.format.xml.ParseEventAcceptor#getValue()
    */
   public Object getValue()
   {
      String sValue = m_writerData.toString();

      if (StringUtil.isEmpty(sValue))
      {
         return null;
      }

      if (m_sType != null)
      {
         Primitive type = (Primitive)s_typePrimitiveMap.get(m_sType);

         if (type != null)
         {
            switch (type.getOrdinal())
            {
               case Primitive.BINARY_ORDINAL:
                  try
                  {
                     if (m_sType.charAt(0) == 'b')  // base64Binary
                     {
                        return Binary.fromBase64(sValue);
                     }

                     return Binary.parse(sValue);  // hexBinary
                  }
                  catch (IOException ex)
                  {
                     throw ObjUtil.rethrow(ex);
                  }

               case Primitive.TIMESTAMP_ORDINAL:
                  if (m_sType.length() == 8)  // dateTime
                  {
                     return SOAPUtil.parseDateTime(sValue, true, true, null);
                  }

                  if (m_sType.charAt(0) == 'd')  // date
                  {
                     return SOAPUtil.parseDateTime(sValue, true, false, null);
                  }

                  return SOAPUtil.parseDateTime(sValue, false, true, null);  // time

               default:
                  return type.convert(sValue);
            }
         }
      }

      return sValue;
   }

   /**
    * @see nexj.core.integration.format.xml.ParseEventAcceptor#acceptCharacters(char[], int, int)
    */
   public boolean acceptCharacters(char[] cbuf, int nOffset, int nLength) throws SAXException
   {
      try
      {
         if (m_sType == null)
         {
            m_writer.writeValue(cbuf, nOffset, nLength);
         }
         else
         {
            m_writer.write(cbuf, nOffset, nLength);
         }
      }
      catch (IOException ex)
      {
         ObjUtil.rethrow(ex);
      }

      return true;
   }

   /**
    * @see nexj.core.integration.format.xml.ParseEventAcceptor#acceptEndElement(java.lang.String, java.lang.String, java.lang.String)
    */
   public boolean acceptEndElement(String sURI, String sLocalName, String sQualName) throws SAXException
   {
      if (m_nLevel > 0)
      {
         m_nLevel--;

         try
         {
            m_writer.endElement(sQualName);
         }
         catch (IOException ex)
         {
            ObjUtil.rethrow(ex);
         }

         return true;
      }

      return false;
   }

   /**
    * @see nexj.core.integration.format.xml.ParseEventAcceptor#acceptStartElement(java.lang.String, java.lang.String, java.lang.String, org.xml.sax.Attributes)
    */
   public boolean acceptStartElement(String sURI, String sLocalName, String sQualName, Attributes attributes) throws SAXException
   {
      m_nLevel++;

      try
      {
         m_writer.openElement(sQualName);

         for (int i = 0, nLength = attributes.getLength(); i < nLength; i++)
         {
            m_writer.writeAttribute(attributes.getQName(i), attributes.getValue(i));
         }

         m_writer.closeElement();
      }
      catch (IOException ex)
      {
         ObjUtil.rethrow(ex);
      }

      return true;
   }

   /**
    * @see nexj.core.integration.format.xml.ParseEventAcceptor#checkFault(nexj.core.rpc.TransferObject)
    */
   public void checkFault(TransferObject root) throws Exception
   {
      // No fault is possible
   }

   /**
    * @see nexj.core.integration.format.xml.ParseEventAcceptor#reset()
    */
   public void reset()
   {
      // Nothing to reset.
   }
}
