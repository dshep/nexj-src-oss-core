// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.xml;

import java.io.IOException;
import java.util.Iterator;
import java.util.Set;

import javax.xml.XMLConstants;
import javax.xml.namespace.QName;

import nexj.core.meta.Argument;
import nexj.core.meta.Attribute;
import nexj.core.meta.Event;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Metadata;
import nexj.core.meta.Primitive;
import nexj.core.meta.Type;
import nexj.core.util.HashHolder;
import nexj.core.util.Undefined;

/**
 * Class to generate XML XSD Schema.
 */
public class XSDGenerator extends XMLSchemaGenerator
{
   /**
    * Generate only the generic XSD.
    */
   public static byte SCOPE_GENERIC = 0;

   /**
    * Generate Generic XSD and typed classes.
    */
   public static byte SCOPE_STATE = 1;

   /**
    * Generate Generic XSD, typed classes and events.
    */
   public static byte SCOPE_BEHAVIOUR = 2;

   /**
    * Delimiter used between Metaclass member name and unique extension when an member name
    * collision is encountered.
    * This should not be a valid metadata member identifier character.
    */
   public final static char MEMBER_COLLISION_SUFFIX_DELIMITER = '.';

   /**
    * If documentation should be included
    */
   protected boolean m_bIncludeDocumentation;

   /**
    * If only isCompatible() members should be included.
    */
   protected boolean m_bCompatible = true;

   /**
    * Only consider Metaclasses in this set as exportable (null == consider all public Metaclasses).
    */
   protected Set/*<String>*/ m_maskSet;

   /**
    * The XSD generation scope, one of SCOPE_* variables dictating XSD generation detail.
    */
   protected byte m_nScope = SCOPE_BEHAVIOUR;

   /**
    * Add the types for all metaclass attribute dependencies to required sets
    * Implementers must call setRequiredType(Type, boolean) to trigger adding required types.
    * @param meta The metaclass which to check for dependency types (not null).
    */
   protected void addAttributeDependencies(Metaclass meta)
   {
      // add all attribute types
      for (Iterator/*<Attribute>*/ itr = meta.getAttributeIterator(); itr.hasNext();)
      {
         Attribute attr = (Attribute)itr.next();

         if (!attr.isStatic() && (!m_bCompatible || attr.isCompatible()))
         {
            setRequiredType(attr.getType(), false); // collections will repeat same element per item
         }
      }
   }

   /**
    * @see nexj.core.rpc.xml.XMLSchemaGenerator#addDependencies(nexj.core.meta.Metaclass)
    */
   protected void addDependencies(Metaclass meta)
   {
      assert meta != null;

      // require parent type
      setRequiredType((meta.getBase() == null) ? DEFAULT_BASE_TYPE : meta.getBase(), false);

      addAttributeDependencies(meta);

      if (isExportedMetaclass(meta))
      {
         addEventDependencies(meta);
      }
   }

   /**
    * Add the types for all metaclass events dependencies to required sets
    * Implementers must call setRequiredType(Type, boolean) to trigger adding required types.
    * @param meta The metaclass which to check for dependency types (not null).
    */
   protected void addEventDependencies(Metaclass meta)
   {
      assert meta != null;

      // add all event argument types
      for (int i = 0, nCount = meta.getEventCount(); i < nCount; ++i)
      {
         Event event = meta.getEvent(i);

         if (!isExportedEvent(event))
         {
            continue; // ignored event
         }

         for (int k = 0, nArgCount = event.getArgumentCount(); k < nArgCount; ++k)
         {
            Argument arg = event.getArgument(k);
            Type type = arg.getType();

            setRequiredType(type, arg.isCollection());

            if (event.isVarArg() && k == nArgCount - 1) //varArg args might require arrays e.g. WSDL
            {
               setRequiredType(type, true);
            }
         }

         if (!isExportedEventResult(event))
         {
            continue; // skip result types that are not exported
         }

         Argument result = event.getResult();

         if (result == null) // undeclared
         {
            setRequiredType(Primitive.ANY, false);
         }
         else
         {
            setRequiredType(result.getType(), result.isCollection());
         }
      }
   }

   /**
    * @see nexj.core.rpc.xml.XMLSchemaGenerator#addMetaclass(nexj.core.meta.Metaclass)
    */
   protected void addMetaclass(Metaclass meta)
   {
      assert meta != null;

      if (isExportedMetaclass(meta))
      {
         setRequiredType(meta, false); // classes that will have to be defined as they are exported
      }
      else if (!m_requiredMetaclassSet.contains(meta))//can be set if came through addDependencies()
      {
         m_optionalMetaclassMap.put(meta.getName(), meta); // classes not explicitly required (yet)
      }
   }

   /**
    * @see nexj.core.rpc.xml.XMLSchemaGenerator#generate(java.lang.String)
    */
   protected void generate(String sURI) throws IOException
   {
      m_writer.openElement("schema");
         m_writer.writeAttribute(XML.XML_NS, XML.XSD_URI);
         m_writer.setNamespace(XML.XML_NS);
            m_writer.writeAttribute(XML.XSD_NS, XML.XSD_URI);
            m_writer.writeAttribute(XML.TNS_NS, XML.NS_URI_TNS);
         m_writer.setNamespace(null);
         m_writer.writeAttribute("targetNamespace", XML.NS_URI_TNS);
         m_writer.writeAttribute("elementFormDefault", "qualified");
      m_writer.closeElement();

         writeTNSXSD();

      m_writer.endElement("schema");
   }

   /**
    * @see nexj.core.rpc.xml.XMLSchemaGenerator#getMIMEType()
    */
   public String getMIMEType()
   {
      return "text/xml; charset=UTF-8";
   }

   /**
    * Check if the requested metaclass should be exported.
    * @param meta The metaclass to check (not null).
    * @return Should the requested metaclass be exported.
    */
   protected boolean isExportedMetaclass(Metaclass meta)
   {
      assert meta != null;

      return m_nScope >= SCOPE_STATE &&
             meta.getVisibility() == Metaclass.PUBLIC &&
             (m_maskSet == null || m_maskSet.contains(meta.getName())) &&
             meta.getName().indexOf(Metadata.SCOPE_SEP) < 0;
   }

   /**
    * Check if the requested event should be exported.
    * @param event The event to check (not null).
    * @return Should the requested event be exported.
    */
   protected boolean isExportedEvent(Event event)
   {
      assert event != null;

      return m_nScope >= SCOPE_BEHAVIOUR &&
             (!m_bCompatible || event.isCompatible()) &&
             event.getVisibility() == Metaclass.PUBLIC &&
             isExportedMetaclass(event.getMetaclass());
   }

   /**
    * Check if the requested event exports a result.
    * @param event The event to check (not null).
    * @return Should the requested event export its result.
    */
   protected boolean isExportedEventResult(Event event)
   {
      return true;
   }

   /**
    * set the documentation inclusion flag
    * @param bIncludeDocumentation if documentation should be included
    */
   public void setIncludeDocumentation(boolean bIncludeDocumentation)
   {
      m_bIncludeDocumentation = bIncludeDocumentation;
   }

   /**
    * Set if only isCompatible() members should be included.
    * @param bCompatible Should only isCompatible() members be included.
    */
   public void setCompatible(boolean bCompatible)
   {
      m_bCompatible = bCompatible;
   }

   /**
    * The list of Metaclasses to consider for generation.
    */
   public void setMask(String[] sArray)
   {
      if (sArray == null)
      {
         m_maskSet = null;

         return;
      }

      if (m_maskSet == null)
      {
         m_maskSet = new HashHolder/*<String>*/(sArray.length);
      }

      m_maskSet.clear();

      for (int i = 0; i < sArray.length; ++i)
      {
         m_maskSet.add(sArray[i]);
      }
   }

   /**
    * The XSD generation scope, one of SCOPE_* variables.
    * @param nScope the XSD generation scope.
    */
   public void setScope(byte nScope)
   {
      assert nScope == SCOPE_GENERIC || nScope == SCOPE_STATE || nScope == SCOPE_BEHAVIOUR;

      m_nScope = nScope;
   }

   /**
    * Outputs a given metaclass to the writer.
    * @param meta The class description to output.
    * @throws IOException On IO error.
    */
   protected void writeMetaclass(Metaclass meta) throws IOException
   {
      m_writer.openElement("complexType");
         m_writer.writeAttribute("name", meta.getName());

         // don't allow instantiation of abstract Metaclasses
         // (still need them because some attributes are of their type)
         if (!isExportedMetaclass(meta))
         {
            m_writer.writeAttribute("abstract", "true");
         }

      m_writer.closeElement();

         if (m_bIncludeDocumentation)
         {
            writeDocumentation(meta.getCaption(), meta.getDescription());
         }

         Metaclass baseMeta = (meta.getBase() == null) ? DEFAULT_BASE_TYPE : meta.getBase();

         m_writer.startElement("complexContent");
            m_writer.openElement("extension");
               writeAttribute("base", getQualifiedTypeCached(baseMeta, false));
            m_writer.closeElement();

               // MS.NET is having problems using "all" within an extension therefore can only
               // use "sequence"
               m_writer.startElement("sequence");

                  for (Iterator/*<Attribute>*/ attrItr = meta.getAttributeIterator();
                       attrItr.hasNext();)
                  {
                     Attribute attribute = (Attribute)attrItr.next();

                     if (!attribute.isStatic() && (!m_bCompatible || attribute.isCompatible()))
                     {
                        Attribute baseAttribute = baseMeta.findAttribute(attribute.getName());

                        // don't include attributes inherited from parent type,
                        // otherwise MS.NET has problems with redeclaring same attributes
                        if (attribute.getVisibility() == Metaclass.PUBLIC &&
                            (baseAttribute == null ||
                             baseAttribute.getVisibility() != Metaclass.PUBLIC ||
                             (m_bCompatible && !baseAttribute.isCompatible())))
                        {
                           writeAttribute(attribute);
                        }
                     }
                  }

               m_writer.endElement("sequence");

            m_writer.endElement("extension");
         m_writer.endElement("complexContent");

      m_writer.endElement("complexType");
   }

   /**
    * Outputs given XML type to the writer (to be used by children).
    * @param type The type description to add.
    * @throws IOException On IO error.
    */
   protected void writeMetatype(XMLMetatype type) throws IOException
   {
      type.writeXSD(m_writer);
   }

   /**
    * Append an attribute to the writer.
    * @param attribute The attribute to append.
    * @throws IOException On IO error.
    */
   protected void writeAttribute(Attribute attribute) throws IOException
   {
      m_writer.openElement("element");
         m_writer.writeAttribute("name", computeElementName(attribute));
         writeAttribute("type", getQualifiedTypeCached(attribute.getType(), false));

         // don't use nillable="true" as that will automatically set minOccurs="1" in MS.NET
         // always use "0" due to Metaclass references lacking any attributes
         m_writer.writeAttribute("minOccurs", "0");

         if (attribute.isCollection())
         {
            m_writer.writeAttribute("maxOccurs", "unbounded"); // inline array
         }

         if (attribute.getValue() != Undefined.VALUE && !attribute.isCalculated())
         {
            m_writer.writeAttribute((attribute.isReadOnly()) ? "fixed" : "default",
                                    attribute.getValue().toString());
         }

      if (m_bIncludeDocumentation)
      {
         m_writer.closeElement();
         
            writeDocumentation(attribute.getCaption(), attribute.getDescription());
            
         m_writer.endElement("element");
      }
      else
      {
         m_writer.closeEmptyElement();
      }
   }

   /**
    * Compute the proper element name to use for this attribute.
    * @param attribute The attribute in question.
    * @return The computed name.
    */
   public static String computeElementName(Attribute attribute)
   {
      String sName = attribute.getName();
      Attribute baseAttr = DEFAULT_BASE_TYPE.findAttribute(sName);

      if (baseAttr != null && // compare name since DEFAULT_BASE_TYPE can be overridden in subclass
          !attribute.getMetaclass().getName().equals(DEFAULT_BASE_TYPE.getName()))
      {
         Metaclass metaclass = attribute.getMetaclass();

         // Increment nCounter until don't have a collision with any existing attributes,
         // then use that as the attribute value
         for (int nCount = 0;; ++nCount)
         {
            String sNewName = sName + MEMBER_COLLISION_SUFFIX_DELIMITER + nCount;

            if (DEFAULT_BASE_TYPE.findAttribute(sNewName) == null &&
               metaclass.findAttribute(sNewName) == null)
            {
               return sNewName;
            }
         }
      }

      return sName;
   }

   /**
    * Compute the proper element name to use for this event.
    * @param event The event to compute element name for (not null).
    * @return The computed name.
    */
   public static String computeElementName(Event event)
   {
      return (event.getArgumentCount() > 0)
             ? event.getName() + MEMBER_COLLISION_SUFFIX_DELIMITER + event.getArgumentCount()
             : event.getName();
   }

   /**
    * Append a documentation node to the writer.
    * @param sCaption The caption to append.
    * @param sDescription The description to append.
    * @throws IOException On IO error.
    */
   protected void writeDocumentation(String sCaption, String sDescription) throws IOException
   {
      boolean bOutput = false;

      m_writer.startElement("annotation");

         m_writer.openElement("documentation");
            m_writer.writeAttribute("xml:lang", "en");
         m_writer.closeElement();

            if (sCaption != null)
            {
               m_writer.writeValue(sCaption);
               bOutput = true;
            }

            if (sDescription != null)
            {
               if (bOutput)
               {
                  m_writer.writeValue("\n");
               }

               m_writer.writeValue(sDescription);
               bOutput = true;
            }

         if (bOutput)
         {
            m_writer.endElement("documentation");
         }
         else
         {
            m_writer.closeEmptyElement();
         }

      m_writer.endElement("annotation");
   }

   /**
    * Write out object as an attribute value.
    * @param sAttribute The attribute name.
    * @throws IOException On IO error.
    */
   protected void writeAttribute(String sAttribute, QName qname) throws IOException
   {
      if (qname.getPrefix() == XMLConstants.DEFAULT_NS_PREFIX)
      {
         m_writer.writeAttribute(sAttribute, qname.getLocalPart());
      }
      else
      {
         m_writer.writeAttribute(sAttribute, qname.getPrefix(), ":", qname.getLocalPart());
      }
   }

   /**
    * Convenience method for writeElement(...) with default values.
    * @see XSDGenerator#writeElement(String, String, QName, int, int, boolean, String)
    * @param minOccurs = 1
    * @param maxOccurs = 1
    * @param bNullable = false
    * @param sDocumentation = null
    */
   protected void writeElement(String sName0, String sName1, String sName2, QName type)
      throws IOException
   {
      writeElement(sName0, sName1, sName2, type.getPrefix(), type.getLocalPart(), null);
   }

   /**
    * Convenience method for writeElement(...) with default values and
    * non-array type lookup via cache.
    * @see XSDGenerator#writeElement(String, String, String, QName)
    */
   protected void writeElement(String sName0, String sName1, Type type) throws IOException
   {
      assert type != null;

      writeElement(sName0, sName1, null, getQualifiedTypeCached(type, false));
   }

   /**
    * Write out object as a named and typed element.
    * @param sName0 The first part of the element name (not null).
    * @param sName1 The second part of the element name, concatenated with first, (null == ignore)
    * @param sNameSuffix The name suffix (null == ignore).
    * @param type The element type attribute value (not null).
    * @param nMinOccurs The minimum number of occurrences of this element (>= 0)
    * @param nMaxOccurs The maximum number of occurrences of this element (<0 == unlimited)
    * @param bNullable This element can use xsi:nil flag to indicate null value.
    * @param sDocumentation The English language element documentation.
    * @throws IOException On output error.
    */
   protected void writeElement(String sName0, String sName1, QName type,
      int nMinOccurs, int nMaxOccurs, boolean bNullable, String sDocumentation) throws IOException
   {
      writeElement(sName0, sName1, null, type.getPrefix(), type.getLocalPart(), null,
                   nMinOccurs, nMaxOccurs, bNullable, sDocumentation);
   }

   /**
    * Convenience method for writeElement(...) with non-array type lookup via cache.
    * @see XSDGenerator#writeElement(String, String, QName, int, int, boolean, String)
    * @param bArray This is an array type.
    */
   protected void writeElement(String sName0, String sName1, Type type,
      int nMinOccurs, int nMaxOccurs, boolean bNullable, String sDocumentation) throws IOException
   {
      assert type != null;

      writeElement(
         sName0, sName1, getQualifiedTypeCached(type, false),
         nMinOccurs, nMaxOccurs, bNullable, sDocumentation);
   }

   /**
    * Convenience method for writeElement(...) with default values.
    * @see XSDGenerator#writeElement(String, String, String, String, String, int, int, boolean, String)
    * @param nMinOccurs = 1
    * @param nMaxOccurs = 1
    * @param bNullable = false
    * @param sDocumentation = null
    */
   protected void writeElement(
      String sName0, String sName1, String sName2, String sTypePrefix, String sType0, String sType1)
      throws IOException
   {
      writeElement(sName0, sName1, sName2, sTypePrefix, sType0, sType1, 1, 1, false, null);
   }

   /**
    * Write out object as a named and typed element.
    * @param sName0 The first part of the element name (not null).
    * @param sName1 The second part of the element name, concatenated with first, (null == ignore)
    * @param sName2 The third part of the element name, concatenated with second, (null == ignore)
    * @param sTypePrefix The element type namespace prefix (not null).
    * @param sType0 The first part of the element type (not null).
    * @param sType1 The second part of the element type, concatenated with first, (null == ignore)
    * @param nMinOccurs The minimum number of occurrences of this element (>= 0)
    * @param nMaxOccurs The maximum number of occurrences of this element (<0 == unlimited)
    * @param bNullable This element can use xsi:nil flag to indicate null value.
    * @param sDocumentation The English language element documentation.
    * @throws IOException On output error.
    */
   protected void writeElement(
      String sName0, String sName1, String sName2, String sTypePrefix, String sType0, String sType1,
      int nMinOccurs, int nMaxOccurs, boolean bNullable, String sDocumentation) throws IOException
   {
      assert sName0 != null;
      assert sName2 == null || sName1 != null;
      assert sTypePrefix != null;
      assert sType0 != null;
      assert nMinOccurs >= 0;

      m_writer.openElement("element");
         m_writer.writeAttribute("name", sName0, sName1, sName2);
         m_writer.writeAttribute("type", sTypePrefix, ":", sType0, sType1);

         if (nMinOccurs != 1) // ignore default value to minimize XML output
         {
            m_writer.writeAttribute("minOccurs", nMinOccurs);
         }

         if (nMaxOccurs < 0)
         {
            m_writer.writeAttribute("maxOccurs", "unbounded");
         }
         else if (nMaxOccurs != 1) // ignore default value to minimize XML output
         {
            m_writer.writeAttribute("maxOccurs", nMaxOccurs);
         }

         if (bNullable) // ignore default value to minimize XML output
         {
            m_writer.writeAttribute("nillable", true);
         }

      if (m_bIncludeDocumentation && sDocumentation != null)
      {
         m_writer.closeElement();
            writeDocumentation(null, sDocumentation);
         m_writer.endElement("element");
      }
      else
      {
         m_writer.closeEmptyElement();
      }
   }

   /**
    * Write the XSD import statements required for TNS XSD (override to add additional XSD imports).
    * @throws IOException On generation output error.
    */
   protected void writeTNSImport() throws IOException
   {
      m_writer.openElement("import"); // append "import" of "namespace" schema
         m_writer.writeAttribute("namespace", XML.NS_URI_XML);
      m_writer.closeEmptyElement();
   }

   /**
    * Write the TNS XSD portion of the schema
    * i.e. the XSD declarations for TNS without the surrounding <schema> tags.
    * @throws IOException On XSD output error.
    */
   protected void writeTNSXSD() throws IOException
   {
      writeTNSImport();

      for (int i = 0; i < m_metatypeArray.length; ++i)
      {
         writeMetatype(m_metatypeArray[i]);
      }

      for (int i = 0; i < m_metaclassArray.length; ++i)
      {
         writeMetaclass(m_metaclassArray[i]);
      }
   }
}