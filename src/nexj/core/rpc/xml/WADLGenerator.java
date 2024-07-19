// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.xml;

import java.io.IOException;
import java.util.Iterator;
import java.util.Set;

import javax.xml.namespace.QName;

import nexj.core.meta.Argument;
import nexj.core.meta.Event;
import nexj.core.meta.Member;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Primitive;
import nexj.core.meta.Selector;
import nexj.core.meta.Type;
import nexj.core.util.HashHolder;
import nexj.core.util.HashTab2D;
import nexj.core.util.Lookup2D;

/**
 * Class to generate WADL API
 */
public class WADLGenerator extends XSDGenerator
{
   // constants

   protected final static XMLMetatype NIL_TYPE =
      new XMLMetatype(XML.getTNSType(XML.BASE_PREFIX + "Nil"));

   // associations

   /**
    * Cache of results from computeElementType.
    */
   protected Lookup2D/*<Type, Boolean, QName>*/ m_elementMap =
      new HashTab2D/*<Type, Boolean, QName>*/();

   /**
    * Set of types that require an array element defined,
    * as opposed to just the complexType requirements tracked via setRequiredType(...).
    */
   protected Set/*<Type>*/ m_requiredArrayTypeSet;

   // operations

   /**
    * @see nexj.core.rpc.xml.XSDGenerator#addEventDependencies(nexj.core.meta.Metaclass)
    */
   protected void addEventDependencies(Metaclass meta)
   {
      super.addEventDependencies(meta);

      // exported events require that all <element> types are defined, need to define "-array" types
      for (int i = 0, nCount = meta.getEventCount(); i < nCount; ++i)
      {
         Event event = meta.getEvent(i);

         if (isExportedEvent(event))
         {
            Argument result = event.getResult();

            // check if array <element> required for return type
            if (result != null && result.isCollection())
            {
               m_requiredArrayTypeSet.add(result.getType());
            }
         }
      }
   }

   /**
    * Compute the proper qualified element type to use for this type.
    * @param type The type to compute qualified element for (not null).
    * @param bArray This is a collection element.
    * @return The computed element type.
    */
   public static QName computeElementType(Type type, boolean bArray)
   {
      QName qType = XML.getQualifiedType(type, false);

      if (qType.getNamespaceURI() == XML.XSD_URI) // elements for XSD types are in TNS NS
      {
         return XML.getTNSType(XML.BASE_PREFIX + qType.getLocalPart(), bArray);
      }

      return (bArray) ? XML.getQualifiedType(type, bArray) : qType;
   }

   /**
    * @see nexj.core.rpc.xml.XSDGenerator#generate(java.lang.String)
    */
   protected void generate(String sURI) throws IOException
   {
      m_writer.openElement("application");
         m_writer.writeAttribute(XML.XML_NS, XML.NS_URI_WADL);
         m_writer.setNamespace(XML.XML_NS);
            m_writer.writeAttribute(XML.XSD_NS,  XML.XSD_URI);
            m_writer.writeAttribute(XML.NS_TAG_WADL, XML.NS_URI_WADL);
            m_writer.writeAttribute(XML.TNS_NS,  XML.NS_URI_TNS);
         m_writer.setNamespace(null);
         m_writer.writeAttribute("targetNamespace", XML.NS_URI_TNS);
      m_writer.closeElement();

         m_writer.startElement("grammars");

            m_writer.openElement("schema");
               m_writer.writeAttribute(XML.XML_NS, XML.XSD_URI);
               m_writer.writeAttribute("targetNamespace", XML.NS_URI_TNS);
               m_writer.writeAttribute("elementFormDefault", "qualified");
            m_writer.closeElement();

               writeTNSXSD();

               // add elements for all XSD types since that is how primitives will be marshalled
               for (int i = 0; i <= Primitive.MAX_COUNT; ++i)
               {
                  Primitive type = Primitive.get(i);
                  QName qType = getQualifiedTypeCached(type, false);

                  if (qType.getNamespaceURI() == XML.XSD_URI) // only consider XSD types
                  {
                     QName element = getElementTypeCached(type, false);

                     writeElement(element.getLocalPart(), null, null, qType);//marshalled non-arrays
                     qType = getQualifiedTypeCached(type, true);
                     element = getElementTypeCached(type, true);
                     writeElement(element.getLocalPart(), null, null, qType); // marshalled arrays
                  }
               }

               writeElement(
                  XML.BASE_PREFIX, "Exception", null, XML.TNS_NS, XML.BASE_PREFIX, "Exception");
               writeElement(
                  XML.BASE_PREFIX, "Request", null, XML.TNS_NS, XML.BASE_PREFIX, "Request");
               writeElement(
                  XML.BASE_PREFIX, "Response", null, XML.TNS_NS, XML.BASE_PREFIX, "Response");
               writeElement(
                  XML.BASE_PREFIX, "Collection", null, XML.TNS_NS, XML.BASE_PREFIX, "Collection");
               writeElement(XML.BASE_PREFIX, "Nil", null, NIL_TYPE.getType());

            m_writer.endElement("schema");
         m_writer.endElement("grammars");

         m_writer.openElement("resources");
            m_writer.writeAttribute("base", sURI);
         m_writer.closeElement();

            m_writer.openElement("resource");
               m_writer.writeAttribute("queryType", "text/xml");
            m_writer.closeElement();

               m_writer.openElement("param");
                  m_writer.writeAttribute("href", "#authorization");
               m_writer.closeEmptyElement();

               m_writer.openElement("method");
                  m_writer.writeAttribute("name", "GET");
               m_writer.closeElement();

                  m_writer.startElement("request");
                     writeParam("xsd", XML.getQualifiedType(Primitive.BOOLEAN));
                  m_writer.endElement("request");

                  m_writer.startElement("response");
                     m_writer.openElement("representation");
                        m_writer.writeAttribute("mediaType", "application/vnd.sun.wadl+xml");
                     m_writer.closeEmptyElement();
                  m_writer.endElement("response");

               m_writer.endElement("method");

               m_writer.openElement("method");
                  m_writer.writeAttribute("name", "POST");
               m_writer.closeElement();

                  m_writer.startElement("request");
                     writeRepresentation(XML.TNS_NS, XML.BASE_PREFIX, "Request", null, null);
                  m_writer.endElement("request");

                  m_writer.startElement("response");

                     writeRepresentation(XML.TNS_NS, XML.BASE_PREFIX, "Response", null, null);

                     m_writer.openElement("fault");
                     m_writer.writeAttribute("href", "#exception");
                  m_writer.closeEmptyElement();

                  m_writer.endElement("response");

               m_writer.endElement("method");

            m_writer.endElement("resource");

            // output metaclass specific resources here
            for (int i = 0; i < m_metaclassArray.length; ++i)
            {
               if (isExportedMetaclass(m_metaclassArray[i]))
               {
                  writeResource(m_metaclassArray[i]);
               }
            }

         m_writer.endElement("resources");

         // definitions referenced elsewhere in the document

         m_writer.openElement("param");
            m_writer.writeAttribute("id", "authorization");
            m_writer.writeAttribute("name", "Authorization");
            m_writer.writeAttribute("style", "header");
         m_writer.closeEmptyElement();

         m_writer.openElement("method");
            m_writer.writeAttribute("id", "read-request");
            m_writer.writeAttribute("name", "GET");
         m_writer.closeElement();

            m_writer.startElement("request");

               writeParam("attributes", null);
               writeParam("where", null);
               writeParam("orderBy", null);

               m_writer.openElement("param");
                  m_writer.writeAttribute("name", "count");
                  m_writer.writeAttribute("style", "query");
                  // @see http://www.w3.org/TR/xmlschema-2/ for list of built-in types
                  m_writer.writeAttribute("type", XML.XSD_NS, ":int");
                  m_writer.writeAttribute("default", "8");
               m_writer.closeEmptyElement();

               writeParam("offset", XML.getQualifiedType(Primitive.INTEGER));

            m_writer.endElement("request");

            m_writer.startElement("response");

               writeRepresentation(XML.TNS_NS, XML.BASE_PREFIX, "Collection", null, null);

               m_writer.openElement("fault");
                  m_writer.writeAttribute("href", "#exception");
               m_writer.closeEmptyElement();
            m_writer.endElement("response");

         m_writer.endElement("method");

         m_writer.openElement("fault");
            m_writer.writeAttribute("id", "exception");
            m_writer.writeAttribute("mediaType", "text/xml");
            m_writer.writeAttribute("element", XML.TNS_NS, ":", XML.BASE_PREFIX, "Exception");
            m_writer.writeAttribute("status", "500");
         m_writer.closeEmptyElement();

      m_writer.endElement("application");
   }

   /**
    * Provide caching for element types.
    * @see XML#getQualifiedType(nexj.core.meta.Type, boolean)
    */
   protected QName getElementTypeCached(Type type, boolean bArray)
   {
      assert type != null;

      QName qType = (QName)m_elementMap.get(type, Boolean.valueOf(bArray));

      if (qType == null)
      {
         qType = computeElementType(type, bArray);
         m_elementMap.put(type, Boolean.valueOf(bArray), qType);
      }

      return qType;
   }

   /**
    * @see nexj.core.rpc.xml.XSDGenerator#getMIMEType()
    */
   public String getMIMEType()
   {
      return "application/vnd.sun.wadl+xml; charset=UTF-8";
   }

   /**
    * @see nexj.core.rpc.xml.XSDGenerator#isExportedEvent(nexj.core.meta.Event)
    */
   protected boolean isExportedEvent(Event event)
   {
      return super.isExportedEvent(event) && event.getTransactionMode() != Event.TX_REQUIRED;
   }

   /**
    * Output WADL doc XML.
    * @param sCaption The parameter caption.
    * @param sDescription The parameter description.
    * @return If a doc element was output.
    * @throws IOException On IO error.
    */
   protected void writeDoc(String sCaption, String sDescription) throws IOException
   {
      m_writer.openElement("doc");
         m_writer.writeAttribute("xml:lang", "en");

         if (sCaption != null)
         {
            m_writer.writeAttribute("title", sCaption);
         }

      if (sDescription != null)
      {
         m_writer.closeElement();
            m_writer.writeValue(sDescription);
         m_writer.endElement("doc");
      }
      else
      {
         m_writer.closeEmptyElement();
      }
   }

   /**
    * Output event method declaration for a given event.
    * @param event The event to output (not null).
    * @param sName The event name to use, (null == XML.BASE_PREFIX + event.getName()).
    * @throws IOException On IO error.
    */
   protected void writeEventMethod(Event event, String sName) throws IOException
   {
      assert event != null;

      m_writer.openElement("method");

         if (sName != null)
         {
            m_writer.writeAttribute("name", sName);
         }
         else
         {
            m_writer.writeAttribute("name", XML.BASE_PREFIX, event.getName());
         }

      m_writer.closeElement();

         if (m_bIncludeDocumentation)
         {
            writeDoc(event.getName(), event.getDescription());
         }

         m_writer.startElement("request");

            for (int i = 0, nCount = event.getArgumentCount(); i < nCount; ++i)
            {
               Argument arg = event.getArgument(i);
               Type type = arg.getType();
               boolean bVarArg = event.isVarArg() && i == nCount - 1;

               if (type instanceof Metaclass)
               {
                  type = Primitive.STRING; // OID parameter
               }

               writeParam(arg.getName(), getQualifiedTypeCached(type, bVarArg),
                          arg.isRequired(), arg.isCollection(),
                          arg.getSymbol().getName(), arg.getDescription());
            }

         m_writer.endElement("request");

         m_writer.startElement("response");

            Argument result = event.getResult();

            if (result != null)
            {
               QName qType = getElementTypeCached(result.getType(), result.isCollection());

               writeRepresentation(qType.getPrefix(), qType.getLocalPart(), null,
                                   null, result.getDescription());
            }

            m_writer.openElement("fault");
               m_writer.writeAttribute("href", "#exception");
            m_writer.closeEmptyElement();

         m_writer.endElement("response");

      m_writer.endElement("method");
   }

   /**
    * Output event resource declaration for a given event.
    * @param meta The metaclass for which to output events (not null).
    * @param bStatic Output only static events (false == output only non-static events)
    * @throws IOException On IO error.
    */
   protected void writeEventResources(Metaclass meta, boolean bStatic) throws IOException
   {
      assert meta != null;

      for (Iterator/*<Selector>*/ selItr = meta.getSelectorIterator(); selItr.hasNext();)
      {
         boolean bStarted = false;
         Selector selector = (Selector)selItr.next();

         for (Iterator/*<Member>*/ memItr = selector.getMemberIterator(); memItr.hasNext();)
         {
            Member member = (Member)memItr.next();

            if (!(member instanceof Event) ||
                bStatic != member.isStatic() ||
                !isExportedEvent((Event)member))
            {
               continue;
            }

            if (!bStarted)
            {
               // add each event as a resource
               m_writer.openElement("resource");
                  m_writer.writeAttribute("path", member.getName());
                  m_writer.writeAttribute("queryType", "text/xml");
               m_writer.closeElement();

               m_writer.openElement("param");
                  m_writer.writeAttribute("href", "#authorization");
               m_writer.closeEmptyElement();

               bStarted = true;
            }

            writeEventMethod((Event)member, "POST");
         }

         if (bStarted)
         {
            m_writer.endElement("resource");
         }
      }
   }

   /**
    * @see nexj.core.rpc.xml.XSDGenerator#writeMetaclass(nexj.core.meta.Metaclass)
    */
   protected void writeMetaclass(Metaclass meta) throws IOException
   {
      // must execute what the parent's method too so that XSD is generated
      super.writeMetaclass(meta);

      if (meta.getVisibility() == Metaclass.PUBLIC)
      {
         writeElement(meta.getName(), null, null, XML.TNS_NS, meta.getName(), null);
      }

      // add an array element if one is required
      // Note: has to be element around array complex type or wadl2java has resolution issues
      if (m_requiredArrayTypeSet.contains(meta))
      {
         writeElement(getElementTypeCached(meta, true).getLocalPart(), null, null,
                      getQualifiedTypeCached(meta, true));
      }
   }

   /**
    * Convenience method for writeParam(...) with default values.
    * @see WADLGenerator#writeParam(String, QName, boolean, boolean, String, String)
    * @param bRequired == false
    * @param bCollection == false
    * @param sCaption == null
    * @param sDescription = null
    */
   protected void writeParam(String sName, QName type) throws IOException
   {
      writeParam(sName, type, false, false, null, null);
   }

   /**
    * Output WADL param XML for a type.
    * @param sName The parameter name (not null).
    * @param type The type of param.
    * @param bRequired Is this parameter required to be present.
    * @param bCollection Does this parameter represent a collection.
    * @param sCaption The parameter caption.
    * @param sDescription The parameter description.
    * @throws IOException On IO error.
    */
   protected void writeParam(String sName, QName type, boolean bRequired, boolean bCollection,
                             String sCaption, String sDescription) throws IOException
   {
      assert sName != null;

      m_writer.openElement("param");
         m_writer.writeAttribute("name", sName);
         m_writer.writeAttribute("style", "query");

         if (type != null)
         {
            m_writer.writeAttribute("type", type.getPrefix(), ":", type.getLocalPart());
         }

         if (bRequired) // ignore default value to minimize XML output
         {
            m_writer.writeAttribute("required", "true");
         }

         if (bCollection) // ignore default value to minimize XML output
         {
            m_writer.writeAttribute("repeating", "true");
         }

      if (m_bIncludeDocumentation && (sCaption != null || sDescription != null))
      {
         m_writer.closeElement();
            writeDoc(sCaption, sDescription);
         m_writer.endElement("param");
      }
      else
      {
         m_writer.closeEmptyElement();
      }
   }

   /**
    * Output WADL representation XML for a type.
    * @param sTypePrefix The element type namespace prefix (not null).
    * @param sType0 The first part of the element type (not null).
    * @param sType1 The second part of the element type, concatenated with first, (null == ignore)
    * @param sCaption The parameter caption.
    * @param sDescription The parameter description.
    * @throws IOException On IO error.
    */
   protected void writeRepresentation(
      String sTypePrefix, String sType0, String sType1,
      String sCaption, String sDescription)
      throws IOException
   {
      assert sTypePrefix != null;
      assert sType0 != null;

      m_writer.openElement("representation");
         m_writer.writeAttribute("mediaType", "text/xml");
         m_writer.writeAttribute("element", sTypePrefix, ":", sType0, sType1);

      if (m_bIncludeDocumentation && (sCaption != null || sDescription != null))
      {
         m_writer.closeElement();
            writeDoc(sCaption, sDescription);
         m_writer.endElement("representation");
      }
      else
      {
         m_writer.closeEmptyElement();
      }
   }

   /**
    * Output WADL resource XML for a metaclass.
    * @param metaclass The metaclass to process.
    * @throws IOException On IO error.
    */
   protected void writeResource(Metaclass metaclass) throws IOException
   {
      m_writer.openElement("resource");
         m_writer.writeAttribute("path", metaclass.getName());
         m_writer.writeAttribute("queryType", "text/xml");
      m_writer.closeElement();

         m_writer.openElement("param");
            m_writer.writeAttribute("href", "#authorization");
         m_writer.closeEmptyElement();

         m_writer.openElement("method");
            m_writer.writeAttribute("href", "#read-request");
         m_writer.closeEmptyElement();

         m_writer.openElement("method");
            m_writer.writeAttribute("name", "POST");
         m_writer.closeElement();

            m_writer.startElement("request");
               writeParam("attributes", null);
               writeRepresentation(XML.TNS_NS, metaclass.getName(), null, null, null);
            m_writer.endElement("request");

            m_writer.startElement("response");

               writeRepresentation(XML.TNS_NS, metaclass.getName(), null, null, null);
               writeRepresentation(XML.TNS_NS, XML.BASE_PREFIX, "Nil", null, null);

               m_writer.openElement("fault");
                  m_writer.writeAttribute("href", "#exception");
               m_writer.closeEmptyElement();
            m_writer.endElement("response");

         m_writer.endElement("method");

         // output static event method hooks
         for (int i = 0, nCount = metaclass.getEventCount(); i < nCount; ++i)
         {
            Event event = metaclass.getEvent(i);

            if (event.isStatic() && isExportedEvent(event))
            {
               writeEventMethod(event, null);
            }
         }

         writeEventResources(metaclass, true); // output static event resource hooks

         m_writer.openElement("resource");
            m_writer.writeAttribute("path", "{oid}");
            m_writer.writeAttribute("queryType", "text/xml");
         m_writer.closeElement();

            m_writer.openElement("param");
               m_writer.writeAttribute("href", "#authorization");
            m_writer.closeEmptyElement();

            m_writer.openElement("param");
               m_writer.writeAttribute("name", "oid");
               m_writer.writeAttribute("style", "template");
               m_writer.writeAttribute("type", XML.XSD_NS, ":hexBinary");
            m_writer.closeEmptyElement();

            m_writer.openElement("method");
               m_writer.writeAttribute("href", "#read-request");
            m_writer.closeEmptyElement();

            m_writer.openElement("method");
               m_writer.writeAttribute("name", "POST");
            m_writer.closeElement();

               m_writer.startElement("request");
                  writeParam("attributes", null);
                  writeRepresentation(XML.TNS_NS, metaclass.getName(), null, null, null);
               m_writer.endElement("request");

               m_writer.startElement("response");

                  writeRepresentation(XML.TNS_NS, metaclass.getName(), null, null, null);
                  writeRepresentation(XML.TNS_NS, XML.BASE_PREFIX, "Nil", null, null);

                  m_writer.openElement("fault");
                     m_writer.writeAttribute("href", "#exception");
                  m_writer.closeEmptyElement();
               m_writer.endElement("response");
            m_writer.endElement("method");

            m_writer.openElement("method");
               m_writer.writeAttribute("name", "PUT");
            m_writer.closeElement();

               m_writer.startElement("request");
                  writeParam("attributes", null);
                  writeRepresentation(XML.TNS_NS, metaclass.getName(), null, null, null);
               m_writer.endElement("request");

               m_writer.startElement("response");

                  writeRepresentation(XML.TNS_NS, metaclass.getName(), null, null, null);

                  m_writer.openElement("fault");
                     m_writer.writeAttribute("href", "#exception");
                  m_writer.closeEmptyElement();
               m_writer.endElement("response");

            m_writer.endElement("method");


            m_writer.openElement("method");
               m_writer.writeAttribute("name", "DELETE");
            m_writer.closeElement();

               m_writer.startElement("request");
               m_writer.endElement("request");

               m_writer.startElement("response");
                  m_writer.openElement("fault");
                     m_writer.writeAttribute("href", "#exception");
                  m_writer.closeEmptyElement();
               m_writer.endElement("response");

            m_writer.endElement("method");

            // output non-static event method hooks
            for (int i = 0, nCount = metaclass.getEventCount(); i < nCount; ++i)
            {
               Event event = metaclass.getEvent(i);

               if (!event.isStatic() && isExportedEvent(event))
               {
                  writeEventMethod(event, null);
               }
            }

            writeEventResources(metaclass, false); // output static event resource hooks

         m_writer.endElement("resource");

      m_writer.endElement("resource");
   }

   /**
    * Reset all member caches.
    * Any classes overriding this function will have to call this function as well
    * (i.e. their parent's function).
    */
   protected void reset()
   {
      super.reset();

      setRequiredType(NIL_TYPE); // add required static NIL type used by response representations
      m_requiredArrayTypeSet = new HashHolder/*<Metaclass>*/();
   }
}