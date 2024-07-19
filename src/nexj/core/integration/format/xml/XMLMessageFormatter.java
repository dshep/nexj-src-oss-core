// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.xml;

import java.io.IOException;
import java.io.Writer;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nexj.core.integration.IntegrationException;
import nexj.core.integration.PrimitiveFormatter;
import nexj.core.integration.MessageFormatter;
import nexj.core.integration.Output;
import nexj.core.meta.MetadataLookupException;
import nexj.core.meta.Primitive;
import nexj.core.meta.integration.CompositeMessagePart;
import nexj.core.meta.integration.CompositeMessagePartRef;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.MessagePart;
import nexj.core.meta.integration.PrimitiveMessagePart;
import nexj.core.meta.integration.format.xml.RootXMLMessagePartMapping;
import nexj.core.meta.integration.format.xml.XMLMessagePartMapping;
import nexj.core.meta.integration.format.xml.XMLNamespace;
import nexj.core.meta.integration.service.Interface;
import nexj.core.rpc.TransferObject;
import nexj.core.rpc.soap.SOAP;
import nexj.core.rpc.xml.XML;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.util.Binary;
import nexj.core.util.HashTab;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.SOAPUtil;
import nexj.core.util.StringUtil;
import nexj.core.util.XMLWriter;

/**
 * XML message formatter.
 */
public class XMLMessageFormatter implements MessageFormatter, InvocationContextAware
{
   // associations

   /**
    * The XML writer.
    */
   protected XMLWriter m_writer;

   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   /**
    * The primitive value formatter.
    */
   protected PrimitiveFormatter m_primitiveFormatter;

   /**
    * Namespace prefix to URI map.
    */
   protected Lookup m_namespaceMap = new HashTab();  // of type String[String]

   /**
    * Holds the old namespaces.
    * [Name1, URI1, Name2, URI2, Marker, ...]
    * Marker is used to delineate the boundary between message parts. The marker that is used
    * is this, the current XMLMessageFormatter instance.
    */
   protected List m_namespaceStack = new ArrayList();

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(XMLMessageFormatter.class);

   /**
    * Map of framework primitive types to their XML schema equivalents.
    */
   protected final static Lookup s_primitiveNameMap = new HashTab(9);

   static
   {
      s_primitiveNameMap.put(Primitive.STRING, "string");
      s_primitiveNameMap.put(Primitive.BINARY, "base64Binary");
      s_primitiveNameMap.put(Primitive.INTEGER, "integer");
      s_primitiveNameMap.put(Primitive.LONG, "long");
      s_primitiveNameMap.put(Primitive.DECIMAL, "decimal");
      s_primitiveNameMap.put(Primitive.FLOAT, "float");
      s_primitiveNameMap.put(Primitive.DOUBLE, "double");
      s_primitiveNameMap.put(Primitive.TIMESTAMP, "dateTime");
      s_primitiveNameMap.put(Primitive.BOOLEAN, "boolean");
   }

   // operations

   /**
    * @see nexj.core.integration.MessageFormatter#format(nexj.core.rpc.TransferObject, nexj.core.meta.integration.Message, nexj.core.integration.Output)
    */
   public void format(TransferObject tobj, Message message, Output out) throws IntegrationException
   {
      try
      {
         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Formatting XML message \"" + message.getName() + "\"");
            s_logger.dump(tobj);
         }

         Writer writer = out.getWriter();

         m_writer = (writer instanceof XMLWriter) ? (XMLWriter)writer : new XMLWriter(writer);
         m_writer.write(XML.HEADER);

         append1(tobj, message.getRoot(), null);
      }
      catch (Exception e)
      {
         throw new IntegrationException("err.integration.format",
            new Object[]{message.getName()}, e);
      }
      finally
      {
         m_namespaceMap.clear();
         m_namespaceStack.clear();
      }
   }

   /**
    * Appends a message part to the writer.
    * @param obj The object representing the message part.
    * @param part The message part metadata.
    * @param parentPart The parent message part metadata.
    * @throws IOException if the writer fails.
    */
   protected void append(Object obj, MessagePart part, CompositeMessagePart parentPart) throws IOException
   {
      if (part.isCollection())
      {
         if (obj == null)
         {
            if (part.isRequired())
            {
               throw new IntegrationException("err.integration.minPartCount", new Object[]{part.getFullPath()});
            }
         }
         else
         {
            List list = (List)obj;
            int nCount = list.size();
            
            if (nCount < part.getMinCount())
            {
               throw new IntegrationException("err.integration.minPartCount", new Object[]{part.getFullPath()});
            }
            
            if (nCount > part.getMaxCount())
            {
               throw new IntegrationException("err.integration.maxPartCount", new Object[]{part.getFullPath()});
            }
            
            for (int i = 0; i < nCount; ++i)
            {
               append1(list.get(i), part, parentPart);
            }
         }
      }
      else
      {
         append1(obj, part, parentPart);
      }
   }

   /**
    * Appends one message part to the writer.
    * @param obj The object representing the message part.
    * @param part The message part metadata.
    * @param parentPart The parent message part metadata.
    * @throws IOException if the writer fails.
    */
   protected void append1(Object obj, MessagePart part, CompositeMessagePart parentPart) throws IOException
   {
      XMLMessagePartMapping derivedMapping = (XMLMessagePartMapping)part.getMapping();
      String sNamespace = (derivedMapping.getNamespace() == null) ? null : derivedMapping.getNamespace().getName();
      String sNodeName = derivedMapping.getNodeName();
      boolean bAppendType = false;

      if (obj == null && part.isRequired())
      {
         throw new IntegrationException("err.integration.minPartCount", new Object[]{part.getFullPath()});
      }

      if (obj != null && part instanceof CompositeMessagePart)
      {
         TransferObject tobj = (TransferObject)obj;
         String sDerivedMessageName = tobj.getClassName();

         if (!StringUtil.isEmpty(sDerivedMessageName))
         {
            Message derivedMessage = m_context.getMetadata().getMessage(sDerivedMessageName);
            Message baseMessage = null;

            if (parentPart == null)  // root of message (includes root part referencing another message)
            {
               baseMessage = ((RootXMLMessagePartMapping)part.getRoot().getMapping()).getRootMessage();
            }
            else if (part instanceof CompositeMessagePartRef)
            {
               CompositeMessagePart referencedPart = ((CompositeMessagePartRef)part).getRefPart();

               baseMessage = ((RootXMLMessagePartMapping)referencedPart.getMapping()).getRootMessage();
            }

            if (baseMessage != null)
            {
               Message.validatePolymorphism(baseMessage, derivedMessage, part);

               if (baseMessage != derivedMessage)
               {
                  part = derivedMessage.getRoot();
                  bAppendType = true;
                  derivedMapping = (XMLMessagePartMapping)part.getMapping();
               }
            }
         }
      }

      if (((RootXMLMessagePartMapping)part.getRoot().getMapping()).getRootMessage().getDerivation() == Message.DERIVATION_ABSTRACT)
      {
         throw new IntegrationException("err.integration.abstractMessage",
            new Object[]{((RootXMLMessagePartMapping)part.getRoot().getMapping()).getRootMessage().getName(), part.getFullPath()});
      }

      if (part instanceof CompositeMessagePart)
      {
         if (obj != null)
         {
            CompositeMessagePart composite = (CompositeMessagePart)part;
            TransferObject tobj = (TransferObject)obj;
            RootXMLMessagePartMapping rootMapping = null;
            MessagePart headersPart = null;
            boolean bEnvelopeStarted = false;

            // append the namespace and the schema
            if (parentPart == null)
            {
               rootMapping = (RootXMLMessagePartMapping)derivedMapping;
               headersPart = rootMapping.getHeaderPart();
               bEnvelopeStarted = appendEnvelopeStart(rootMapping, tobj);
               m_writer.setNamespace(sNamespace);
               m_writer.openElement(sNodeName);
               appendNamespaces(rootMapping, false, !bEnvelopeStarted && bAppendType);
               appendSchemaLocation(rootMapping);
            }
            else
            {
               m_writer.setNamespace(sNamespace);
               m_writer.openElement(sNodeName);

               // Top-level elements of the header need to declare the root
               // namespaces, because they are not under the root element.

               rootMapping = (RootXMLMessagePartMapping)parentPart.getRoot().getMapping();

               if (rootMapping.getHeaderPart() == parentPart)
               {
                  appendNamespaces(rootMapping, false, false);
               }
               else
               {
                  rootMapping = null; // prevent call to cleanupNamespaces
               }
            }

            appendAttributes(tobj, composite);

            if (bAppendType)
            {
               appendType((RootXMLMessagePartMapping)derivedMapping);
            }

            if (appendContents(tobj, composite, headersPart))
            {
               m_writer.setNamespace(sNamespace);
               m_writer.endElement(sNodeName);
            }

            if (bEnvelopeStarted)
            {
               appendEnvelopeEnd(rootMapping);
            }

            if (rootMapping != null)
            {
               cleanupNamespaces();
            }
         }
         else if (!derivedMapping.hasRequiredAttributes()) // skip element with null data if it has required attributes
         {
            // write element with null data: either <elementName/> or <elementName xsi:nil="1"/>
            m_writer.setNamespace(sNamespace);
            m_writer.openElement(sNodeName);

            if (derivedMapping.isNillable())
            {
               m_writer.setNamespace(XMLNamespace.XSI_NAME);
               m_writer.writeAttribute("nil", "1");
            }

            m_writer.closeEmptyElement();
         }
      }
      else
      {
         PrimitiveMessagePart primitive = (PrimitiveMessagePart)part;

         if (obj == null)
         {
            if (derivedMapping.isNillable())
            {
               m_writer.setNamespace(sNamespace);
               m_writer.openElement(sNodeName);
               m_writer.setNamespace(XMLNamespace.XSI_NAME);
               m_writer.writeAttribute("nil", "1");
               m_writer.closeEmptyElement();
            }
         }
         else
         {
            m_writer.setNamespace(sNamespace);
            m_writer.openElement(sNodeName);

            // Write xsi:type for ANY parts
            if (primitive.getType() == Primitive.ANY && derivedMapping.getSubtype() == XMLMessagePartMapping.SUBTYPE_XSI)
            {
               appendType((String)s_primitiveNameMap.get(Primitive.primitiveOf(obj)));
            }

            m_writer.closeElement();
            appendValue(obj, primitive, false);
            m_writer.setNamespace(sNamespace);
            m_writer.endElement(sNodeName);
         }
      }
   }

   /**
    * Appends the element and value content of a given element. The given element's
    * start tag will always be closed. If the element is empty, the tag will be closed
    * with the empty element suffix "/>".
    * @param tobj The element data.
    * @param part The element.
    * @param headersPart The message header part (to be skipped).
    * @return True if the element has contents; false if this element is empty.
    * @throws IOException If the writer fails.
    */
   protected boolean appendContents(TransferObject tobj, CompositeMessagePart part, MessagePart headersPart)
      throws IOException
   {
      boolean bEmpty = true;
      boolean bMatch = false;
      boolean bRequired = false;
      boolean bSingle = part.getAggregation() == CompositeMessagePart.SINGLE;

      for (int i = 0, nPartCount = part.getPartCount(); i < nPartCount; ++i)
      {
         MessagePart childPart = part.getPart(i);
         XMLMessagePartMapping childMapping = (XMLMessagePartMapping)childPart.getMapping();
         byte nNodeType = childMapping.getNodeType();

         // Skip the header
         if (childPart == headersPart)
         {
            continue;
         }

         if (nNodeType == XMLMessagePartMapping.ELEMENT)
         {
            if (bSingle)
            {
               bRequired |= childPart.isRequired();
            }

            if (childMapping.isNillable() || tobj.hasValue(childPart.getName()))
            {
               if (bSingle)
               {
                  if (bMatch)
                  {
                     throw new IntegrationException("err.integration.multipleParts",
                           new Object[]{part.getFullPath()});
                  }

                  bMatch = true;
               }

               if (bEmpty)
               {
                  m_writer.closeElement();
               }

               bEmpty = false;
               append(tobj.findValue(childPart.getName()), childPart, part);
            }
            else if (!bSingle && childPart.isRequired())
            {
               throw new IntegrationException("err.integration.minPartCount", new Object[]{childPart.getFullPath()});
            }
         }
         else if (nNodeType == XMLMessagePartMapping.VALUE)
         {
            PrimitiveMessagePart contentPart = (PrimitiveMessagePart)childPart;

            if (contentPart.getType() == Primitive.ANY)
            {
               Object value = tobj.findValue(contentPart.getName());

               if (bEmpty)
               {
                  m_writer.closeElement();
               }

               bEmpty = false;
               appendValue(value, contentPart, true);
            }
         }
      }

      if (bRequired && !bMatch)
      {
         throw new IntegrationException("err.integration.missingPart",
               new Object[]{part.getFullPath()});
      }

      // Append element content, if exists
      XMLMessagePartMapping mapping = (XMLMessagePartMapping)part.getMapping();
      PrimitiveMessagePart contentPart = mapping.getValuePart();
      Object content = null;

      if (contentPart != null && contentPart.getType() != Primitive.ANY)
      {
         content = tobj.findValue(mapping.getValuePart().getName());
      }

      if (content != null)
      {
         if (bEmpty)
         {
            m_writer.closeElement();
         }

         appendValue(content, mapping.getValuePart(), true);
         bEmpty = false;
      }
      else if (bEmpty)
      {
         m_writer.closeEmptyElement();
      }

      return !bEmpty;
   }

   /**
    * Appends the attributes.
    * @param tobj The key and values for the attributes.
    * @param part The part whose attributes shall be appended.
    * @throws IOException If the writer fails.
    */
   protected void appendAttributes(TransferObject tobj, CompositeMessagePart part) throws IOException
   {
      for (int i = 0, nPartCount = part.getPartCount(); i < nPartCount; ++i)
      {
         MessagePart childPart = part.getPart(i);
         XMLMessagePartMapping childMapping = (XMLMessagePartMapping)childPart.getMapping();
         int nChildNodeType = childMapping.getNodeType();

         if (nChildNodeType == XMLMessagePartMapping.ATTRIBUTE)
         {
            if (tobj.hasValue(childPart.getName()))
            {
               m_writer.setNamespace((childMapping.getNamespace() == null) ? null : childMapping.getNamespace().getName());
               m_writer.writeAttribute(childMapping.getNodeName(), toString((PrimitiveMessagePart)childPart, tobj.getValue(childPart.getName())));
            }
            else
            {
               if (childPart.isRequired())
               {
                  throw new IntegrationException("err.integration.minPartCount", new Object[]{childPart.getFullPath()});
               }
            }
         }
      }

      XMLMessagePartMapping mapping = (XMLMessagePartMapping)part.getMapping();
      MessagePart valuePart = mapping.getValuePart();

      if (valuePart != null && mapping.isNillable() && tobj.findValue(valuePart.getName()) == null)
      {
         m_writer.setNamespace(XMLNamespace.XSI_NAME);
         m_writer.writeAttribute("nil", "1");
      }
   }

   /**
    * Writes an element value.
    * 
    * @param data The value.
    * @param part The message part metadata.
    * @param bIterate True to iterate a collection; false otherwise.
    * @throws IOException If the writer fails.
    */
   protected void appendValue(Object data, PrimitiveMessagePart part, boolean bIterate) throws IOException
   {
      if (part.getType() == Primitive.ANY)
      {
         List list = null;
         int nCount = 1;

         if (bIterate && part.isCollection())
         {
            list = (List)data;
            nCount = list.size();

            if (nCount < part.getMinCount())
            {
               throw new IntegrationException("err.integration.minPartCount", new Object[]{part.getFullPath()});
            }

            if (nCount > part.getMaxCount())
            {
               throw new IntegrationException("err.integration.maxPartCount", new Object[]{part.getFullPath()});
            }
         }

         for (int i = 0; i < nCount; i++)
         {
            if (list != null)
            {
               data = list.get(i);
            }

            if (data instanceof TransferObject)
            {
               // Write non-primitive ANY-type data
               String sClassName = ((TransferObject)data).getClassName();
               Interface iface = ((XMLMessagePartMapping)part.getMapping()).getInterface();
               Message msg;

               try
               {
                  if (iface != null)
                  {
                     msg = iface.getRequestTable().findMessage(sClassName);

                     if (msg == null)
                     {
                        msg = iface.getResponseTable().findMessage(sClassName);
                     }

                     // Support inheritance in xsd:anyType tables
                     if (msg == null)
                     {
                        Message derivedMessage = m_context.getMetadata().getMessage(sClassName);

                        msg = iface.getRequestTable().findBaseMessage(derivedMessage);

                        if (msg == null)
                        {
                           msg = iface.getResponseTable().findBaseMessage(derivedMessage);
                        }

                        if (msg == null)
                        {
                           throw new IntegrationException("err.integration.xml.unknownMessage",
                              new Object[] {sClassName, part.getFullPath()});
                        }
                     }
                  }
                  else
                  {
                     msg = m_context.getMetadata().getMessage(sClassName);
                  }
               }
               catch (MetadataLookupException ex)
               {
                  throw new IntegrationException("err.integration.xml.unknownMessage",
                     new Object[] {sClassName, part.getFullPath()}, ex);
               }

               append(data, msg.getRoot(), null);
            }
            else
            {
               if (((XMLMessagePartMapping)part.getMapping()).getSubtype() == XMLMessagePartMapping.SUBTYPE_XSI)
               {
                  // Write primitive ANY-type data
                  if (data instanceof Timestamp)
                  {
                     m_writer.writeValue(SOAPUtil.formatDateTime((Timestamp)part.validateValue(data)));
                  }
                  else if (data instanceof Binary)
                  {
                     m_writer.writeValue(((Binary)part.validateValue(data)).toBase64());
                  }
                  else
                  {
                     m_writer.writeValue(Primitive.toString(data));
                  }
               }
               else
               {
                  // Write literal XML data
                  m_writer.write((String)data);
               }
            }
         }
      }
      else
      {
         m_writer.writeValue(toString(part, data));
      }
   }

   /**
    * Appends the namespace attributes and pushes a new namespace tracking record.
    * @param rootMapping The root XML mapping.
    * @param bInEnvelope We are placing namespaces in the envelope's opening tag.
    * @param bAppendXSI True to ensure that the XSI namespace is appended, even if it is not in the mapping.
    */
   protected void appendNamespaces(RootXMLMessagePartMapping rootMapping, boolean bInEnvelope, boolean bAppendXSI) throws IOException
   {
      m_namespaceStack.add(this);  // push new record

      if (rootMapping.getNamespaceCount() == 0 && !bAppendXSI)
      {
         return;
      }

      m_writer.setNamespace(SOAP.XML_NS);

      for (Iterator itr = rootMapping.getNamespaceIterator(); itr.hasNext();)
      {
         XMLNamespace ns = (XMLNamespace)itr.next();

         if (ns.isDeclaredByEnvelope() == bInEnvelope)
         {
            appendNamespace(ns.getName(), ns.getURI());

            if (XMLNamespace.XSI.equals(ns.getURI()))
            {
               bAppendXSI = false;
            }
         }
      }

      if (bAppendXSI)
      {
         appendNamespace(XMLNamespace.XSI_NAME, XMLNamespace.XSI);
      }
   }

   /**
    * Pops and discards the namespace definitions at the current level.
    */
   protected void cleanupNamespaces()
   {
      int nLast = m_namespaceStack.size() - 1;

      while (nLast >= 0)
      {
         Object value = m_namespaceStack.remove(nLast--);

         if (value == this)
         {
            break;
         }

         if (value == null)
         {
            m_namespaceMap.remove(m_namespaceStack.remove(nLast--));
         }
         else
         {
            m_namespaceMap.put(m_namespaceStack.remove(nLast--), value);
         }
      }
   }

   /**
    * Appends a namespace attribute.
    * @param sAlias The namespace alias.
    * @param sURI The namespace URI.
    * @throws IOException If the writer fails.
    */
   protected void appendNamespace(String sAlias, String sURI) throws IOException
   {
      String sOldURI = (String)m_namespaceMap.put(sAlias, sURI);

      if (sOldURI == null || !sOldURI.equals(sURI))
      {
         m_writer.writeAttribute(sAlias, sURI);
         m_namespaceStack.add(sAlias);
         m_namespaceStack.add(sOldURI);
      }
   }

   /**
    * Appends the start of the envelope. The message body is be written between
    * the envelope start and the envelope end.
    * @param rootMapping The root part mapping.
    * @param tobj The message data (for header retrieval).
    * @return True if an envelope was appended; false otherwise.
    * @throws IOException If the writer fails.
    */
   protected boolean appendEnvelopeStart(RootXMLMessagePartMapping rootMapping,
      TransferObject tobj) throws IOException
   {
      String sEnvelopeNamespace;

      switch (rootMapping.getEnvelope())
      {
         case RootXMLMessagePartMapping.ENVELOPE_SOAP:
            sEnvelopeNamespace = XMLNamespace.SOAP_NAME;
            break;

         case RootXMLMessagePartMapping.ENVELOPE_SOAP12:
            sEnvelopeNamespace = XMLNamespace.SOAP12_NAME;
            break;

         default:
            return false;
      }

      m_writer.setNamespace(sEnvelopeNamespace);
      m_writer.openElement("Envelope");
      appendNamespaces(rootMapping, true, false);
      m_writer.closeElement();

      MessagePart headersPart = rootMapping.getHeaderPart();

      if (headersPart != null)
      {
         Object value = tobj.findValue(headersPart.getName());

         if (value != null || headersPart.isRequired())
         {
            append(value, headersPart, (CompositeMessagePart)rootMapping.getMessagePart());
         }
      }

      m_writer.setNamespace(sEnvelopeNamespace);
      m_writer.startElement("Body");

      return true;
   }

   /**
    * Appends the end of the envelope. The message body is written between the
    * envelope start and the envelope end.
    * @param rootMapping The root part mapping.
    * @throws IOException If the writer fails.
    */
   protected void appendEnvelopeEnd(RootXMLMessagePartMapping rootMapping) throws IOException
   {
      switch (rootMapping.getEnvelope())
      {
         case RootXMLMessagePartMapping.ENVELOPE_SOAP:
            m_writer.setNamespace(XMLNamespace.SOAP_NAME);
            break;

         case RootXMLMessagePartMapping.ENVELOPE_SOAP12:
            m_writer.setNamespace(XMLNamespace.SOAP12_NAME);
            break;
      }

      m_writer.endElement("Body");
      m_writer.endElement("Envelope");
      cleanupNamespaces();
   }

   /**
    * Appends the "schemaLocation" attribute.
    * @param rootMapping The root part mapping.
    * @throws IOException If the writer fails.
    */
   protected void appendSchemaLocation(RootXMLMessagePartMapping rootMapping) throws IOException
   {
      Iterator itr = rootMapping.getNamespaceIterator();
      boolean bWroteSchemaLocation = false;

      while (itr.hasNext())
      {
         XMLNamespace namespace = (XMLNamespace)itr.next();

         if (namespace.getSchema() != null && !namespace.getSchema().equals(namespace.getURI()))
         {
            if (bWroteSchemaLocation)
            {
               m_writer.write(' ');
            }
            else
            {
               m_writer.setNamespace(XMLNamespace.XSI_NAME);
               m_writer.openAttribute("schemaLocation");
               bWroteSchemaLocation = true;
            }

            m_writer.writeValue(namespace.getURI());
            m_writer.write(' ');
            m_writer.writeValue(namespace.getSchema());
         }
      }

      if (bWroteSchemaLocation)
      {
         m_writer.closeAttribute();
      }
   }

   /**
    * Appends the xsi:type attribute.
    * @param mapping The part mapping (to get namespace alias and type name).
    * @throws IOException If the writer fails.
    */
   protected void appendType(RootXMLMessagePartMapping mapping) throws IOException
   {
      if (mapping != null && mapping.getXSDType() != null)
      {
         XMLNamespace namespace = mapping.getNamespace();

         m_writer.setNamespace(XMLNamespace.XSI_NAME);

         if (namespace != null)
         {
            m_writer.writeAttribute("type", namespace.getName(), ":", mapping.getXSDType());
         }
         else
         {
            m_writer.writeAttribute("type", mapping.getXSDType());
         }
      }
   }

   /**
    * Appends the xsi:type attribute for a primitive type.
    * @param sXSIType The primitive type name.
    * @throws IOException If the writer fails.
    */
   protected void appendType(String sXSIType) throws IOException
   {
      if (sXSIType != null)
      {
         m_writer.setNamespace(XMLNamespace.XSI_NAME);
         m_writer.writeAttribute("type", XMLNamespace.XSD_NAME + ":", sXSIType);
      }
   }

   /**
    * Converts an value to string.
    * @param part The primitive message part.
    * @param value The value to convert.
    */
   protected String toString(PrimitiveMessagePart part, Object value)
   {
      if (value == null)
      {
         return null;
      }

      XMLMessagePartMapping mapping = (XMLMessagePartMapping)part.getMapping();

      if (mapping.getFormat() != null)
      {
         if (m_primitiveFormatter == null)
         {
            m_primitiveFormatter = new PrimitiveFormatter(m_context);
         }

         return m_primitiveFormatter.format(value, part);
      }

      switch (mapping.getSubtype())
      {
         case XMLMessagePartMapping.SUBTYPE_DATE:
            return SOAPUtil.formatDate((Timestamp)part.validateValue(Primitive.toTimestamp(value)));

         case XMLMessagePartMapping.SUBTYPE_TIME:
            return SOAPUtil.formatTime((Timestamp)part.validateValue(Primitive.toTimestamp(value)));

         case XMLMessagePartMapping.SUBTYPE_DATETIME:
            return SOAPUtil.formatDateTime((Timestamp)part.validateValue(Primitive.toTimestamp(value)));

         case XMLMessagePartMapping.SUBTYPE_BASE64:
            return ((Binary)part.validateValue(Primitive.toBinary(value))).toBase64();
      }

      return Primitive.toString(part.convertValue(value));
   }

   /**
    * @see nexj.core.runtime.InvocationContextAware#setInvocationContext(nexj.core.runtime.InvocationContext)
    */
   public void setInvocationContext(InvocationContext context)
   {
      m_context = context;
   }
}
