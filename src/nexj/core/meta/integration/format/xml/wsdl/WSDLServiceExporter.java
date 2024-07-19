// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.xml.wsdl;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.Set;

import nexj.core.meta.integration.format.xml.XMLNamespace;
import nexj.core.meta.integration.format.xml.schema.Element;
import nexj.core.meta.integration.format.xml.schema.ElementRef;
import nexj.core.meta.integration.format.xml.schema.Schema;
import nexj.core.meta.integration.format.xml.schema.SchemaUniverse;
import nexj.core.meta.integration.format.xml.schema.XSDSchema;
import nexj.core.meta.integration.format.xml.schema.XSDSchemaExporter;
import nexj.core.rpc.xml.XML;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.util.HashHolder;
import nexj.core.util.Lookup;
import nexj.core.util.Named;
import nexj.core.util.ObjUtil;
import nexj.core.util.XMLWriter;

/**
 * Exports a web service definition as WSDL 1.1 (WS-I Basic Profile 1.1).
 */
public class WSDLServiceExporter implements InvocationContextAware
{
   // constants

   /**
    * WSDL body parameters name.
    */
   public static final String BODY_PARAMETERS = "parameters";

   // associations

   /**
    * The output writer.
    */
   protected XMLWriter m_writer;

   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   // operations

   /**
    * @see nexj.core.runtime.InvocationContextAware#setInvocationContext(nexj.core.runtime.InvocationContext)
    */
   public void setInvocationContext(InvocationContext context)
   {
      m_context = context;
   }

   /**
    * Export a web service definition.
    * @param service The definition to export.
    * @param writer The output writer.
    * @throws IOException If an I/O error occurs.
    */
   public void exportService(SOAPService service, Writer writer) throws IOException
   {
      init(writer);

      // WSDL namespaces
      SchemaUniverse universe = service.getUniverse();
      Schema tnsSchema = universe.getSchema(service.getURI(), "tns");
      Schema soapBindSchema = universe.getSchema(XMLNamespace.WSDL_SOAP, XMLNamespace.WSDL_SOAP_NAME);
      Set excludedSchemaSet = new HashHolder();

      excludedSchemaSet.add(tnsSchema);
      excludedSchemaSet.add(soapBindSchema);
      excludedSchemaSet.add(XSDSchema.XSD);
      universe.addSchema(XSDSchema.XSD);
      universe.resolvePrefixes();

      String sTNSPrefix = universe.getPrefix(tnsSchema);
      String sSOAPBindPrefix = universe.getPrefix(soapBindSchema);

      // WSDL definitions tag
      m_writer.openElement("definitions");
      m_writer.writeAttribute("name", service);
      m_writer.writeAttribute("targetNamespace", tnsSchema.getURI());
      m_writer.writeAttribute(XML.XML_NS, XMLNamespace.WSDL);

      // Write namespace declarations
      String[] sPrefixArray = new String[universe.getPrefixCount()];

      ObjUtil.copy(sPrefixArray, universe.getPrefixIterator());
      Arrays.sort(sPrefixArray);

      m_writer.setNamespace(XML.XML_NS);

      for (int i = 0; i < sPrefixArray.length; i++)
      {
         String sPrefix = sPrefixArray[i];
         String sURI = universe.findSchema(sPrefix).getURI();

         m_writer.writeAttribute(sPrefix, sURI);
      }

      m_writer.setNamespace(null);
      m_writer.closeElement();

      writeTypes(service, sPrefixArray, excludedSchemaSet);
      writeMessages(service);

      ArrayList operationList = new ArrayList();

      for (Iterator itr = service.getOperationIterator(); itr.hasNext(); )
      {
         operationList.add(itr.next());
      }

      Collections.sort(operationList, Named.COMPARATOR);

      writePortType(service, operationList, sTNSPrefix);
      writeBinding(service, operationList, sTNSPrefix, sSOAPBindPrefix);
      writeService(service, sTNSPrefix, sSOAPBindPrefix);

      m_writer.endElement("definitions");
   }

   /**
    * Initializes the XML writer.
    * @param writer The destination writer.
    * @throws IOException If an I/O error occurs.
    */
   protected void init(Writer writer) throws IOException
   {
      m_writer = (writer instanceof XMLWriter) ? (XMLWriter)writer : new XMLWriter(writer);
      m_writer.setNamespace(null);
      m_writer.write(XML.HEADER);
   }

   /**
    * Writes the type definitions using XML Schema.
    * @param service The service definition.
    * @param sPrefixArray The namespace prefixes of the schemas to write, in the order in which they should
    * be written.
    * @param excludedSchemaSet Set of schemas to skip writing.
    * @throws IOException If an I/O error occurs.
    */
   protected void writeTypes(SOAPService service, String[] sPrefixArray, Set excludedSchemaSet) throws IOException
   {
      XSDSchemaExporter schemaExporter = new XSDSchemaExporter(service.getUniverse(), true);

      schemaExporter.setInvocationContext(m_context);
      m_writer.startElement("types");

      for (int i = 0; i < sPrefixArray.length; i++)
      {
         String sPrefix = sPrefixArray[i];
         Schema schema = service.getUniverse().findSchema(sPrefix);
         String sURI = schema.getURI();

         if (sURI.equals(XMLNamespace.SOAP) || sURI.equals(XMLNamespace.SOAP12))
         {
            continue;
         }

         if (schema != null && !excludedSchemaSet.contains(schema) && schema.getItemCount() > 0)
         {
            schemaExporter.exportSchema(schema, m_writer);
         }
      }

      m_writer.endElement("types");
   }

   /**
    * Writes the WSDL messages.
    * @param service The service definition.
    * @throws IOException If an I/O error occurs.
    */
   protected void writeMessages(SOAPService service) throws IOException
   {
      ArrayList messageList = new ArrayList();

      for (Iterator itr = service.getMessageIterator(); itr.hasNext(); )
      {
         messageList.add(itr.next());
      }

      Collections.sort(messageList, Named.COMPARATOR);

      for (int i = 0, nCount = messageList.size(); i < nCount; i++)
      {
         Message msg = (Message)messageList.get(i);

         m_writer.openElement("message");
         m_writer.writeAttribute("name", msg);
         m_writer.closeElement();
         m_writer.openElement("part");
         m_writer.writeAttribute("name", BODY_PARAMETERS);
         m_writer.writeAttribute("element", service.getUniverse().getQualifiedName(msg.getRoot()));
         m_writer.closeEmptyElement();

         for (Lookup.Iterator itr = msg.getHeaderIterator(); itr.hasNext();)
         {
            itr.next();

            String sName = (String)itr.getKey();
            Element element = (Element)itr.getValue();
            Element elementWithSchema = element;

            while (elementWithSchema instanceof ElementRef)
            {
               elementWithSchema = ((ElementRef)elementWithSchema).getReferent();
            }

            m_writer.openElement("part");
            m_writer.writeAttribute("name", sName);
            m_writer.writeAttribute("element", service.getUniverse().getPrefix(elementWithSchema.getSchema()),
                  ":", element.getName());
            m_writer.closeEmptyElement();
         }

         m_writer.endElement("message");
      }
   }

   /**
    * Writes the WSDL portType.
    * @param service The service definition.
    * @param operationList The list of operations to write for the port.
    * @param sTNSPrefix The WSDL target namespace prefix.
    * @throws IOException If an I/O error occurs.
    */
   protected void writePortType(SOAPService service, ArrayList operationList, String sTNSPrefix) throws IOException
   {
      m_writer.openElement("portType");
      m_writer.writeAttribute("name", service.getName());
      m_writer.closeElement();

      for (int i = 0, nCount = operationList.size(); i < nCount; i++)
      {
         Operation operation = (Operation)operationList.get(i);

         m_writer.openElement("operation");
         m_writer.writeAttribute("name", operation);
         m_writer.closeElement();

         if (operation.getInput() != null)
         {
            m_writer.openElement("input");
            m_writer.writeAttribute("message", sTNSPrefix, ":", operation.getInput().getName());
            m_writer.closeEmptyElement();
         }

         if (operation.getOutput() != null)
         {
            m_writer.openElement("output");
            m_writer.writeAttribute("message", sTNSPrefix, ":", operation.getOutput().getName());
            m_writer.closeEmptyElement();
         }

         m_writer.endElement("operation");
      }

      m_writer.endElement("portType");
   }

   /**
    * Writes the WSDL binding.
    * @param service The service definition.
    * @param operationList The list of operations to write the bindings.
    * @param sTNSPrefix The WSDL target namespace prefix.
    * @param sSOAPBindPrefix The namespace prefix for the "soapbind" namespace.
    * @throws IOException If an I/O error occurs.
    */
   protected void writeBinding(SOAPService service, ArrayList operationList, String sTNSPrefix, String sSOAPBindPrefix)
      throws IOException
   {
      m_writer.openElement("binding");
      m_writer.writeAttribute("name", service);
      m_writer.writeAttribute("type", sTNSPrefix, ":", service.getName());
      m_writer.closeElement();
      m_writer.setNamespace(sSOAPBindPrefix);
      m_writer.openElement("binding");
      m_writer.setNamespace(null);
      m_writer.writeAttribute("style", "document");
      m_writer.writeAttribute("transport", "http://schemas.xmlsoap.org/soap/http");
      m_writer.closeEmptyElement();

      for (int i = 0, nCount = operationList.size(); i < nCount; i++)
      {
         Operation operation = (Operation)operationList.get(i);

         m_writer.openElement("operation");
         m_writer.writeAttribute("name", operation);
         m_writer.closeElement();
         m_writer.setNamespace(sSOAPBindPrefix);
         m_writer.openElement("operation");
         m_writer.setNamespace(null);
         m_writer.writeAttribute("soapAction", (operation.getAction() == null) ? "" : operation.getAction());
         m_writer.closeEmptyElement();
         writeMessageBind(operation.getInput(), "input", sTNSPrefix, sSOAPBindPrefix);
         writeMessageBind(operation.getOutput(), "output", sTNSPrefix, sSOAPBindPrefix);
         m_writer.endElement("operation");
      }

      m_writer.endElement("binding");
   }

   /**
    * Writes a message binding.
    * @param message The message to bind.
    * @param sType The operation message type; either "input" or "output".
    * @param sTNSPrefix The WSDL target namespace prefix.
    * @param sSOAPBindPrefix The namespace prefix for the "soapbind" namespace.
    * @throws IOException If an I/O error occurs.
    */
   protected void writeMessageBind(Message message, String sType, String sTNSPrefix, String sSOAPBindPrefix)
         throws IOException
   {
      if (message != null)
      {
         m_writer.startElement(sType);
         m_writer.setNamespace(sSOAPBindPrefix);
         m_writer.openElement("body");
         m_writer.setNamespace(null);
         m_writer.writeAttribute("use", "literal");
         m_writer.writeAttribute("parts", BODY_PARAMETERS);
         m_writer.closeEmptyElement();

         for (Lookup.Iterator itr = message.getHeaderIterator(); itr.hasNext();)
         {
            itr.next();
            m_writer.setNamespace(sSOAPBindPrefix);
            m_writer.openElement("header");
            m_writer.setNamespace(null);
            m_writer.writeAttribute("message", sTNSPrefix, ":", message.getName());
            m_writer.writeAttribute("part", (String)itr.getKey());
            m_writer.writeAttribute("use", "literal");
            m_writer.closeEmptyElement();
         }

         m_writer.endElement(sType);
      }
   }

   /**
    * Writes the WSDL service definition.
    * @param service The service definition.
    * @param sTNSPrefix The WSDL target namespace prefix.
    * @param sSOAPBindPrefix The namespace prefix for the "soapbind" namespace.
    * @throws IOException If an I/O error occurs.
    */
   protected void writeService(SOAPService service, String sTNSPrefix, String sSOAPBindPrefix) throws IOException
   {
      m_writer.openElement("service");
      m_writer.writeAttribute("name", service.getName());
      m_writer.closeElement();
      m_writer.openElement("port");
      m_writer.writeAttribute("name", service.getName());
      m_writer.writeAttribute("binding", sTNSPrefix, ":", service.getName());
      m_writer.closeElement();
      m_writer.setNamespace(sSOAPBindPrefix);
      m_writer.openElement("address");
      m_writer.setNamespace(null);
      m_writer.writeAttribute("location", service.getEndpoint());
      m_writer.closeEmptyElement();
      m_writer.endElement("port");
      m_writer.endElement("service");
   }
}
