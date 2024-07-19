// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.xml;

import java.io.IOException;

import javax.xml.namespace.QName;

import nexj.core.meta.Argument;
import nexj.core.meta.Event;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Primitive;
import nexj.core.meta.Type;

/**
 * Class to generate WSDL v1.1 API.
 */
public class WSDLGenerator extends XSDGenerator
{
   // attributes

   /**
    * Delimiter used between Metaclass name and the rest of the string in metaclass prefixed or
    * suffixed strings. e.g. Used for event message names.
    */
   protected final static String METACLASS_NAME_DELIMITER = "_";

   /**
    * The value for the soap:operation soapAction header.
    */
   protected String m_sSOAPAction;

   /**
    * MS.NET WSDL proxy generator does not generate types for arrays unless the types are explicitly
    * referenced in some other element. As a consequence MS.NET cannot dynamically use array types
    * unless at least one element in the XSD uses the specified array type. This is just such a
    * dummy element which is injected into the XSD for the purpose of referencing every declared
    * array type. This element is not used and cannot even be unmarshalled by the framework.
    */
   protected XMLMetatype m_arrayDeclarator =
      new XMLMetatype(XML.getTNSType("array-declarator"), true);

   // constructors

   /**
    * Constructor.
    * @param sSoapAction The value for the soapAction header to expect.
    */
   public WSDLGenerator(String sSOAPAction)
   {
      m_sSOAPAction = sSOAPAction;
   }

   // operations

   /**
    * @see nexj.core.rpc.xml.XSDGenerator#generate(java.lang.String)
    */
   protected void generate(String sURI) throws IOException
   {
      m_writer.openElement("definitions");
         m_writer.writeAttribute(XML.XML_NS, XML.NS_URI_WSDL);
         m_writer.setNamespace(XML.XML_NS);
            m_writer.writeAttribute(XML.XSD_NS, XML.XSD_URI);
            m_writer.writeAttribute(XML.NS_TAG_WSDL, XML.NS_URI_WSDL);
            m_writer.writeAttribute(XML.NS_TAG_WSDL_SOAP, XML.NS_URI_WSDL_SOAP);
            m_writer.writeAttribute(XML.ENC_NS, XML.ENC_URI);
            m_writer.writeAttribute(XML.TNS_NS, XML.NS_URI_TNS);
         m_writer.setNamespace(null);
         m_writer.writeAttribute("targetNamespace", XML.NS_URI_TNS);
      m_writer.closeElement();

         m_writer.startElement("types");

            m_writer.openElement("schema");
               m_writer.writeAttribute(XML.XML_NS, XML.XSD_URI);
               m_writer.writeAttribute("targetNamespace", XML.NS_URI_TNS);
               m_writer.writeAttribute("elementFormDefault", "qualified");
            m_writer.closeElement();

               writeTNSXSD();
               writeElement(
                  XML.BASE_PREFIX, "Request", null, XML.TNS_NS, XML.BASE_PREFIX, "Request");
               writeElement(
                  XML.BASE_PREFIX, "Response", null, XML.TNS_NS, XML.BASE_PREFIX, "Response");
               writeElement(
                  XML.BASE_PREFIX, "Exception", null, XML.TNS_NS, XML.BASE_PREFIX, "Exception");

               m_writer.openElement("element");
                  m_writer.writeAttribute("name", XML.BASE_PREFIX, "Change-Request");
               m_writer.closeElement();

                  m_writer.startElement("complexType");
                     m_writer.startElement("sequence");
                        writeElement("objects", null, Primitive.ANY, 0, -1, false, null);
                        writeElement("attributes", null, Primitive.STRING);
                     m_writer.endElement("sequence");
                  m_writer.endElement("complexType");

               m_writer.endElement("element");

               writeElement(
                  XML.BASE_PREFIX, "Change-Response", null,
                  XML.TNS_NS, XML.BASE_PREFIX, "Collection");

               m_writer.openElement("element");
                  m_writer.writeAttribute("name", XML.BASE_PREFIX, "Read-Request");
               m_writer.closeElement();

                  m_writer.startElement("complexType");
                     m_writer.startElement("sequence");
                        writeElement("class", null, Primitive.STRING);
                        writeElement("attributes", null, Primitive.STRING);
                        writeElement("where", null, Primitive.STRING);
                        writeElement("orderBy", null, Primitive.STRING);
                        writeElement("count", null, Primitive.INTEGER);
                       writeElement("offset", null, Primitive.INTEGER);
                     m_writer.endElement("sequence");
                  m_writer.endElement("complexType");

               m_writer.endElement("element");

               writeElement(
                  XML.BASE_PREFIX, "Read-Response", null,
                  XML.TNS_NS, XML.BASE_PREFIX, "Collection");

            m_writer.endElement("schema");

            for (int i = 0; i < m_metaclassArray.length; ++i)
            {
               Metaclass meta = m_metaclassArray[i];

               if (isExportedMetaclass(meta)) // for exported metaclasses only
               {
                  writeMetaclassSchema(meta); // metaclass specific event schema
               }
            }

         m_writer.endElement("types");

         m_writer.openElement("message");
            m_writer.writeAttribute("name", "invoke-request");
         m_writer.closeElement();

            m_writer.openElement("part");
               m_writer.writeAttribute("name", "request");
               m_writer.writeAttribute("element", XML.TNS_NS, ":", XML.BASE_PREFIX, "Request");
            m_writer.closeEmptyElement();

         m_writer.endElement("message");

         m_writer.openElement("message");
            m_writer.writeAttribute("name", "invoke-response");
         m_writer.closeElement();

            m_writer.openElement("part");
               m_writer.writeAttribute("name", "response");
               m_writer.writeAttribute("element", XML.TNS_NS, ":", XML.BASE_PREFIX, "Response");
            m_writer.closeEmptyElement();

         m_writer.endElement("message");

         writeMessage(
            "change-request", null, null, null, null, XML.BASE_PREFIX, "Change-Request", null);
         writeMessage(
            "change-response", null, null, null, null, XML.BASE_PREFIX, "Change-Response", null);
         writeMessage(
            "read-request", null, null, null, null, XML.BASE_PREFIX, "Read-Request", null);
         writeMessage(
            "read-response", null, null, null, null, XML.BASE_PREFIX, "Read-Response", null);

         for (int i = 0; i < m_metaclassArray.length; ++i)
         {
            Metaclass meta = m_metaclassArray[i];

            if (isExportedMetaclass(meta))
            {
               writeMessages(meta); // metaclass specific messages
            }
         }

         m_writer.openElement("portType");
            m_writer.writeAttribute("name", "Server");
         m_writer.closeElement();

            writePortTypeOperation(
               "invoke", null, "invoke-request", null, null, "invoke-response", null, null, null);
            writePortTypeOperation(
               "change", null, "change-request", null, null, "change-response", null, null, null);
            writePortTypeOperation(
               "read", null, "read-request", null, null, "read-response", null, null, null);

         m_writer.endElement("portType");

         for (int i = 0; i < m_metaclassArray.length; ++i)
         {
            Metaclass meta = m_metaclassArray[i];

            if (isExportedMetaclass(meta))
            {
               writePortType(meta); // metaclass specific PortType
            }
         }

         m_writer.openElement("binding");
            m_writer.writeAttribute("name", "GenericServer"); // MS.NET uses this as class name
            m_writer.writeAttribute("type", XML.TNS_NS, ":Server");
         m_writer.closeElement();

            m_writer.openElement("binding");
               m_writer.writeAttribute(XML.XML_NS, XML.NS_URI_WSDL_SOAP);
               m_writer.writeAttribute("style", "document");
               m_writer.writeAttribute("transport", XML.NS_URI_SOAP_HTTP);
            m_writer.closeEmptyElement();

            writeBindingOperation("invoke", null, true);
            writeBindingOperation("change", null, true);
            writeBindingOperation("read", null, true);

         m_writer.endElement("binding");

         for (int i = 0; i < m_metaclassArray.length; ++i)
         {
            Metaclass meta = m_metaclassArray[i];

            if (isExportedMetaclass(meta))
            {
               writeBinding(meta); // metaclass specific binding
            }
         }

         m_writer.openElement("service");
            m_writer.writeAttribute("name", "GenericServer");
         m_writer.closeElement();

            m_writer.openElement("port");
               m_writer.writeAttribute("name", "Server");
               m_writer.writeAttribute("binding", XML.TNS_NS, ":GenericServer");
            m_writer.closeElement();

               m_writer.openElement("address");
                  m_writer.writeAttribute(XML.XML_NS, XML.NS_URI_WSDL_SOAP);
                  m_writer.writeAttribute("location", sURI);
               m_writer.closeEmptyElement();

            m_writer.endElement("port");

            for (int i = 0; i < m_metaclassArray.length; ++i)
            {
               Metaclass meta = m_metaclassArray[i];

               if (isExportedMetaclass(meta))
               {
                  writeServicePort(meta, sURI); // metaclass specific service port
               }
            }

         m_writer.endElement("service");
      m_writer.endElement("definitions");
   }

   /**
    * @see nexj.core.rpc.xml.XMLSchemaGenerator#reset()
    */
   protected void reset()
   {
      super.reset();
      setRequiredType(m_arrayDeclarator); // MS.NET kludge to enable use of array types

      QName qType = getQualifiedTypeCached(DEFAULT_BASE_TYPE, true);

      m_arrayDeclarator.addElement( // add TransferObject-array to the MS.NET kludge
         new XMLMetatype.XMLMetatypeElement(DEFAULT_BASE_TYPE.getName(), qType, 0, 1, false));
   }

   /**
    * @see nexj.core.rpc.xml.XMLSchemaGenerator#setRequiredType(nexj.core.meta.Type, boolean)
    */
   protected void setRequiredType(Type type, boolean bArray)
   {
      if (bArray)
      {
         QName qType = getQualifiedTypeCached(type, true);

         // if this is a new array type then add it to the m_arrayDeclarator as an element
         // check the currently registered array types map rather then iterating list in the holder
         if (!m_requiredXMLMetatypeMap.contains(qType))
         {
            m_arrayDeclarator.addElement(
               new XMLMetatype.XMLMetatypeElement(type.getName(), qType, 0, 1, false));
         }
      }

      super.setRequiredType(type, bArray);
   }

   /**
    * Output WSDL SOAP binding XML for a metaclass.
    * @param metaclass The metaclass to process.
    * @throws IOException On IO error.
    */
   protected void writeBinding(Metaclass meta) throws IOException
   {
      // MS.NET uses this object as class containing all events
      m_writer.openElement("binding");
         m_writer.writeAttribute("name", meta.getName(), METACLASS_NAME_DELIMITER, "Server");
         m_writer.writeAttribute(
            "type", XML.TNS_NS, ":", meta.getName(), METACLASS_NAME_DELIMITER, "Server");
      m_writer.closeElement();

         m_writer.openElement("binding");
            m_writer.writeAttribute(XML.XML_NS, XML.NS_URI_WSDL_SOAP);
            m_writer.writeAttribute("style", "document");
            m_writer.writeAttribute("transport", XML.NS_URI_SOAP_HTTP);
         m_writer.closeEmptyElement();

         for (int i = 0, nCount = meta.getEventCount(); i < nCount; ++i)
         {
            Event event = meta.getEvent(i);

            if (isExportedEvent(event))
            {
               writeBindingOperation(computeElementName(event), null, isExportedEventResult(event));
            }
         }

      m_writer.endElement("binding");
   }

   /**
    * Output WSDL binding operation block.
    * @param sName0 The operation name (not null).
    * @param sName1 The second part of the operation name, concatenated with first, (null == ignore)
    * @param bOutput Include output binding.
    * @throws IOException On IO error.
    */
   protected void writeBindingOperation(String sName0, String sName1, boolean bOutput)
      throws IOException
   {
      assert sName0 != null;

      m_writer.openElement("operation");
         m_writer.writeAttribute("name", sName0, sName1);
      m_writer.closeElement();

         m_writer.openElement("operation");
            m_writer.writeAttribute(XML.XML_NS, XML.NS_URI_WSDL_SOAP);
            m_writer.writeAttribute("soapAction", m_sSOAPAction);
         m_writer.closeEmptyElement();

         m_writer.startElement("input");
            m_writer.openElement("body");
               m_writer.writeAttribute(XML.XML_NS, XML.NS_URI_WSDL_SOAP);
               m_writer.writeAttribute("encodingStyle", XML.ENC_URI);
               m_writer.writeAttribute("use", "literal");
            m_writer.closeEmptyElement();
         m_writer.endElement("input");

         if (bOutput)
         {
            m_writer.startElement("output");
               m_writer.openElement("body");
                  m_writer.writeAttribute(XML.XML_NS, XML.NS_URI_WSDL_SOAP);
                  m_writer.writeAttribute("encodingStyle", XML.ENC_URI);
                  m_writer.writeAttribute("use", "literal");
               m_writer.closeEmptyElement();
            m_writer.endElement("output");
         }

      m_writer.endElement("operation");
   }

   /**
    * Output WSDL message XML with a specific message name and element type.
    * Note: Must use single-part messages in document-literal binding style,
    * (MS.NET complains otherwise)
    * @param sName0 The first part of the message name (not null).
    * @param sName1 The second part of the message name, concatenated with first, (null == ignore).
    * @param sName2 The third part of the message name, concatenated with second, (null == ignore).
    * @param sName3 The fourth part of the message name, concatenated with third, (null == ignore).
    * @param namespace The metaclass namespace where the element is defined (null == XML.TNS_NS).
    * @param sType0 The first part of the element type (not null).
    * @param sType1 The second part of the element type, concatenated with first, (null == ignore).
    * @param sType2 The third part of the message type, concatenated with second, (null == ignore).
    * @throws IOException On IO error.
    */
   protected void writeMessage(
      String sName0, String sName1, String sName2, String sName3,
      Metaclass namespace, String sType0, String sType1, String sType2)
      throws IOException
   {
      assert sName0 != null;
      assert sName1 != null || sName2 == null;
      assert sName2 != null || sName3 == null;
      assert sType0 != null;
      assert sType1 != null || sType2 == null;

      m_writer.openElement("message");
         m_writer.writeAttribute("name", sName0, sName1, sName2, sName3);

         if (namespace != null)
         {
            m_writer.writeAttribute(
               XML.XML_NS + ':' + namespace.getName(), XML.NS_URI_TNS, "/", namespace.getName());
         }

      m_writer.closeElement();

         m_writer.openElement("part");
            m_writer.writeAttribute("name", "parameters");
            m_writer.writeAttribute(
               "element",
               (namespace == null) ? XML.TNS_NS : namespace.getName(), ":", sType0, sType1, sType2);
         m_writer.closeEmptyElement();

      m_writer.endElement("message");
   }

   /**
    * Output WSDL message XML for each applicable event of a metaclass.
    * @param metaclass The metaclass to process (not null).
    * @throws IOException On IO error.
    */
   protected void writeMessages(Metaclass meta) throws IOException
   {
      for (int i = 0, nCount = meta.getEventCount(); i < nCount; ++i)
      {
         Event event = meta.getEvent(i);

         if (isExportedEvent(event))
         {
            String sEvent = computeElementName(event);

            writeMessage(
               meta.getName(), METACLASS_NAME_DELIMITER, sEvent, null, meta, sEvent, null, null);

            if (isExportedEventResult(event))
            {
               // NOTE: MS.NET _requires_ distinct "element" names for all messages,
               // i.e. the same "element" _cannot_ be shared between multiple "message" objects
               writeMessage(
                  meta.getName(), METACLASS_NAME_DELIMITER, sEvent, "_Response",
                  meta, sEvent, "_Response", null);
            }
         }
      }
   }

   /**
    * Output WSDL schema XML for a metaclass.
    * @param metaclass The metaclass to process.
    * @throws IOException On IO error.
    */
   protected void writeMetaclassSchema(Metaclass meta) throws IOException
   {
      m_writer.openElement("schema");
         m_writer.writeAttribute(XML.XML_NS, XML.XSD_URI);
         m_writer.writeAttribute("targetNamespace", XML.NS_URI_TNS, "/", meta.getName());
         m_writer.writeAttribute("elementFormDefault", "qualified");
      m_writer.closeElement();

         m_writer.openElement("import"); // append "import" of NexJ schema, required for MS.NET
            m_writer.writeAttribute("namespace", XML.NS_URI_TNS);
         m_writer.closeEmptyElement();

         for (int i = 0, nCount = meta.getEventCount(); i < nCount; ++i)
         {
            Event event = meta.getEvent(i);

            if (!isExportedEvent(event))
            {
               continue;
            }

            // use separate/distinct names for identically named events because MS.NET WSDL
            // import/export does not allow function overloading and does not support "choice"
            String sEvent = computeElementName(event);

            m_writer.openElement("element");
               m_writer.writeAttribute("name", sEvent);
            m_writer.closeElement();

               if (m_bIncludeDocumentation)
               {
                  writeDocumentation(null, event.getDescription());
               }

               m_writer.startElement("complexType");
                  m_writer.startElement("sequence");

                     // instance events take the instance as first argument
                     if (!event.isStatic())
                     {
                        writeElement(XML.BASE_PREFIX, "instance", meta);
                     }

                     for (int k = 0, nArgCount = event.getArgumentCount(); k < nArgCount; ++k)
                     {
                        Argument arg = event.getArgument(k);
                        Type type = arg.getType();
                        boolean bVarArg = event.isVarArg() && k == nArgCount - 1;
                        QName qType = getQualifiedTypeCached(type, bVarArg);

                        // don't use nillable="true" as that will automatically set minOccurs="1"
                        // in MS.NET
                        writeElement(
                           arg.getName(), null, qType,
                           (arg.isRequired()) ? 1 : 0, (arg.isCollection()) ? -1 : 1, false,
                           arg.getDescription());
                     }

                  m_writer.endElement("sequence");
               m_writer.endElement("complexType");

            m_writer.endElement("element");

            if (!isExportedEventResult(event))
            {
               continue; // skip result types that are not exported e.g. for WSDL Basic
            }

            Argument result = event.getResult();
            Type type = (result == null) ? null : result.getType();
            boolean bTyped = type != null;

            // NOTE: MS.NET requires the result to be wrapped if the arguments are wrapped
            //       hence must place result inside a complexType
            // @see http://servicefactory.codeplex.com/Thread/View.aspx?ThreadId=21981
            if (bTyped && result.isCollection())
            {
               m_writer.openElement("element");
                  m_writer.writeAttribute("name", sEvent, "_Response");

                  if (!result.isRequired())
                  {
                     m_writer.writeAttribute("nillable", true);
                  }

               m_writer.closeElement();

                  if (m_bIncludeDocumentation)
                  {
                     writeDocumentation(null, result.getDescription());
                  }

                  // NOTE: MS.NET complains about "(Elt:Elt -- NameAndTypeOK)" with "restriction"
                  m_writer.startElement("complexType");
                     m_writer.startElement("sequence");
                        writeElement("item", null, type, 0, -1, true, null);
                     m_writer.endElement("sequence");
                  m_writer.endElement("complexType");

               m_writer.endElement("element");
            }
            else // single element
            {
               m_writer.openElement("element");
                  m_writer.writeAttribute("name", sEvent, "_Response");
               m_writer.closeElement();
                  m_writer.startElement("complexType");
                     m_writer.startElement("sequence");

                        if (bTyped)
                        {
                           writeElement(
                              "item", null, type,
                              1, 1, !result.isRequired(), result.getDescription());
                        }
                        else
                        {
                           writeElement("item", null, Primitive.ANY, 1, 1, true, null);
                        }

                     m_writer.endElement("sequence");
                  m_writer.endElement("complexType");
               m_writer.endElement("element");
            }
         }

      m_writer.endElement("schema");
   }

   /**
    * Output WSDL port type XML for a metaclass.
    * @param metaclass The metaclass to process.
    * @throws IOException On IO error.
    */
   protected void writePortType(Metaclass meta) throws IOException
   {
      m_writer.openElement("portType");
         m_writer.writeAttribute("name", meta.getName(), METACLASS_NAME_DELIMITER, "Server");
      m_writer.closeElement();

         for (int i = 0, nCount = meta.getEventCount(); i < nCount; ++i)
         {
            Event event = meta.getEvent(i);

            if (!isExportedEvent(event))
            {
               continue;
            }

            String sEvent = computeElementName(event);

            if (isExportedEventResult(event))
            {
               writePortTypeOperation(
                  sEvent, null,
                  meta.getName(), METACLASS_NAME_DELIMITER, sEvent,
                  meta.getName(), METACLASS_NAME_DELIMITER, sEvent, "_Response");
            }
            else // skip result types that are not exported e.g. for WSDL Basic
            {
               writePortTypeOperation(
                  sEvent, null, meta.getName(), "_", sEvent, null, null, null, null);
            }
         }

      m_writer.endElement("portType");
   }

   /**
    * Output WSDL port type operation block.
    * @param sName0 The first part of the operation name (not null).
    * @param sName1 The second part of the operation name, concat. with first, (null == ignore).
    * @param sInType0 The first part of the input message type (not null).
    * @param sInType1 The second part of input message type, concat. with first, (null == ignore).
    * @param sInType2 The third part of input message type, concat. with second, (null == ignore)
    * @param sOutType0 The first part of the output message type (null == no operation output).
    * @param sOutType1 The second part of output message type, concat. with first, (null == ignore).
    * @param sOutType2 The third part of output message type, concat. with second, (null == ignore).
    * @param sOutType3 The fourth part of output message type, concat. with third, (null == ignore).
    * @throws IOException On IO error.
    */
   protected void writePortTypeOperation(
      String sName0, String sName1,
      String sInType0, String sInType1, String sInType2,
      String sOutType0, String sOutType1, String sOutType2, String sOutType3)
      throws IOException
   {
      assert sName0 != null;
      assert sInType0 != null;
      assert sInType1 != null || sInType2 == null;
      assert sOutType0 != null || sOutType1 == null;
      assert sOutType1 != null || sOutType2 == null;
      assert sOutType2 != null || sOutType3 == null;

      m_writer.openElement("operation");
         m_writer.writeAttribute("name", sName0, sName1);
      m_writer.closeElement();

         m_writer.openElement("input");
            m_writer.writeAttribute("message", XML.TNS_NS, ":", sInType0, sInType1, sInType2);
         m_writer.closeEmptyElement();

         if (sOutType0 != null)
         {
            m_writer.openElement("output");
               m_writer.writeAttribute(
                  "message", XML.TNS_NS, ":", sOutType0, sOutType1, sOutType2, sOutType3);
            m_writer.closeEmptyElement();
         }

      m_writer.endElement("operation");
   }

   /**
    * Output WSDL service port XML for a metaclass.
    * @param metaclass The metaclass to process.
    * @param sURI Base URI to use for binding generation.
    * @throws IOException On IO error.
    */
   protected void writeServicePort(Metaclass meta, String sURI) throws IOException
   {
      m_writer.openElement("port");
         m_writer.writeAttribute("name", meta.getName(), METACLASS_NAME_DELIMITER, "Server");
         m_writer.writeAttribute(
            "binding", XML.TNS_NS, ":", meta.getName(), METACLASS_NAME_DELIMITER, "Server");
      m_writer.closeElement();

         m_writer.openElement("address");
            m_writer.writeAttribute(XML.XML_NS, XML.NS_URI_WSDL_SOAP);
            m_writer.writeAttribute("location", sURI);
         m_writer.closeEmptyElement();

      m_writer.endElement("port");
   }

   /**
    * @see nexj.core.rpc.xml.XSDGenerator#writeTNSImport()
    */
   protected void writeTNSImport() throws IOException
   {
      super.writeTNSImport();

      m_writer.openElement("import"); // append "import" of SOAP "envelope" schema
         m_writer.writeAttribute("namespace", XML.ENV_URI);
      m_writer.closeEmptyElement();
   }
}