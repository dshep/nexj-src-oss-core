// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.xml.wsdl;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import nexj.core.meta.integration.format.xml.schema.Element;
import nexj.core.meta.integration.format.xml.schema.Schema;
import nexj.core.meta.integration.format.xml.schema.SchemaItem;
import nexj.core.rpc.xml.XML;
import nexj.core.util.HashTab;
import nexj.core.util.HashTab2D;
import nexj.core.util.IOUtil;
import nexj.core.util.Lookup;
import nexj.core.util.Lookup2D;
import nexj.core.util.URLUtil;
import nexj.core.util.XMLUtil;

/**
 * Imports a WSDL 1.1 as a web service definition WS-I Basic Profile 1.1.
 */
public class WSDLServiceImporter extends GenericWebServiceImporter
{
   // constants

   /**
    * Namespace for WSDL binding extension SOAP 1.2.
    */
   protected final static String WSDL_SOAP12_NAMESPACE = "http://schemas.xmlsoap.org/wsdl/soap12/";

   /**
    * Namespace for XML Schema.
    */
   protected final static String XSD_SCHEMA_NAMESPACE = "http://www.w3.org/2001/XMLSchema";

   // associations

   /**
    * Handler for XSD Schema tags.
    */
   protected XSDSchemaImporter m_schemaHandler;

   /**
    * The current web service definition.
    */
   protected SOAPService m_curService;

   /**
    * The current message definition.
    */
   protected Message m_curMessage;

   /**
    * The current operation.
    */
   protected Operation m_curOperation;

   /**
    * The current port type.
    */
   protected WSDLPortType m_curPortType;

   /**
    * The current port.
    */
   protected WSDLPort m_curPort;

   /**
    * List of services created during parsing.
    */
   protected List m_serviceList = new ArrayList(); // SOAPService[]

   /**
    * Map of web service definitions to a list of their ports.
    */
   protected Lookup m_portListMap = new HashTab(); // List[SOAPService], where List elements are WSDLPort

   /**
    * Map of namespace and name to bindings.
    */
   protected Lookup2D m_bindingMap = new HashTab2D(); // WSDLBinding[String, String]

   /**
    * Map of namespace and name to port types.
    */
   protected Lookup2D m_portTypeMap = new HashTab2D(); // WSDLPortType[String, String]

   /**
    * Map of namespace and name to messages.
    */
   protected Lookup2D m_messageMap = new HashTab2D(); // Message[String, String]

   // constructors

   /**
    * Constructor.
    */
   public WSDLServiceImporter()
   {
      super();
   }

   // operations

   /**
    * @see nexj.core.meta.integration.format.xml.wsdl.GenericWebServiceImporter#endPrefixMapping(java.lang.String)
    */
   public void endPrefixMapping(String sPrefix) throws SAXException
   {
      if (m_schemaHandler != null)
      {
         m_schemaHandler.endPrefixMapping(sPrefix);
      }
      else
      {
         super.endPrefixMapping(sPrefix);
      }
   }

   /**
    * @see nexj.core.meta.integration.format.xml.wsdl.GenericWebServiceImporter#startPrefixMapping(java.lang.String, java.lang.String)
    */
   public void startPrefixMapping(String sPrefix, String sURI) throws SAXException
   {
      if (m_schemaHandler != null)
      {
         m_schemaHandler.startPrefixMapping(sPrefix, sURI);
      }
      else
      {
         super.startPrefixMapping(sPrefix, sURI);
      }
   }

   /**
    * Takes the location of a WSDL and returns a list of web service definitions.
    * @param wsdlURL The location of a WSDL file.
    * @return a list of SOAPService objects representing web service definitions.
    * @throws IOException if the URL cannot be used.
    */
   public static List parse(URL wsdlURL) throws IOException
   {
      return new WSDLServiceImporter().parseWSDL(wsdlURL);
   }

   /**
    * Returns the requested element or null if none found.
    * @param sURI The namespace of the element.
    * @param sName The name of the element.
    * @return The element or null if none found.
    */
   protected Element findElement(String sURI, String sName)
   {
      Schema schema = m_universe.findSchemaByURI(sURI);

      if (schema != null)
      {
         return (Element)schema.findItem(sName, SchemaItem.ELEMENT);
      }

      return null;
   }

   /**
    * @see org.xml.sax.helpers.DefaultHandler#startElement(java.lang.String, java.lang.String, java.lang.String, org.xml.sax.Attributes)
    */
   public void startElement(String sURI, String sLocalName, String sQName, Attributes attributes) throws SAXException
   {
      if (sURI.equals(XML.NS_URI_WSDL))
      {
         if (sLocalName.equals("definitions"))
         {
            m_sTargetNamespace = attributes.getValue("targetNamespace");
         }
         else if (sLocalName.equals("types"))
         {
            m_schemaHandler = new XSDSchemaImporter(this, true);
            m_schemaHandler.m_url = m_url;
         }
         else if (sLocalName.equals("service"))
         {
            m_curService = new SOAPService(attributes.getValue("name"), m_universe);

            m_serviceList.add(m_curService);
            m_portListMap.put(m_curService, new ArrayList());
         }
         else if (sLocalName.equals("port"))
         {
            m_curPort = new WSDLPort();
            m_curPort.m_sName = attributes.getValue("name");
            m_curPort.m_sBindingURI = getURI(getAlias(attributes.getValue("binding")));
            m_curPort.m_sBindingName = getLocalName(attributes.getValue("binding"));

            ((List)m_portListMap.get(m_curService)).add(m_curPort);
         }
         else if (sLocalName.equals("binding"))
         {
            WSDLBinding curBinding = new WSDLBinding();
            curBinding.m_sName = attributes.getValue("name");
            curBinding.m_sTypeURI = getURI(getAlias(attributes.getValue("type")));
            curBinding.m_sTypeName = getLocalName(attributes.getValue("type"));

            m_bindingMap.put(m_sTargetNamespace, curBinding.m_sName, curBinding);
         }
         else if (sLocalName.equals("operation"))
         {
            if (m_curPortType != null)
            {
               m_curOperation = new Operation(attributes.getValue("name"));
               m_curPortType.getOperations().add(m_curOperation);
            }
         }
         else if (sLocalName.equals("input") || sLocalName.equals("output"))
         {
            if (m_curOperation != null)
            {
               String sNS = getURI(getAlias(attributes.getValue("message")));
               String sLocal = getLocalName(attributes.getValue("message"));
               Message message = (Message)m_messageMap.get(sNS, sLocal);

               if (sLocalName.equals("input"))
               {
                  m_curOperation.m_input = message;
               }
               else
               {
                  m_curOperation.m_output = message;
               }
            }
         }
         else if (sLocalName.equals("portType"))
         {
            m_curPortType = new WSDLPortType();
            m_curPortType.m_sName = attributes.getValue("name");

            m_portTypeMap.put(m_sTargetNamespace, m_curPortType.m_sName, m_curPortType);
         }
         else if (sLocalName.equals("message"))
         {
            m_curMessage = new Message(attributes.getValue("name"));

            m_messageMap.put(m_sTargetNamespace, m_curMessage.m_sName, m_curMessage);
         }
         else if (sLocalName.equals("part"))
         {
            if (m_curMessage != null && attributes.getValue("element") != null)
            {
               String sNS = getURI(getAlias(attributes.getValue("element")));
               String sLocal = getLocalName(attributes.getValue("element"));

               m_curMessage.setRoot(findElement(sNS, sLocal));
            }
         }
      }
      else if (sURI.equals(XML.NS_URI_WSDL_SOAP) || sURI.equals(WSDL_SOAP12_NAMESPACE))
      {
         if (sLocalName.equals("address"))
         {
            if (m_curPort != null)
            {
               m_curPort.m_sLocation = attributes.getValue("location");
            }
         }
      }
      else if (sURI.equals(XSD_SCHEMA_NAMESPACE))
      {
         m_schemaHandler.startElement(sURI, sLocalName, sQName, attributes);
      }
   }

   /**
    * @see org.xml.sax.helpers.DefaultHandler#endElement(java.lang.String, java.lang.String, java.lang.String)
    */
   public void endElement(String sURI, String sLocalName, String sQName) throws SAXException
   {
      if (sURI.equals(XML.NS_URI_WSDL))
      {
         if (sLocalName.equals("types"))
         {
            m_schemaHandler.resolveReferences();
            m_schemaHandler.runFixups();
            m_schemaHandler = null;
         }
         else if (sLocalName.equals("port"))
         {
            m_curPort = null;
         }
         else if (sLocalName.equals("portType"))
         {
            m_curPortType = null;
         }
         else if (sLocalName.equals("operation"))
         {
            m_curOperation = null;
         }
         else if (sLocalName.equals("message"))
         {
            m_curMessage = null;
         }
      }
      else if (sURI.equals(XSD_SCHEMA_NAMESPACE))
      {
         m_schemaHandler.endElement(sURI, sLocalName, sQName);
      }
   }


   /**
    * Creates a list of web service definitions.
    * @param url the WSDL location.
    * @return a list of SOAPService objects representing web service definitions.
    * @throws IOException if a stream cannot be opened from the wsdlURL.
    */
   public List parseWSDL(URL url) throws IOException
   {
      m_url = url;
      InputStream is = null;
      Reader r = null;

      try
      {
         is = URLUtil.openStream(m_url);
         r = new InputStreamReader(is, "UTF-8");

         XMLUtil.parse(r, this);

         List retValList = new ArrayList();

         for (int i = 0; i < m_serviceList.size(); i++)
         {
            SOAPService service = (SOAPService)m_serviceList.get(i);
            List portList = (List)m_portListMap.get(service);

            // returns a service for every port in every service
            retValList.addAll(assembleSOAPServices(service, portList));
         }

         return retValList;
      }
      finally
      {
         IOUtil.close(is);
         IOUtil.close(r);
      }
   }

   /**
    * @see org.xml.sax.helpers.DefaultHandler#characters(char[], int, int)
    */
   public void characters(char[] ch, int nStart, int nLength) throws SAXException
   {
      if (m_schemaHandler != null)
      {
         m_schemaHandler.characters(ch, nStart, nLength);
      }
      else
      {
         super.characters(ch, nStart, nLength);
      }
   }

   /**
    * Creates web service definitions from the parsed information.
    * @param service The service definition.
    * @param portList The ports associated with this service.
    * @param bindingMap Map from binding name to Binding.
    * @param portTypeMap Map from port type name to PortType.
    * @param messageMap Map from message name to message.
    * @return A list containing SOAPService objects, one for each port in service.
    */
   protected List assembleSOAPServices(SOAPService service, List portList)
   {
      List serviceList = new ArrayList(2);

      for (int i = 0; i < portList.size(); i++)
      {
         WSDLPort port = (WSDLPort)portList.get(i);
         SOAPService newService;

         if (portList.size() > 1)
         {
            // name the service according to which port is being associated with it
            newService = new SOAPService(service.m_sName + '_' + port.m_sName, m_universe);
         }
         else
         {
            newService = service;
         }

         if (port.getLocation() != null)
         {
            newService.setEndpoint(port.getLocation());
         }

         WSDLBinding wsdlBinding = (WSDLBinding)m_bindingMap.get(port.m_sBindingURI, port.m_sBindingName);

         if (wsdlBinding != null)
         {
            WSDLPortType portType = (WSDLPortType)m_portTypeMap.get(wsdlBinding.m_sTypeURI, wsdlBinding.m_sTypeName);

            if (portType != null)
            {
               for (int j = 0; j < portType.getOperations().size(); j++)
               {
                  Operation operation = (Operation)portType.getOperations().get(j);

                  newService.addOperation(operation);

                  if (operation.m_input != null)
                  {
                     newService.addMessage(operation.m_input);
                  }

                  if (operation.m_output != null)
                  {
                     newService.addMessage(operation.m_output);
                  }
               }
            }
         }

         serviceList.add(newService);
      }

      return serviceList;
   }

   // inner classes

   /**
    * Represents a binding.
    */
   protected static class WSDLBinding
   {
      // associations

      /**
       * The binding name.
       */
      protected String m_sName;

      /**
       * The namespace of the binding type.
       */
      protected String m_sTypeURI;

      /**
       * The name of the binding type.
       */
      protected String m_sTypeName;
   }

   /**
    * Represents a port.
    */
   protected static class WSDLPort
   {
      // attributes

      /**
       * The port name.
       */
      protected String m_sName;

      /**
       * The namespace of the binding.
       */
      protected String m_sBindingURI;

      /**
       * The name of the binding.
       */
      protected String m_sBindingName;

      /**
       * The location to access the port.
       */
      protected String m_sLocation;

      // operations

      /**
       * Returns the location.
       * @return The location.
       */
      protected String getLocation()
      {
         return m_sLocation;
      }
   }

   /**
    * Represents a port type.
    */
   protected static class WSDLPortType
   {
      // associations

      /**
       * The name of the port type.
       */
      protected String m_sName;

      /**
       * List of operations.
       */
      private List m_operationList; // Operation[]

      //operations

      /**
       * Returns the operation list.
       * @return the operation list.
       */
      public List getOperations()
      {
         if (m_operationList == null)
         {
            m_operationList = new ArrayList(2);
         }

         return m_operationList;
      }
   }
}
