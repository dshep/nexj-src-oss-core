// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.xml.wsdl;

import java.util.Iterator;

import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.channel.http.HTTPChannel;
import nexj.core.meta.integration.format.xml.RootXMLMessagePartMapping;
import nexj.core.meta.integration.format.xml.schema.Element;
import nexj.core.meta.integration.format.xml.schema.MessageSchemaConverter;
import nexj.core.meta.integration.service.Interface;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.Named;
import nexj.core.util.SysUtil;

/**
 * Converts metadata objects to web service definitions.
 */
public class MetadataServiceConverter
{
   // constants

   /**
    * The default host name to use for generating a web service location.
    */
   protected final static String DEFAULT_HOST_NAME = "host.domain.ext";

   /**
    * The default HTTP context root path.
    */
   protected final static String DEFAULT_CONTEXT_ROOT = "/" + SysUtil.NAMESPACE + "/";

   // attributes

   /**
    * The host name of the service end-point location.
    */
   protected String m_sHostName = DEFAULT_HOST_NAME;

   /**
    * The context root of the service end-point location.
    */
   protected String m_sContextRoot = DEFAULT_CONTEXT_ROOT;

   // operations

   /**
    * Sets the host name.
    * @param sHostName The host name to use for the service end-point location; null for default.
    */
   public void setHostName(String sHostName)
   {
      m_sHostName = (sHostName != null) ? sHostName : DEFAULT_HOST_NAME;
   }

   /**
    * Gets the host name.
    * @return The host name that will be used for the service end-point location.
    */
   public String getHostName()
   {
      return m_sHostName;
   }

   /**
    * Sets the root to use for generating the service end-point location.
    * @param sRoot The relative path of the context root; null to use default.
    */
   public void setContextRoot(String sContextRoot)
   {
      if (sContextRoot != null)
      {
         if (!sContextRoot.endsWith("/"))
         {
            sContextRoot += "/";
         }

         m_sContextRoot = sContextRoot;
      }
      else
      {
         m_sContextRoot = DEFAULT_CONTEXT_ROOT;
      }
   }

   /**
    * Gets the root to use for generating the service end-point location.
    * @return The relative path of the context root.
    */
   public String getContextRoot()
   {
      return m_sContextRoot;
   }

   /**
    * Exports all the Messages in an Interface.
    * @param iface The Interface whose request Messages will be exported.
    * @param channel The Channel for the Interface; null if not known.
    * @return The service definition.
    */
   public SOAPService export(Interface iface, Channel channel)
   {
      SOAPService service = (channel == null) ? getService(iface) : getService(channel);
      MessageSchemaConverter converter = new MessageSchemaConverter(service.getUniverse());

      for (Iterator itr = iface.getRequestTable().getMessageIterator(); itr.hasNext(); )
      {
         exportMessage((nexj.core.meta.integration.Message)itr.next(), service, converter);
      }

      return service;
   }

   /**
    * Exports all Messages of all Interfaces on the Channel.
    * @param channel The Channel whose messages will be exported.
    * @return The service definition.
    */
   public SOAPService export(Channel channel)
   {
      SOAPService service = getService(channel);
      MessageSchemaConverter converter = new MessageSchemaConverter(service.getUniverse());

      for (Iterator itr = channel.getMessageTable().getMessageIterator(); itr.hasNext(); )
      {
         exportMessage((nexj.core.meta.integration.Message)itr.next(), service, converter);
      }

      return service;
   }

   /**
    * Exports a Message.
    * @param message The Message to export.
    * @return The service definition.
    */
   public SOAPService export(nexj.core.meta.integration.Message message)
   {
      SOAPService service = getService(message);
      StringBuilder buf = new StringBuilder();

      buf.append("http://");
      buf.append(m_sHostName);
      buf.append(m_sContextRoot);
      buf.append("channel/CHANNEL");

      service.setEndpoint(buf.toString());

      MessageSchemaConverter converter = new MessageSchemaConverter(service.getUniverse());

      exportMessage(message, service, converter);

      return service;
   }

   /**
    * Exports an integration Message definition as an Operation.
    * @param msg The Message to export.
    * @param service The service definition.
    * @param converter The schema converter for creating the message schemas.
    */
   protected void exportMessage(nexj.core.meta.integration.Message msg, SOAPService service, MessageSchemaConverter converter)
   {
      Lookup envelopeHeaderMap = new HashTab();

      converter.setEnvelopeHeaderMap(envelopeHeaderMap);

      Element element = converter.add(msg);
      RootXMLMessagePartMapping rootMapping = (RootXMLMessagePartMapping)msg.getRoot().getMapping();
      Operation operation = new Operation((rootMapping.getOperation() == null) ? msg.getName() : rootMapping.getOperation());
      Message input = service.findMessage(msg.getName());

      if (input == null)
      {
         input = new Message(msg.getName());
         input.setRoot(element);
         input.addHeaders(envelopeHeaderMap);
         service.addMessage(input);
      }

      operation.setInput(input);
      operation.setAction(rootMapping.getAction());

      if (msg.getResponse() != null)
      {
         msg = msg.getResponse();
         element = converter.add(msg);

         Message output = service.findMessage(msg.getName());

         if (output == null)
         {
            output = new Message(msg.getName());
            output.setRoot(element);
            output.addHeaders(envelopeHeaderMap);
            service.addMessage(output);
         }

         operation.setOutput(output);
      }

      service.addOperation(operation);
   }

   /**
    * Creates a service with the same name as the argument.
    * @param named The argument to use for the service name.
    * @return The service.
    */
   protected SOAPService getService(Named named)
   {
      String sName = (named == null) ? "ANONYMOUS" : named.getName();
      SOAPService service = new SOAPService(sName);

      service.setURI("urn:" + sName);

      return service;
   }

   /**
    * Creates a service with configuration from the given channel.
    * @param channel The channel to use for configuring the service.
    * @return The service.
    */
   protected SOAPService getService(Channel channel)
   {
      SOAPService service = getService((Named)channel);

      if (channel instanceof HTTPChannel)
      {
         HTTPChannel httpChannel = (HTTPChannel)channel;
         StringBuilder buf = new StringBuilder();

         buf.append((httpChannel.isSecure()) ? "https://" : "http://");
         buf.append(m_sHostName);
         buf.append(m_sContextRoot);
         buf.append(((httpChannel.getAuthMode() == HTTPChannel.AUTH_CERT) ? "cert/channel/" : "channel/")
            + httpChannel.getName());

         service.setEndpoint(buf.toString());
      }

      return service;
   }
}
