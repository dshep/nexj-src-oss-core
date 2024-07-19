// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.format.xml;

import java.io.IOException;
import java.io.Writer;
import java.util.Iterator;

import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.format.xml.schema.MessageSchemaConverter;
import nexj.core.meta.integration.format.xml.schema.Schema;
import nexj.core.meta.integration.format.xml.schema.SchemaUniverse;
import nexj.core.meta.integration.format.xml.schema.XSDSchemaExporter;
import nexj.core.meta.integration.format.xml.wsdl.MetadataServiceConverter;
import nexj.core.meta.integration.format.xml.wsdl.SOAPService;
import nexj.core.meta.integration.format.xml.wsdl.WSDLServiceExporter;
import nexj.core.meta.integration.service.Interface;
import nexj.core.runtime.InvocationContext;
import nexj.core.util.SingletonIterator;
import nexj.core.util.XMLWriter;

/**
 * An exporter that exports Messages, Interfaces, and Channels to
 * one of two output modes: XSD or WSDL.
 */
public class XSDMessageExporter
{
   // constants

   /**
    * Output XSD.
    */
   public final static int OUTMODE_XSD = 0;

   /**
    * Output WSDL.
    */
   public final static int OUTMODE_WSDL = 1;

   // attributes

   /**
    * The data output mode, one of the OUTMODE_* constants.
    */
   protected int m_nOutputMode = OUTMODE_XSD;

   /**
    * The host name to use for generating a WSDL service location.
    */
   protected String m_sHostName;

   /**
    * The HTTP context root path for generating a WSDL service location.
    */
   protected String m_sContextRoot;

   // associations

   /**
    * The output writer.
    */
   protected XMLWriter m_writer;

   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   // constructors

   /**
    * Constructs a new exporter.
    * @param context The invocation context.
    */
   public XSDMessageExporter(InvocationContext context)
   {
      m_context = context;
   }

   // operations

   /**
    * Exports a single Message.
    * @param message The Message to export.
    * @param writer The Writer to which the exported data will be written.
    * @throws IOException If an I/O error occurs.
    */
   public void export(Message message, Writer writer) throws IOException
   {
      if (m_nOutputMode == OUTMODE_XSD)
      {
         exportMessages(new SingletonIterator(message), writer);
      }
      else if (m_nOutputMode == OUTMODE_WSDL)
      {
         MetadataServiceConverter converter = new MetadataServiceConverter();
         WSDLServiceExporter exporter = new WSDLServiceExporter();
         SOAPService service = converter.export(message);

         exporter.setInvocationContext(m_context);
         exporter.exportService(service, writer);
      }
      else
      {
         throw new IllegalStateException("Unknown output mode " + m_nOutputMode);
      }
   }

   /**
    * Exports all the request Messages in an Interface.
    * @param iface The Interface whose request Messages will be exported.
    * @param writer The Writer to which the exported data will be written.
    * @throws IOException If an I/O error occurs.
    */
   public void export(Interface iface, Writer writer) throws IOException
   {
      export(iface, null, writer);
   }

   /**
    * Exports all the Messages in an Interface.
    * @param iface The Interface whose request Messages will be exported.
    * @param channel The Channel to which the Interface may be used; null if not known.
    * @param writer The Writer to which the exported data will be written.
    * @throws IOException If an I/O error occurs.
    */
   public void export(Interface iface, Channel channel, Writer writer) throws IOException
   {
      if (m_nOutputMode == OUTMODE_XSD)
      {
         exportMessages(iface.getRequestTable().getMessageIterator(), writer);
      }
      else if (m_nOutputMode == OUTMODE_WSDL)
      {
         exportService(iface, channel, writer);
      }
      else
      {
         throw new IllegalStateException("Unknown output mode " + m_nOutputMode);
      }
   }

   /**
    * Exports all the Messages in a Channel. (This will be all of the
    * request messages from all of the Interfaces bound to the Channel)
    * @param channel The Channel whose Messages will be exported.
    * @param writer The Writer to which the exported data will be written.
    * @throws IOException If an I/O error occurs.
    */
   public void export(Channel channel, Writer writer) throws IOException
   {
      if (m_nOutputMode == OUTMODE_XSD)
      {
         exportMessages(channel.getMessageTable().getMessageIterator(), writer);
      }
      else if (m_nOutputMode == OUTMODE_WSDL)
      {
         exportService(null, channel, writer);
      }
      else
      {
         throw new IllegalStateException("Unknown output mode " + m_nOutputMode);
      }
   }

   /**
    * Exports messages as XML schema.
    * @param itr The message iterator.
    * @param writer The output writer.
    * @throws IOException If an I/O error occurs.
    */
   private void exportMessages(Iterator itr, Writer writer) throws IOException
   {
      SchemaUniverse universe = new SchemaUniverse();
      MessageSchemaConverter converter = new MessageSchemaConverter(universe);
      XSDSchemaExporter exporter = new XSDSchemaExporter(universe);
      Schema firstSchema = null;

      for (; itr.hasNext(); )
      {
         Schema temp = converter.add((Message)itr.next()).getSchema();

         if (firstSchema == null)
         {
            firstSchema = temp;
         }
      }

      exporter.setInvocationContext(m_context);
      exporter.exportSchema(firstSchema, writer);
   }

   /**
    * Exports the messages on an Interface or Channel as WSDL.
    * @param iface The interface to get the messages; null to get all messages from channel.
    * @param channel The channel to get the service endpoint information.
    * @param writer The output writer.
    * @throws IOException If an I/O error occurs.
    */
   private void exportService(Interface iface, Channel channel, Writer writer) throws IOException
   {
      MetadataServiceConverter converter = new MetadataServiceConverter();
      WSDLServiceExporter exporter = new WSDLServiceExporter();
      SOAPService service = (iface == null) ? converter.export(channel) : converter.export(iface, channel);

      exporter.setInvocationContext(m_context);
      exporter.exportService(service, writer);
   }

   /**
    * Sets the host name.
    * @param sHostName The host name to use for the WSDL service location; null for default.
    */
   public void setHostName(String sHostName)
   {
      m_sHostName = sHostName;
   }

   /**
    * Gets the host name.
    * @return The host name that will be used for the WSDL service location.
    */
   public String getHostName()
   {
      return m_sHostName;
   }

   /**
    * Sets the context root.
    * @param sRoot The relative path of the context root; null to use default.
    */
   public void setContextRoot(String sContextRoot)
   {
      m_sContextRoot = sContextRoot;
   }

   /**
    * Gets the context root.
    * @return The relative path of the context root.
    */
   public String getContextRoot()
   {
      return m_sContextRoot;
   }

   /**
    * Sets the output mode.
    * @param nOutputMode One of the OUTMODE_* constants.
    */
   public void setOutputMode(int nOutputMode)
   {
      m_nOutputMode = nOutputMode;
   }

   /**
    * Gets the output mode.
    * @return One of the OUTMODE_* constants.
    */
   public int getOutputMode()
   {
      return m_nOutputMode;
   }
}
