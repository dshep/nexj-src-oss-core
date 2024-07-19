// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.channel;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.TransactionalChannel;
import nexj.core.meta.integration.XMLIntegrationMetadataExporter;
import nexj.core.meta.j2ee.J2EEEnvRef;
import nexj.core.meta.j2ee.J2EEProperty;
import nexj.core.meta.j2ee.J2EEResourceRef;
import nexj.core.meta.xml.XMLMetadataExporter;
import nexj.core.util.J2EEUtil;
import nexj.core.util.SysUtil;
import nexj.core.util.XMLWriter;

/**
 * XML MDB integration metadata exporter.
 */
public abstract class XMLMDBIntegrationMetadataExporter implements XMLIntegrationMetadataExporter
{
   // associations

   /**
    * The main metadata exporter.
    */
   protected XMLMetadataExporter m_exporter;

   /**
    * The output stream.
    */
   protected XMLWriter m_writer;

   // constructors

   /**
    * Creates the exporter with an XML print stream.
    * @param exporter The main metadata exporter.
    */
   protected XMLMDBIntegrationMetadataExporter(XMLMetadataExporter exporter)
   {
      m_exporter = exporter;
      m_writer = exporter.getWriter();
   }

   // operations

   /**
    * @see nexj.core.meta.integration.XMLIntegrationMetadataExporter#exportJ2EEDescriptor(nexj.core.meta.integration.Channel, int, java.lang.String, int, int)
    */
   public void exportJ2EEDescriptor(Channel channel, int nPart, String sNamespace, int nContainer, int nContext)
      throws IOException
   {
      String sEnvName = getEnvironmentName(channel);
      
      switch (nPart)
      {
         case XMLMetadataExporter.J2EE_RESOURCE_ENV_REF:
         case XMLMetadataExporter.J2EE_PLATFORM_RESOURCE_ENV_REF:
            if (channel.isEnabled())
            {
               List list = new ArrayList(2);

               addResourceEnvRefs(channel, nContainer, list);

               for (int i = 0, n = list.size(); i < n; ++i)
               {
                  m_exporter.exportJ2EEEnvRef((J2EEEnvRef)list.get(i), nPart, sNamespace, nContainer, nContext);
               }
            }

            break;

         case XMLMetadataExporter.J2EE_RESOURCE_REF:
         case XMLMetadataExporter.J2EE_PLATFORM_RESOURCE_REF:
         case XMLMetadataExporter.J2EE_PLATFORM_RESOURCE_REF_EXT:
            if (channel.isEnabled())
            {
               List list = new ArrayList(2);

               addResourceRefs(channel, nContainer, list);

               for (int i = 0, n = list.size(); i < n; ++i)
               {
                  m_exporter.exportJ2EEResourceRef(channel.getType().getMetadata(), (J2EEResourceRef)list.get(i), nPart, sNamespace, nContainer, nContext);
               }
            }

            break;

         case XMLMetadataExporter.J2EE_PLATFORM_CONNECTION_FACTORY:
            if (channel.isEnabled() && sNamespace.equals(channel.getType().getName()))
            {
               List list = new ArrayList(2);

               addConnectionRefs(channel, nContainer, list);

               for (int i = 0, n = list.size(); i < n; ++i)
               {
                  m_exporter.exportJ2EEResourceRef(channel.getType().getMetadata(), (J2EEResourceRef)list.get(i), nPart, sNamespace, nContainer, nContext);
               }
            }

            break;
            
         case XMLMetadataExporter.J2EE_PLATFORM_ADMIN_OBJECT:
            if (channel.isEnabled() && sNamespace.equals(channel.getType().getName()))
            {
               List list = new ArrayList(2);

               addAdminObjectRefs(channel, nContainer, list);

               for (int i = 0, n = list.size(); i < n; ++i)
               {
                  m_exporter.exportJ2EEResourceRef(channel.getType().getMetadata(), (J2EEResourceRef)list.get(i), nPart, sNamespace, nContainer, nContext);
               }
            }

            break;

         case XMLMetadataExporter.J2EE_MESSAGE_DRIVEN:
            if (channel.isReceivable())
            {
               m_writer.openElement("message-driven");
               m_writer.writeAttribute("id", "mdb-", sNamespace, channel.getName(), "MDB");
               m_writer.closeElement();

               m_writer.writeElement("display-name", channel.getName(), " MDB");
               m_writer.writeElement("ejb-name", channel.getName(), "MDB");
               m_writer.writeElement("ejb-class", getMDBClass(channel));
               m_writer.writeElement("messaging-type", getListenerClass(channel));
               m_writer.writeElement("transaction-type", getTransactionType(channel));

               m_writer.startElement("activation-config");

               List propertyList = new ArrayList();

               //Export properties specific to this adapter
               addActivationProperties(channel, propertyList);
               exportActivationProperties(propertyList, nContainer, nContext);

               m_writer.endElement("activation-config");

               // Entry for channel name so IntegrationMDB can find metadata object
               m_writer.startElement("env-entry");
               m_writer.writeElement("env-entry-name", "channel");
               m_writer.writeElement("env-entry-type", "java.lang.String");
               m_writer.writeElement("env-entry-value", channel.getName());
               m_writer.endElement("env-entry");

               exportResourceReferences(channel, XMLMetadataExporter.J2EE_RESOURCE_REF, nContainer, nContext);
               exportResourceReferences(channel, XMLMetadataExporter.J2EE_RESOURCE_ENV_REF, nContainer, nContext);

               m_writer.endElement("message-driven");
            }

            break;

         case XMLMetadataExporter.J2EE_CONTAINER_TRANSACTION:
            if (channel.isReceivable() && channel.isTransactional())
            {
               m_writer.startElement("container-transaction");
               m_writer.startElement("method");
               m_writer.writeElement("ejb-name", channel.getName(), "MDB");
               m_writer.writeElement("method-name", "onMessage");
               m_writer.endElement("method");
               m_writer.writeElement("trans-attribute", "Required");
               m_writer.endElement("container-transaction");
            }

            break;

         case XMLMetadataExporter.J2EE_PLATFORM_MESSAGE_DRIVEN:
            if (channel.isReceivable())
            {
               List propertyList;

               switch (nContainer)
               {
               case J2EEUtil.TEEE:
                  m_writer.openElement("message-driven");
                  m_writer.writeAttribute("name", channel.getName(), "MDB");
                  m_writer.writeAttribute("ra-name", getResourceAdapterName(channel));

                  propertyList = new ArrayList();
                  addPlatformActivationProperties(channel, nContainer, propertyList);

                  if (!propertyList.isEmpty())
                  {
                     m_writer.closeElement();
                     m_writer.startElement("activation-config");
                     exportActivationProperties(propertyList, nContainer, nContext);
                     m_writer.endElement("activation-config");
                     m_writer.endElement("message-driven");
                  }
                  else
                  {
                     m_writer.closeEmptyElement();
                  }

                  break;

               case J2EEUtil.JBOSS:
                  m_writer.startElement("message-driven");
                  m_writer.writeElement("ejb-name", channel.getName(), "MDB");
                  m_writer.startElement("activation-config");
                  propertyList = new ArrayList();
                  addPlatformActivationProperties(channel, nContainer, propertyList);
                  exportActivationProperties(propertyList, nContainer, nContext);
                  m_writer.endElement("activation-config");
                  m_writer.writeElement("resource-adapter-name", SysUtil.NAMESPACE, "-", sEnvName,".ear#", getResourceAdapterName(channel));
                  exportResourceReferences(channel, XMLMetadataExporter.J2EE_PLATFORM_RESOURCE_REF, nContainer, nContext);
                  exportResourceReferences(channel, XMLMetadataExporter.J2EE_PLATFORM_RESOURCE_ENV_REF, nContainer, nContext);
                  m_writer.endElement("message-driven");

                  break;

               case J2EEUtil.WEBSPHERE:
                  m_writer.openElement("ejbBindings");
                  m_writer.writeAttribute("xmi:type", "ejbbnd:MessageDrivenBeanBinding");
                  m_writer.writeAttribute("ejbName", channel.getName(), "MDB");
                  m_writer.writeAttribute("activationSpecJndiName", SysUtil.NAMESPACE, "/", sEnvName, "/activation/", channel.getName(), "MDB");
                  m_writer.closeElement();
                  m_writer.openElement("enterpriseBean");
                  m_writer.writeAttribute("xmi:type", "ejb:MessageDriven");
                  m_writer.writeAttribute("href", "META-INF/ejb-jar.xml#mdb-", sNamespace, channel.getName(), "MDB");
                  m_writer.closeEmptyElement();
                  exportResourceReferences(channel, XMLMetadataExporter.J2EE_PLATFORM_RESOURCE_REF, nContainer, nContext);
                  exportResourceReferences(channel, XMLMetadataExporter.J2EE_PLATFORM_RESOURCE_ENV_REF, nContainer, nContext);
                  m_writer.endElement("ejbBindings");

                  break;
               }
            }

            break;

         case XMLMetadataExporter.J2EE_PLATFORM_MESSAGE_DRIVEN_EXT:
            if (channel.isReceivable())
            {
               switch (nContainer)
               {
               case J2EEUtil.WEBSPHERE:
                  m_writer.openElement("ejbExtensions");
                  m_writer.writeAttribute("xmi:type", "ejbext:MessageDrivenExtension");
                  m_writer.closeElement();
                  m_writer.openElement("enterpriseBean");
                  m_writer.writeAttribute("xmi:type", "ejb:MessageDriven");
                  m_writer.writeAttribute("href", "META-INF/ejb-jar.xml#mdb-", sNamespace, channel.getName(), "MDB");
                  m_writer.closeEmptyElement();
                  exportResourceReferences(channel, XMLMetadataExporter.J2EE_PLATFORM_RESOURCE_REF_EXT, nContainer, nContext);

                  if (channel.isTransactional() && channel instanceof TransactionalChannel)
                  {
                     TransactionalChannel txc = (TransactionalChannel)channel;

                     if (txc.getTransactionTimeout() > 0)
                     {
                        m_writer.openElement("globalTransaction");
                        m_writer.writeAttribute("componentTransactionTimeout", txc.getTransactionTimeout());
                        m_writer.closeEmptyElement();
                     }
                  }

                  m_writer.endElement("ejbExtensions");

                  break;
               }
            }

            break;
            
         case XMLMetadataExporter.J2EE_PLATFORM_ACTIVATION_SPEC:
            if (channel.isReceivable() && sNamespace.equals(channel.getType().getName()))
            {
               List propertyList;

               switch (nContainer)
               {
               case J2EEUtil.WEBSPHERE:
                  m_writer.openElement("j2cActivationSpec");
                  m_writer.writeAttribute("name", channel.getName(), "MDB");
                  m_writer.writeAttribute("jndiName", SysUtil.NAMESPACE, "/", sEnvName, "/activation/", channel.getName(), "MDB");
                  m_writer.writeAttribute("activationSpec", "activationSpec-", channel.getType().getName());
                  m_writer.closeElement();

                  propertyList = new ArrayList();
                  addActivationProperties(channel, propertyList);
                  addPlatformActivationProperties(channel, nContainer, propertyList);
                  exportActivationProperties(propertyList, nContainer, nContext);

                  m_writer.endElement("j2cActivationSpec");

                  break;
               }
            }

            break;
      }
   }

   /**
    * Adds J2EE resource refs to a list.
    * @param channel The channel.
    * @param nContainer The container.
    * @param list The destination list, J2EEResourceRef[].
    */
   protected abstract void addResourceRefs(Channel channel, int nContainer, List list);

   /**
    * Adds J2EE resource evironment refs to a list.
    * @param channel The channel.
    * @param nContainer The container.
    * @param list The destination list, J2EEResourceRef[].
    */
   protected abstract void addResourceEnvRefs(Channel channel, int nContainer, List list);

   /**
    * Adds J2EE connection refs to a list.
    * @param channel The channel.
    * @param nContainer The container.
    * @param list The destination list, J2EEResourceRef[].
    */
   protected abstract void addConnectionRefs(Channel channel, int nContainer, List list);

   /**
    * Adds J2EE admin objects to a list.
    * @param channel The channel.
    * @param nContainer The container.
    * @param list The destination list, J2EEResourceRef[].
    */
   protected abstract void addAdminObjectRefs(Channel channel, int nContainer, List list);

   /**
    * Adds J2EE MDB activation properties to a list.
    * @param channel The channel.
    * @param nContainer The container.
    * @param list The destination list, J2EEProperty[].
    */
   protected void addActivationProperties(Channel channel, List list)
   {
      list.add(new J2EEProperty("channel", channel.getName()));
   }

   /**
    * Adds platform-specific J2EE MDB activation properties to a list.
    * @param channel The channel.
    * @param nContainer The container.
    * @param list The destination list, J2EEProperty[].
    */
   protected abstract void addPlatformActivationProperties(Channel channel, int nContainer, List list);
   
   /**
    * Gets the resource adapter name.
    * @param channel The channel.
    * @return The resource adapter name.
    */
   protected abstract String getResourceAdapterName(Channel channel);
   
   /**
    * Gets the MDB class name.
    * @param channel The channel.
    * @return The MDB class name.
    */
   protected abstract String getMDBClass(Channel channel);

   /**
    * Gets the listener interface class name.
    * @param channel The channel.
    * @return The listener interface class name.
    */
   protected abstract String getListenerClass(Channel channel);

   /**
    * Gets the transaction type.
    * @param channel The channel.
    * @return Container or Bean
    */
   protected abstract String getTransactionType(Channel channel);

   /**
    * Exports the channel resource references.
    * @see nexj.core.meta.integration.XMLIntegrationMetadataExporter#exportJ2EEDescriptor(nexj.core.meta.integration.Channel, int, java.lang.String, int, int)
    */
   protected void exportResourceReferences(Channel channel, int nPart, int nContainer, int nContext) throws IOException
   {
      m_exporter.exportJ2EEDescriptor(channel.getType().getMetadata(), nPart,
         "mdb-" + channel.getName() + "MDB", nContainer, nContext);
   }

   /**
    * Exports the activation properties.
    * @param propertyList The J2EE property list: J2EEProperty[].
    * @param channel The channel.
    * @param nContainer The container.
    * @param nContext The descriptor context.
    */
   protected void exportActivationProperties(List propertyList, int nContainer, int nContext) throws IOException
   {
      for (int i = 0, n = propertyList.size(); i < n; ++i)
      {
         m_exporter.exportJ2EEActivationProperty((J2EEProperty)propertyList.get(i), nContainer, nContext);
      }
   }
   
   /**
    * @see nexj.core.meta.integration.XMLIntegrationMetadataExporter#exportChannel(nexj.core.meta.integration.Channel)
    */
   public void exportChannel(Channel channel) throws IOException
   {
      throw new UnsupportedOperationException();
   }
   
   /**
    * Helper method to retrive the environment name from a channel's metadata.
    * @param channel The channel from which to retrieve the metadata.
    * @return The environment name.
    */
   protected static String getEnvironmentName(Channel channel)
   {
      return channel.getType().getMetadata().getEnvironment();
   }
}
