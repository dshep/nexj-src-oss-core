package nexj.core.meta.xml;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Properties;
import java.util.Set;

import org.w3c.dom.Element;

import nexj.core.meta.Event;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Metadata;
import nexj.core.meta.MetadataLoader;
import nexj.core.meta.MetadataLoaderDispatcher;
import nexj.core.meta.Repository;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.ChannelType;
import nexj.core.meta.integration.XMLIntegrationMetadataLoader;
import nexj.core.runtime.Context;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.ThreadContextHolder;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Pair;
import nexj.core.util.HashTab;
import nexj.core.util.J2EEUtil;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;
import nexj.core.util.StringUtil;
import nexj.core.util.URLUtil;
import nexj.core.util.XMLUtil;


/**
 * External metadata loading hook for cloning template channels.
 *
 * In order to provide connection parameters to the framework, a metaclass (ex. TemplateChannelLoader) needs to be added to the model.
 * The name of the metaclass can be set on the .environment or .server file using the "meta.template.class" property.
 * 
 * The metaclass TemplateChannelLoader will need to implement two protected and static events as such:
 *
 * TemplateChannelLoader'getProperties(channelName):
 * Returns a collection of collection of pairs containing channel properties for template channel <channelName>. Both <propertyName> and <propertyValue> must be strings.
 * If the "name" property is not specified for a clone, framework will automatically set the name to "Cloned_<channelName>_<count>"
 *
 * Example:
 * (collection
 *    (collection (<propertyName1> . <propertyValue1>) (<propertyName2> . <propertyValue2>) ("name" . "clone 1") ... (<propertyNameN> . <propertyValueN>))  ; properties for "clone 1"
 *    (collection (<propertyName1> . <propertyValue1>) (<propertyName2> . <propertyValue2>) ("name" . "clone 2") ... (<propertyNameN> . <propertyValueN>))  ; properties for "clone 2"
 *    (collection (<propertyName1> . <propertyValue1>) (<propertyName2> . <propertyValue2>) ("name" . "") ... (<propertyNameN> . <propertyValueN>))         ; properties for "Cloned_<channelName>_1"
 *    (collection (<propertyName1> . <propertyValue1>) (<propertyName2> . <propertyValue2>) ... (<propertyNameN> . <propertyValueN>))                       ; properties for "Cloned_<channelName>_2"
 *    ...
 *    ...
 *    (collection (<propertyName1> . <propertyValue1>) (<propertyName2> . <propertyValue2>) ("name" . "clone N") ... (<propertyNameN> . <propertyValueN>))  ; properties for "clone N"
 * )
 *
 *
 * TemplateChannelLoader'getConnectionProperties(channel):
 * Returns a collection of pairs containing connection properties cloned channel <channel>.
 * Both <propertyName> and <propertyValue> must be strings.
 *
 * Example:
 * (collection (<propertyName1> . <propertyValue1>) (<propertyName2> . <propertyValue2>)  ... (<propertyNameN> . <propertyValueN>)) ; connection properties for cloned channel <channel>
 *
 * Note: This event is only called if the template channel is listed in the connections/environment file.
 */
public class TemplateChannelXMLMetadataLoaderHook extends GenericXMLMetadataLoaderHook
{
   // constants

   /**
    * The name of the template channel metaclass.
    */
   protected final static String TEMPLATE_CLASS_NAME_PROPERTY = "meta.template.class";

   /**
    * Prefix used to identify cloned channel.
    */
   protected final static String CLONED_CHANNEL_PREFIX = "Cloned";

   // associations

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(TemplateChannelXMLMetadataLoaderHook.class);

   /**
    * The loaded metadata.
    */
   protected XMLMetadata m_metadata;

   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   /**
    * The VM for script loading.
    */
   protected Machine m_machine;

   /**
    * The current wrapped loader.
    */
   protected XMLMetadataLoader m_loader;

   /**
    * The environment properties.
    */
   protected Properties m_properties;

   /**
    * Map of cloned channels, indexed by template channel name.
    */
   protected Lookup m_clonedChannelMap = new HashTab();

   // operations

   /**
    * @see nexj.core.meta.xml.XMLMetadataLoaderHook#initialize(XMLMetadataLoader, Properties)
    */
   public void initialize(XMLMetadataLoader loader, Properties properties)
   {
      m_loader = loader;
      m_properties = properties;
   }

   /**
    * @param element The DOM element containing the channel.
    * @param sName The data source name.
    * @return True, if channel is a template.
    */
   private boolean isTemplate(Element element, String sName)
   {
      return "template".equals(XMLUtil.getStringAttr(element, "category")) || m_clonedChannelMap.contains(sName);
   }

   /**
    * Complete all the units of work for the created invocation context.
    */
   private void complete()
   {
      if (m_context != null)
      {
         m_context.complete(false);
      }
   }

   /**
    * Loads the metadata.
    */
   private void loadMetadata()
   {
      if (m_metadata != null)
      {
         return;
      }

      Metadata metadata = Repository.isLoaded() ? Repository.getMetadata() : null;
      InvocationContext context = null;

      if (metadata instanceof XMLMetadata)
      {
         m_metadata = (XMLMetadata)metadata;
         context = (InvocationContext)ThreadContextHolder.getContext();
      }
      else
      {
         boolean bContained = J2EEUtil.isMinimal();
         Context ctxSaved = ThreadContextHolder.getContext();

         try
         {
            s_logger.info("Loading metadata to access template channel cloning environment");

            // Forces the loader to use the default method
            Properties properties = new Properties(m_properties);

            if (properties.containsKey("dynamic.enabled"))
            {
               properties.setProperty("dynamic.enabled", "false");
            }

            properties.setProperty(XMLMetadataLoader.METADATA_BOOTSTRAP_PROPERTY, "true");

            if (properties.getProperty(MetadataLoader.METADATA_URL_PROPERTY) == null)
            {
               properties.setProperty(MetadataLoader.METADATA_URL_PROPERTY,
                  URLUtil.toURL(m_loader.getHelper().getRootURL().toString()));
            }

            J2EEUtil.setMinimal(true);

            m_metadata = (XMLMetadata)new MetadataLoaderDispatcher().load(null, properties, 0, null);
 
            context = (InvocationContext)m_metadata.getComponent("System.InvocationContext")
               .getInstance(null);

            context.initialize(null);
            context.setLocale(Locale.ENGLISH);
            context.getUnitOfWork().commit();
            context.setSecure(false);
            m_context = context;
         }
         catch (Throwable t)
         {
            ObjUtil.rethrow(t);
         }
         finally
         {
            complete();
            J2EEUtil.setMinimal(bContained);
            ThreadContextHolder.setContext(ctxSaved);
         }
      }

      m_machine = context.getMachine();
   }

   /**
    * Set properties on the DOM element.
    * @param element The DOM element.
    * @param propertyList List of properties to set.
    * @return The value of the name property. Can be null.
    */
   private String applyProperties(Element element, List propertyList)
   {
      String sName = null;

      for (Iterator propItr = propertyList.iterator(); propItr.hasNext();)
      {
         Pair property = (Pair)propItr.next();
         String sKey = (String)property.getHead();
         String sValue = (String)property.getTail();

         if (sName == null && sKey.equalsIgnoreCase("name") && !StringUtil.isEmpty(sValue))
         {
            sName = (String)property.getTail();

            continue;
         }

         element.setAttribute(sKey, sValue);
      }

      return sName;
   }

   /**
    * @return List of channel properties used for cloning the template channel.
    * @param sName The name of the template channel.
    */
   private List getChannelProperties(String sName)
   {
      String sClassName = m_properties.getProperty(TEMPLATE_CLASS_NAME_PROPERTY);
      Metaclass channelClass = (Metaclass)m_metadata.findClassMeta(sClassName);

      if (channelClass != null)
      {
         Event getPropertiesEvent = channelClass.findEvent("getProperties", 1);

         if (getPropertiesEvent != null)
         {
            Context ctxSaved = ThreadContextHolder.getContext();

            try
            {
               ThreadContextHolder.setContext(m_context);

               return (List)getPropertiesEvent.invoke(channelClass, new Object[] {sName}, m_machine);
            }
            finally
            {
               complete();
               ThreadContextHolder.setContext(ctxSaved);
            }
         }
      }

      return null;
   }

   /**
    * Loads cloned channels from a template DOM element.
    * @param element The DOM element containing the template channel.
    * @param sName The data source name.
    * @param type The channel type.
    * @return List of cloned channels.
    */
   private List loadClones(Element element, String sName, ChannelType type)
   {
      List clonedChannelList = null;
      String sBootstrap = m_properties.getProperty(XMLMetadataLoader.METADATA_BOOTSTRAP_PROPERTY, "false");
      String sClassName = m_properties.getProperty(TEMPLATE_CLASS_NAME_PROPERTY);

      if (!StringUtil.isEmpty(sClassName) && !StringUtil.parseBoolean(sBootstrap))
      {
         loadMetadata();

         List propertiesList = getChannelProperties(sName);

         if (propertiesList != null && !propertiesList.isEmpty())
         {
            XMLMetadataHelper helper = m_loader.getHelper();
            int nSize = propertiesList.size();

            clonedChannelList = new ArrayList(nSize);

            for (int nIdx = 0; nIdx < nSize; nIdx++)
            {
               Element clonedElement = (Element)element.cloneNode(true);
               String sCloneName = applyProperties(clonedElement, (List)propertiesList.get(nIdx));

               if (sCloneName == null)
               {
                  sCloneName = CLONED_CHANNEL_PREFIX + '_' + sName + '_' + (nIdx + 1);
               }

               Channel clonedChannel = ((XMLIntegrationMetadataLoader)helper.getClassInstance(type.getLoader()))
                  .loadChannel(clonedElement, sCloneName, type, m_loader);

               clonedChannel.setCategory(null);
               clonedChannelList.add(clonedChannel);

               if (s_logger.isDebugEnabled())
               {
                  s_logger.debug("Channel \"" + sName + "\" cloned. New channel \"" + sCloneName
                     + "\" loaded.");
               }
            }
         }
      }

      m_clonedChannelMap.put(sName, clonedChannelList);

      return clonedChannelList;
   }

   /**
    * @see nexj.core.meta.xml.XMLMetadataLoaderHook#loadChannel(Element, String)
    */
   public boolean loadChannel(Element channelElement, String sName)
   {
      XMLMetadata metadata = m_loader.getMetadata();
      ChannelType type = metadata.getChannelTypeByElement(channelElement);

      if (isTemplate(channelElement, sName))
      {
         List cloneList = loadClones(channelElement, sName, type);

         if (cloneList != null && !cloneList.isEmpty())
         {
            for (Iterator channelItr = cloneList.iterator(); channelItr.hasNext();)
            {
               metadata.addChannel((Channel)channelItr.next());
            }
         }

         return true;
      }

      return false;
   }

   /**
    * @return List of connection properties.
    * @param clonedChannel The cloned channel.
    */
   private List getConnectionProperties(Channel clonedChannel)
   {
      String sClassName = m_properties.getProperty(TEMPLATE_CLASS_NAME_PROPERTY);
      Metaclass channelClass = (Metaclass)m_metadata.findClassMeta(sClassName);

      if (channelClass != null)
      {
         Event getPropertiesEvent = channelClass.findEvent("getConnectionProperties", 1);

         if (getPropertiesEvent != null)
         {
            Context ctxSaved = ThreadContextHolder.getContext();

            try
            {
               ThreadContextHolder.setContext(m_context);

               return (List)getPropertiesEvent.invoke(channelClass, new Object[] {clonedChannel}, m_machine);
            }
            finally
            {
               complete();
               ThreadContextHolder.setContext(ctxSaved);
            }
         }
      }

      return null;
   }

   /**
    * Loads the connection for cloned channels.
    * @param connectionElement The DOM element containing channel connection.
    * @param sChannelName The name of the template channel.
    * @param sConnectionsName The name of the environment.
    * @param connectionSet Set of loaded connections.
    */
   private void loadConnectionForClones(Element connectionElement, String sChannelName,
      String sConnectionsName, Set connectionSet)
   {
      List clonedChannelList = (List)m_clonedChannelMap.get(sChannelName);

      if (clonedChannelList != null && !clonedChannelList.isEmpty())
      {
         for (Iterator cloneItr = clonedChannelList.iterator(); cloneItr.hasNext();)
         {
            Channel channel = (Channel)cloneItr.next();
            List propertyList = getConnectionProperties(channel);
            Element clonedConnectionElement = connectionElement;

            if (propertyList != null && !propertyList.isEmpty())
            {
               clonedConnectionElement = (Element)connectionElement.cloneNode(true);

               applyProperties(clonedConnectionElement, propertyList);
            }

            m_loader.loadConnection(clonedConnectionElement, sConnectionsName, channel, connectionSet);
         }
      }
   }

   /**
    * @see nexj.core.meta.xml.XMLMetadataLoaderHook#loadConnection(Element, String, String, Set)
    */
   public boolean loadConnection(Element connectionElement, String sConnectionsName, String sChannelName,
      Set connectionSet)
   {
      if (isTemplate(connectionElement, sChannelName))
      {
         loadConnectionForClones(connectionElement, sChannelName, sConnectionsName, connectionSet);

         return true;
      }

      return false;
   }
}
