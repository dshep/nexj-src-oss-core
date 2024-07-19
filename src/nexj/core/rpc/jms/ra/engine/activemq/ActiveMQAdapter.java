// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.jms.ra.engine.activemq;

import java.io.File;
import java.io.FileFilter;
import java.lang.reflect.Method;
import java.net.URI;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.util.Collections;
import java.util.Map;
import java.util.Properties;

import javax.jms.ConnectionFactory;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.Session;
import javax.jms.XAConnection;
import javax.jms.XAConnectionFactory;

import nexj.core.rpc.jms.ra.JMSConsumerConfig;
import nexj.core.rpc.jms.ra.engine.JMSEngineAdapter;
import nexj.core.util.BeanAccessor;
import nexj.core.util.GUIDUtil;
import nexj.core.util.IOUtil;
import nexj.core.util.J2EEUtil;

/**
 * Used to instantiate JMS objects specific to the ActiveMQ JMS engine. 
 */
public class ActiveMQAdapter implements JMSEngineAdapter
{
   // constants
   
   /**
    * ActiveMQ class names.
    */
   protected final static String AMQ_CONNECTION_FACTORY_CLASS_NAME = "org.apache.activemq.ActiveMQXAConnectionFactory";
   protected final static String AMQ_SESSION_CLASS_NAME = "org.apache.activemq.ActiveMQSession";
   protected final static String AMQ_XARESOURCE_CLASS_NAME = "org.apache.activemq.TransactionContext";
   protected final static String AMQ_MESSAGE_CLASS_NAME = "org.apache.activemq.command.ActiveMQMessage";
   protected final static String AMQ_URI_SUPPORT_CLASS_NAME = "org.apache.activemq.util.URISupport";
   protected final static String AMQ_COMPOSITE_DATA_CLASS_NAME = "org.apache.activemq.util.URISupport$CompositeData";
   
   /**
    * The broker url property name.
    */
   protected final static String AMQ_BROKER_URL_PROPERTY_NAME = "brokerURL";
   
   /**
    * The persistent property name.
    */
   protected final static String AMQ_PERSISTENT_PROPERTY_NAME = "persistent";
   
   /**
    * The useJmx property name.
    */
   protected final static String AMQ_USE_JMX_PROPERTY_NAME = "useJmx";
   
   /**
    * The clientId property name.
    */
   protected final static String AMQ_CLIENTID_PROPERTY_NAME = "jms.clientID";
   
   /**
    * The data directory property name.
    */
   protected final static String AMQ_DATA_DIR_PROPERTY_NAME = "dataDirectory";
   
   /**
    * The data store index file name.
    */
   protected final static String AMQ_DATA_STORE_INDEX_FILE_NAME = "db.data";

   // attributes
   
   /**
    * Flag to indicate first access to this class.
    */
   protected static boolean s_bAccessed;
   
   /**
    * True if running in test mode.
    */
   protected boolean m_bTest;
   
   // associations

   /**
    * Used to set properties on a bean.
    */
   protected final BeanAccessor m_beanAccessor;
   
   /**
    * Used to set transaction context on a Session.
    */
   protected final static Method s_setTxCtxMethod;
   
   /**
    * Used to get transaction context from a Session.
    */
   protected final static Method s_getTxCtxMethod;
   
   /**
    * Used to make a copy of an ActiveMQ message
    */
   protected final static Method s_messageCopyMethod;
   
   /**
    * The following ActiveMQ helper methods are used to set the default data directory.
    */
   protected final static Method s_parseCompositeMethod;
   protected final static Method s_getComponentsMethod;
   protected final static Method s_getParametersMethod;
   protected final static Method s_toURIMethod;
   
   static
   {
      try
      {
         ClassLoader ccl = ActiveMQAdapter.class.getClassLoader();
         
         s_setTxCtxMethod = getClass(AMQ_SESSION_CLASS_NAME, ccl).getMethod("setTransactionContext", new Class[]{Class.forName(AMQ_XARESOURCE_CLASS_NAME)});
         s_getTxCtxMethod = getClass(AMQ_SESSION_CLASS_NAME, ccl).getMethod("getTransactionContext", null);
         s_messageCopyMethod = getClass(AMQ_MESSAGE_CLASS_NAME, ccl).getMethod("copy", null);
         s_parseCompositeMethod = getClass(AMQ_URI_SUPPORT_CLASS_NAME, ccl).getMethod("parseComposite", new Class[] {URI.class});
         s_getComponentsMethod = getClass(AMQ_COMPOSITE_DATA_CLASS_NAME, ccl).getMethod("getComponents", null);
         s_getParametersMethod = getClass(AMQ_COMPOSITE_DATA_CLASS_NAME, ccl).getMethod("getParameters", null);
         s_toURIMethod = getClass(AMQ_COMPOSITE_DATA_CLASS_NAME, ccl).getMethod("toURI", null);
      }
      catch (Throwable t)
      {
         throw new RuntimeException("Error initializing JMS engine object factory: ActiveMQAdapter", t);
      }
   }
   
   /**
    * The JMS connection factory properties.
    */
   protected Properties m_jmsEngineConnectionFactoryProperties;

   /**
    * Constructs the ActiveMQAdapter
    */
   public ActiveMQAdapter()
   {
      super();
      m_beanAccessor = new BeanAccessor();
   }

   // operations
   
   /**
    * Initializes a class object but does not load it.
    * @param sClassName The name of the class to initialize.
    * @param classLoader The class loader to use for initializing the class.
    * @return The initialized class.
    * @throws ClassNotFoundException
    */
   protected static Class getClass(String sClassName, ClassLoader classLoader) throws ClassNotFoundException
   {
      return Class.forName(sClassName, false, classLoader);
   }

   /**
    * @see nexj.core.util.PropertiesAware#setProperties(java.util.Properties)
    */
   public void setProperties(Properties jmsEngineConnectionFactoryProperties) throws Throwable
   {
      String sAMQDataDirDefaultValue = 
         (J2EEUtil.getDataDir().getAbsolutePath().replaceAll("\\\\", "/") + "/activemq-data");
      
      m_jmsEngineConnectionFactoryProperties = 
         (Properties)jmsEngineConnectionFactoryProperties.clone();
      
      String sDataDir = (String)m_jmsEngineConnectionFactoryProperties.remove(AMQ_DATA_DIR_PROPERTY_NAME);
      
      if (sDataDir != null && sDataDir.length() > 0)
      {
         if (!new File(sDataDir).isAbsolute())
         {
            sDataDir = sAMQDataDirDefaultValue + "/" + sDataDir;
         }
      }
      else
      {
         sDataDir = sAMQDataDirDefaultValue;
      }

      addConfigURLProperty(AMQ_DATA_DIR_PROPERTY_NAME, sDataDir, false);

      m_bTest = "true".equals(m_jmsEngineConnectionFactoryProperties.remove("test"));

      synchronized (ActiveMQAdapter.class)
      {
         if (!s_bAccessed)
         {
            s_bAccessed = true;
            
            File dataDir = new File(sDataDir);
            
            if (m_bTest)
            {
               IOUtil.deleteRecursively(dataDir);
            }
            else
            {
               IOUtil.deleteRecursively(dataDir, new FileFilter()
               {
                  /**
                   * @see java.io.FileFilter#accept(java.io.File)
                   */
                  public boolean accept(File file)
                  {
                     return file.getName().equals(AMQ_DATA_STORE_INDEX_FILE_NAME);
                  }
               });
            }
         }
      }
   }

   /**
    * @see nexj.core.rpc.jms.ra.engine.JMSEngineAdapter#getConnectionConsumerMaxMessageCount(nexj.core.rpc.jms.ra.JMSConsumerConfig)
    */
   public int getConnectionConsumerMaxMessageCount(JMSConsumerConfig consumerConfig)
   {
      return consumerConfig.getMaxPoolSize();
   }
   
   /**
    * @see nexj.core.rpc.jms.ra.engine.JMSEngineAdapter#getMaxPoolSize(nexj.core.rpc.jms.ra.JMSConsumerConfig)
    */
   public int getMaxPoolSize(JMSConsumerConfig consumerConfig)
   {
      if (consumerConfig.getMaxPoolSize() == 0)
      {
         return 0;
      }
      
      return consumerConfig.getMaxPoolSize() + 1;
   }

   /**
    * @see nexj.core.rpc.jms.ra.engine.JMSEngineAdapter#getJMSConnectionFactory(boolean, boolean)
    */
   public Object getJMSConnectionFactory(boolean bTransacted, boolean bRecovery) throws Throwable
   {
      ConnectionFactory jmsConnFactory = 
         (ConnectionFactory)Class.forName(AMQ_CONNECTION_FACTORY_CLASS_NAME).newInstance();

      if (m_bTest || bRecovery)
      {
         addConfigURLProperty(AMQ_USE_JMX_PROPERTY_NAME, "false", true);
         addConfigURLProperty(AMQ_PERSISTENT_PROPERTY_NAME, "false", true);

         if (bRecovery)
         {
            addConfigURLProperty(AMQ_CLIENTID_PROPERTY_NAME, "recovery-" + GUIDUtil.generateGUID().toString(), true);
         }
      }
      else
      {
         if (isPeerTransport())
         {
            addConfigURLProperty(AMQ_PERSISTENT_PROPERTY_NAME, "true", false);
         }
      }

      m_beanAccessor.setProperties(jmsConnFactory, m_jmsEngineConnectionFactoryProperties);

      return new ActiveMQConnectionFactoryWrapper(jmsConnFactory);
   }

   /**
    * @see nexj.core.rpc.jms.ra.engine.JMSEngineAdapter#setSharedTransactions(javax.jms.Session, javax.jms.Session)
    */
   public void setSharedTransactions(Session newJMSSession, Session sharedJMSSession) throws Throwable
   {
      s_setTxCtxMethod.invoke(newJMSSession, new Object[]{s_getTxCtxMethod.invoke(sharedJMSSession, null)});
   }
   
   /**
    * @see nexj.core.rpc.jms.ra.engine.JMSEngineAdapter#prepareMessage(javax.jms.Message)
    */
   public Message prepareMessage(Message message) throws Throwable
   {
      return (Message)s_messageCopyMethod.invoke(message, null);
   }

   /**
    * @return True if the broker is configured to use peer transport.
    * @throws Throwable
    */
   protected boolean isPeerTransport() throws Throwable
   {
      String sConfURL = m_jmsEngineConnectionFactoryProperties.getProperty(AMQ_BROKER_URL_PROPERTY_NAME);

      return sConfURL != null && sConfURL.startsWith("peer:");
   }

   /**
    * @see nexj.core.rpc.jms.ra.engine.JMSEngineAdapter#isSameRMUsed()
    */
   public boolean isSameRMUsed()
   {
      return false;
   }

   /**
    * @see nexj.core.rpc.jms.ra.engine.JMSEngineAdapter.getSession()
    */
   public Session getSession(Session session) throws JMSException
   {
      return session;
   }

   /**
    * Adds a configuration property to the broker configuration URL.
    * @param configProperties The connection factory configuration properties
    * @param sPropertyName The name of the property to add.
    * @param sPropertyValue The value of the property to add.
    * @param bOverride True to override the property if it is already configured.
    * @throws Throwable
    */
   protected void addConfigURLProperty(String sPropertyName, String sPropertyValue, boolean bOverride) throws Throwable
   {
      sPropertyName = URLEncoder.encode(sPropertyName, "UTF-8");
      sPropertyValue = URLEncoder.encode(sPropertyValue, "UTF-8");
      String sConfURL = m_jmsEngineConnectionFactoryProperties.getProperty(AMQ_BROKER_URL_PROPERTY_NAME);

      if (sConfURL != null)
      {
         URI confURI = new URI(sConfURL);

         // only vm and peer transports can contain broker config URL
         if(confURI.getScheme().equals("vm") || confURI.getScheme().equals("peer"))
         {
            // using AMQ helper methods to parse brokerURL
            Object confData = s_parseCompositeMethod.invoke(null, new Object[] {confURI}); 
            URI[] confDataComponents = (URI[])s_getComponentsMethod.invoke(confData, null);
            boolean bAddedProp = false;

            // composite brokerURL (vm transport only)
            if (confDataComponents.length == 1 && "broker".equals(confDataComponents[0].getScheme()))
            {
               URI brokerURI = confDataComponents[0];
               Object brokerData = s_parseCompositeMethod.invoke(null, new Object[] {brokerURI});
               Map brokerPropsMap = (Map)s_getParametersMethod.invoke(brokerData, null);
               
               // empty URI query
               if (brokerPropsMap.equals(Collections.EMPTY_MAP))
               {
                  confDataComponents[0] = new URI(brokerURI.toASCIIString() + '?' + 
                     sPropertyName + '=' + sPropertyValue);
                  bAddedProp = true;
               }
               else if (!brokerPropsMap.containsKey(sPropertyName) || bOverride)
               {
                  brokerPropsMap.put(sPropertyName, sPropertyValue);
                  confDataComponents[0] = (URI)s_toURIMethod.invoke(brokerData, null);
                  bAddedProp = true;
               }
               
               if (bAddedProp)
               {
                  confURI = (URI)s_toURIMethod.invoke(confData, null);
                  sConfURL = URLDecoder.decode(confURI.toASCIIString(), "UTF-8");
               }
            }
            // simple brokerURL (vm and peer transports)
            else
            {
               if (confURI.getScheme().equals("vm"))
               {
                  sPropertyName = "broker." + sPropertyName;
               }
               
               String sQuery = confURI.getQuery();
               
               if (sQuery == null)
               {
                  sQuery = "";
                  bAddedProp = true;
               }
               else if (!sQuery.contains(sPropertyName) || bOverride)
               {
                  if (bOverride)
                  {
                     String sToReplace = sPropertyName + '=' + sPropertyValue;
                     sQuery.replaceAll('^' + sToReplace + "?&|&?" + sToReplace, "");
                  }
                  
                  sQuery += '&';
                  bAddedProp = true;
               }
               
               if (bAddedProp)
               {
                  sQuery += sPropertyName + '=' + sPropertyValue;
                  confURI = new URI(confURI.getScheme(), confURI.getUserInfo(), confURI.getHost(),
                     confURI.getPort(), confURI.getPath(), sQuery, confURI.getFragment());
                  sConfURL = confURI.toASCIIString();
               }
            }
            
            if (bAddedProp)
            {
               m_jmsEngineConnectionFactoryProperties.setProperty(AMQ_BROKER_URL_PROPERTY_NAME, sConfURL);
            }
         }
      }
   }

   /**
    * Used to wrap around the actual ActiveMQ Connection Factory - necessary for disabling prefetch extension.
    */
   protected static class ActiveMQConnectionFactoryWrapper implements XAConnectionFactory
   {
      // constants

      /**
       * all static finals are necessary for disabling prefetch extension.
       */
      protected final static String AMQ_BROKER_REGISTRY_CLASS_NAME = "org.apache.activemq.broker.BrokerRegistry";
      protected final static String AMQ_BROKER_SERVICE_CLASS_NAME = "org.apache.activemq.broker.BrokerService";
      protected final static String AMQ_POLICY_ENTRY_CLASS_NAME = "org.apache.activemq.broker.region.policy.PolicyEntry";
      protected final static String AMQ_POLICY_MAP_CLASS_NAME = "org.apache.activemq.broker.region.policy.PolicyMap";
      protected final static String AMQ_BROKER_INFO_CLASS_NAME = "org.apache.activemq.command.BrokerInfo";
      protected final static String AMQ_CONNECTION_CLASS_NAME = "org.apache.activemq.ActiveMQConnection";

      /**
       * The following ActiveMQ helper methods are used to disable prefetch extension.
       */
      protected final static Method s_setDefaultEntryMethod;
      protected final static Method s_getBrokerNameMethod;
      protected final static Method s_setUsePrefetchExtensionMethod;
      protected final static Method s_getInstanceMethod;
      protected final static Method s_lookupMethod;
      protected final static Method s_setDestinationPolicyMethod;

      /**
       * Wrapped ActiveMQ Connection Factory
       */
      private XAConnectionFactory wrappedConnectionFactory;
      
      static
      {
         try
         {
            ClassLoader ccl = ActiveMQConnectionFactoryWrapper.class.getClassLoader();

            s_setDefaultEntryMethod = ActiveMQAdapter.getClass(AMQ_POLICY_MAP_CLASS_NAME, ccl).getMethod("setDefaultEntry", new Class[]{ActiveMQAdapter.getClass(AMQ_POLICY_ENTRY_CLASS_NAME, ccl)});
            s_getBrokerNameMethod = ActiveMQAdapter.getClass(AMQ_CONNECTION_CLASS_NAME, ccl).getMethod("getBrokerName", null);
            s_setUsePrefetchExtensionMethod = ActiveMQAdapter.getClass(AMQ_POLICY_ENTRY_CLASS_NAME, ccl).getMethod("setUsePrefetchExtension", new Class[]{Boolean.TYPE});
            s_getInstanceMethod = ActiveMQAdapter.getClass(AMQ_BROKER_REGISTRY_CLASS_NAME, ccl).getMethod("getInstance", null);
            s_lookupMethod = ActiveMQAdapter.getClass(AMQ_BROKER_REGISTRY_CLASS_NAME, ccl).getMethod("lookup", new Class[]{String.class});
            s_setDestinationPolicyMethod = ActiveMQAdapter.getClass(AMQ_BROKER_SERVICE_CLASS_NAME, ccl).getMethod("setDestinationPolicy", new Class[]{ActiveMQAdapter.getClass(AMQ_POLICY_MAP_CLASS_NAME, ccl)});
         }
         catch (Throwable t)
         {
            throw new RuntimeException("Error initializing JMS engine object factory: ActiveMQConnectionFactoryWrapper", t);
         }
      }

      /**
       * @param wrappedConnectionFactory - real Connection Factory
       * 
       * For now we support only XA AMQ Connection Factories
       */
      public ActiveMQConnectionFactoryWrapper(ConnectionFactory wrappedConnectionFactory)
      {
         this((XAConnectionFactory)wrappedConnectionFactory);
      }

      /**
       * @param wrappedConnectionFactory - real Connection Factory
       */
      public ActiveMQConnectionFactoryWrapper(XAConnectionFactory wrappedConnectionFactory)
      {
         this.wrappedConnectionFactory = wrappedConnectionFactory;
      }

      /**
       * @see javax.jms.XAConnectionFactory#createXAConnection()
       */
      public XAConnection createXAConnection() throws JMSException
      {
         XAConnection xac = wrappedConnectionFactory.createXAConnection();

         disablePrefetchExtension(xac);

         return xac;
      }

      /**
       * @see javax.jms.XAConnectionFactory#createXAConnection(java.lang.String,java.lang.String)
       */
      public XAConnection createXAConnection(String username, String password) throws JMSException
      {
         XAConnection xac = wrappedConnectionFactory.createXAConnection(username, password);

         disablePrefetchExtension(xac);

         return xac;
      }

      /**
       * Disables prefetch extension for the specified connection
       * 
       * @param xac connection
       */
      private void disablePrefetchExtension(XAConnection xac) throws JMSException
      {
         try
         {
            String sBrokerName = (String)s_getBrokerNameMethod.invoke(xac, null);
            Object broker = s_lookupMethod.invoke(s_getInstanceMethod.invoke(null, null), new Object[]{sBrokerName});
            Object policy = Class.forName(AMQ_POLICY_ENTRY_CLASS_NAME).newInstance();
            Object policyMap = Class.forName(AMQ_POLICY_MAP_CLASS_NAME).newInstance();
            
            s_setUsePrefetchExtensionMethod.invoke(policy, new Object[]{Boolean.FALSE});
            s_setDefaultEntryMethod.invoke(policyMap, new Object[]{policy});
            s_setDestinationPolicyMethod.invoke(broker, new Object[]{policyMap});
         }
         catch (Throwable t)
         {
            throw new JMSException("Error disabling prefetch extension in ActiveMQConnectionFactoryWrapper: " + t);
         }
      }
   }
}
