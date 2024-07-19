package nexj.core.rpc.jms.ra.platform.activemq;

import java.lang.reflect.Method;
import java.util.Properties;

import javax.jms.Message;
import javax.jms.Session;
import javax.resource.spi.ActivationSpec;
import javax.resource.spi.ResourceAdapter;

import nexj.core.rpc.jms.JMSUtil;
import nexj.core.rpc.jms.ra.JMSConsumerConfig;
import nexj.core.rpc.jms.ra.platform.JMSPlatformAdapter;
import nexj.core.rpc.jms.ra.platform.JMSPlatformConsumerPool;
import nexj.core.rpc.jms.ra.platform.JMSPlatformMessageEndpoint;
import nexj.core.util.BeanAccessor;
import nexj.core.util.J2EEUtil;
import nexj.core.util.ObjUtil;

public class ActiveMQPlatformAdapter implements JMSPlatformAdapter
{
   // constants

   /**
    * ActiveMQ class names.
    */
   protected final static String AMQ_MESSAGE_CLASS_NAME = "org.apache.activemq.command.ActiveMQMessage";

   // associations

   /**
    * The bean accessor for setting properties on wrapped objects.
    */
   protected final BeanAccessor m_beanAccessor = new BeanAccessor();

   /**
    * The variable that will contain the class for the ActiveMQ resource adapter.
    */
   protected static Class s_amqRAClass;

   /**
    * The variable that will contain the class for the ActiveMQ activation spec.
    */
   protected static Class s_amqActSpecClass;

   /**
    * org.apache.activemq.ra.ActiveMQResourceAdapter.getInfo().
    */
   protected static Method s_getInfoMethod;

   /**
    * org.apache.activemq.ra.ActiveMQConnectionRequestInfo.setServerUrl(String);
    */
   protected static Method s_setServerURLMethod;

   /**
    * Used to make a copy of an ActiveMQ message
    */
   protected static Method s_copyMethod;

   static
   {
      try
      {
         s_copyMethod = Class.forName(AMQ_MESSAGE_CLASS_NAME, false, ActiveMQPlatformAdapter.class.getClassLoader()).getMethod("copy", null);
         s_amqRAClass = Class.forName("org.apache.activemq.ra.ActiveMQResourceAdapter");
         s_amqActSpecClass = Class.forName("org.apache.activemq.ra.ActiveMQActivationSpec");
         s_getInfoMethod = s_amqRAClass.getMethod("getInfo", null);
         s_setServerURLMethod = Class.forName("org.apache.activemq.ra.ActiveMQConnectionRequestInfo")
            .getMethod("setServerUrl", new Class[]{String.class});
         
      }
      catch (Throwable t)
      {
         ObjUtil.rethrow(t);
      }
   }

   // operations

   /**
    * @see nexj.core.rpc.jms.ra.platform.JMSPlatformAdapter#createResourceAdapter(nexj.core.rpc.jms.ra.JMSConsumerConfig)
    */
   public ResourceAdapter createResourceAdapter(JMSConsumerConfig config) throws Throwable
   {
      ClassLoader classLoaderSaved = JMSUtil.setContextClassLoader(getClassLoader());

      try
      {
         ResourceAdapter adapter = (ResourceAdapter)s_amqRAClass.newInstance();
         Object info = s_getInfoMethod.invoke(adapter, null);
         Properties properties = config.getConnectionFactoryPropertyMap();
         String sBrokerURL = (properties == null) ? null : properties.getProperty("brokerURL");

         if (sBrokerURL == null)
         {
            throw new Exception("Missing connection factory property \"brokerURL\"");
         }

         s_setServerURLMethod.invoke(info, new Object[]{sBrokerURL});

         return adapter;
      }
      finally
      {
         JMSUtil.setContextClassLoader(classLoaderSaved);
      }
   }

   /**
    * @see nexj.core.rpc.jms.ra.platform.JMSPlatformAdapter#createActivationSpec(nexj.core.rpc.jms.ra.JMSConsumerConfig)
    */
   public ActivationSpec createActivationSpec(JMSConsumerConfig config) throws Throwable
   {
      ClassLoader classLoaderSaved = JMSUtil.setContextClassLoader(getClassLoader());
      ActivationSpec spec = null;

      try
      {
         spec = (ActivationSpec)s_amqActSpecClass.newInstance();
      }
      finally
      {
         JMSUtil.setContextClassLoader(classLoaderSaved);
      }

      m_beanAccessor.setProperty(spec, "acknowledgeMode",
         (config.getAckMode() == Session.AUTO_ACKNOWLEDGE) ? "Auto-acknowledge" : "Dups-ok-acknowledge");
      m_beanAccessor.setProperty(spec, "clientId", config.getClientId());
      m_beanAccessor.setProperty(spec, "destinationType", (config.isBroadcast()) ? "javax.jms.Topic" : "javax.jms.Queue");
      m_beanAccessor.setProperty(spec, "destination", config.getDestinationPropertyMap().getProperty("physicalName"));
      m_beanAccessor.setProperty(spec, "password", config.getPassword());
      m_beanAccessor.setProperty(spec, "subscriptionDurability", (config.isDurable()) ? "Durable" : "NonDurable");
      m_beanAccessor.setProperty(spec, "subscriptionName", config.getSubscriptionName());
      m_beanAccessor.setProperty(spec, "userName", config.getUser());

      return spec;
   }

   /**
    * @see nexj.core.rpc.jms.ra.platform.JMSPlatformAdapter#setMessageSelector(javax.resource.spi.ActivationSpec,
    *      java.lang.String)
    */
   public void setMessageSelector(ActivationSpec spec, String sMessageSelector) throws Throwable
   {
   }

   /**
    * @see nexj.core.rpc.jms.ra.platform.JMSPlatformAdapter#beforeDelivery(JMSPlatformMessageEndpoint)
    */
   public void beforeDelivery(JMSPlatformMessageEndpoint endpoint)
   {
   }

   /**
    * @see nexj.core.rpc.jms.ra.platform.JMSPlatformAdapter#afterDelivery(JMSPlatformMessageEndpoint)
    */
   public void afterDelivery(JMSPlatformMessageEndpoint endpoint)
   {
   }

   /**
    * @see nexj.core.rpc.jms.ra.platform.JMSPlatformAdapter#beforeStop(JMSPlatformConsumerPool)
    */
   public void beforeStop(JMSPlatformConsumerPool pool)
   {
   }

   /**
    * @see nexj.core.rpc.jms.ra.platform.JMSPlatformAdapter#afterStop(JMSPlatformConsumerPool)
    */
   public void afterStop(JMSPlatformConsumerPool pool)
   {
   }

   /**
    * @return The class loader needed for ActiveMQ components.
    */
   protected ClassLoader getClassLoader()
   {
      if (J2EEUtil.CONTAINER == J2EEUtil.WEBSPHERE)
      {
         return s_amqRAClass.getClassLoader();
      }

      return null;
   }

   /**    
    * @see nexj.core.rpc.jms.ra.platform.JMSPlatformAdapter#prepareMessage(javax.jms.Message)
    * 
    * This method is used for copying the message for ActiveMQ, as ActiveMQ does not do it.
    * As other message brokers copy the message before sending it to our application,
    * the method just returns the argument 
    */
   public Message prepareMessage(Message message)
   {
      try
      {
         return (Message)s_copyMethod.invoke(message, null);
      }
      catch (Throwable t)
      {
         throw ObjUtil.rethrow(t);
      }
   }
}
