// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.jms.ra;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import javax.jms.ConnectionFactory;
import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Queue;
import javax.jms.Topic;
import javax.naming.InitialContext;
import javax.resource.ResourceException;
import javax.resource.spi.ActivationSpec;
import javax.resource.spi.ConnectionManager;
import javax.resource.spi.InvalidPropertyException;
import javax.resource.spi.ManagedConnectionFactory;
import javax.resource.spi.UnavailableException;
import javax.resource.spi.endpoint.MessageEndpointFactory;
import javax.transaction.xa.XAException;
import javax.transaction.xa.XAResource;
import javax.transaction.xa.Xid;

import nexj.core.rpc.jms.ra.engine.GenericJMSEngineAdapter;
import nexj.core.rpc.jms.ra.engine.JMSEngineAdapter;
import nexj.core.rpc.jms.ra.platform.JMSPlatformAdapter;
import nexj.core.rpc.jms.ra.platform.JMSPlatformConsumerPool;
import nexj.core.rpc.ra.GenericConnectionManager;
import nexj.core.rpc.ra.GenericConsumerPool;
import nexj.core.rpc.ra.TransactionalResourceAdapter;
import nexj.core.util.BeanAccessor;
import nexj.core.util.Logger;
import nexj.core.util.PropertiesAware;

/**
 * JMS resource adapter.
 */
public class JMSResourceAdapter extends TransactionalResourceAdapter
{
   // constants

   /**
    * The bean URL scheme.
    */
   public final static String BEAN_SCHEME = "class:";

   // associations

   /**
    * The default connection manager.
    */
   private static GenericConnectionManager s_defaultConnectionManager;
   
   /**
    * The class logger.
    */
   private final static Logger s_logger = Logger.getLogger(JMSResourceAdapter.class);

   // constructors

   /**
    * Constructs the resource adapter.
    */
   public JMSResourceAdapter()
   {
      super(s_logger);
   }

   // operations

   /**
    * @see nexj.core.rpc.ra.GenericResourceAdapter#createConsumerPool(javax.resource.spi.endpoint.MessageEndpointFactory, javax.resource.spi.ActivationSpec)
    */
   protected GenericConsumerPool createConsumerPool(MessageEndpointFactory factory, ActivationSpec cfg) throws ResourceException
   {
      JMSConsumerConfig config = (JMSConsumerConfig)cfg;
      JMSPlatformAdapter platform = getJMSPlatformAdapter(config.getConnectionFactory());
      
      if (platform != null)
      {
         return new JMSPlatformConsumerPool(this, factory, config, platform);
      }
      
      return new JMSConsumerPool(this, factory, config);
   }
   
   /**
    * @param sFactory The JMS connection factory name.
    * @return An instance of a platform adapter if sFactory was configured to be an
    *         implementation of JMSPlatformAdapter, null otherwise.
    * @throws ResourceException
    */
   protected JMSPlatformAdapter getJMSPlatformAdapter(String sFactory) throws ResourceException
   {
      if (sFactory != null && sFactory.startsWith(BEAN_SCHEME))
      {
         Class factoryClass;

         try
         {
            factoryClass = Class.forName(sFactory.substring(BEAN_SCHEME.length()));
         }
         catch (Throwable t)
         {
            throw new InvalidPropertyException("Invalid connection factory \"" + sFactory + "\"", t);
         }

         if (JMSPlatformAdapter.class.isAssignableFrom(factoryClass))
         {
            try
            {
               return (JMSPlatformAdapter)factoryClass.newInstance();
            }
            catch (Throwable t)
            {
               throw new UnavailableException("Unable to instantiate the platform adapter \"" +
                  factoryClass.getName() + "\"", t);
            }
         }
      }
      
      return null;
   }

   /**
    * Looks up a JNDI resource or instantiates a bean.
    * @param sName The JNDI name or bean URL (class:name).
    * @param properties The bean or JNDI context property map.
    */
   public static Object getResource(String sName, Properties properties) throws Throwable
   {
      Object obj = null;
      
      if (sName.startsWith(BEAN_SCHEME))
      {
         obj = Class.forName(sName.substring(BEAN_SCHEME.length())).newInstance();

         if (!(obj instanceof PropertiesAware))
         {
            new BeanAccessor().setProperties(obj, properties);
            
         }
      }

      if (obj == null)
      {
         obj = new InitialContext(properties).lookup(sName);
      }

      if (obj instanceof PropertiesAware)
      {
         ((PropertiesAware)obj).setProperties(properties);
      }

      if (obj instanceof JMSDestination)
      {
         obj = ((JMSDestination)obj).getEngineJMSDestination();
      }

      return obj;
   }
   
   /**
    * Returns a JMSEngineAdapter capable of returning a JMS connection factory based on the parameters passed.
    * @param sName The JNDI name or bean URL (class:name) of the JMS connection factory of JMSEngineAdapter implementation.
    * @param properties The JNDI context or JMS connection factory property map.
    */
   public static JMSEngineAdapter getJMSEngineAdapter(String sName, Properties properties) throws Throwable
   {
      Object obj = getResource(sName, properties);

      if (sName.startsWith(BEAN_SCHEME))
      {
         if (obj instanceof JMSEngineAdapter)
         {
            return (JMSEngineAdapter)obj;
         }

         if (obj instanceof ManagedConnectionFactory)
         {
            return new GenericJMSEngineAdapter((ConnectionFactory)((ManagedConnectionFactory)obj).createConnectionFactory());
         }
      }

      return new GenericJMSEngineAdapter((ConnectionFactory)obj);
   }

   /**
    * @return The default connection manager.
    */
   public static synchronized ConnectionManager getDefaultConnectionManager()
   {
      if (s_defaultConnectionManager == null)
      {
         s_defaultConnectionManager = new GenericConnectionManager(GenericConnectionManager.ASSOCIATED);
      }

      return s_defaultConnectionManager;
   }
   
   /**
    * Unwraps a JMS destination from JMSDestination or return the original.
    * @param jmsDestination The destination to check if unwrapping is necessary.
    * @return The unwrapped or the original JMS destination.
    */
   public static Destination getJMSDestination(Destination jmsDestination)
   {
      if (jmsDestination instanceof JMSDestination)
      {
         return ((JMSDestination)jmsDestination).getEngineJMSDestination();
      }
      
      return jmsDestination;
   }
   
   /**
    * Unwraps a JMS queue from JMSDestination or return the original.
    * @param jmsQueue The queue to check if unwrapping is necessary.
    * @return The unwrapped or the original JMS queue.
    */
   public static Queue getJMSDestination(Queue jmsQueue)
   {
      return (Queue)getJMSDestination((Destination)jmsQueue);
   }
   
   /**
    * Unwraps a JMS topic from JMSDestination or return the original.
    * @param jmsTopic The topic to check if unwrapping is necessary.
    * @return The unwrapped or the original JMS topic.
    */
   public static Topic getJMSDestination(Topic jmsTopic)
   {
      return (Topic)getJMSDestination((Destination)jmsTopic);
   }
   
   /**
    * Initializes a JMSException from a ResourceException
    * @param resEx The JMSException to initialize a ResourceException from.
    * @return The initialized JMSException.
    */
   public static JMSException createJMSException(ResourceException resEx)
   {
      if (resEx.getCause() instanceof JMSException)
      {
         return (JMSException)resEx.getCause();
      }
      
      return createJMSException(resEx.getLocalizedMessage(), resEx);
   }
   
   /**
    * Initializes a JMSException.
    * @param sMessage The message to use as the exception reason.
    * @param cause The throwable to used as the exception cause.
    * @return The initialized JMSException. 
    */
   public static JMSException createJMSException(String sMessage, Throwable cause)
   {
      JMSException jmsEx = new JMSException(sMessage);

      jmsEx.initCause(cause);

      return jmsEx;
   }
   
   /**
    * @see javax.resource.spi.ResourceAdapter#getXAResources(javax.resource.spi.ActivationSpec[])
    */
   public XAResource[] getXAResources(ActivationSpec[] specs) throws ResourceException
   {
      List xaResList = new ArrayList();

      for (int i = 0; i < specs.length; i++)
      {
         JMSConsumerConfig jmsConsumerConfig = (JMSConsumerConfig)specs[i];
         JMSPlatformAdapter jmsPlatformAdapter = getJMSPlatformAdapter(jmsConsumerConfig.getConnectionFactory());
         
         if (jmsPlatformAdapter == null)
         {
            try
            {
               xaResList.add(new JMSXAResource(getJMSEngineAdapter(
                  jmsConsumerConfig.getConnectionFactory(), jmsConsumerConfig.getConnectionFactoryPropertyMap()), jmsConsumerConfig.getAcknowledgeMode() == null));
            }
            catch (Throwable t)
            {
               s_logger.error("Error retrieving JMSEngineAdapter for JMS connection factory " +
                  jmsConsumerConfig.getConnectionFactory(), t);
            }
         }
         else
         {
            try
            {
               XAResource[] platformXAResArray = jmsPlatformAdapter.createResourceAdapter(jmsConsumerConfig).
                  getXAResources(new ActivationSpec[]{jmsPlatformAdapter.createActivationSpec(jmsConsumerConfig)});
               
               if (platformXAResArray != null)
               {
                  for (int k = 0; k < platformXAResArray.length; k++)
                  {
                     xaResList.add(platformXAResArray[k]);
                  }
               }
               else
               {
                  xaResList.add(new XAResource()
                  {
                     // operations

                     /**
                      * @see javax.transaction.xa.XAResource#commit(javax.transaction.xa.Xid, boolean)
                      */
                     public void commit(Xid xid, boolean bOnePhase) throws XAException
                     {
                     }

                     /**
                      * @see javax.transaction.xa.XAResource#end(javax.transaction.xa.Xid, int)
                      */
                     public void end(Xid xid, int nFlags) throws XAException
                     {
                     }

                     /**
                      * @see javax.transaction.xa.XAResource#forget(javax.transaction.xa.Xid)
                      */
                     public void forget(Xid xid) throws XAException
                     {
                     }

                     /**
                      * @see javax.transaction.xa.XAResource#getTransactionTimeout()
                      */
                     public int getTransactionTimeout() throws XAException
                     {
                        return 0;
                     }

                     /**
                      * @see javax.transaction.xa.XAResource#isSameRM(javax.transaction.xa.XAResource)
                      */
                     public boolean isSameRM(XAResource xar) throws XAException
                     {
                        return xar == this;
                     }

                     /**
                      * @see javax.transaction.xa.XAResource#prepare(javax.transaction.xa.Xid)
                      */
                     public int prepare(Xid xid) throws XAException
                     {
                        return 0;
                     }

                     /**
                      * JMS message recovery is handled by the JMS engine.
                      * 
                      * @see javax.transaction.xa.XAResource#recover(int)
                      */
                     public Xid[] recover(int nFlag) throws XAException
                     {
                        return new Xid[0];
                     }

                     /**
                      * @see javax.transaction.xa.XAResource#rollback(javax.transaction.xa.Xid)
                      */
                     public void rollback(Xid xid) throws XAException
                     {
                     }

                     /**
                      * @see javax.transaction.xa.XAResource#setTransactionTimeout(int)
                      */
                     public boolean setTransactionTimeout(int nSeconds) throws XAException
                     {
                        return true;
                     }

                     /**
                      * @see javax.transaction.xa.XAResource#start(javax.transaction.xa.Xid, int)
                      */
                     public void start(Xid xid, int nFlag) throws XAException
                     {
                     }
                  });
               }
            }
            catch (Throwable t)
            {
               s_logger.error("Error retrieving platform resource adapter for platform adapter" +
                  jmsPlatformAdapter.getClass().getName(), t);
            }
         }
      }
      
      XAResource[] xaResArray = new XAResource[xaResList.size()];
      
      xaResList.toArray(xaResArray);
      
      return xaResArray;
   }
}