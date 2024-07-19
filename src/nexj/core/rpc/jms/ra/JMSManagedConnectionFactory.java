// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.jms.ra;

import java.util.Properties;

import javax.jms.Connection;
import javax.jms.ConnectionFactory;
import javax.jms.XAConnectionFactory;
import javax.resource.ResourceException;
import javax.resource.spi.ConnectionManager;
import javax.resource.spi.ConnectionRequestInfo;
import javax.resource.spi.ManagedConnection;
import javax.security.auth.Subject;

import nexj.core.rpc.jms.ra.engine.JMSEngineAdapter;
import nexj.core.rpc.ra.GenericManagedConnectionFactory;
import nexj.core.util.Logger;
import nexj.core.util.PropertyUtil;

/**
 * The ManagedConnectionFactory implementation for the JMS resource adapter.
 */
public class JMSManagedConnectionFactory extends GenericManagedConnectionFactory
{
   // constants
   
   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = -5553953894406978343L;

   // associations
   
   /**
    * Used to retrieve a JMS ConnectionFactory.
    * Can be either a JNDI name used to lookup the JMS ConnectionFactory (must be a full JNDI name containing ':') or 
    * A fully qualified class name prefixed by "class:" to instantiate the JMS ConnectionFactory directly.
    */
   protected String m_sJMSConnectionFactoryName;
   
   /**
    * Used to indicate whether the connection is transacted.
    */
   protected boolean m_bTransacted;

   /**
    * The JMS connection factory bean properties specific to a JMS engine provider.
    */
   protected Properties m_jmsConnectionFactoryProperties;
   
   /**
    * An implementation of a JMSEngineAdapter used to instantiate JMS objects specific to a JMS engine provider.
    */
   protected JMSEngineAdapter m_jmsEngineAdapter;
   
   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(JMSManagedConnectionFactory.class);

   // operations
   
   /**
    * Sets transaction mode (transacted or non-transacted) of the connection.
    * @param bTransacted boolean to contain true if the connection is transacted, false otherwise.
    * @throws ResourceException
    */
   public void setTransacted(boolean bTransacted)
   {
      m_bTransacted = bTransacted;
   }

   /**
    * Sets the implementation of a JMSEngineAdapter used to instantiate JMS objects specific to a JMS engine provider.
    * @param sJMSConnectionFactoryName The fully qualified class name of the JMSEngineAdapter implementation.
    * @throws ResourceException 
    */
   public void setJMSConnectionFactoryName(String sJMSConnectionFactoryName) throws ResourceException
   {
      try
      {
         m_sJMSConnectionFactoryName = sJMSConnectionFactoryName;
         
         if (m_jmsConnectionFactoryProperties != null)
         {
            setJMSConnectionFactory();
         }
      }
      catch (Throwable t)
      {
         throw new ResourceException("Error instantiating JMS connection factory with " + sJMSConnectionFactoryName + " for resource adapter " + this.getResourceAdapter(), t);
      }
   }

   /**
    * Sets the JMS connection factory bean properties specific to a JMS engine provider.
    * @param sJMSConnectionFactoryProperties The implementation specific JMS connection factory bean properties to set.
    * @throws ResourceException 
    */
   public void setJMSConnectionFactoryProperties(String sJMSConnectionFactoryProperties) throws ResourceException
   {
      try
      {
         m_jmsConnectionFactoryProperties = PropertyUtil.fromString(sJMSConnectionFactoryProperties);
         
         if (m_sJMSConnectionFactoryName != null)
         {
            setJMSConnectionFactory();
         }
      }
      catch (Throwable t)
      {
         throw new ResourceException("Error instantiating JMS connection factory with " + m_sJMSConnectionFactoryName + " for resource adapter " + this.getResourceAdapter(), t);
      }
   }
   
   /**
    * @return The fully qualified class name of the JMSEngineAdapter implementation.
    */
   public String getJMSConnectionFactoryName()
   {
      return m_sJMSConnectionFactoryName;
   }

   /**
    * @return Transaction mode (transacted or non-transacted) of the connection.
    */
   public boolean getTransacted()
   {
      return m_bTransacted;
   }

   /**
    * @return JMS connection factory bean properties specific to a JMS engine provider.
    */
   public Properties getJmsConnectionFactoryProperties()
   {
      return m_jmsConnectionFactoryProperties;
   }

   /**
    * Sets the JMS connection factory.
    * @throws Throwable
    */
   protected void setJMSConnectionFactory() throws Throwable
   {
      m_jmsEngineAdapter = JMSResourceAdapter.getJMSEngineAdapter(m_sJMSConnectionFactoryName, m_jmsConnectionFactoryProperties);
   }

   /**
    * @see nexj.core.rpc.ra.GenericManagedConnectionFactory#getDefaultConnectionManager()
    */
   protected ConnectionManager getDefaultConnectionManager()
   {
      return JMSResourceAdapter.getDefaultConnectionManager();
   }

   /**
    * @see javax.resource.spi.ManagedConnectionFactory#createConnectionFactory(javax.resource.spi.ConnectionManager)
    */
   public Object createConnectionFactory(ConnectionManager manager) throws ResourceException
   {
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Creating new connection factory handle in " + this);
      }
      
      return new JMSConnectionFactory(this, manager);
   }

   /**
    * @see javax.resource.spi.ManagedConnectionFactory#createManagedConnection(javax.security.auth.Subject, javax.resource.spi.ConnectionRequestInfo)
    */
   public ManagedConnection createManagedConnection(Subject subject, ConnectionRequestInfo cri) throws ResourceException
   {
      Object jmsConnFactory;
      Connection physicalJMSConn;
      JMSConnectionRequestInfo jmsCRI = (JMSConnectionRequestInfo)cri;
      
      try
      {
         if (jmsCRI == null)
         {
            jmsConnFactory = m_jmsEngineAdapter.getJMSConnectionFactory(m_bTransacted, true);
            
            if (jmsConnFactory instanceof XAConnectionFactory)
            {
               physicalJMSConn = ((XAConnectionFactory)jmsConnFactory).createXAConnection();
            }
            else
            {
               physicalJMSConn = ((ConnectionFactory)jmsConnFactory).createConnection();
            }
         }
         else
         {
            jmsConnFactory = m_jmsEngineAdapter.getJMSConnectionFactory(m_bTransacted, false);
            
            if (jmsConnFactory instanceof XAConnectionFactory)
            {
               physicalJMSConn = ((XAConnectionFactory)jmsConnFactory).createXAConnection(jmsCRI.getUserName(), jmsCRI.getPassword());
            }
            else
            {
               physicalJMSConn = ((ConnectionFactory)jmsConnFactory).createConnection(jmsCRI.getUserName(), jmsCRI.getPassword());
            }
         }

         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Creating new managed connection in " + this);
         }

         return new JMSManagedConnection(m_jmsEngineAdapter, physicalJMSConn, jmsCRI);
      }
      catch (Throwable t)
      {
         throw new ResourceException("Error creating physical connection to the JMS engine" + " for resource adapter " + this.getResourceAdapter(), t);
      }
   }
}
