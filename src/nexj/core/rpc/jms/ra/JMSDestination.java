// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.jms.ra;

import java.io.Serializable;
import java.util.Properties;

import javax.jms.Destination;
import javax.resource.ResourceException;

import nexj.core.util.PropertyUtil;

/**
 * A JMS Destination that serves as a wrapper around a JMS Destination specific to a JMS engine provider.
 */
public class JMSDestination implements Destination, Serializable
{
   // properties
   
   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = -6689325922829926518L;
   
   // associations

   /**
    * Either a JNDI name or a fully qualified class name prefixed with "class:" of a JMS destination
    * specific to a JMS engine provider to be looked up/instantiated.
    */
   protected String m_sJMSDestinationName;
   
   /**
    * The JMS destination properties specific to a JMS engine provider.
    */
   protected Properties m_jmsDestinationProperties;
   
   /**
    * The JMS destination specific to a JMS engine provider.
    */
   protected Destination m_jmsEngineDestination;
   
   // operations
   
   /**
    * @param sJMSDestinationName Either a JNDI name or a fully qualified class name prefixed with "class:"
    * of a JMS destination specific to a JMS engine provider to be looked up/instantiated. 
    * @throws ResourceException
    */
   public void setJMSDestinationName(String sJMSDestinationName) throws ResourceException
   {
      try
      {
         m_sJMSDestinationName = sJMSDestinationName;
         
         if (m_jmsDestinationProperties != null)
         {
            setEngineJMSDestination();
         }
      }
      catch (Throwable t)
      {
         throw new ResourceException("Error instantiating JMS destination " + sJMSDestinationName, t);
      }
   }

   /**
    * @param sJMSDestinationProperties The JMS destination properties specific to a JMS engine provider.
    * @throws ResourceException
    */
   public void setJMSDestinationProperties(String sJMSDestinationProperties) throws ResourceException
   {
      try
      {
         m_jmsDestinationProperties = PropertyUtil.fromString(sJMSDestinationProperties);
         
         if (m_sJMSDestinationName != null)
         {
            setEngineJMSDestination();
         }
      }
      catch (Throwable t)
      {
         throw new ResourceException("Error instantiating JMS destination " + m_sJMSDestinationName, t);
      }
   }

   /**
    * Sets the engine JMS destination.
    * @throws Throwable
    */
   protected void setEngineJMSDestination() throws Throwable
   {
      m_jmsEngineDestination = (Destination)JMSResourceAdapter.getResource(m_sJMSDestinationName, m_jmsDestinationProperties);
   }
   
   /**
    * @return The JMS destination specific to a JMS engine provider.
    */
   public Destination getEngineJMSDestination()
   {
      return m_jmsEngineDestination;
   }
}
