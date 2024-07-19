// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.jms.ra;

import java.io.IOException;
import java.util.Properties;

import javax.jms.Session;
import javax.resource.spi.InvalidPropertyException;

import nexj.core.rpc.ra.GenericConsumerConfig;
import nexj.core.rpc.ra.TransactionalConsumerConfig;
import nexj.core.util.PropertyUtil;
import nexj.core.util.SysUtil;
import nexj.core.util.cipher.CharacterStreamCipherDispatcher;

/**
 * JMS consumer endpoint configuration, corresponding to one MDB metadata spec.
 */
public class JMSConsumerConfig extends TransactionalConsumerConfig
{
   // attributes

   /**
    * The acknowledge mode ("Auto-acknowledge", "Dups-ok-acknowledge").
    */
   protected String m_sAckMode;

   /**
    * The subscription durability ("Durable", "NonDurable").
    */
   protected String m_sDurability;

   /**
    * The message selection filter.
    */
   protected String m_sMessageSelector;

   /**
    * The message destination type ("javax.jms.Queue", "javax.jms.Topic").
    */
   protected String m_sDestinationType;

   /**
    * The connection factory name.
    */
   protected String m_sConnectionFactory;

   /**
    * The message destination name.
    */
   protected String m_sDestination;

   /**
    * The durable subscription name.
    */
   protected String m_sSubscriptionName;

   /**
    * The connection client id.
    */
   protected String m_sClientId;

   /**
    * The user name (null for user).
    */
   protected String m_sUser;

   /**
    * The password.
    */
   protected String m_sPassword;

   /**
    * The connection factory properties.
    */
   protected String m_sConnectionFactoryProperties;
   
   /**
    * The destination properties.
    */
   protected String m_sDestinationProperties;
   
   /**
    * The session acknowledgment mode (Session.AUTO_ACKNOWLEDGE, Session.DUPS_OK_ACKNOWLEDGE).
    */
   protected int m_nAckMode;

   /**
    * True if the subscription is durable.
    */
   protected boolean m_bDurable;
   
   /**
    * True if the destination is a topic.
    */
   protected boolean m_bBroadcast;
   
   /**
    * The message selector update interval in milliseconds.
    */
   protected long m_lSelectorUpdateInterval = 30000;

   // associations

   /**
    * The connection factory properties.
    */
   protected Properties m_connectionFactoryProperties;

   /**
    * The destination properties.
    */
   protected Properties m_destinationProperties;
   
   // operations

   /**
    * Sets the acknowledge mode.
    * @param sAckMode The acknowledge mode to set.
    */
   public void setAcknowledgeMode(String sAckMode)
   {
      m_sAckMode = sAckMode;
   }

   /**
    * @return The acknowledge mode.
    */
   public String getAcknowledgeMode()
   {
      return m_sAckMode;
   }

   /**
    * @return The session acknowledgment mode (Session.AUTO_ACKNOWLEDGE, Session.DUPS_OK_ACKNOWLEDGE).
    */
   public int getAckMode()
   {
      return m_nAckMode;
   }
   
   /**
    * Sets the subscription durability.
    * @param sDurability The subscription durability to set.
    */
   public void setSubscriptionDurability(String sDurability)
   {
      m_sDurability = sDurability;
   }

   /**
    * @return The subscription durability.
    */
   public String getSubscriptionDurability()
   {
      return m_sDurability;
   }

   /**
    * @return True if the subscription is durable.
    */
   public boolean isDurable()
   {
      return m_bDurable;
   }
   
   /**
    * Sets the message selection filter.
    * @param sMessageSelector The message selection filter to set.
    */
   public void setMessageSelector(String sMessageSelector)
   {
      m_sMessageSelector = sMessageSelector;
   }

   /**
    * @return The message selection filter.
    */
   public String getMessageSelector()
   {
      return m_sMessageSelector;
   }

   /**
    * Sets the message destination type.
    * @param sDestinationType The message destination type to set.
    */
   public void setDestinationType(String sDestinationType)
   {
      m_sDestinationType = sDestinationType;
   }

   /**
    * @return The message destination type.
    */
   public String getDestinationType()
   {
      return m_sDestinationType;
   }

   /**
    * @return True if the destination is a topic.
    */
   public boolean isBroadcast()
   {
      return m_bBroadcast;
   }

   /**
    * Sets the connection factory name.
    * @param sConnectionFactory The connection factory name to set.
    */
   public void setConnectionFactory(String sConnectionFactory)
   {
      m_sConnectionFactory = sConnectionFactory;
   }

   /**
    * @return The connection factory name.
    */
   public String getConnectionFactory()
   {
      return m_sConnectionFactory;
   }

   /**
    * Sets the message destination name or bean URL.
    * @param sDestination The message destination name to set.
    */
   public void setDestination(String sDestination)
   {
      m_sDestination = sDestination;
   }

   /**
    * @return The message destination name.
    */
   public String getDestination()
   {
      return m_sDestination;
   }

   /**
    * Sets the durable subscription name.
    * @param sSubscriptionName The durable subscription name to set.
    */
   public void setSubscriptionName(String sSubscriptionName)
   {
      m_sSubscriptionName = sSubscriptionName;
   }

   /**
    * @return The durable subscription name.
    */
   public String getSubscriptionName()
   {
      return m_sSubscriptionName;
   }

   /**
    * Sets the connection client id.
    * @param sClientId The connection client id to set.
    */
   public void setClientId(String sClientId)
   {
      m_sClientId = sClientId;
   }

   /**
    * @return The connection client id.
    */
   public String getClientId()
   {
      return m_sClientId;
   }

   /**
    * Sets the user name.
    * @param sUser The user name to set.
    */
   public void setUser(String sUser)
   {
      m_sUser = sUser;
   }

   /**
    * @return The user name.
    */
   public String getUser()
   {
      return m_sUser;
   }

   /**
    * Sets the password.
    * @param sPassword The password to set.
    */
   public void setPassword(String sPassword)
   {
      CharacterStreamCipherDispatcher dec = new CharacterStreamCipherDispatcher();

      dec.init(SysUtil.getConfigProperties());
      m_sPassword = dec.decrypt(sPassword);
   }

   /**
    * @return The password.
    */
   public String getPassword()
   {
      return m_sPassword;
   }

   /**
    * Sets the connection factory properties.
    * @param sProperties The connection factory properties to set.
    */
   public void setConnectionFactoryProperties(String sProperties)
   {
      m_sConnectionFactoryProperties = sProperties;
   }

   /**
    * @return The connection factory properties.
    */
   public String getConnectionFactoryProperties()
   {
      return m_sConnectionFactoryProperties;
   }

   /**
    * @return The connection factory property map.
    */
   public Properties getConnectionFactoryPropertyMap()
   {
      return m_connectionFactoryProperties;
   }

   /**
    * Sets the destination properties.
    * @param sDestinationProperties The destination properties to set.
    */
   public void setDestinationProperties(String sDestinationProperties)
   {
      m_sDestinationProperties = sDestinationProperties;
   }

   /**
    * @return The destination properties.
    */
   public String getDestinationProperties()
   {
      return m_sDestinationProperties;
   }

   /**
    * @return The destination property map.
    */
   public Properties getDestinationPropertyMap()
   {
      return m_destinationProperties;
   }
   
   /**
    * Sets the selector update interval.
    * @param lSelectorUpdateInterval The selector update interval to set, in milliseconds.
    */
   public void setSelectorUpdateInterval(long lSelectorUpdateInterval)
   {
      m_lSelectorUpdateInterval = lSelectorUpdateInterval;
   }

   /**
    * @return The selector update interval in milliseconds.
    */
   public long getSelectorUpdateInterval()
   {
      return m_lSelectorUpdateInterval;
   }

   /**
    * @see javax.resource.spi.ActivationSpec#validate()
    */
   public void validate() throws InvalidPropertyException
   {
      if (m_sAckMode == null || m_sAckMode.equals("Auto-acknowledge"))
      {
         m_nAckMode = Session.AUTO_ACKNOWLEDGE;
      }
      else if (m_sAckMode.equals("Dups-ok-acknowledge"))
      {
         m_nAckMode = Session.DUPS_OK_ACKNOWLEDGE;
      }
      else
      {
         throw new InvalidPropertyException("Invalid acknowledgement mode \"" + m_sAckMode +
            "\" (expected \"Auto-acknowledge\" or \"Dups-ok-acknowledge\")");
      }

      if (m_sDurability == null || m_sDurability.equals("NonDurable"))
      {
         m_bDurable = false;
      }
      else if (m_sDurability.equals("Durable"))
      {
         m_bDurable = true;
      }
      else
      {
         throw new InvalidPropertyException("Invalid subscription durability \"" + m_sDurability +
            "\" (expected \"Durable\" or \"NonDurable\")");
      }

      if (m_sDestinationType == null || m_sDestinationType.equals("javax.jms.Queue"))
      {
         m_bBroadcast = false;

         if (m_bDurable)
         {
            throw new InvalidPropertyException("Destination type \"" +
               m_sDestinationType + "\" cannot be durable");
         }
      }
      else if (m_sDestinationType.equals("javax.jms.Topic"))
      {
         m_bBroadcast = true;
      }
      else
      {
         throw new InvalidPropertyException("Invalid destination type \"" + m_sDestinationType +
            "\" (expected \"javax.jms.Queue\" or \"javax.jms.Topic\")");
      }

      if (m_sDestination == null)
      {
         throw new InvalidPropertyException("Missing destination name");
      }

      if (m_sConnectionFactory == null)
      {
         throw new InvalidPropertyException("Missing connection factory name");
      }

      if (m_sConnectionFactoryProperties != null && m_sConnectionFactoryProperties.trim().length() != 0)
      {
         try
         {
            m_connectionFactoryProperties = PropertyUtil.fromString(m_sConnectionFactoryProperties);
         }
         catch (IOException e)
         {
            throw new InvalidPropertyException("Invalid connection factory properties syntax", e);
         }
      }
      else
      {
         m_connectionFactoryProperties = null;
      }

      if (m_sDestinationProperties != null && m_sDestinationProperties.trim().length() != 0)
      {
         try
         {
            m_destinationProperties = PropertyUtil.fromString(m_sDestinationProperties);
         }
         catch (IOException e)
         {
            throw new InvalidPropertyException("Invalid destination properties syntax", e);
         }
      }
      else
      {
         m_destinationProperties = null;
      }
      
      if (m_lSelectorUpdateInterval <= 0)
      {
         throw new InvalidPropertyException("Invalid message selector update value");
      }
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuilder buf = new StringBuilder(128);

      buf.append(super.toString()); 
      buf.append("(cf=");
      buf.append(m_sConnectionFactory);
      buf.append(", destination=");
      buf.append(m_sDestination);
      buf.append(", broadcast=");
      buf.append(m_bBroadcast);
      buf.append(", durable=");
      buf.append(m_bDurable);

      if (m_bDurable)
      {
         buf.append(", subscriptionName=");
         buf.append(m_sSubscriptionName);
         buf.append(", clientId=");
         buf.append(m_sClientId);
      }

      if (m_sAckMode != null)
      {
         buf.append(", ackMode=");
         buf.append(m_sAckMode);
      }

      buf.append(", maxPoolSize=");
      buf.append(m_nMaxPoolSize);
      
      buf.append(", selectorUpdateInterval=");
      buf.append(m_lSelectorUpdateInterval);

      if (m_sUser != null)
      {
         buf.append(", user=");
         buf.append(m_sUser);
      }

      if (m_sConnectionFactoryProperties != null)
      {
         buf.append(", connectionFactoryProperties={");
         buf.append(m_sConnectionFactoryProperties);
         buf.append('}');
      }

      if (m_sDestinationProperties != null)
      {
         buf.append(", destinationProperties={");
         buf.append(m_sDestinationProperties);
         buf.append('}');
      }

      buf.append(')');

      return buf.toString();
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumerConfig#update(nexj.core.rpc.ra.GenericConsumerConfig)
    */
   public void update(GenericConsumerConfig newConfig)
   {
      setMaxPoolSize(newConfig.getMaxPoolSize());
   }
}
