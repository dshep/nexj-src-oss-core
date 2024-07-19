// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.jms;

/**
 * Class to contain common JMS constants and functionality.
 */
public class JMS
{
   // constants
   
   /**
    * The error count property.
    */
   public final static String ERROR_COUNT = "jmsErrorCount";

   /**
    * The maximum error count message property.
    */
   public final static String MAX_ERROR_COUNT = "jmsMaxErrorCount";

   /**
    * The backoff delay in milliseconds message property.
    */
   public final static String BACKOFF_DELAY = "jmsBackoffDelay";

   /**
    * The maximum backoff delay in milliseconds message property.
    */
   public final static String MAX_BACKOFF_DELAY = "jmsMaxBackoffDelay";

   /**
    * The old message Id property.
    */
   public final static String OLD_MESSAGE_ID = "jmsOldMessageId";

   /**
    * The old message destination name property.
    */
   public final static String OLD_DESTINATION = "jmsOldDestination";
   
   /**
    * The delivery time property.
    */
   public final static String DELIVERY_TIME = "jmsDeliveryTime";
   
   /**
    * Message rejected flag.
    */
   public final static String MESSAGE_REJECTED = "jmsRejected";
   
   /**
    * Message rejected flag.
    */
   public final static String REDIRECT = "jmsRedirect";

   /**
    * The delivery count standard message property.
    */
   public final static String JMS_DELIVERY_COUNT = "JMSXDeliveryCount";

   /**
    * The JBoss MQ redelivery count message property.
    */
   public final static String JBOSS_REDELIVERY_COUNT = "JMS_JBOSS_REDELIVERY_COUNT";

   /**
    * The JBoss scheduled delivery time message property.
    */
   public final static String JBOSS_SCHEDULED_DELIVERY = "JMS_JBOSS_SCHEDULED_DELIVERY";
   
   /**
    * The ActiveMQ broker in time message property.
    */
   public final static String ACTIVEMQ_BROKER_IN_TIME = "JMSActiveMQBrokerInTime";
   
   /**
    * The ActiveMQ scheduled delay property.
    */
   public final static String ACTIVEMQ_SCHEDULED_DELAY = "AMQ_SCHEDULED_DELAY";
}