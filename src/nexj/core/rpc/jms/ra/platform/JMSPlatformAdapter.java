// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.jms.ra.platform;

import javax.jms.Message;
import javax.resource.spi.ActivationSpec;
import javax.resource.spi.ResourceAdapter;

import nexj.core.rpc.jms.ra.JMSConsumerConfig;

/**
 * Interface implemented by platform-specific RA adapters.
 */
public interface JMSPlatformAdapter
{
   /**
    * Creates the platform resource adapter.
    * @param The consumer config.
    * @return The platform resource adapter.
    */
   ResourceAdapter createResourceAdapter(JMSConsumerConfig config) throws Throwable;

   /**
    * Converts a consumer config into a platform-specific activation spec.
    * @param config The consumer config to convert.
    * @return The platform-specific activation spec.
    */
   ActivationSpec createActivationSpec(JMSConsumerConfig config) throws Throwable;

   /**
    * Sets the message selector on the activation spec
    * @param activationSpec The activation spec.
    * @param sSelector The message selector to set.
    */
   void setMessageSelector(ActivationSpec activationSpec, String sSelector) throws Throwable;

   /**
    * Performs any required platform-specific tasks prior to calling beforeDelivery on the endpoint
    * @param endpoint The platform message endpoint.
    */
   void beforeDelivery(JMSPlatformMessageEndpoint endpoint);

   /**
    * Performs any required platform-specific tasks required after calling afterDelivery on the endpoint
    * @param endpoint The platform message endpoint.
    */
   void afterDelivery(JMSPlatformMessageEndpoint endpoint);

   /**
    * Performs any required platform-specific tasks required prior to stopping the consumer pool.
    * @param pool The platform consumer pool.
    */
   void beforeStop(JMSPlatformConsumerPool pool);

   /**
    * Performs any required platform-specific tasks after stopping the consumer pool.
    * @param pool The platform consumer pool.
    */
   void afterStop(JMSPlatformConsumerPool pool);
   
   /**
    * This method can be used to perform any JMS engine provider specific
    * transformation required on a message before processing.
    * 
    * @param message The message to perform transformations on.
    * @return The transformed message.
    * @throws Throwable
    */
   Message prepareMessage(Message message);
}
