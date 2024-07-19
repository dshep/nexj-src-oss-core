// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.jms.ra.platform;

import java.lang.reflect.Method;

import javax.resource.spi.UnavailableException;
import javax.resource.spi.endpoint.MessageEndpoint;
import javax.resource.spi.endpoint.MessageEndpointFactory;
import javax.transaction.xa.XAResource;


/**
 * Message endpoint factory for message endpoints implementing java.jms.MessageListener interface.
 */
public class JMSPlatformMessageEndpointFactory implements MessageEndpointFactory
{
   // associations

   /**
    * The platform consumer pool.
    */
   protected JMSPlatformConsumerPool m_pool;

   // constructors

   /**
    * Constructs the factory.
    * @param pool The platform consumer pool.
    */
   public JMSPlatformMessageEndpointFactory(JMSPlatformConsumerPool pool)
   {
      m_pool = pool;
   }

   // operations

   /**
    * @see javax.resource.spi.endpoint.MessageEndpointFactory#createEndpoint(javax.transaction.xa.XAResource)
    */
   public MessageEndpoint createEndpoint(XAResource xar) throws UnavailableException
   {
      return new JMSPlatformMessageEndpoint(m_pool, m_pool.getFactory().createEndpoint(xar));
   }

   /**
    * @see javax.resource.spi.endpoint.MessageEndpointFactory#isDeliveryTransacted(java.lang.reflect.Method)
    */
   public boolean isDeliveryTransacted(Method method) throws NoSuchMethodException
   {
      return m_pool.isTransactional();
   }
}