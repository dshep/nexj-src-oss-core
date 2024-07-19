package nexj.core.rpc.queueing.ra;

import javax.resource.ResourceException;
import javax.resource.spi.endpoint.MessageEndpointFactory;

/**
 * Default consumer pool for ObjectQueue adapter.
 */
public class DefaultObjectConsumerPool extends ObjectConsumerPool
{
   // constructors

   /**
    * Constructs the consumer pool.
    * @param adapter The resource adapter.
    * @param factory The endpoint factory.
    * @param config The pool configuration.
    * @throws ResourceException if an initialization error occurs.
    */
   public DefaultObjectConsumerPool(ObjectQueueResourceAdapter adapter, MessageEndpointFactory factory,
      ObjectConsumerConfig config) throws ResourceException
   {
      super(adapter, factory, config);
   }
}
