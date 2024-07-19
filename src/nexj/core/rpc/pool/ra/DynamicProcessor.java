package nexj.core.rpc.pool.ra;

import nexj.core.util.pool.consumer.ConsumerConfig;

/**
 * Processor for transactional and non-transactional methods.
 */
public interface DynamicProcessor
{
   /**
    * Processes the request without a transaction.
    * @param request The request object.
    * @param config The consumer configuration.
    */
   void process(Object request, ConsumerConfig config) throws Throwable;
}
