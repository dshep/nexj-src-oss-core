package nexj.core.util.pool.consumer;

/**
 * Consumer pool configuration.
 */
public interface ConsumerConfig
{
   /**
    * @return The maximum number of active consumers running on separate threads.
    * -1 means unlimited.
    */
   int getMaxPoolSize();

   /**
    * @return The initial value of the pool retry delay in milliseconds.
    */
   long getRetryDelay();

   /**
    * @return The maximum value of the pool retry delay in milliseconds.
    */
   long getMaxRetryDelay();

   /**
    * @return True if the consumer is transactional.
    */
   boolean isTransactional();

   /**
    * @return The transaction timeout in seconds.
    * 0 means system default. Negative means infinite. 
    */
   int getTransactionTimeout();

   /**
    * Determines if the configuration is compatible with another one.
    * Updating a pool configuration to an incompatible one requires a pool restart.
    * @param config The other configuration object.
    * @return True if the configuration is compatible.
    * @see ConsumerPool#setConfig(ConsumerConfig)
    */
   boolean isCompatible(ConsumerConfig config);
}
