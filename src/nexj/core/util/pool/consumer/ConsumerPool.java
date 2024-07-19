package nexj.core.util.pool.consumer;

/**
 * Consumer pool.
 */
public interface ConsumerPool
{
   /**
    * @return The consumer adapter.
    */
   ConsumerAdapter getAdapter();

   /**
    * Sets a new consumer pool configuration.
    * Updating a pool configuration to an incompatible one requires a pool restart.
    * @param config The consumer configuration to set. Immutable.
    * @see ConsumerConfig#isCompatible(ConsumerConfig)
    */
   void setConfig(ConsumerConfig config) throws ConsumerException;

   /**
    * @return The current consumer configuration. Immutable.
    */
   ConsumerConfig getConfig();

   /**
    * Starts receiving requests.
    */
   void start() throws ConsumerException;

   /**
    * Stops receiving requests.
    * @param bWait True to block until all the consumers
    * have finished processing the requests.
    */
   void stop(boolean bWait);

   /**
    * Invoked periodically by a system timer to maintain the pool,
    * including idle consumer removal.
    */
   void maintain();
}
