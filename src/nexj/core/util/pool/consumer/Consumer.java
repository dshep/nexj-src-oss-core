package nexj.core.util.pool.consumer;

import javax.transaction.xa.XAResource;

/**
 * Request consumer.
 */
public interface Consumer extends Runnable
{
   /**
    * @return The consumer configuration.
    */
   ConsumerConfig getConfig();

   /**
    * @return The associated XA resource, or null if none.
    */
   XAResource getXAResource();

   /**
    * Runs the consumer logic, typically on a dedicated thread.
    */
   void run();

   /**
    * Puts the consumer back into the pool.
    */
   void deactivate();

   /**
    * Resets any consumer state so that it could be returned to the pool.
    */
   void reset();

   /**
    * Disposes of any resources allocated by the consumer.
    */
   void dispose();
}