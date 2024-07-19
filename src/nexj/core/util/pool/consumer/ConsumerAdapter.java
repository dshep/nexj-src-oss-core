package nexj.core.util.pool.consumer;

import javax.transaction.TransactionManager;

/**
 * Run-time environment adapter for consumer pooling.
 */
public interface ConsumerAdapter
{
   /**
    * @return The transaction manager.
    */
   TransactionManager getTransactionManager();

   /**
    * Runs a given object on a dedicated thread,
    * which can block indefinitely.
    * @param runnable The object to run.
    */
   void run(Runnable runnable) throws Throwable;

   /**
    * Runs a given consumer on a separate pooled thread,
    * which is not supposed to block indefinitely.
    * @param consumer The consumer to run.
    * @see Consumer#run()
    */
   void run(Consumer consumer) throws Throwable;

   /**
    * Obtains an interceptor object for a given consumer.
    * It can be used for managing per-thread settings during request processing.
    * @param consumer The consumer.
    * @return The interceptor.
    */
   Object getInterceptor(Consumer consumer) throws Throwable;

   /**
    * Prepares the interceptor for processing the request.
    * @param consumer The consumer.
    * @param interceptor The interceptor to prepare.
    */
   void begin(Consumer consumer, Object interceptor) throws Throwable;

   /**
    * Processes the request.
    * @param consumer The consumer.
    * @param interceptor The interceptor to use.
    * @param request The request to process.
    */
   void process(Consumer consumer, Object interceptor, Object request) throws Throwable;

   /**
    * Cleans up the interceptor after the request has been processed,
    * regardless of whether processing has succeeded.
    * @param interceptor The interceptor to prepare.
    */
   void end(Object interceptor) throws Throwable;

   /**
    * Releases a previously obtained interceptor.
    * @param interceptor The interceptor to release.
    */
   void releaseInterceptor(Object interceptor) throws Throwable;

   /**
    * Disposes of all allocated resources.
    */
   void dispose();

   /**
    * Invoked periodically by a system timer to maintain the pool,
    * including idle consumer removal.
    */
   void maintain();
}
