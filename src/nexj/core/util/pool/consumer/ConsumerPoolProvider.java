package nexj.core.util.pool.consumer;

/**
 * Interface implemented by objects owning a consumer pool.
 */
public interface ConsumerPoolProvider
{
   /**
    * Creates a new instance of a consumer pool specific to the object.
    * @param adapter The consumer adapter.
    * @return The consumer pool.
    */
   ConsumerPool createConsumerPool(ConsumerAdapter adapter);

   /**
    * Associates a consumer pool with the object.
    * @param pool The pool to associate. Can be null.
    */
   void setConsumerPool(ConsumerPool pool);

   /**
    * @return The associated consumer pool. Can be null.
    */
   ConsumerPool getConsumerPool();
}
