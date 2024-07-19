package nexj.core.util.pool.consumer;

/**
 * Exception indicating that a work item could not be run on another thread.
 */
public class RunException extends ConsumerException
{
   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = -7083084527081555906L;

   public RunException(Throwable cause)
   {
      super("err.pool.consumer.run", cause);
   }
}
