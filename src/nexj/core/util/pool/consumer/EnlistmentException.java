package nexj.core.util.pool.consumer;

/**
 * Exception indicating a consumer XA resource enlistment failure.
 */
public class EnlistmentException extends ConsumerException
{
   /**
    * Serialization UID.
    */
   private static final long serialVersionUID = 532701342942059677L;

   public EnlistmentException()
   {
      super("err.pool.consumer.tx.enlistment");
   }

   public EnlistmentException(String sErrCode)
   {
      super(sErrCode);
   }

   public EnlistmentException(Throwable cause)
   {
      super("err.pool.consumer.tx.enlistment", cause);
   }
}
