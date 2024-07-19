package nexj.core.util.pool.resource;

/**
 * Exception indicating a resource enlistment failure.
 */
public class EnlistmentException extends ResourceException
{
   /**
    * Serialization UID.
    */
   private static final long serialVersionUID = -1540593158861867434L;

   public EnlistmentException()
   {
      super("err.pool.resource.tx.enlistment");
   }

   public EnlistmentException(String sErrCode)
   {
      super(sErrCode);
   }

   public EnlistmentException(Throwable cause)
   {
      super("err.pool.resource.tx.enlistment", cause);
   }
}
