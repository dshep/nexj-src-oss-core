package nexj.core.util.pool.resource;

/**
 * Exception thrown when a new resource cannot be created.
 */
public class ResourceFactoryException extends ResourceException
{
   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = 8654649983554198305L;

   public ResourceFactoryException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }

   public ResourceFactoryException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public ResourceFactoryException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public ResourceFactoryException(String sErrCode)
   {
      super(sErrCode);
   }
}
