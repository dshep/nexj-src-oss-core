package nexj.core.util.pool.resource;

import nexj.core.util.UncheckedException;

/**
 * Resource pooling exception.
 */
public class ResourceException extends UncheckedException
{
   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = -8656252708348712054L;

   public ResourceException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }

   public ResourceException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public ResourceException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public ResourceException(String sErrCode)
   {
      super(sErrCode);
   }
}
