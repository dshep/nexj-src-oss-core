package nexj.core.util.pool.consumer;

import nexj.core.util.UncheckedException;

/**
 * Consumer pooling exception.
 */
public class ConsumerException extends UncheckedException
{
   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = -8469617544298111741L;

   public ConsumerException(String sErrCode)
   {
      super(sErrCode);
   }

   public ConsumerException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public ConsumerException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public ConsumerException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
}
