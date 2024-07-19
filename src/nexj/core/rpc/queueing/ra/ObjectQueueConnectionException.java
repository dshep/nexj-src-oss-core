package nexj.core.rpc.queueing.ra;

import nexj.core.rpc.RPCException;

/**
 * Exception raised if a failure occurs in a ObjectQueueConnection.
 */
public class ObjectQueueConnectionException extends RPCException
{
   // constants

   /**
    * Serialization version.
    */
   private final static long serialVersionUID = -4356632960550820445L;

   // constructors

   public ObjectQueueConnectionException(String sErrCode)
   {
      super(sErrCode);
   }

   public ObjectQueueConnectionException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public ObjectQueueConnectionException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public ObjectQueueConnectionException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }

}
