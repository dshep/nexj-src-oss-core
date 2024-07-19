package nexj.core.rpc.json;

import nexj.core.rpc.MarshallerException;

/**
 * Exception thrown when a JSON marshalling error occurs.
 */
public class JSONMarshallerException extends MarshallerException
{
   // constants

   /**
    * Serialization version.
    */
   private final static long serialVersionUID = 7195623810526920508L;

   // constructors

   public JSONMarshallerException(String sErrCode)
   {
      super(sErrCode);
   }

   public JSONMarshallerException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public JSONMarshallerException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public JSONMarshallerException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
}