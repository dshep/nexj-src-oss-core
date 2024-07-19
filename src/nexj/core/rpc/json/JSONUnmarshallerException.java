package nexj.core.rpc.json;

import nexj.core.rpc.UnmarshallerException;

public class JSONUnmarshallerException extends UnmarshallerException
{
   // constants

   /**
    * Serialization version.
    */
   private static final long serialVersionUID = 7977383118288060159L;

   // constructors

   public JSONUnmarshallerException(String sErrCode)
   {
      super(sErrCode);
   }

   public JSONUnmarshallerException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public JSONUnmarshallerException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public JSONUnmarshallerException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
}