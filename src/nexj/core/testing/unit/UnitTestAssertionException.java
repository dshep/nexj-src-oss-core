package nexj.core.testing.unit;

import nexj.core.util.UncheckedException;

/**
 * Exception thrown as part of unit testing framework
 */
public class UnitTestAssertionException extends UncheckedException
{
   // constants
   
   /**
    * Serialization version.
    */
   private final static long serialVersionUID = 7669035897465659409L;
   
   // constructors

   public UnitTestAssertionException(String sErrCode)
   {
      super(sErrCode);
   }

   public UnitTestAssertionException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public UnitTestAssertionException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public UnitTestAssertionException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
}
