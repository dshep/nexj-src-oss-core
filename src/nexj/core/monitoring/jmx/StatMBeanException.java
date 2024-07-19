package nexj.core.monitoring.jmx;

import nexj.core.util.UncheckedException;

/**
 * Statistics exception.
 */
public class StatMBeanException extends UncheckedException
{
   // constants

   /**
    * Version id.
    */
   private final static long serialVersionUID = -6355011035131617503L;

   // constructors

   /**
    * Construct a statistics exception.
    * @param sErrCode Error code.
    */
   public StatMBeanException(String sErrCode)
   {
      super(sErrCode);
   }

   /**
    * Construct a statistics exception.
    * @param sErrCode Error code.
    * @param argArray Arguments.
    */
   public StatMBeanException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   /**
    * Construct a statistics exception.
    * @param sErrCode Error code.
    * @param cause Cause.
    */
   public StatMBeanException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   /**
    * Construct a statistics exception.
    * @param sErrCode Error code.
    * @param argArray Arguments.
    * @param cause Cause.
    */
   public StatMBeanException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
}
