// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

/**
 * Exception indicating that too many iterations have been applied in an algorithm. 
 */
public class IterationCountException extends ResourceLimitException
{
   // constants

   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = -127569529067247459L;

   // constructors

   public IterationCountException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }

   public IterationCountException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }
}
