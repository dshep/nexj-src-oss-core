// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

import nexj.core.util.UncheckedException;

/**
 * Exception indicating a transaction management failure.
 */
public class TransactionException extends UncheckedException
{
   // constants
   
   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = -7311616406739783030L;

   // constructors
   
   public TransactionException(String sErrCode)
   {
      super(sErrCode);
   }

   public TransactionException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public TransactionException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public TransactionException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
}
