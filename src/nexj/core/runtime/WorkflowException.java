// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

import nexj.core.util.UncheckedException;

/**
 * Flow management exception.
 */
public class WorkflowException extends UncheckedException
{
   // constants

   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = 1810088765619028601L;

   // constructors
   
   public WorkflowException(String sErrCode)
   {
      super(sErrCode);
   }

   public WorkflowException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public WorkflowException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public WorkflowException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
}
