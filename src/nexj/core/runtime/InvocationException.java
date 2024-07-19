// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

import nexj.core.util.UncheckedException;

/**
 * Method invocation exception.
 */
public class InvocationException extends UncheckedException
{
   // constants
   
   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = 5667573141654207733L;

   // constructors
   
   public InvocationException(String sErrCode)
   {
      super(sErrCode);
   }

   public InvocationException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public InvocationException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public InvocationException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
}
