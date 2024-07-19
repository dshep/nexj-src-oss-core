// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

import nexj.core.util.UncheckedException;

/**
 * Exception thrown when an arbitrary resource limit has been exceeded.
 * Arbitrary limits are used to restrict the impact of programming errors
 * on the runtime environment. 
 */
public class ResourceLimitException extends UncheckedException
{
   // constants
   
   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = -7934606716858939750L;

   // constructors

   public ResourceLimitException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }

   public ResourceLimitException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }
}
