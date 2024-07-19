// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

import nexj.core.util.UncheckedException;

/**
 * Application lifecycle exception.
 */
public class LifecycleException extends UncheckedException
{
   // constants

   /**
    * Serialization UID.
    */
   private static final long serialVersionUID = -5012235617568957901L;

   // constructors

   public LifecycleException(String sErrCode)
   {
      super(sErrCode);
   }

   public LifecycleException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public LifecycleException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }

   public LifecycleException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }
}
