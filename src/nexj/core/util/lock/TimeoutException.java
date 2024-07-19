// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util.lock;

import nexj.core.util.UncheckedException;

/**
 * Exception indicating that a resource has been locked for too long.
 */
public class TimeoutException extends UncheckedException
{
   // constants

   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = 9129398420510584179L;

   // constructors

   public TimeoutException()
   {
      super("err.lock.timeout");
   }

   // operations

   /**
    * @see nexj.core.util.ErrorCode#isSystem()
    */
   public boolean isSystem()
   {
      return false;
   }
}

