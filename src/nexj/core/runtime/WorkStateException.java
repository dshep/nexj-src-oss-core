// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

import nexj.core.util.UncheckedException;

/**
 * Exception indicating an out-of-order operation in the Unit-of-Work.
 */
public class WorkStateException extends UncheckedException
{
   // attributes
   
   /**
    * Serial version UID
    */
   private final static long serialVersionUID = 492540740681216684L;

   // constructors
   
   public WorkStateException(String sErrCode)
   {
      super(sErrCode);
   }

   public WorkStateException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public WorkStateException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public WorkStateException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
}
