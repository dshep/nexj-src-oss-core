// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import nexj.core.util.UncheckedException;

/**
 * Exception indicating a component instantiation error.
 */
public class ComponentException extends UncheckedException
{
   // constants
   
   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = 9190824178857027295L;

   // constructors
   
   public ComponentException(String sErrCode)
   {
      super(sErrCode);
   }

   public ComponentException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public ComponentException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public ComponentException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
}
