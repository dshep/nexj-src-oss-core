// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

/**
 * Exception thrown when an item cannot be found in a map.
 */
public class LookupException extends UncheckedException
{
   // constants
   
   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = 5794697525969248145L;

   // constructors
   
   public LookupException(String sErrCode)
   {
      super(sErrCode);
   }

   public LookupException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public LookupException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public LookupException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
}
