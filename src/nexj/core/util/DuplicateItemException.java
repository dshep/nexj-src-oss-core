// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

/**
 * Exception thrown when a duplicate item is added to a collection.
 */
public class DuplicateItemException extends UncheckedException
{
   // constants
   
   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = 8947857793548408897L;

   // constructors
   
   public DuplicateItemException(String sErrCode)
   {
      super(sErrCode);
   }

   public DuplicateItemException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public DuplicateItemException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public DuplicateItemException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
}
