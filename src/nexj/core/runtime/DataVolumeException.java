// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;


/**
 * Exception indicating that too much data is involved
 * in one transaction. 
 */
public class DataVolumeException extends ResourceLimitException
{
   // constants

   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = -1016105118923573333L;

   // constructors

   public DataVolumeException(String errCode, Object[] argArray, Throwable cause)
   {
      super(errCode, argArray, cause);
   }

   public DataVolumeException(String errCode, Object[] argArray)
   {
      super(errCode, argArray);
   }
}
