// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;

/**
 * Exception indicating that a value does not fit within the allowed range of values.
 */
public class ValueRangeException extends LocatorPersistenceException
{
   // constants

   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = -655693198866342425L;

   // constructors

   public ValueRangeException(String errCode)
   {
      super(errCode);
   }

   public ValueRangeException(String errCode, Object[] argArray)
   {
      super(errCode, argArray);
   }

   public ValueRangeException(String errCode, Throwable cause)
   {
      super(errCode, cause);
   }

   public ValueRangeException(String errCode, Object[] argArray, Throwable cause)
   {
      super(errCode, argArray, cause);
   }
}
