// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;

/**
 * Base class for persistence exceptions caused by constraint violations in the persistent store
 */
public class ConstraintViolationException extends LocatorPersistenceException
{
   // constants

   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = -1700449225424419026L;

   // constructors

   public ConstraintViolationException(String sErrCode)
   {
      super(sErrCode);
   }

   public ConstraintViolationException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public ConstraintViolationException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public ConstraintViolationException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
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
