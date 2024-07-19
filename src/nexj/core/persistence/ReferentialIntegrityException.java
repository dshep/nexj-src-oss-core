// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;

/**
 * Exception indicating a referential integrity error (bad data). 
 */
public class ReferentialIntegrityException extends LocatorPersistenceException
{
   // constants

   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = -7983907962336248029L;

   // constructors

   public ReferentialIntegrityException(String sErrCode)
   {
      super(sErrCode);
   }

   public ReferentialIntegrityException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public ReferentialIntegrityException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public ReferentialIntegrityException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
}
