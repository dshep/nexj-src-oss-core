// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;

/**
 * Exception thrown when an invalid persistence query is encountered.  
 */
public class InvalidQueryException extends PersistenceException
{
   // constants
   
   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = 6045911920100796046L;

   // constructors

   public InvalidQueryException(String sErrCode)
   {
      super(sErrCode);
   }

   public InvalidQueryException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public InvalidQueryException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public InvalidQueryException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
}
