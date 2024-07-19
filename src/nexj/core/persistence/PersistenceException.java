// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;

import nexj.core.util.UncheckedException;

/**
 * Exception indicating a problem in the persistence layer.
 */
public class PersistenceException extends UncheckedException
{
   // constants
   
   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = 1337083422002767838L;

   // constructors
   
   public PersistenceException(String sErrCode)
   {
      super(sErrCode);
   }

   public PersistenceException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public PersistenceException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public PersistenceException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
}
