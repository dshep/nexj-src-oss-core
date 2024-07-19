// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.file.ra;

import nexj.core.util.UncheckedException;

/**
 * Exception thrown when an error occurs while performing an operation
 * on a transactional journal.
 */
public class JournalManagementException extends UncheckedException
{
   // constants

   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = -6152592086223405305L;


   // constructors

   public JournalManagementException(String sErrCode)
   {
      super(sErrCode);
   }

   public JournalManagementException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public JournalManagementException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public JournalManagementException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
}
