// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration;

import nexj.core.util.UncheckedException;

/**
 * Exception thrown when an integration operation fails.
 */
public class IntegrationException extends UncheckedException
{
   // constants
   
   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = 6967482014267156771L;

   // constructors
   
   public IntegrationException(String sErrCode)
   {
      super(sErrCode);
   }

   public IntegrationException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public IntegrationException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public IntegrationException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
}
