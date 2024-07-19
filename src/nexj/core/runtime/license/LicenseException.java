// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime.license;

import nexj.core.util.UncheckedException;

/**
 * License exception, thrown when license check fails.
 */
public class LicenseException extends UncheckedException
{
   /**
    * Serializetion UID.
    */
   private final static long serialVersionUID = 6526155112478206278L;

   public LicenseException(String sErrCode)
   {
      super(sErrCode);
   }

   public LicenseException(String sErrCode, Object[] argArray)
   {
      super(sErrCode, argArray);
   }

   public LicenseException(String sErrCode, Throwable cause)
   {
      super(sErrCode, cause);
   }

   public LicenseException(String sErrCode, Object[] argArray, Throwable cause)
   {
      super(sErrCode, argArray, cause);
   }
}
