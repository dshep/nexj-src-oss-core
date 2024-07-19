// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime.license;


/**
 * License validation class.
 */
public class License
{
   // attributes

   /**
    * Start date for licensing.
    */
   private final static long s_lStartDate = Long.MIN_VALUE; // Primitive.toTimestamp("2006-10-01 00:00:00").getTime();

   /**
    * End date for licensing.
    */
   private final static long s_lEndDate = Long.MAX_VALUE; // Primitive.toTimestamp("2006-12-15 00:00:00").getTime();

   // constructors

   /**
    * Prevents construction.
    */
   private License()
   {
   }

   // operations

   /**
    * Checks the licensing at a given instant of time.
    * @param lTimestamp Timestamp in msec since 1-Jan-1970.
    * @throws LicenseException if the code is not licensed.
    */
   public static void check(long lTimestamp) throws LicenseException
   {
      if (lTimestamp < s_lStartDate || lTimestamp > s_lEndDate)
      {
         throw new LicenseException("err.license.expired");
      }
   }
}
