// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.Locale;

/**
 * ISO 8601 date formatting and parsing helper functions.
 */
public class ISO8601Util
{
   // associations

   /**
    * The ISO 8601 date time format, basic format.
    */
   protected final static SimpleDateFormat s_dtmFormat = new SimpleDateFormat("yyyyMMdd'T'HHmmss'Z'", Locale.ENGLISH);

   /**
    * The ISO 8601 date format, basic format.
    */
   protected final static SimpleDateFormat s_dtFormat = new SimpleDateFormat("yyyyMMdd", Locale.ENGLISH);

   static
   {
      s_dtmFormat.setTimeZone(TZ.UTC);
      s_dtFormat.setTimeZone(TZ.UTC);
   }


   // operations

   /**
    * Formats an ISO 8601 date time.
    * 
    * @param ts The value to format.
    * @return The formatted value.
    */
   public static String formatDateTime(Timestamp ts)
   {
      return ((SimpleDateFormat)s_dtmFormat.clone()).format(ts);
   }

   /**
    * Formats an ISO 8601 date.
    * 
    * @param ts The value to format.
    * @return The formatted value.
    */
   public static String formatDate(Timestamp ts)
   {
      return ((SimpleDateFormat)s_dtFormat.clone()).format(ts);
   }
}
