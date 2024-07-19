// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.sql.Timestamp;
import java.text.FieldPosition;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.Locale;
import java.util.SimpleTimeZone;
import java.util.TimeZone;

/**
 * HL7 helper functions.
 */
public class HL7Util
{
   // associations

   /**
    * The DTM format.
    */
   protected final static SimpleDateFormat s_dtmFormat = new SimpleDateFormat("yyyyMMddHHmmss.SSSZ", Locale.ENGLISH);

   /**
    * The DT format.
    */
   protected final static SimpleDateFormat s_dtFormat = new SimpleDateFormat("yyyyMMdd", Locale.ENGLISH);

   /**
    * The TM format.
    */
   protected final static SimpleDateFormat s_tmFormat = new SimpleDateFormat("HHmmss.SSSZ", Locale.ENGLISH);

   static
   {
      s_dtmFormat.setTimeZone(TZ.UTC);
      s_dtFormat.setTimeZone(TZ.UTC);
      s_tmFormat.setTimeZone(TZ.UTC);
   }

   // constructors

   /**
    * Prevents construction.
    */
   protected HL7Util()
   {
   }

   /**
    * Parses a DTM field.
    * @param s The value to parse.
    * @param bDate True if the field must contain date.
    * @param bTime True if the field must contain time.
    * @param defTZ The default time zone. Can be null to use the system default.
    * @return The resulting time stamp.
    * @throws IllegalArgumentException if an error occurs.
    */
   public static Timestamp parseDateTime(String s, boolean bDate, boolean bTime, TimeZone defTZ)
   {
      TimeZone tz = (bTime) ? parseTimeZone(s) : null;

      if (tz == null)
      {
         tz = defTZ;

         if (tz == null)
         {
            tz = TimeZone.getDefault();
         }
      }
      else
      {
         s = s.substring(0, findTimeZone(s));
      }

      int n = s.length();

      GregorianCalendar cal = new GregorianCalendar(tz);

      cal.setLenient(false);
      cal.clear();

      if (n < ((bDate) ? 4 : 2) || n < ((bDate) ? (bTime) ? 14 : 8 : 6) && (n & 1) != 0)
      {
         throw new IllegalArgumentException("Incomplete date/time");
      }

      if (bDate)
      {
         cal.set(Calendar.YEAR, StringUtil.parseInt(s, 0, 4));

         if (n > 4)
         {
            cal.set(Calendar.MONTH, StringUtil.parseInt(s, 4, 6) - 1);

            if (n > 6)
            {
               cal.set(Calendar.DAY_OF_MONTH, StringUtil.parseInt(s, 6, 8));

               if (bTime)
               {
                  s = s.substring(8);
                  n -= 8;
               }
            }
         }
      }

      if (bTime)
      {
         if (n > 0)
         {
            cal.set(Calendar.HOUR_OF_DAY, StringUtil.parseInt(s, 0, 2));

            if (n > 2)
            {
               cal.set(Calendar.MINUTE, StringUtil.parseInt(s, 2, 4));

               if (n > 4)
               {
                  cal.set(Calendar.SECOND, StringUtil.parseInt(s, 4, 6));

                  if (n > 6)
                  {
                     if (s.charAt(6) != '.')
                     {
                        throw new IllegalArgumentException("Missing .");
                     }

                     if (n > 7)
                     {
                        if (n > 11)
                        {
                           throw new IllegalArgumentException("Too long time");
                        }

                        int m = StringUtil.parseInt(s, 7, n);

                        if (m < 0)
                        {
                           throw new IllegalArgumentException("Invalid fractional part");
                        }

                        for (n = 11 - n; n > 0; --n)
                        {
                           m *= 10;
                        }

                        cal.set(Calendar.MILLISECOND, (m + 5) / 10);
                     }
                  }
               }
            }
         }
      }

      return new Timestamp(cal.getTimeInMillis());
   }

   /**
    * Finds the time zone offset in a given time string.
    * @param s The time string.
    * @return The offset, or -1 if not found.
    */
   protected static int findTimeZone(String s)
   {
      return StringUtil.findSetIndex(s, "+-", s.length() - 5, s.length());
   }

   /**
    * Gets a time zone from a HL7 time string.
    * @param s The time string.
    * @return The time zone, or null if none found.
    */
   public static SimpleTimeZone parseTimeZone(String s)
   {
      int i = findTimeZone(s);

      if (i >= 0)
      {
         char ch = s.charAt(i);

         switch (ch)
         {
            case '+':
            case '-':
               int nOfs = StringUtil.parseInt(s, i + 1, s.length());
               int nHrs = nOfs / 100;
               int nMin = nOfs % 100;

               if (nHrs > 24 || nMin > 59)
               {
                  throw new IllegalArgumentException("Invalid time offset");
               }

               return new SimpleTimeZone(((ch == '-') ? -1 : 1) * (60 * nHrs + nMin) * 60000, s);
         }
      }

      return null;
   }

   /**
    * Formats a DTM value.
    * @param ts The value to format.
    * @return The formatted value.
    */
   public static String formatDateTime(Timestamp ts)
   {
      return ((SimpleDateFormat)s_dtmFormat.clone()).format(ts);
   }

   /**
    * Formats a DT value.
    * @param ts The value to format.
    * @return The formatted value.
    */
   public static String formatDate(Timestamp ts)
   {
      return ((SimpleDateFormat)s_dtFormat.clone()).format(ts);
   }

   /**
    * Formats a TM value.
    * @param ts The value to format.
    * @return The formatted value.
    */
   public static String formatTime(Timestamp ts)
   {
      return ((SimpleDateFormat)s_tmFormat.clone()).format(ts);
   }

   /**
    * Appends a DTM value to a string buffer.
    * @param buf The string buffer.
    * @param ts The value to append.
    */
   public static void appendDateTime(StringBuffer buf, Timestamp ts)
   {
      ((SimpleDateFormat)s_dtmFormat.clone()).format(ts, buf, new FieldPosition(SimpleDateFormat.DEFAULT));
   }

   /**
    * Appends a DT value to a string buffer.
    * @param buf The string buffer.
    * @param ts The value to append.
    */
   public static void appendDate(StringBuffer buf, Timestamp ts)
   {
      ((SimpleDateFormat)s_dtFormat.clone()).format(ts, buf, new FieldPosition(SimpleDateFormat.DEFAULT));
   }

   /**
    * Appends a TM value to a string buffer.
    * @param buf The string buffer.
    * @param ts The value to append.
    */
   public static void appendTime(StringBuffer buf, Timestamp ts)
   {
      ((SimpleDateFormat)s_tmFormat.clone()).format(ts, buf, new FieldPosition(SimpleDateFormat.DEFAULT));
   }
}
