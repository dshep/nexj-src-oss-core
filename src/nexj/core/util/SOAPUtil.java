// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.sql.Timestamp;
import java.text.DateFormat;
import java.text.DateFormatSymbols;
import java.text.FieldPosition;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Locale;
import java.util.SimpleTimeZone;
import java.util.TimeZone;

/**
 * SOAP helper functions.
 */
public class SOAPUtil
{
   // associations

   /**
    * The datetime format, in UTC.
    */
   protected final static SimpleDateFormat s_dateTimeFormat = new SimpleDateFormat("Gyyyy'-'MM'-'dd'T'HH':'mm':'ss.SSS'Z'", Locale.ENGLISH);
   
   /**
    * The local datetime format, with offset from UTC.
    */
   protected final static SimpleDateFormat s_dateTimeWithOffsetFormat = new SimpleDateFormat("Gyyyy'-'MM'-'dd'T'HH':'mm':'ss.SSSZ", Locale.ENGLISH);
   
   /**
    * The local datetime format, without offset from UTC.
    */
   protected final static SimpleDateFormat s_dateTimeWithoutOffsetFormat = new SimpleDateFormat("Gyyyy'-'MM'-'dd'T'HH':'mm':'ss.SSS", Locale.ENGLISH);

   /**
    * The date format, in UTC.
    */
   protected final static SimpleDateFormat s_dateFormat = new SimpleDateFormat("Gyyyy'-'MM'-'dd'Z'", Locale.ENGLISH);

   /**
    * The local date format, with offset from UTC.
    */
   protected final static SimpleDateFormat s_dateWithOffsetFormat = new SimpleDateFormat("Gyyyy'-'MM'-'ddZ", Locale.ENGLISH);

   /**
    * The local date format, without offset from UTC.
    */
   protected final static SimpleDateFormat s_dateWithoutOffsetFormat = new SimpleDateFormat("Gyyyy'-'MM'-'dd", Locale.ENGLISH);

   /**
    * The time format, in UTC.
    */
   protected final static SimpleDateFormat s_timeFormat = new SimpleDateFormat("HH':'mm':'ss.SSS'Z'", Locale.ENGLISH);
   
   /**
    * The local time format, with offset from UTC.
    */
   protected final static SimpleDateFormat s_timeWithOffsetFormat = new SimpleDateFormat("HH':'mm':'ss.SSSZ", Locale.ENGLISH);

   /**
    * The local time format, without offset from UTC.
    */
   protected final static SimpleDateFormat s_timeWithoutOffsetFormat = new SimpleDateFormat("HH':'mm':'ss.SSS", Locale.ENGLISH);

   static
   {
      s_dateTimeFormat.setTimeZone(TZ.UTC);
      s_dateFormat.setTimeZone(TZ.UTC);
      s_timeFormat.setTimeZone(TZ.UTC);

      DateFormatSymbols dfs = s_dateTimeFormat.getDateFormatSymbols();

      dfs.setEras(new String[]{"-", ""});
      s_dateTimeFormat.setDateFormatSymbols(dfs);
      
      dfs = s_dateFormat.getDateFormatSymbols();
      dfs.setEras(new String[]{"-", ""});
      s_dateFormat.setDateFormatSymbols(dfs);
      
      dfs = s_dateWithOffsetFormat.getDateFormatSymbols();
      dfs.setEras(new String[]{"-", ""});
      s_dateWithOffsetFormat.setDateFormatSymbols(dfs);
      
      dfs = s_dateTimeWithOffsetFormat.getDateFormatSymbols();
      dfs.setEras(new String[]{"-", ""});
      s_dateTimeWithOffsetFormat.setDateFormatSymbols(dfs);
      
      dfs = s_dateWithoutOffsetFormat.getDateFormatSymbols();
      dfs.setEras(new String[]{"-", ""});
      s_dateWithoutOffsetFormat.setDateFormatSymbols(dfs);
      
      dfs = s_dateTimeWithoutOffsetFormat.getDateFormatSymbols();
      dfs.setEras(new String[]{"-", ""});
      s_dateTimeWithoutOffsetFormat.setDateFormatSymbols(dfs);
   }

   // constructors

   /**
    * Prevents construction.
    */
   protected SOAPUtil()
   {
   }

   // operations

   /**
    * Parses a dateTime value.
    * @param s The value to parse.
    * @param bDate True if the field must contain date.
    * @param bTime True if the field must contain time.
    * @param tz The default time zone. Null to use the default.
    * @return The resulting time stamp.
    * @throws IllegalArgumentException if the syntax is incorrect.
    */
   public static Timestamp parseDateTime(String s, boolean bDate, boolean bTime, TimeZone tz)
   {
      boolean bBC = false;
      int nYear = 0;
      int nMonth = 0;
      int nDay = 0;
      int nHour = 0;
      int nMinute = 0;
      int nSecond = 0;
      int nMillis = 0;
      int n = s.length();
      int i = 0;
      int j;
      
      if (bDate)
      {
         if (n > 0 && s.charAt(i) == '-')
         {
            ++i;
         }
         
         j = s.indexOf('-', i);
         
         if (j < 0)
         {
            throw new IllegalArgumentException("Missing date separators");
         }
         
         nYear = StringUtil.parseInt(s, i, j);
         
         if (nYear == 0)
         {
            throw new IllegalArgumentException("The year cannot be 0");
         }
         
         if (i > 0)
         {
            bBC = true;
         }
         
         if (n - j < 6)
         {
            throw new IllegalArgumentException("Too short date field");
         }
         
         StringUtil.verifyDelimiter(s, j + 3, '-');
         nMonth = StringUtil.parseInt(s, j + 1, j + 3);
         nDay = StringUtil.parseInt(s, j + 4, j + 6);
         i = j + 6;
         
         if (bTime)
         {
            StringUtil.verifyDelimiter(s, i++, 'T');
         }
      }
      
      if (bTime)
      {
         if (n - i < 8)
         {
            throw new IllegalArgumentException("Too short time field");
         }
         
         StringUtil.verifyDelimiter(s, i + 2, ':');
         StringUtil.verifyDelimiter(s, i + 5, ':');
         nHour = StringUtil.parseInt(s, i, i + 2);
         nMinute = StringUtil.parseInt(s, i + 3, i + 5);
         nSecond = StringUtil.parseInt(s, i + 6, i + 8);
         i += 8;

         if (i < n && s.charAt(i) == '.')
         {
            double d = 0;
            int k;

            ++i;

            for (j = i; j < n; ++j)
            {
               k = Character.digit(s.charAt(j), 10);
               
               if (k < 0)
               {
                  break;
               }

               d = d * 10 + k;
            }

            k = j - i;
            i = j;
            
            while (k < 3)
            {
               d *= 10;
               ++k;
            }
            
            while (k > 3)
            {
               d /= 10;
               --k;
            }

            nMillis = (int)(d + 0.5);
         }
      }
      
      if (i < n)
      {
         int nHrsOfs = 0;
         int nMntOfs = 0;
         int nTZSign = 1;

         switch (s.charAt(i))
         {
            case 'Z':
               ++i;
               break;
               
            case '-':
               nTZSign = -1;
               
            case '+':
               ++i;
               
               if (n - i < 5)
               {
                  throw new IllegalArgumentException("Too short time zone field");
               }
               
               StringUtil.verifyDelimiter(s, i + 2, ':');
               nHrsOfs = StringUtil.parseInt(s, i, i + 2);
               nMntOfs = StringUtil.parseInt(s, i + 3, i + 5);
               
               if (nHrsOfs > 24 || nMntOfs > 59)
               {
                  throw new IllegalArgumentException("Invalid time zone offset");
               }
               
               i += 5;
               
               break;
         }
         
         tz = new SimpleTimeZone(nTZSign * (60 * nHrsOfs + nMntOfs) * 60000, "SOAP");
      }
      else if (tz == null)
      {
         tz = TimeZone.getDefault();
      }

      if (i != n)
      {
         throw new IllegalArgumentException("Extra characters found");
      }

      GregorianCalendar cal = new GregorianCalendar(tz, Locale.ENGLISH);

      cal.setLenient(false);
      cal.clear();

      if (bDate)
      {
         cal.set(Calendar.ERA, (bBC) ? GregorianCalendar.BC : GregorianCalendar.AD);
         cal.set(Calendar.YEAR, nYear);
         cal.set(Calendar.MONTH, nMonth - 1);
         cal.set(Calendar.DAY_OF_MONTH, nDay);
      }

      if (bTime)
      {
         cal.set(Calendar.HOUR_OF_DAY, nHour);
         cal.set(Calendar.MINUTE, nMinute);
         cal.set(Calendar.SECOND, nSecond);
         cal.add(Calendar.MILLISECOND, nMillis);
      }

      return new Timestamp(cal.getTimeInMillis());
   }

   /**
    * Formats a datetime value.
    * @param dt The date value.
    * @return The formatted value.
    */
   public static String formatDateTime(Date dt)
   {
      return ((SimpleDateFormat)s_dateTimeFormat.clone()).format(dt);
   }

   /**
    * Formats a date value.
    * @param dt The date value.
    * @return The formatted value.
    */
   public static String formatDate(Date dt)
   {
      return ((SimpleDateFormat)s_dateFormat.clone()).format(dt);
   }

   /**
    * Formats a time value.
    * @param dt The date value.
    * @return The formatted value.
    */
   public static String formatTime(Date dt)
   {
      return ((SimpleDateFormat)s_timeFormat.clone()).format(dt);
   }
   
   /**
    * Formats a datetime value in a local time zone, appending an offset
    * of the form (+/-)hh:mm.
    * @param dt The date value.
    * @param tz The local time zone.
    * @return The formatted value.
    */
   public static String formatDateTimeWithOffset(Date dt, TimeZone tz)
   {
      return formatLocal(dt, tz, s_dateTimeWithOffsetFormat, true);
   }

   /**
    * Formats a date value in a local time zone, appending an offset
    * of the form (+/-)hh:mm.
    * @param dt The date value.
    * @param tz The local time zone.
    * @return The formatted value.
    */
   public static String formatDateWithOffset(Date dt, TimeZone tz)
   {
      return formatLocal(dt, tz, s_dateWithOffsetFormat, true);
   }

   /**
    * Formats a time value in a local time zone, appending an offset
    * of the form (+/-)hh:mm.
    * @param dt The date value.
    * @param tz The local time zone.
    * @return The formatted value.
    */
   public static String formatTimeWithOffset(Date dt, TimeZone tz)
   {
      return formatLocal(dt, tz, s_timeWithOffsetFormat, true);
   }
   
   /**
    * Formats a datetime value in a local time zone, without appending an offset.
    * @param dt The date value.
    * @param tz The local time zone.
    * @return The formatted value.
    */
   public static String formatDateTimeWithoutOffset(Date dt, TimeZone tz)
   {
      return formatLocal(dt, tz, s_dateTimeWithoutOffsetFormat, false);
   }

   /**
    * Formats a date value in a local time zone, without appending an offset.
    * @param dt The date value.
    * @param tz The local time zone.
    * @return The formatted value.
    */
   public static String formatDateWithoutOffset(Date dt, TimeZone tz)
   {
      return formatLocal(dt, tz, s_dateWithoutOffsetFormat, false);
   }

   /**
    * Formats a time value in a local time zone, without appending an offset.
    * @param dt The date value.
    * @param tz The local time zone.
    * @return The formatted value.
    */
   public static String formatTimeWithoutOffset(Date dt, TimeZone tz)
   {
      return formatLocal(dt, tz, s_timeWithoutOffsetFormat, false);
   }
   
   /**
    * Formats a time value in a local time zone, optionally appending an offset
    * of the form (+/-)hh:mm.
    * @param dt The date value.
    * @param tz The local time zone.
    * @param fmt The format to use.
    * @param bIncludesOffset True if fmt includes an offset from UTC.
    * @return The formated value.
    */
   protected static String formatLocal(Date dt, TimeZone tz, SimpleDateFormat fmt, boolean bIncludesOffset)
   {
      SimpleDateFormat format = (SimpleDateFormat)fmt.clone();
      
      format.setTimeZone(tz);
      
      if (bIncludesOffset)
      {
         StringBuffer buffer = new StringBuffer(20);
         FieldPosition field = new FieldPosition(DateFormat.TIMEZONE_FIELD);
         
         format.format(dt, buffer, field);
         buffer.insert(field.getEndIndex() - 2, ':');
         
         return buffer.toString();
      }
      else
      {
         return format.format(dt);
      }
   }
}
