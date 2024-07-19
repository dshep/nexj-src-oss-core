// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.util.Calendar;
import java.util.Locale;
import java.util.SimpleTimeZone;
import java.util.TimeZone;
import java.util.Date;

/**
 * Structure representing a timezone from an external system (not a java.util.TimeZone)
 */
public class ExternalTimeZone extends BinaryUtil
{
   // constants

   /** 
    * Current year used to validate daylight change times
    */
   private final static int CURRENT_YEAR = Calendar.getInstance().get(Calendar.YEAR);

   /**
    * Mapping of binary represented day of week to Java.util.Calendar day of week.
    */
   private final static int[] DAYS_OF_WEEK =
   {
      Calendar.SUNDAY,
      Calendar.MONDAY,
      Calendar.TUESDAY,
      Calendar.WEDNESDAY,
      Calendar.THURSDAY,
      Calendar.FRIDAY,
      Calendar.SATURDAY,
   };
   
   /**
    * Fake ID assigned to generated time zones
    */
   private final static String GENERATED_TIMEZONE_ID = "Generated from external";

   // associations

   /**
    * STANDARD part of the time zone
    */
   private final ExternalTimeZone.TimeZonePart m_standard;

   /**
    * DAYLIGHT part of the time zone
    */
   private final ExternalTimeZone.TimeZonePart m_daylight;

   // constructors

   /**
    * Constructs ExternalTimeZone by parsing its string representation
    */
   public ExternalTimeZone(String sVtimezone)
   {
      m_standard = new TimeZonePart(sVtimezone, endIndexOf(sVtimezone, "BEGIN:STANDARD", 0), sVtimezone.indexOf("END:STANDARD"));
      m_daylight = new TimeZonePart(sVtimezone, endIndexOf(sVtimezone, "BEGIN:DAYLIGHT", 0), sVtimezone.indexOf("END:DAYLIGHT"));
   }
   
   /**
    * Constructs ExternalTimeZone by parsing its binary representation.  The documentation for this format comes from 
    * http://www.geocities.com/cainrandom/dev/MAPIRecurrence.html 
    * and the Windows API documentation of the TIME_ZONE_INFORMATION struct.
    */
   public ExternalTimeZone(byte[] binVtimezone)
   {
      int nBiasMinutes = (int)getInt(binVtimezone, 0);

      m_standard = new TimeZonePart(binVtimezone, 4, 14, nBiasMinutes);
      m_daylight = new TimeZonePart(binVtimezone, 8, 32, nBiasMinutes);
   }
   
   /**
    * Constructs ExternalTimeZone from explict parameters.  For a timezone without Daylight time,
    * make nStandardOffset == nDaylightOffset, set all daylight integer parameters to 0, 
    * DaylightTimeStart = null.  Months, days and dayOfWeekInMonths are all specified in
    * java Calendar values for Locale.ENGLISH.
    * @param nStandardOffset milliseconds offset from UTC during standard time.
    * @param nStandardMonth the month in which standard time begins, counting from 1.
    * @param nStandardDayOfWeek day on which standard time begins.
    * @param nStandardDayOfWeekInMonth the ordinal of the day-of-week, eg. 1st (thursday of month) on which standard time begins.
    * @param standardTimeStart the time at which standard time begins.
    * @param nDaylightOffset milliseconds offset from UTC during daylight savings time.
    * @param nDaylightMonth the month in which daylight savings time begins, counting from 1.
    * @param nDaylightDayOfWeek day on which daylight savings time begins.
    * @param nDaylightDayOfWeekInMonth the ordinal of the day-of-week, eg. 1st (thursday of month) on which daylight savings time begins.
    * @param daylightTimeStart the time at which daylight savings time begins.
    */
   public ExternalTimeZone(
      Number nStandardOffset, 
      Number nStandardMonth, 
      Number nStandardDayOfWeek, 
      Number nStandardDayOfWeekInMonth,
      Date standardTimeStart,
      Number nDaylightOffset, 
      Number nDaylightMonth, 
      Number nDaylightDayOfWeek, 
      Number nDaylightDayOfWeekInMonth,
      Date daylightTimeStart)
   {
      m_standard = new TimeZonePart(nStandardOffset, nStandardMonth, nStandardDayOfWeek, nStandardDayOfWeekInMonth, standardTimeStart);
      m_daylight = new TimeZonePart(nDaylightOffset, nDaylightMonth, nDaylightDayOfWeek, nDaylightDayOfWeekInMonth, daylightTimeStart);
   }

   // operations

   /**
    * @return standard time offset from UTC in milliseconds
    */
   public int getStandardOffset()
   {
      return m_standard.m_nOffset;
   }
   
   /**
    * @return daylight time offset from UTC in milliseconds
    */
   public int getDaylightOffset()
   {
      return m_daylight.m_nOffset;
   }
   
   /**
    * @return timestamp of the standard time start, using the given timezone for calendar arithmetics
    */
   public long getStandardStart(TimeZone tz)
   {
      return m_standard.getStartTime(tz);
   }
   
   /**
    * @return timestamp of the daylight time start, using the given timezone for calendar arithmetics
    */
   public long getDaylightStart(TimeZone tz)
   {
      return m_daylight.getStartTime(tz);
   }
   
   /**
    * @return end index of the given substring within the given string, starting search from the given index
    */
   private static int endIndexOf(String sSource, String sSubstring, int nFromIndex)
   {
      int nStart = sSource.indexOf(sSubstring, nFromIndex);

      return (nStart == -1) ? -1 : nStart + sSubstring.length();
   }

   // inner classes

   /** 
    * Encapsulates time zone structure part for STANDARD or DAYLIGHT time
    */
   private static class TimeZonePart
   {
      // attributes

      /**
       * Offset in milliseconds from UTC
       */
      private final int m_nOffset;
      
      /**
       * Month number of the daylight change time
       */
      private final int m_nMonth;
      
      /**
       * Day of week of the daylight change time
       */
      private final int m_nDayOfWeek;
      
      /**
       * Day of week within the month of the daylight change time
       */
      private final int m_nDayOfWeekInMonth;
      
      /**
       * Hour of the daylight change time
       */
      private final int m_nHour;
      
      /**
       * Minute of the daylight change time
       */
      private final int m_nMinute;
      
      /**
       * Second of the daylight change time
       */
      private final int m_nSecond;

      // associations

      /**
       * Calendar instance used for start time calculation (optimization)
       */
      private Calendar cal = Calendar.getInstance(Locale.ENGLISH);

      // constructors

      /**
       * Creates time zone part from exact parameters.
       * @param nOffset The offset in milliseconds from UTC.
       * @param nMonth The month in which this part begins, counting from 1.
       * @param nDayOfWeek The day of week in which this part begins.
       * @param nDayOfWeekInMonth The ordinal (1st, 2nd, etc.) of the day in which this part begins.
       * @param time The time at which this part begins, the date part will be ignored.
       */
      public TimeZonePart(
         Number nOffset, 
         Number nMonth, 
         Number nDayOfWeek, 
         Number nDayOfWeekInMonth,
         Date time)
      {
         m_nOffset = nOffset.intValue();
         m_nMonth = nMonth.intValue();
         m_nDayOfWeek = nDayOfWeek.intValue();
         m_nDayOfWeekInMonth = nDayOfWeekInMonth.intValue();
         
         if (time != null)
         {
            Calendar cal = Calendar.getInstance(TZ.UTC, Locale.ENGLISH);
            
            cal.setTime(time);
            m_nHour = cal.get(Calendar.HOUR_OF_DAY);
            m_nMinute = cal.get(Calendar.MINUTE);
            m_nSecond = cal.get(Calendar.SECOND);
         }
         else
         {
            m_nHour = 0;
            m_nMinute = 0;
            m_nSecond = 0;
         }
      }
      
      /**
       * Creates time zone part part by parsing the given string within the given index range
       */
      public TimeZonePart(String sVTimeZone, int nStart, int nEnd)
      {
         int nDTStart = endIndexOf(sVTimeZone, "DTSTART:", nStart);

         m_nHour = Integer.parseInt(sVTimeZone.substring(nDTStart+9, nDTStart+11));
         m_nMinute = Integer.parseInt(sVTimeZone.substring(nDTStart+11, nDTStart+13));
         m_nSecond = Integer.parseInt(sVTimeZone.substring(nDTStart+13, nDTStart+15));

         int nOffset = endIndexOf(sVTimeZone, "TZOFFSETTO:", nStart);
         boolean bNegative = sVTimeZone.charAt(nOffset) == '-';
         int nHours = Integer.parseInt(sVTimeZone.substring(nOffset+1, nOffset+3));
         int nMinutes = Integer.parseInt(sVTimeZone.substring(nOffset+3, nOffset+5));
         int nMilliseconds = ((nHours * 60) + nMinutes) * 60 * 1000;

         m_nOffset = bNegative ? - nMilliseconds : nMilliseconds;
         
         int nMonth = endIndexOf(sVTimeZone, "BYMONTH=", nStart);

         m_nMonth = (nMonth == -1) ? 0
            : Character.isDigit(sVTimeZone.charAt(nMonth+1)) ? 
               sVTimeZone.charAt(nMonth+1) + (sVTimeZone.charAt(nMonth) * 10) - ('0' * 11)
               : sVTimeZone.charAt(nMonth) - '0';

         int nDay = endIndexOf(sVTimeZone, "BYDAY=", nStart);

         if (nDay != -1)
         {
            boolean bLastDay = sVTimeZone.charAt(nDay) == '-';
            int nDayNo = (bLastDay ? sVTimeZone.charAt(nDay+1) : sVTimeZone.charAt(nDay)) - '0';
            m_nDayOfWeekInMonth = bLastDay ? -nDayNo: nDayNo;
            String sDayOfWeek = bLastDay ? sVTimeZone.substring(nDay+2, nDay+4) : sVTimeZone.substring(nDay+1, nDay+3);
            
            if (sDayOfWeek.equals("MO"))
            {
               m_nDayOfWeek = Calendar.MONDAY;
            }
            else if (sDayOfWeek.equals("TU"))
            {
               m_nDayOfWeek = Calendar.TUESDAY;
            }
            else if (sDayOfWeek.equals("WE"))
            {
               m_nDayOfWeek = Calendar.WEDNESDAY;
            }
            else if (sDayOfWeek.equals("TH"))
            {
               m_nDayOfWeek = Calendar.THURSDAY;
            }
            else if (sDayOfWeek.equals("FR"))
            {
               m_nDayOfWeek = Calendar.FRIDAY;
            }
            else if (sDayOfWeek.equals("SA"))
            {
               m_nDayOfWeek = Calendar.SATURDAY;
            }
            else
            {
               m_nDayOfWeek = Calendar.SUNDAY;
            }
         }
         else
         {
            m_nDayOfWeekInMonth = 0;
            m_nDayOfWeek = 0;
         }
      }

      /**
       * Creates time zone part by parsing the given byte array at the specified offsets.
       * @param data The byte array.
       * @param nBiasOffset The offset in data of the bias parameter.
       * @param nStartOffset The offset in data of the start date parameter.
       * @param nBiasMinutes The number of minutes to be added to nBiasOffset.
       */
      public TimeZonePart(byte[] data, int nBiasOffset, int nStartOffset, int nBiasMinutes)
      {
         int nDayOfMonthUnspecified = ExternalTimeZone.getShort(data, nStartOffset + 0);
         int nPartBiasMinutes = 0;

         m_nMonth = ExternalTimeZone.getShort(data, nStartOffset + 2);

         // m_nMonth = 0 signifies that this VTimeZonePart is unused.
         if (m_nMonth != 0)
         {
            if (nDayOfMonthUnspecified == 0)
            {
               int nDayOfWeekIndex = ExternalTimeZone.getShort(data, nStartOffset + 4);
               
               m_nDayOfWeek = ExternalTimeZone.DAYS_OF_WEEK[nDayOfWeekIndex];
               
               int nWeekOfMonth = ExternalTimeZone.getShort(data, nStartOffset + 6);
               
               m_nDayOfWeekInMonth = ((nWeekOfMonth == 5) ? -1: nWeekOfMonth);
            }
            else
            {
               m_nDayOfWeek = 0;
               m_nDayOfWeekInMonth = 0;
            }

            m_nHour = ExternalTimeZone.getShort(data, nStartOffset + 8);
            m_nMinute = ExternalTimeZone.getShort(data, nStartOffset + 10);
            m_nSecond = ExternalTimeZone.getShort(data, nStartOffset + 12);
            nPartBiasMinutes = (int)ExternalTimeZone.getInt(data, nBiasOffset);
         }
         else
         {
            m_nDayOfWeek = 0;
            m_nDayOfWeekInMonth = 0;
            m_nHour = 0;
            m_nMinute = 0;
            m_nSecond = 0;
         }

         // The biases all have the opposite sign to the offset Java uses
         m_nOffset = -(nBiasMinutes + nPartBiasMinutes) * 60 * 1000;
      }

      // operations

      /**
       * @return timestamp of the start time of this part, using timezone specified in the provided calendar instance
       */
      public long getStartTime(TimeZone tz)
      {
         cal.setTimeZone(tz);
         cal.clear();
         cal.set(Calendar.YEAR, CURRENT_YEAR);
         cal.set(Calendar.MONTH, m_nMonth - 1); // Java months are 0 - based
         cal.set(Calendar.DAY_OF_WEEK, m_nDayOfWeek);
         cal.set(Calendar.DAY_OF_WEEK_IN_MONTH, (m_nDayOfWeekInMonth == 5) ? -1 : m_nDayOfWeekInMonth);
         cal.set(Calendar.HOUR, m_nHour);
         cal.set(Calendar.MINUTE, m_nMinute);
         cal.set(Calendar.SECOND, m_nSecond);
         cal.set(Calendar.MILLISECOND, 0);

         return cal.getTime().getTime();
      }

      /**
       * @return startDay used in SimpleTimeZone constructor
       */
      public int getDay()
      {
         if (m_nDayOfWeekInMonth > 0)
         {
            return 1 + (7 * (m_nDayOfWeekInMonth - 1));
         }
         
         cal.clear();
         cal.set(Calendar.YEAR, CURRENT_YEAR);
         cal.set(Calendar.MONTH, m_nMonth - 1); // Java months are 0 - based
         return - cal.getMaximum(Calendar.MONTH) - (7 * (m_nDayOfWeekInMonth + 1));
      }

      /**
       * @return startDayOfWeek used in SimpleTimeZone constructor
       */
      public int getDayOfWeek()
      {
         return -m_nDayOfWeek;
      }

      /**
       * @return startMonth used in SimpleTimeZone constructor
       */
      public int getMonth()
      {
         return m_nMonth - 1;  // Java months are 0 - based
      }
   }

   /**
    * Create a SimpleTimeZone for this 
    * @return a new SimpleTimeZone
    */
   public TimeZone createTimeZone()
   {
      return new SimpleTimeZone(m_standard.m_nOffset, GENERATED_TIMEZONE_ID,
         m_standard.getMonth(), m_standard.getDay(), m_standard.getDayOfWeek(), 1000 * (m_standard.m_nSecond + 60 * (m_standard.m_nMinute + 60 * m_standard.m_nHour)),
         m_daylight.getMonth(), m_daylight.getDay(), m_daylight.getDayOfWeek(), 1000 * (m_daylight.m_nSecond + 60 * (m_daylight.m_nMinute + 60 * m_daylight.m_nHour)),
         m_daylight.m_nOffset - m_standard.m_nOffset);
   }
}
