// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

/**
 * Time zone-related utility functions.
 */
public class TimeZoneUtil
{
   // constants

   /**
    * The UTC time zone.
    * @deprecated See {@link TZ#UTC}. This should not be removed because Scheme
    *             code depends on it.
    */
   public final static TimeZone UTC = TZ.UTC;

   /**
    * Allowed daylight start / end time error in milliseconds
    */
   public final static long MIN_DST_ERROR = 2000; // 2 seconds
   
   /**
    * Maximum allowed daylight start / end time error in milliseconds
    */
   private final static long MAX_DST_ERROR = 90L * 24 * 60 * 60 * 1000; // 90 days

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(TimeZoneUtil.class);

   /** 
    * Time zone cache: TimeZone[TimeZoneKey]
    */
   private final static Lookup s_timeZoneOffsetMap = new SoftHashTab();
   
   /**
    * Common timezones used for search by offset
    */
   private final static String[] COMMON_TIMEZONES = new String[] 
   {
      "Pacific/Auckland", // GMT+12:00
      "Asia/Magadan", // GMT+11:00
      "Australia/Brisbane", // GMT+10:00
      "Australia/Adelaide", // GMT+09:30
      "Asia/Tokyo", // GMT+09:00
      "Asia/Hong_Kong", // GMT+08:00
      "Asia/Bangkok", // GMT+07:00
      "Asia/Almaty", // GMT+06:00
      "Asia/Calcutta", // GMT+05:30
      "Asia/Karachi", // GMT+05:00
      "Asia/Kabul", // GMT+04:30
      "Asia/Muscat", // GMT+04:00
      "Asia/Tehran", // GMT+03:30
      "Asia/Baghdad", // GMT+03:00
      "Europe/Bucharest", // GMT+02:00
      "Europe/Berlin", // GMT+01:00
      "Europe/Dublin", // GMT
      "Atlantic/Azores", // GMT-01:00
      "Atlantic/South_Georgia", // GMT-02:00
      "America/Argentina/Buenos_Aires", // GMT-03:00
      "America/St_Johns", // GMT-03:30
      "America/Halifax", // GMT-04:00
      "America/New_York", // GMT-05:00
      "America/Chicago", // GMT-06:00
      "America/Denver", // GMT-07:00
      "America/Los_Angeles", // GMT-08:00
      "America/Anchorage", // GMT-09:00
      "Pacific/Honolulu", // GMT-10:00
      "Pacific/Apia", // GMT-11:00
      "Pacific/Kwajalein", // GMT-12:00
   };

   /**
    * Timezones used in the client.
    */
   private final static Lookup TIMEZONE_IDS = new HashTab(76);
   
   static
   {
      TIMEZONE_IDS.put("Etc/GMT+12", null);
      TIMEZONE_IDS.put("Pacific/Apia", null);
      TIMEZONE_IDS.put("Pacific/Honolulu", null);
      TIMEZONE_IDS.put("America/Anchorage", null);
      TIMEZONE_IDS.put("America/Los_Angeles", null);
      TIMEZONE_IDS.put("America/Denver", null);
      TIMEZONE_IDS.put("America/Tegucigalpa", null);
      TIMEZONE_IDS.put("America/Phoenix", null);
      TIMEZONE_IDS.put("America/Winnipeg", null);
      TIMEZONE_IDS.put("America/Mexico_City", null);
      TIMEZONE_IDS.put("America/Chicago", null);
      TIMEZONE_IDS.put("America/Costa_Rica", null);
      TIMEZONE_IDS.put("America/New_York", null);
      TIMEZONE_IDS.put("America/Bogota", null);
      TIMEZONE_IDS.put("America/Santiago", null);
      TIMEZONE_IDS.put("America/Caracas", null);
      TIMEZONE_IDS.put("America/Anguilla", null);
      TIMEZONE_IDS.put("America/St_Johns", null);
      TIMEZONE_IDS.put("America/Thule", null);
      TIMEZONE_IDS.put("America/Argentina/Buenos_Aires", null);
      TIMEZONE_IDS.put("America/Sao_Paulo", null);
      TIMEZONE_IDS.put("Atlantic/South_Georgia", null);
      TIMEZONE_IDS.put("Atlantic/Cape_Verde", null);
      TIMEZONE_IDS.put("Atlantic/Azores", null);
      TIMEZONE_IDS.put("Africa/Casablanca", null);
      TIMEZONE_IDS.put("Europe/Dublin", null);
      TIMEZONE_IDS.put("Etc/UTC", null);
      TIMEZONE_IDS.put("Europe/Amsterdam", null);
      TIMEZONE_IDS.put("Europe/Belgrade", null);
      TIMEZONE_IDS.put("Europe/Brussels", null);
      TIMEZONE_IDS.put("Europe/Warsaw", null);
      TIMEZONE_IDS.put("Africa/Lagos", null);
      TIMEZONE_IDS.put("Europe/Athens", null);
      TIMEZONE_IDS.put("Europe/Bucharest", null);
      TIMEZONE_IDS.put("Africa/Cairo", null);
      TIMEZONE_IDS.put("Africa/Harare", null);
      TIMEZONE_IDS.put("Europe/Helsinki", null);
      TIMEZONE_IDS.put("Asia/Jerusalem", null);
      TIMEZONE_IDS.put("Asia/Baghdad", null);
      TIMEZONE_IDS.put("Asia/Kuwait", null);
      TIMEZONE_IDS.put("Europe/Moscow", null);
      TIMEZONE_IDS.put("Africa/Nairobi", null);
      TIMEZONE_IDS.put("Asia/Tehran", null);
      TIMEZONE_IDS.put("Asia/Dubai", null);
      TIMEZONE_IDS.put("Asia/Baku", null);
      TIMEZONE_IDS.put("Asia/Kabul", null);
      TIMEZONE_IDS.put("Asia/Yekaterinburg", null);
      TIMEZONE_IDS.put("Asia/Karachi", null);
      TIMEZONE_IDS.put("Asia/Calcutta", null);
      TIMEZONE_IDS.put("Asia/Katmandu", null);
      TIMEZONE_IDS.put("Asia/Almaty", null);
      TIMEZONE_IDS.put("Asia/Dhaka", null);
      TIMEZONE_IDS.put("Asia/Colombo", null);
      TIMEZONE_IDS.put("Asia/Rangoon", null);
      TIMEZONE_IDS.put("Asia/Bangkok", null);
      TIMEZONE_IDS.put("Asia/Krasnoyarsk", null);
      TIMEZONE_IDS.put("Asia/Hong_Kong", null);
      TIMEZONE_IDS.put("Asia/Irkutsk", null);
      TIMEZONE_IDS.put("Asia/Kuala_Lumpur", null);
      TIMEZONE_IDS.put("Australia/Perth", null);
      TIMEZONE_IDS.put("Asia/Taipei", null);
      TIMEZONE_IDS.put("Asia/Tokyo", null);
      TIMEZONE_IDS.put("Asia/Seoul", null);
      TIMEZONE_IDS.put("Asia/Yakutsk", null);
      TIMEZONE_IDS.put("Australia/Adelaide", null);
      TIMEZONE_IDS.put("Australia/Darwin", null);
      TIMEZONE_IDS.put("Australia/Brisbane", null);
      TIMEZONE_IDS.put("Australia/Sydney", null);
      TIMEZONE_IDS.put("Pacific/Guam", null);
      TIMEZONE_IDS.put("Australia/Hobart", null);
      TIMEZONE_IDS.put("Asia/Vladivostok", null);
      TIMEZONE_IDS.put("Asia/Magadan", null);
      TIMEZONE_IDS.put("Pacific/Auckland", null);
      TIMEZONE_IDS.put("Pacific/Fiji", null);
      TIMEZONE_IDS.put("Pacific/Tongatapu", null);
      TIMEZONE_IDS.put("Asia/Yerevan", null);
      TIMEZONE_IDS.put("Asia/Beirut", null);
      TIMEZONE_IDS.put("America/Montevideo", null);
      TIMEZONE_IDS.put("Asia/Novosibirsk", null);
      TIMEZONE_IDS.put("America/Chihuahua", null);
      TIMEZONE_IDS.put("America/Campo_Grande", null);
      TIMEZONE_IDS.put("America/Araguaina", null);
      TIMEZONE_IDS.put("America/Ensenada", null);
      TIMEZONE_IDS.put("Europe/Kaliningrad", null);
      TIMEZONE_IDS.put("Etc/GMT+11", null);
      TIMEZONE_IDS.put("Etc/GMT-12", null);
   }

   // constructors

   /**
    * Prevents construction.
    */
   protected TimeZoneUtil()
   {
   }

   // operations

   /**
    * Return an existing TimeZone (from the java.util.TimeZone.getAvailableIDs() array) that matches the parameters.
    * @param nStandardOffset standard time offset in milliseconds from UTC
    * @param nDaylightOffset daylight time offset in milliseconds from UTC
    * @param lStandardStart long value of a timestamp of standard time start. Ignored when nStandardOffset = nDaylightOffset
    * @param lDaylightStart long value of a timestamp of daylight time start. Ignored when nStandardOffset = nDaylightOffset
    * @return a matching TimeZone. If several time zones match, any one is returned; if none match, null is returned.
    */
   public static TimeZone findTimeZone(int nStandardOffset, int nDaylightOffset, long lStandardStart, long lDaylightStart)
   {
      TimeZoneOffsetKey key = new TimeZoneOffsetKey(nStandardOffset, nDaylightOffset, lStandardStart, lDaylightStart);
      Object cached = s_timeZoneOffsetMap.get(key);

      if (cached != null)
      {
         return (cached == Undefined.VALUE) ? null : (TimeZone) cached;
      }

      String[] sTimeZoneIds = TimeZone.getAvailableIDs(nStandardOffset);
      
      for (int i = 0; i < sTimeZoneIds.length; i++)
      {
         TimeZone tz = TimeZone.getTimeZone(sTimeZoneIds[i]);

         if (matches(tz, nStandardOffset, nDaylightOffset, lStandardStart, lDaylightStart, MIN_DST_ERROR))
         {
            if (TIMEZONE_IDS.contains(tz.getID()))
            {
               s_timeZoneOffsetMap.put(key, tz);
               return tz;
            }
         }
      }
      
      s_timeZoneOffsetMap.put(key, Undefined.VALUE);

      return null;
   }
   
   /**
    * Return whether the given timezone matches the given combination of offsets and start times
    * @param tz time zone
    * @param nStandardOffset standard time offset in milliseconds from UTC
    * @param nDaylightOffset daylight time offset in milliseconds from UTC
    * @param lStandardStart long value of a timestamp of standard time start. Ignored when nStandardOffset = nDaylightOffset
    * @param lDaylightStart long value of a timestamp of daylight time start. Ignored when nStandardOffset = nDaylightOffset
    * @param lDSTError Allowed daylight start / end time error in milliseconds
    * @return true if the timezone matches 
    */
   private static boolean matches(TimeZone tz, int nStandardOffset, int nDaylightOffset, long lStandardStart, long lDaylightStart, long lDSTError)
   {
      if (nStandardOffset == nDaylightOffset)
      {
         return (tz.getDSTSavings() == 0);
      }
      
      if (nDaylightOffset - nStandardOffset ==  tz.getDSTSavings())
      { // SUN timezones do not always report this properly (e.g. "Australia/Perth"), but we'll rather check it to make sure we do not match wrong timezone.
         return tz.inDaylightTime(new Date(lStandardStart - lDSTError - tz.getDSTSavings()))
            && !tz.inDaylightTime(new Date(lStandardStart + lDSTError))
            && !tz.inDaylightTime(new Date(lDaylightStart - lDSTError))
            && tz.inDaylightTime(new Date(lDaylightStart + lDSTError));
      }
      
      return false;
   }
   
   /**
    * Return an existing TimeZone (from the java.util.TimeZone.getAvailableIDs() array) that matches the provided VTIMEZONE structure.
    * This method only supports simple VTIMEZONE structures like ones generated by Microsoft Outlook for the recurrence meeting timezone field
    * @param sVTimeZone a VTIMEZONE structure, expressed as an iCalendar string.
    * @return a matching TimeZone. If several time zones match, any one is returned; if none match, null is returned.
    */
   public static TimeZone findTimeZone(String sVTimeZone)
   {
      ExternalTimeZone externalTimezone = new ExternalTimeZone(sVTimeZone);

      // No cache by timezone is required since timezone is obtained from Exchange and parsing it every time does not add significant overhead over RPC.
      for (long lDSTError = MIN_DST_ERROR; lDSTError <= MAX_DST_ERROR; lDSTError *= 2)
      {
         TimeZone tz = findTimeZone(externalTimezone, lDSTError);
         
         if (tz != null)
         {
            if (lDSTError > MIN_DST_ERROR)
            {
               s_logger.warn("Cannot find an exact match for external time zone; DTS error is "
                  + lDSTError + " ms. Java time zone " + tz.getID() + " is used to represent external time zone " + sVTimeZone + ".");
            }
            
            return tz;
         }
      }
      
      s_logger.warn("Cannot find matching Java time zone for external time zone " + sVTimeZone + ".");

      return null;
   }
   

   /**
    * @return a new SimpleTimeZone builds for the provided VTIMEZONE structure.
    * @param sVtimezone a VTIMEZONE structure, expressed as an iCalendar string.
    */
   public static TimeZone newTimeZone(String sVtimezone)
   {
      return new ExternalTimeZone(sVtimezone).createTimeZone();
   }
   
   /**
    * Return an existing TimeZone (from the java.util.TimeZone.getAvailableIDs() array) that matches the provided timezone data.
    * @param lStandardOffset milliseconds offset from UTC during standard time.
    * @param lStandardMonth the month in which standard time begins, counting from 1.
    * @param lStandardDayOfWeek day on which standard time begins.
    * @param lStandardDayOfWeekInMonth the ordinal of the day-of-week, eg. 1st (thursday of month) on which standard time begins.
    * @param standardTimeStart the time at which standard time begins.
    * @param lDaylightOffset milliseconds offset from UTC during daylight savings time.
    * @param lDaylightMonth the month in which daylight savings time begins, counting from 1.
    * @param lDaylightDayOfWeek day on which daylight savings time begins.
    * @param lDaylightDayOfWeekInMonth the ordinal of the day-of-week, eg. 1st (thursday of month) on which daylight savings time begins.
    * @param daylightTimeStart the time at which daylight savings time begins.
    */
   public static TimeZone findParsedTimeZone(
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
      return findTimeZone(new ExternalTimeZone(
         nStandardOffset, 
         nStandardMonth, 
         nStandardDayOfWeek, 
         nStandardDayOfWeekInMonth,
         standardTimeStart,
         nDaylightOffset, 
         nDaylightMonth, 
         nDaylightDayOfWeek, 
         nDaylightDayOfWeekInMonth,
         daylightTimeStart
         ), MIN_DST_ERROR);
   }
   
   /**
    * Return an existing TimeZone (from the java.util.TimeZone.getAvailableIDs() array) that matches the provided timezone structure.
    * @param binTimezone a timezone structure, as an Outlook-generated binary structure
    * @return a matching TimeZone. If several time zones match, any one is returned; if none match, null is returned.
    */
   public static TimeZone findTimeZone(Binary binTimezone)
   {
      byte[] data = binTimezone.getData();
    
      return findTimeZone(data);
   }
   
   /**
    * Return an existing TimeZone (from the java.util.TimeZone.getAvailableIDs() array) that matches the provided timezone structure.
    * @param vtimezone a VTIMEZONE structure, expressed as an Outlook-generated binary structure
    * @return a matching TimeZone. If several time zones match, any one is returned; if none match, null is returned.
    */
   public static TimeZone findTimeZone(byte[] binTimezone)
   {
      return findTimeZone(new ExternalTimeZone(binTimezone), MIN_DST_ERROR);
   }
   
   /**
    * @param externalTimezone a ExternalTimeZone object
    * @param lDSTError Allowed daylight start / end time error in milliseconds
    * @return a matching TimeZone. If several time zones match, any one is returned; if none match, null is returned.
    */
   protected static TimeZone findTimeZone(ExternalTimeZone externalTimezone, long lDSTError)
   {
      String[] timeZoneIds = TimeZone.getAvailableIDs(externalTimezone.getStandardOffset());
      
      for (int i = 0; i < timeZoneIds.length; i++)
      {
         TimeZone tz = TimeZone.getTimeZone(timeZoneIds[i]);

         if (matches(tz, externalTimezone.getStandardOffset(), externalTimezone.getDaylightOffset(), externalTimezone.getStandardStart(tz), externalTimezone.getDaylightStart(tz), lDSTError))
         {
            return tz;
         }
      }
      
      return null;
   }
   
   /**
    * Return a compatible TimeZone (from the java.util.TimeZone.getAvailableIDs() array) that has the same offsets as the TimeZone passed in.
    * @param tz a TimeZone object
    * @return a matching TimeZone that is supported by the client in the TIMEZONE_IDS map.  If there is no match then null is returned.
    */
   public static TimeZone findTimeZone(TimeZone tz)
   {
      if (TIMEZONE_IDS.contains(tz.getID()))
      {
         return tz;
      }
      
      return findCanonicalTimeZone(tz);
   }
   
   /**
    * Return a compatible TimeZone (from the java.util.TimeZone.getAvailableIDs() array) that has the same offsets as the TimeZone passed in.
    * Maps all timezones with identical rules for the current year to a single "canonical" timezone.
    * @param tz a TimeZone object
    * @return a matching TimeZone that is supported by the client in the TIMEZONE_IDS map.  If there is no match then null is returned.
    */
   public static TimeZone findCanonicalTimeZone(TimeZone tz)
   {
      // Timezone doesn't support DST so just return offset value
      if (!tz.useDaylightTime())
      {
         return findTimeZone(tz.getRawOffset(), tz.getRawOffset(), 0, 0);
      }
      else
      {
         Calendar cal = Calendar.getInstance();
         long lStartTime = 0, lEndTime = 0, lMidMonthTime = 0, lFirstSwitchTime = 0;
         
         cal.set(cal.get(Calendar.YEAR), 0, 1, 0, 0, 0);
         cal.set(Calendar.MILLISECOND, 0);

         // Iterate over the days of the year 1/2 month at a time looking for the start of an offset change.
         // Reason why we do 2 weeks at a time is because we want to handle any potential cases where
         // a DST change lasts for less than a month and we assume a change would at least last 2 weeks.
         for (int nMonth = 0; nMonth < 12; ++nMonth)
         {
            long lSwitchTime;
            cal.set(Calendar.DATE, 1);
            cal.set(Calendar.MONTH, nMonth);
            lStartTime = cal.getTimeInMillis();
            cal.add(Calendar.MONTH, 1);
            cal.add(Calendar.DATE, -1);
            lEndTime = cal.getTimeInMillis();
            lMidMonthTime = (lStartTime + lEndTime) / 2;
            
            lSwitchTime = findSwitchOverTime(tz, lStartTime, lMidMonthTime);

            // Search second half of month if no change found in the first half
            if (lSwitchTime == -1)
            {
               lSwitchTime = findSwitchOverTime(tz, lMidMonthTime, lEndTime);
            }

            if (lSwitchTime != -1)
            {
               lFirstSwitchTime = lSwitchTime;
               break;
            }
         }

         // Find change over date
         cal.set(Calendar.MONTH, Calendar.DECEMBER);
         cal.set(Calendar.DATE, 31);
         lEndTime = cal.getTimeInMillis();

         long lSecondSwitchTime = findSwitchOverTime(tz, lFirstSwitchTime, lEndTime);
         int nFirstOffset = tz.getOffset(lFirstSwitchTime);
         int nSecondOffset = tz.getOffset(lSecondSwitchTime);

         // If the difference between the 2 offsets is a negative 1hour than it implies that the second time offset
         // is the standard offset and the first one is actually the DST offset. 
         if ((nSecondOffset - nFirstOffset) <= -3600000)
         {
             return findTimeZone(nSecondOffset, nFirstOffset, lSecondSwitchTime, lFirstSwitchTime);
         }
         
         return findTimeZone(nFirstOffset, nSecondOffset, lFirstSwitchTime, lSecondSwitchTime);
      }
   }

   /**
    * Returns the time when the DST changes for the given time zone between the given range, or -1 if there is no change in the range.
    * @param tz the time zone used when searching for the time when DST changes.
    * @param lStartTime the start time range used to find when DST changes.
    * @param lEndTime the end time range used to find when DST changes.
    * @return the point in time when the time zone changes DST.  If there is no change between the given times then -1 is returned.
    */
   private static long findSwitchOverTime(TimeZone tz, long lStartTime, long lEndTime)
   {
      while (lEndTime - lStartTime > 1)
      {
         long lMid = (lStartTime + lEndTime) / 2;
         int nMidTimezoneOffset = tz.getOffset(lMid);

         if (tz.getOffset(lStartTime) != nMidTimezoneOffset)
         {
            lEndTime = lMid;
         }
         else if (nMidTimezoneOffset != tz.getOffset(lEndTime))
         {
            lStartTime = lMid;
         }
         else
         {
            return -1;
         }
      }

      return lEndTime;
   }

   /**
    * Returns an existing Java TimeZone that has the given offset in miliseconds from UTC
    * @param lOffset offset in miliseconds
    * @param lDate date for which the offset is specified (depending on this date, the offset could be standard or daylight)
    * @return a matching TimeZone (trying to get the most likely time zone when only standard offset is available)
    */
   public static TimeZone findTimeZoneForOffset(long lDate, long lOffset)
   {
      for (int i = 0; i < COMMON_TIMEZONES.length; i++)
      {
         TimeZone tz = TimeZone.getTimeZone(COMMON_TIMEZONES[i]);
         
         if (tz.getOffset(lDate) == lOffset)
         {
            return tz;
         }
      }
      
      return null;
   }

   // inner classes

   /**
    * Key used to cache time zones
    */
   private static class TimeZoneOffsetKey
   {
      private final int m_nStandardOffset;
      private final int m_nDaylightOffset;
      private final long m_lStandardStart;
      private final long m_lDaylightStart;

      // constructors

      public TimeZoneOffsetKey(int nStandardOffset, int nDaylightOffset, long lStandardStart, long lDaylightStart)
      {
         m_nStandardOffset = nStandardOffset;
         m_nDaylightOffset = nDaylightOffset;
         m_lStandardStart = lStandardStart;
         m_lDaylightStart = lDaylightStart;
      }

      // operations

      /**
       * @see java.lang.Object#hashCode()
       */
      public int hashCode()
      {
         final int PRIME = 31;
         int nHashCode = 1;
         nHashCode = PRIME * nHashCode + (int) (m_lDaylightStart ^ (m_lDaylightStart >>> 32));
         nHashCode = PRIME * nHashCode + (int) (m_lStandardStart ^ (m_lStandardStart >>> 32));
         nHashCode = PRIME * nHashCode + m_nDaylightOffset;
         nHashCode = PRIME * nHashCode + m_nStandardOffset;
         
         return nHashCode;
      }

      /**
       * @see java.lang.Object#equals(java.lang.Object)
       */
      public boolean equals(Object obj)
      {
         if (this == obj)
         {
            return true;
         }
         
         if (!(obj instanceof TimeZoneOffsetKey))
         {
            return false;
         }
         
         final TimeZoneOffsetKey other = (TimeZoneOffsetKey) obj;
         
         return (m_lDaylightStart == other.m_lDaylightStart && m_lStandardStart == other.m_lStandardStart 
            && m_nDaylightOffset == other.m_nDaylightOffset && m_nStandardOffset == other.m_nStandardOffset);
      }
   }
}
