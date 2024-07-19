// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;

import java.util.Calendar;
import java.util.Locale;
import java.util.TimeZone;

import nexj.core.meta.Primitive;
import nexj.core.meta.UnaryFunction;
import nexj.core.runtime.Initializable;

/**
 * Time zone converter for timestamps.
 */
public class TimeZoneConverter implements Converter, CalendarFactory, Initializable
{
   // attributes

   /**
    * The time zone name.
    */
   protected String m_sTimeZoneName;

   // associations

   /**
    * The calendar for conversion.
    */
   protected Calendar m_calendar;

   // operations

   /**
    * Sets the time zone name.
    * @param sTimeZoneName The time zone name to set.
    */
   public void setTimeZoneName(String sTimeZoneName)
   {
      m_sTimeZoneName = sTimeZoneName;
   }

   /**
    * @return The time zone name.
    */
   public String getTimeZoneName()
   {
      return m_sTimeZoneName;
   }
   
   /**
    * @see nexj.core.runtime.Initializable#initialize()
    */
   public void initialize() throws Exception
   {
      m_calendar = Calendar.getInstance(
         (m_sTimeZoneName == null) ? TimeZone.getDefault() :
            TimeZone.getTimeZone(m_sTimeZoneName), Locale.ENGLISH);
   }

   /**
    * @see nexj.core.persistence.Converter#getSourceType()
    */
   public Primitive getSourceType()
   {
      return Primitive.TIMESTAMP;
   }

   /**
    * @see nexj.core.persistence.Converter#getDestinationType()
    */
   public Primitive getDestinationType()
   {
      return Primitive.TIMESTAMP;
   }

   /**
    * @see nexj.core.persistence.Converter#getForwardFunction()
    */
   public UnaryFunction getForwardFunction()
   {
      return Primitive.IDENTITY_FUNCTION;
   }

   /**
    * @see nexj.core.persistence.Converter#getInverseFunction()
    */
   public UnaryFunction getInverseFunction()
   {
      return Primitive.IDENTITY_FUNCTION;
   }

   /**
    * @see nexj.core.persistence.Converter#isOrderPreserved()
    */
   public boolean isOrderPreserved()
   {
      return true;
   }

   /**
    * @see nexj.core.persistence.CalendarFactory#createCalendar()
    */
   public Calendar createCalendar()
   {
      return (Calendar)m_calendar.clone();
   }
}
