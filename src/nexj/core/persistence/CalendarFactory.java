// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;

import java.util.Calendar;

/**
 * Interface implemented by time zone converters.
 */
public interface CalendarFactory
{
   /**
    * @return New calendar instance for conversion.
    */
   Calendar createCalendar();
}
