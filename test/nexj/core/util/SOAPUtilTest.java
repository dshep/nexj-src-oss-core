// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.sql.Timestamp;
import java.util.TimeZone;

import junit.framework.TestCase;

public class SOAPUtilTest extends TestCase
{
   protected Timestamp m_ts = new Timestamp(3661234);

   protected void setUp() throws Exception
   {
      super.setUp();
   }

   public void testParseDateTime()
   {
      assertEquals(m_ts, SOAPUtil.parseDateTime("1970-01-01T01:01:01.234Z", true, true, null));
      assertEquals(m_ts, SOAPUtil.parseDateTime("1970-01-01T03:01:01.234+02:00", true, true, null));
      assertEquals(0, SOAPUtil.parseDateTime("1970-01-01Z", true, false, null).getTime());
      assertEquals(-7200000, SOAPUtil.parseDateTime("1970-01-01+02:00", true, false, null).getTime());
      assertEquals(3661234, SOAPUtil.parseDateTime("01:01:01.234Z", false, true, null).getTime());
      assertEquals(3661234, SOAPUtil.parseDateTime("03:01:01.234+02:00", false, true, null).getTime());

      assertEquals(3661235, SOAPUtil.parseDateTime("01:01:01.2349Z", false, true, null).getTime());
      assertEquals(3661235, SOAPUtil.parseDateTime("03:01:01.2349+02:00", false, true, null).getTime());
      assertEquals(3662000, SOAPUtil.parseDateTime("01:01:01.9999Z", false, true, null).getTime());
      assertEquals(3662000, SOAPUtil.parseDateTime("03:01:01.9999+02:00", false, true, null).getTime());
      assertEquals(3661000, SOAPUtil.parseDateTime("01:01:01.0000Z", false, true, null).getTime());
   }

   public void testFormatDateTime()
   {
      assertEquals("1970-01-01T01:01:01.234Z", SOAPUtil.formatDateTime(m_ts));
      assertEquals("1970-01-01T02:01:01.234+01:00", SOAPUtil.formatDateTimeWithOffset(m_ts, TimeZone.getTimeZone("Europe/Warsaw")));
      assertEquals("1970-01-01T02:01:01.234", SOAPUtil.formatDateTimeWithoutOffset(m_ts, TimeZone.getTimeZone("Europe/Warsaw")));
   }

   public void testFormatDate()
   {
      assertEquals("1970-01-01Z", SOAPUtil.formatDate(m_ts));
      assertEquals("1969-12-31-03:00", SOAPUtil.formatDateWithOffset(m_ts, TimeZone.getTimeZone("America/Sao_Paulo")));
      assertEquals("1969-12-31", SOAPUtil.formatDateWithoutOffset(m_ts, TimeZone.getTimeZone("America/Sao_Paulo")));
   }

   public void testFormatTime()
   {
      assertEquals("01:01:01.234Z", SOAPUtil.formatTime(m_ts));
      assertEquals("02:01:01.234+01:00", SOAPUtil.formatTimeWithOffset(m_ts, TimeZone.getTimeZone("Europe/Warsaw")));
      assertEquals("02:01:01.234", SOAPUtil.formatTimeWithoutOffset(m_ts, TimeZone.getTimeZone("Europe/Warsaw")));
   }
}
