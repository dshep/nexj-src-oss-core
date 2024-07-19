// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

import java.sql.Timestamp;
import java.util.Calendar;
import java.util.TimeZone;

import junit.framework.TestCase;

import nexj.core.meta.Repository;
import nexj.core.util.LocaleUtil;
import nexj.core.util.TZ;

public class TestFormat extends TestCase
{
   private InvocationContext m_ctx;
   private Timestamp m_time;
   
   /**
    * @see junit.framework.TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      m_ctx = new InvocationContext(Repository.getMetadata());
      m_ctx.setLocale(LocaleUtil.parse("en"));
      m_ctx.setTimeZone(TimeZone.getTimeZone("EST"));

      Calendar c = Calendar.getInstance(LocaleUtil.parse("en"));

      c.setTimeZone(TZ.UTC);
      c.set(Calendar.YEAR, 2003);
      c.set(Calendar.MONTH, 10);
      c.set(Calendar.DATE, 12);
      c.set(Calendar.HOUR_OF_DAY, 15);
      c.set(Calendar.MINUTE, 0);
      c.set(Calendar.SECOND, 0);
      c.set(Calendar.MILLISECOND, 0);
      m_time = new Timestamp(c.getTimeInMillis());
   }

   /**
    * @see junit.framework.TestCase#tearDown()
    */
   protected void tearDown() throws Exception
   {
      super.tearDown();

      m_ctx = null;
   }

   public void testTimeZone()
   {
      String s = m_ctx.formatString("ids.testTimeZone", new Object[]{m_time});
      
      assertEquals("10:00 AM", s);

      s = m_ctx.formatString("ids.testTimeZone2", new Object[]{m_time});
      
      assertEquals("11/12/03 10:00 AM", s);

      s = m_ctx.formatString("ids.testTimeZone3", new Object[]{m_time, m_time});
      
      assertEquals("10:00 AM 10:00:00 AM EST", s);

      s = m_ctx.formatString("ids.testTimeZone4", new Object[]{m_time});
      
      assertEquals("11/12/03 10:00 AM 11/12/03 10:00 AM", s);

      s = m_ctx.formatString("ids.testTimeZone5", new Object[]{m_time});
      
      assertEquals("10:00 AM 10:00:00 AM EST", s);
   }
}
