// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.sql.Timestamp;

import junit.framework.TestCase;

public class HTTPTest extends TestCase
{
   protected void setUp() throws Exception
   {
      super.setUp();
   }

   public void testGetHeaderType()
   {
      assertEquals(HTTP.TYPE_INTEGER, HTTP.getHeaderType("age"));
      assertEquals(HTTP.TYPE_DATE, HTTP.getHeaderType("expires"));
      assertEquals(HTTP.TYPE_STRING, HTTP.getHeaderType("cookie"));
   }

   public void testFormatDateTime()
   {
      assertEquals("Sun, 06 Nov 1994 08:49:37 GMT", HTTP.formatDateTime(new Timestamp(784111777000L)));
   }

   public void testParseDateTime()
   {
      assertEquals(784111777000L, HTTP.parseDateTime("Sun, 06 Nov 1994 08:49:37 GMT").getTime());
      assertEquals(784111777000L, HTTP.parseDateTime("Sunday, 06-Nov-94 08:49:37 GMT").getTime());
      assertEquals(784111777000L, HTTP.parseDateTime("Sun Nov  6 08:49:37 1994").getTime());
   }

   public void testFormatCookieDateTime()
   {
      assertEquals("Sun, 06-Nov-1994 08:49:37 GMT", HTTP.formatCookieDateTime(new Timestamp(784111777000L)));
   }

   public void testParseCookieDateTime()
   {
      assertEquals(784111777000L, HTTP.parseCookieDateTime("Sun, 06-Nov-1994 08:49:37 GMT").getTime());
   }

   public void testToStringObject()
   {
      assertEquals("123", HTTP.toString(new Integer(123)));
      assertEquals("Sun, 06 Nov 1994 08:49:37 GMT", HTTP.toString(new Timestamp(784111777000L)));
      assertEquals("abc", HTTP.toString("abc"));
   }
}
