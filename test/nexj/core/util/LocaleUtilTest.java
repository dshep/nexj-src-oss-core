// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.util.Locale;

import junit.framework.TestCase;

public class LocaleUtilTest extends TestCase
{
   public void testParse()
   {
      assertEquals(new Locale("en"), LocaleUtil.parse("en"));
      assertEquals(new Locale("en", "CA"), LocaleUtil.parse("en_CA"));
      assertEquals(new Locale("en", "CA", "ON"), LocaleUtil.parse("en_CA_ON"));
      assertEquals(new Locale("en", "CA", "ON_TO"), LocaleUtil.parse("en_CA_ON_TO"));
   }

   public void testGetBase()
   {
      assertEquals("fr_CA", LocaleUtil.getBase("fr_CA_QC", "en"));
      assertEquals("fr", LocaleUtil.getBase("fr_CA", "en"));
      assertEquals("en", LocaleUtil.getBase("fr", "en"));
      assertEquals("en", LocaleUtil.getBase(null, "en"));
   }

   public void testIsBaseOf()
   {
      assertTrue(LocaleUtil.isBaseOf("en_CA", "en_CA_TO"));
      assertTrue(LocaleUtil.isBaseOf("en", "en_CA_TO"));
      assertFalse(LocaleUtil.isBaseOf("en_CA_TO", "en_CA"));
      assertFalse(LocaleUtil.isBaseOf("fr", "en_CA_TO"));
   }
}
