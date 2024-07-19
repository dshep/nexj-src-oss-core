// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.util.Arrays;

import junit.framework.TestCase;

public class StringUtilTest extends TestCase
{
   public void testCompareVersions()
   {
      assertEquals(0, StringUtil.compareVersions("", ""));
      assertEquals(3, StringUtil.compareVersions("123", ""));
      assertEquals(-3, StringUtil.compareVersions("", "123"));
      assertEquals(-1, StringUtil.compareVersions("123", "456"));
      assertEquals(1, StringUtil.compareVersions("456", "123"));
      assertEquals(-3, StringUtil.compareVersions("abc", "123"));
      assertEquals(3, StringUtil.compareVersions("123", "abc"));
      assertEquals(-3, StringUtil.compareVersions("123", "123abc"));
      assertEquals(-3, StringUtil.compareVersions("123", "abc123"));
      assertEquals(-3, StringUtil.compareVersions("123.abc", "123abc"));
      assertEquals(-1, StringUtil.compareVersions("123abc.def", "123abc.defg"));
      assertEquals(0, StringUtil.compareVersions("123abc.def", "123abc.def"));
      assertEquals(-1, StringUtil.compareVersions("123abc.345.def", "123abc.345.defg"));
      assertEquals(-1, StringUtil.compareVersions("123abc.345.def", "123abc.3456.def"));
      assertEquals(0, StringUtil.compareVersions("123abc345def", "123abc345def"));
      assertEquals(3, StringUtil.compareVersions("123abc345def", "123abc345"));
      assertEquals(3, StringUtil.compareVersions("123abc345def", "abc345def"));
      assertEquals(0, StringUtil.compareVersions("abc", "AbC"));
      assertEquals(-2, StringUtil.compareVersions("abc", "abc.1"));
   }

   public void testIntern()
   {
      assertNull(StringUtil.intern(null));

      String s = StringUtil.intern("abc");

      assertEquals("abc", s);
      assertSame(s, StringUtil.intern("abc"));
      assertSame(s, StringUtil.intern(new String(new char[]{'a', 'b', 'c'})));
   }

   public void testSplit()
   {
      String[] results = StringUtil.split("a b c d e", ' ');
      
      assertEquals(true, Arrays.equals(results, new String[]{"a", "b", "c", "d", "e"}));
      
      results = StringUtil.split("a !b !c !d !e", '!');
      
      assertEquals(true, Arrays.equals(results, new String[]{"a ", "b ", "c ", "d ", "e"}));
      
      results = StringUtil.split("a !b !c !d !e", '@');
      
      assertEquals(true, Arrays.equals(results, new String[]{"a !b !c !d !e"}));
      
      results = StringUtil.split("a\\!b\\!c\\!d\\!e", '\\');
      
      assertEquals(true, Arrays.equals(results, new String[]{"a", "!b", "!c", "!d", "!e"}));
      
      results = StringUtil.split("a/b/c/d/e/", '/');
      
      assertEquals(true, Arrays.equals(results, new String[]{"a", "b", "c", "d", "e"}));
      
      results = StringUtil.split("a/b\\c/d\\e/f", '/');
      
      assertEquals(true, Arrays.equals(results, new String[]{"a", "b\\c", "d\\e", "f"}));

      results = StringUtil.split("  a b c  d   ef   g  ");

      assertTrue(Arrays.equals(results, new String[]{"a", "b", "c", "d", "ef", "g"}));

      results = StringUtil.split("a b c  d   ef   g");

      assertTrue(Arrays.equals(results, new String[]{"a", "b", "c", "d", "ef", "g"}));

      results = StringUtil.split("a");

      assertTrue(Arrays.equals(results, new String[]{"a"}));

      results = StringUtil.split("");

      assertTrue(Arrays.equals(results, new String[0]));
   }

   /**
    * Test for the trimToNull() method.
    */
   public void testTrimToNull() throws Exception
   {
      assertNull(StringUtil.trimToNull(null));
      assertNull(StringUtil.trimToNull(""));
      assertNull(StringUtil.trimToNull(" "));
      assertNull(StringUtil.trimToNull("   \t\n   "));

      assertEquals("abc", StringUtil.trimToNull(" abc "));
      assertEquals("a bc", StringUtil.trimToNull(" a bc"));
   }
   
   /**
    * Test for the equalEOL method.
    */
   public void testEqualEOL()
   {
      // basic comparisons
      assertTrue(StringUtil.equalEOL(null, null));
      assertTrue(StringUtil.equalEOL(null, ""));
      assertTrue(StringUtil.equalEOL("", null));
      assertTrue(!StringUtil.equalEOL("left", null));
      assertTrue(!StringUtil.equalEOL(null, "right"));
      assertTrue(!StringUtil.equalEOL("left", "right"));
      assertTrue(!StringUtil.equalEOL("righ", "right"));
      assertTrue(!StringUtil.equalEOL("left", "lef"));
      assertTrue(StringUtil.equalEOL("left", "left"));

      String sSelf = "self";

      assertTrue(StringUtil.equalEOL(sSelf, sSelf));
      
      // end of line tests
      assertTrue(StringUtil.equalEOL("\rone\ntwo\n\r\r\nthree\r","\rone\r\ntwo\n\r\n\nthree\n"));
      assertTrue(StringUtil.equalEOL("\r\none\ntwo\n\r\r\nthree\r","\rone\ntwo\n\n\nthree\r\n"));
      assertTrue(!StringUtil.equalEOL("\rown","\rpwn"));
      assertTrue(!StringUtil.equalEOL("one\r\rtwo","one\r\ntwo"));
      assertTrue(!StringUtil.equalEOL("one\n\rtwo","one\r\ntwo"));
      assertTrue(!StringUtil.equalEOL("one\r\ntwo","one\n\ntwo"));
      assertTrue(!StringUtil.equalEOL("one\r\r","one\r\n"));
      assertTrue(!StringUtil.equalEOL("one\n\r","one\r\n"));
      assertTrue(!StringUtil.equalEOL("one\r\ntwo","one\n\ntwo"));
   }

   public void testToCaption()
   {
      assertEquals("first name", StringUtil.toCaption("a:firstName", ':', false));
      assertEquals("First Name", StringUtil.toCaption("a:firstName", ':', true));
      assertEquals("my url name", StringUtil.toCaption("a.myURLName", '.', false));
      assertEquals("My URL Name", StringUtil.toCaption("myURLName", '.', true));
   }
}
