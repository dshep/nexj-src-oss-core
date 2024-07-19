// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.ByteArrayInputStream;
import java.util.Iterator;

import junit.framework.TestCase;

public class MIMEUtilTest extends TestCase
{
   public void testFindMIMEType()
   {
      assertEquals("text/html", MIMEUtil.findMIMEType("html"));
      assertNull(MIMEUtil.findMIMEType("text/null"));
   }

   public void testFindExt()
   {
      assertEquals("jpg", MIMEUtil.findExt("image/jpeg"));
      assertEquals("tif", MIMEUtil.findExt("image/tiff"));
      assertEquals("txt", MIMEUtil.findExt("text/plain"));
      assertEquals("txt", MIMEUtil.findExt("text/Plain; charset=utf-8"));
      assertNull(MIMEUtil.findExt("image/painting"));
   }

   public void testGetContentType()
   {
      assertNull(MIMEUtil.getMIMEType(null));
      assertEquals("application/octet-stream", MIMEUtil.getMIMEType(new Binary((byte[])null)));
      assertEquals("application/octet-stream",
                   MIMEUtil.getMIMEType(new ByteArrayInputStream("abc".getBytes())));
      assertEquals("text/html", MIMEUtil.getMIMEType(new StringBuffer("  <html>abc</html> ")));
      assertEquals("text/html; charset=abc",
                   MIMEUtil.getMIMEType(
                     new String("<!DOCTYPE HTML PLAIN><meta content= charset=abc <body ")));
      assertEquals("text/plain", MIMEUtil.getMIMEType(new String("abcd")));
      assertEquals("text/xml; charset=UTF-8",
                   MIMEUtil.getMIMEType(new StringBuilder("  <xml>abc</xml>")));
   }

   public void testIsSafeMIMEType()
   {
      assertTrue(MIMEUtil.isSafeMIMEType("text/plain"));
      assertFalse(MIMEUtil.isSafeMIMEType("text/html"));
      assertFalse(MIMEUtil.isSafeMIMEType("text/null"));
   }

   public void testIsSafeExt()
   {
      assertTrue(MIMEUtil.isSafeExt("tiff"));
      assertFalse(MIMEUtil.isSafeExt("html"));
      assertFalse(MIMEUtil.isSafeExt("none"));
   }

   public void testIsSafeImageExt()
   {
      assertTrue(MIMEUtil.isSafeImageExt("jpeg"));
      assertFalse(MIMEUtil.isSafeImageExt("tiff"));
   }

   public void testGetSafeImageExtIterator()
   {
      Iterator itr = MIMEUtil.getSafeImageExtIterator();

      assertNotNull(itr.next());
   }

   public void testIsBinaryMIMEType()
   {
      assertFalse(MIMEUtil.isBinaryMIMEType("text/plain"));
      assertFalse(MIMEUtil.isBinaryMIMEType("text/html; charset=utf-8"));
      assertFalse(MIMEUtil.isBinaryMIMEType("text/unknown; charset=utf-8"));
      assertFalse(MIMEUtil.isBinaryMIMEType("x/x; CharSet = utf-8"));
      assertFalse(MIMEUtil.isBinaryMIMEType("x/x; charset=utf-8"));
      assertFalse(MIMEUtil.isBinaryMIMEType("x/x;charset=utf-8"));
      assertTrue(MIMEUtil.isBinaryMIMEType("application/octet-stream"));
      assertTrue(MIMEUtil.isBinaryMIMEType("image/jpeg"));
      assertTrue(MIMEUtil.isBinaryMIMEType("x/x"));
   }

   public void testNormalizeMIMEType()
   {
      assertEquals("text/plain", MIMEUtil.normalizeMIMEType("text/plain"));
      assertEquals("text/plain", MIMEUtil.normalizeMIMEType("text/Plain"));
      assertEquals("text/plain", MIMEUtil.normalizeMIMEType("Text/Plain; charset=utf-8"));
      assertEquals("text/plain", MIMEUtil.normalizeMIMEType("Text/Plain;charset = utf-8"));
   }
}
