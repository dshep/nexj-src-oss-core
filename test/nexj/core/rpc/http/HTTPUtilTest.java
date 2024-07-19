// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.http;

import nexj.core.util.HTTP;

import junit.framework.TestCase;

/**
 * Tests the utility methods in HTTPUtil.
 */
public class HTTPUtilTest extends TestCase
{
   /**
    * Test for the getHostURI() method.
    */
   public void testGetHostURI() throws Exception
   {
      assertNull(HTTPUtil.getHostURI(null));
      assertNull(HTTPUtil.getHostURI(""));
      assertNull(HTTPUtil.getHostURI(" "));
      assertNull(HTTPUtil.getHostURI("http"));
      assertNull(HTTPUtil.getHostURI("http:"));
      assertNull(HTTPUtil.getHostURI("http://"));

      assertEquals("http://example.com", HTTPUtil.getHostURI("http://example.com"));
      assertEquals("http://example.com", HTTPUtil.getHostURI("http://example.com/"));
      assertEquals("http://example.com", HTTPUtil.getHostURI("http://example.com/path"));
      assertEquals("https://example.com", HTTPUtil.getHostURI("https://example.com/path/subpath/"));
      assertEquals("http://example.com:42", HTTPUtil.getHostURI("http://example.com:42/path/subpath/"));

      assertNull(HTTPUtil.getHostURI("/path"));
      assertNull(HTTPUtil.getHostURI("/path/"));
      assertNull(HTTPUtil.getHostURI("/path/subpath"));
      assertNull(HTTPUtil.getHostURI("file.ext"));
   }

   /**
    * Test for the getContextPath() method.
    */
   public void testGetContextPath() throws Exception
   {
      assertNull(HTTP.getContextPath(null));
      assertNull(HTTP.getContextPath(""));
      assertNull(HTTP.getContextPath(" "));
      assertNull(HTTP.getContextPath("http://"));

      assertNull(HTTP.getContextPath("http://example.com"));
      assertNull(HTTP.getContextPath("http://example.com:80"));

      assertEquals("/", HTTP.getContextPath("http://example.com/"));
      assertEquals("/", HTTP.getContextPath("http://example.com:80/"));

      assertEquals("/path", HTTP.getContextPath("/path"));
      assertEquals("/path", HTTP.getContextPath("/path/"));
      assertEquals("/path", HTTP.getContextPath("http://example.com/path"));
      assertEquals("/path", HTTP.getContextPath("http://example.com/path/"));
      assertEquals("/path/subpath", HTTP.getContextPath("http://example.com/path/subpath"));
      assertEquals("/path/subpath", HTTP.getContextPath("http://example.com/path/subpath/"));
   }
}
