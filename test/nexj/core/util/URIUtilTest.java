// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.net.URI;

import junit.framework.TestCase;

public class URIUtilTest extends TestCase
{
   /**
    * @see junit.framework.TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();
   }

   public void testParse() throws Exception
   {
      URI uri = URIUtil.parse("http://www.nexj.com/sync/{123-345}\u1234[+] uber-x0rz@;,");

      assertEquals("/sync/%7B123-345%7D%E1%88%B4%5B%20%5D%20uber-x0rz@;,", uri.getRawPath());
   }

   public void testHostPort() throws Exception
   {
      assertNull(URIUtil.getHostPort(null));
      assertNull(URIUtil.getHostPort(""));
      assertNull(URIUtil.getHostPort("http"));
      assertNull(URIUtil.getHostPort("http://"));
      assertNull(URIUtil.getHostPort("/"));
      assertNull(URIUtil.getHostPort("/root"));
      assertNull(URIUtil.getHostPort("/root/"));
      assertNull(URIUtil.getHostPort("/root/sub"));

      assertEquals("host", URIUtil.getHostPort("http://host"));
      assertEquals("host", URIUtil.getHostPort("http://host/"));
      assertEquals("host", URIUtil.getHostPort("http://host/root"));
      assertEquals("host.domain", URIUtil.getHostPort("http://host.domain"));
      assertEquals("host.domain", URIUtil.getHostPort("http://host.domain/"));
      assertEquals("host.domain:8080", URIUtil.getHostPort("http://host.domain:8080"));
      assertEquals("host.domain:8080", URIUtil.getHostPort("http://host.domain:8080/"));
      assertEquals("host.domain:8080", URIUtil.getHostPort("http://host.domain:8080/root"));
      assertEquals("host:8080", URIUtil.getHostPort("http://host:8080"));
      assertEquals("host:8080", URIUtil.getHostPort("http://host:8080/"));
   }
}
