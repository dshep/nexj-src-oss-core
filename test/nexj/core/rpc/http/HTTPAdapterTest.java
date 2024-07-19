// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.http;

import nexj.core.rpc.TransferObject;
import nexj.core.scripting.Pair;
import nexj.core.util.MIMEHeaderMap;

import junit.framework.TestCase;

/**
 * Tests the HTTPAdapter.
 */
public class HTTPAdapterTest extends TestCase
{
   /**
    * Tests the static methods responsible for encoding parameters and deciding
    * whether the parameters go in the URL query string or in the request body.
    * @throws Exception If an error occurs.
    */
   public void testParametrize() throws Exception
   {
      Pair result;
      TransferObject params = new TransferObject(2);
      MIMEHeaderMap emptyHeaderMap = new MIMEHeaderMap();
      MIMEHeaderMap formHeaderMap = new MIMEHeaderMap();

      params.setValue("a", "x");
      params.setValue("b", "y");

      formHeaderMap.add("Content-Type", "application/x-www-form-urlencoded");

      // Parameters go in URL (with or without ? on end of input URL)
      result = HTTPAdapter.parametrize("http://www.example.com/test1", null, params, emptyHeaderMap);
      assertEquals("http://www.example.com/test1?a=x&b=y", result.getHead());
      assertNull(result.getTail());
      result = HTTPAdapter.parametrize("http://www.example.com/test1?", null, params, emptyHeaderMap);
      assertEquals("http://www.example.com/test1?a=x&b=y", result.getHead());
      assertNull(result.getTail());

      // Parameters go in body (because using form-encoded content type)
      result = HTTPAdapter.parametrize("http://www.example.com/test1", null, params, formHeaderMap);
      assertEquals("http://www.example.com/test1", result.getHead());
      assertEquals("a=x&b=y", result.getTail());
      result = HTTPAdapter.parametrize("http://www.example.com/test1?", null, params, formHeaderMap);
      assertEquals("http://www.example.com/test1?", result.getHead());
      assertEquals("a=x&b=y", result.getTail());

      // Parameters mixed between URL and body (also try with fragment)
      result = HTTPAdapter.parametrize("http://www.example.com/test1?a=d&c=f", null, params, formHeaderMap);
      assertEquals("http://www.example.com/test1?a=x&c=f", result.getHead());
      assertEquals("b=y", result.getTail());
      result = HTTPAdapter.parametrize("http://www.example.com/test1?a=d&c=f#fragment1", null, params, formHeaderMap);
      assertEquals("http://www.example.com/test1?a=x&c=f#fragment1", result.getHead());
      assertEquals("b=y", result.getTail());

      // Parameters to URL, with defaults
      result = HTTPAdapter.parametrize("http://www.example.com/test1?a=d&c=f", null, params, emptyHeaderMap);
      assertEquals("http://www.example.com/test1?a=x&c=f&b=y", result.getHead());
      assertNull(result.getTail());

      // Parameters to body, but leave question mark at end of URL
      result = HTTPAdapter.parametrize("http://www.example.com/test1?", null, params, formHeaderMap);
      assertEquals("http://www.example.com/test1?", result.getHead());
      assertEquals("a=x&b=y", result.getTail());

      // Don't overwrite body if body already specified
      result = HTTPAdapter.parametrize("http://www.example.com/test1", "Hello, World!", params, formHeaderMap);
      assertEquals("http://www.example.com/test1", result.getHead());
      assertEquals("Hello, World!", result.getTail());

      // Handles fragments
      result = HTTPAdapter.parametrize("http://www.example.com/test1#f1", null, params, emptyHeaderMap);
      assertEquals("http://www.example.com/test1?a=x&b=y#f1", result.getHead());
      assertNull(result.getTail());
      result = HTTPAdapter.parametrize("http://www.example.com/test1#", null, params, emptyHeaderMap);
      assertEquals("http://www.example.com/test1?a=x&b=y#", result.getHead());
      assertNull(result.getTail());
   }
}
