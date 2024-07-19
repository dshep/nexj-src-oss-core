// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of
// the Eclipse Public License 1.0
package nexj.core.rpc.http;

import junit.framework.Test;
import nexj.test.junit.TestSuite;

public class AllTests
{

   public static Test suite()
   {
      TestSuite suite = new TestSuite("Test for nexj.core.rpc.http");
      // $JUnit-BEGIN$
      suite.addTestSuite(HTTPAdapterTest.class);
      suite.addTestSuite(HTTPUtilTest.class);
      // $JUnit-END$
      return suite;
   }

}
