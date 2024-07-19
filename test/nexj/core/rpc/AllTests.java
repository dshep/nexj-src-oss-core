// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc;

import junit.framework.Test;

import nexj.test.junit.TestSuite;

public class AllTests
{

   public static Test suite()
   {
      TestSuite suite = new TestSuite("Test for nexj.core.rpc");
      suite.addTest(nexj.core.rpc.file.AllTests.suite());
      suite.addTest(nexj.core.rpc.http.AllTests.suite());
      suite.addTest(nexj.core.rpc.soap.AllTests.suite());
      suite.addTest(nexj.core.rpc.text.AllTests.suite());
      suite.addTest(nexj.core.rpc.json.AllTests.suite());
      suite.addTest(nexj.core.rpc.xml.AllTests.suite());
      suite.addTest(nexj.core.rpc.timer.AllTests.suite());
      suite.addTest(nexj.core.rpc.udp.AllTests.suite());

      suite.addTest("nexj.core.rpc.mapi.AllTests");
      suite.addTest("nexj.core.rpc.sql.AllTests");

      if (new GenericServerTest("").isEnabled())
      {
         suite.addTest(new junit.framework.TestSuite(GenericServerTest.class));
      }

      //$JUnit-BEGIN$
      suite.addTest(new junit.framework.TestSuite(RequestTest.class));
      suite.addTest(new junit.framework.TestSuite(TransferObjectTest.class));
      //$JUnit-END$
      return suite;
   }
}
