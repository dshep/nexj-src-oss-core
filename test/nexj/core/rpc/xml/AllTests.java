// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.xml;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests
{

   public static Test suite()
   {
      TestSuite suite = new TestSuite("Test for nexj.core.rpc.xml");
      //$JUnit-BEGIN$
      suite.addTestSuite(WADLGeneratorTest.class);
      suite.addTestSuite(WSDLGeneratorTest.class);
      //suite.addTestSuite(XMLHTTPServerTest.class);
      suite.addTestSuite(XMLMarshallerTest.class);
      suite.addTestSuite(XMLMetatypeTest.class);
      suite.addTestSuite(XMLUnmarshallerTest.class);
      suite.addTestSuite(XSDGeneratorTest.class);
      //$JUnit-END$
      return suite;
   }
}
