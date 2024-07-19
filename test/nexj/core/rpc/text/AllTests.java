// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.text;

import nexj.test.junit.TestSuite;

import junit.framework.Test;

public class AllTests
{
   public static Test suite()
   {
      TestSuite suite = new TestSuite("Test for nexj.core.rpc.text");
      
      suite.addTestSuite("nexj.core.rpc.text.ClientStateMarshallerTest");
      //$JUnit-BEGIN$
      suite.addTest(new junit.framework.TestSuite(TextMarshallerTest.class));
      suite.addTest(new junit.framework.TestSuite(RefTextMarshallerTest.class));
      //$JUnit-END$
      return suite;
   }
}
