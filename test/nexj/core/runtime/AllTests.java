// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

import nexj.test.junit.TestSuite;

import junit.framework.Test;

public class AllTests
{

   public static Test suite()
   {
      TestSuite suite = new TestSuite("Test for nexj.core.runtime");
      
      suite.addTest("nexj.core.runtime.sys.AllTests");
      //$JUnit-BEGIN$
      suite.addTestSuite(TestFormat.class);
      suite.addTestSuite(InstanceArrayListTest.class);
      suite.addTestSuite(InstanceTest.class);
      suite.addTestSuite(InvocationContextTest.class);
      suite.addTestSuite(WeakInstanceArrayListTest.class);
      suite.addTestSuite(GenericSerializablePropertyMapTest.class);
      //$JUnit-END$
      return suite;
   }
}
