// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;

import junit.framework.Test;
import junit.framework.TestSuite;

/**
 * 
 */
public class AllTests
{

   public static Test suite()
   {
      TestSuite suite = new TestSuite("Test for nexj.core.persistence");
      suite.addTest(nexj.core.persistence.operator.AllTests.suite());
      //$JUnit-BEGIN$
      suite.addTestSuite(HeterogeneousQueryStepTest.class);
      suite.addTestSuite(OIDTest.class);
      //$JUnit-END$
      suite.addTest(nexj.core.persistence.file.AllTests.suite());
      suite.addTest(nexj.core.persistence.virtual.AllTests.suite());
      suite.addTest(nexj.core.persistence.sql.AllTests.suite());

      return suite;
   }
}
