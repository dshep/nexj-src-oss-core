// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.service;

import junit.framework.Test;
import junit.framework.TestSuite;

/**
 * 
 */
public class AllTests
{

   public static Test suite()
   {
      TestSuite suite = new TestSuite("Test for nexj.core.meta.integration.service");
      //$JUnit-BEGIN$
      suite.addTestSuite(ServiceTest.class);
      //$JUnit-END$
      return suite;
   }

}
