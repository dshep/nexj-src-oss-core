// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

import junit.framework.Test;
import junit.framework.TestSuite;

/**
 * 
 */
public class AllTests
{

   public static Test suite()
   {
      TestSuite suite = new TestSuite("Test for nexj.core.meta.workflow");
      //$JUnit-BEGIN$
      suite.addTestSuite(FlowMacroTest.class);
      suite.addTestSuite(WorkflowTest.class);
      suite.addTestSuite(StepTest.class);
      suite.addTestSuite(StateTest.class);
      //$JUnit-END$
      return suite;
   }
}