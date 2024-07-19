// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core;

import nexj.test.junit.TestSuite;

import junit.framework.Test;

public class AllTests
{
   public static Test suite() throws Exception
   {
      TestSuite suite = new TestSuite("Test for nexj.core");

      suite.addTest(nexj.core.admin.platform.jboss.AllTests.suite());
      suite.addTest(nexj.core.integration.AllTests.suite());
      suite.addTest(nexj.core.meta.AllTests.suite());
      suite.addTest(nexj.core.rpc.AllTests.suite());
      suite.addTest(nexj.core.runtime.AllTests.suite());
      suite.addTest(nexj.core.scripting.AllTests.suite());
      suite.addTest(nexj.core.util.AllTests.suite());
      suite.addTest("nexj.core.build.AllTests");
      suite.addTest("nexj.core.hmvc.AllTests");
      suite.addTest("nexj.core.view.converter.AllTests");
      suite.addTest("nexj.core.controller.AllTests");
      suite.addTest("nexj.core.controller.flat.AllTests");
      suite.addTest("nexj.core.controller.flat.servlet.AllTests");
      suite.addTest("nexj.core.view.swt.complex.index.AllTests");
      suite.addTest("nexj.core.template.AllTests");
      suite.addTest("nexj.core.testing.AllTests");
      suite.addTest("nexj.core.reporting.AllTests");
      suite.addTest("nexj.core.reporting.jasper.AllTests");
      suite.addTest("nexj.core.model.server.AllTests");
      suite.addTest("nexj.core.view.AllTests");
      suite.addTest("nexj.core.persistence.xml.AllTests");
      suite.addTest("nexj.test.unit.UnitTestSuite");
      suite.addTest(nexj.core.persistence.AllTests.suite());
      //$JUnit-BEGIN$
      //$JUnit-END$
      return suite;
   }
}
