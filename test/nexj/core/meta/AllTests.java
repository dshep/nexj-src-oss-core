// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import nexj.test.junit.TestSuite;

import junit.framework.Test;

public class AllTests
{

   public static Test suite()
   {
      TestSuite suite = new TestSuite("Test for nexj.core.meta");
      suite.addTest(nexj.core.meta.persistence.sql.AllTests.suite());
      suite.addTest(nexj.core.meta.workflow.AllTests.suite());
      suite.addTest(nexj.core.meta.integration.AllTests.suite());
      suite.addTest(nexj.core.meta.xml.AllTests.suite());

      suite.addTest("nexj.core.meta.ui.AllTests");
      suite.addTest("nexj.core.meta.xmi.AllTests");
      suite.addTest("nexj.core.meta.etl.AllTests");

      if (!suite.addTestSuite("nexj.core.meta.EnterpriseMetadataLoadingFailureTest"))
      {
         suite.addTestSuite("nexj.core.meta.MetadataLoadingFailureTest");
      }

      //$JUnit-BEGIN$
      suite.addTestSuite(RepositoryTest.class);
      suite.addTestSuite(SelectorTest.class);
      suite.addTestSuite(PrimitivePrivilegeTest.class);
      suite.addTestSuite(ClassAspectTest.class);
      suite.addTestSuite(EventTest.class);
      suite.addTestSuite(PrimitiveTest.class);
      suite.addTestSuite(ActionTest.class);
      suite.addTestSuite(ComponentTest.class);
      suite.addTestSuite(PrivilegeGroupTest.class);
      suite.addTestSuite(ArgumentTest.class);
      suite.addTestSuite(AttributeTest.class);
      suite.addTestSuite(AttributeDependencyTest.class);
      suite.addTestSuite(MetaclassTest.class);
      suite.addTestSuite(MetaLoaderDispatcherTest.class);
      //$JUnit-END$
      return suite;
   }
}