// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests
{
   public static Test suite()
   {
      TestSuite suite = new TestSuite("Test for nexj.core.integration");
      suite.addTest(nexj.core.integration.format.csv.AllTests.suite());
      suite.addTest(nexj.core.integration.format.fixed.AllTests.suite());
      suite.addTest(nexj.core.integration.format.hl7.AllTests.suite());
      suite.addTest(nexj.core.integration.format.json.AllTests.suite());
      suite.addTest(nexj.core.integration.format.object.AllTests.suite());
      suite.addTest(nexj.core.integration.format.vcard.AllTests.suite());
      suite.addTest(nexj.core.integration.format.xml.AllTests.suite());
      suite.addTest(nexj.core.integration.format.zip.AllTests.suite());
      suite.addTestSuite(nexj.core.integration.io.DebugInputTest.class);
      //$JUnit-BEGIN$
      suite.addTestSuite(TransformerTest.class);
      //$JUnit-END$
      return suite;
   }
}
