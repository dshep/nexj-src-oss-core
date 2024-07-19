// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.zip;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests
{

   public static Test suite()
   {
      TestSuite suite = new TestSuite("Test for nexj.core.integration.format.zip");
      suite.addTestSuite(ZipMessageFormatterTest.class);
      suite.addTestSuite(ZipMessageParserTest.class);
      //$JUnit-BEGIN$
      //$JUnit-END$
      return suite;
   }

}
