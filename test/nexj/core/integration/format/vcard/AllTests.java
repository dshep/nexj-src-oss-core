// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.vcard;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests
{

   public static Test suite()
   {
      TestSuite suite = new TestSuite("Test for nexj.core.integration.format.vcard");
      suite.addTestSuite(VCardMessageFormatterTest.class);
      //$JUnit-BEGIN$
      //$JUnit-END$
      return suite;
   }

}
