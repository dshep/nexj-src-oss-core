// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.file;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests
{

   public static Test suite()
   {
      TestSuite suite = new TestSuite("Test for nexj.core.rpc.file");
      suite.addTest(nexj.core.rpc.file.ra.AllTests.suite());
      
      //$JUnit-BEGIN$
      //$JUnit-END$
      return suite;
   }

}
