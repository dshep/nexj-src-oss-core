// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.file.ra;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests
{

   public static Test suite()
   {
      TestSuite suite = new TestSuite("Test for nexj.core.rpc.file.ra");
      //$JUnit-BEGIN$
      suite.addTestSuite(AppendingJournalTest.class);
      suite.addTestSuite(FileConnectionTest.class);
      suite.addTestSuite(FileXAResourceTest.class);
      suite.addTestSuite(PersistenceFileConnectionTest.class);
      suite.addTestSuite(ThreadFileLockTest.class);
      //$JUnit-END$
      return suite;
   }

}
