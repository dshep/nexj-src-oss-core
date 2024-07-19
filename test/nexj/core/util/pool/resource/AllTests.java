package nexj.core.util.pool.resource;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests
{

   public static Test suite()
   {
      TestSuite suite = new TestSuite(AllTests.class.getName());
      //$JUnit-BEGIN$
      suite.addTestSuite(GenericResourcePoolTest.class);
      suite.addTestSuite(MonitoredGenericResourcePoolTest.class);
      //$JUnit-END$
      return suite;
   }

}
