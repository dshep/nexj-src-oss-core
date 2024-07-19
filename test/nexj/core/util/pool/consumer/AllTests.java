package nexj.core.util.pool.consumer;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests
{

   public static Test suite()
   {
      TestSuite suite = new TestSuite(AllTests.class.getName());
      //$JUnit-BEGIN$
      suite.addTestSuite(GenericConsumerPoolTest.class);
      suite.addTestSuite(MonitoredGenericConsumerPoolTest.class);
      //$JUnit-END$
      return suite;
   }

}
