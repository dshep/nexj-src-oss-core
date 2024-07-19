package nexj.core.rpc.timer;

import junit.framework.Test;
import junit.framework.TestSuite;

/**
 * 
 */
public class AllTests
{

   public static Test suite()
   {
      TestSuite suite = new TestSuite(AllTests.class.getName());
      //$JUnit-BEGIN$
      suite.addTestSuite(TimerTest.class);
      suite.addTestSuite(TimerAssumptionTest.class);
      //$JUnit-END$
      return suite;
   }

}
