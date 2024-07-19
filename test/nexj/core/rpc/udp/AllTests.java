package nexj.core.rpc.udp;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests
{

   public static Test suite()
   {
      TestSuite suite = new TestSuite(AllTests.class.getName());
      //$JUnit-BEGIN$
      suite.addTestSuite(UDPTest.class);
      suite.addTestSuite(UDPMulticastTest.class);
      suite.addTestSuite(UDPAssumptionTest.class);
      //$JUnit-END$
      return suite;
   }
}
