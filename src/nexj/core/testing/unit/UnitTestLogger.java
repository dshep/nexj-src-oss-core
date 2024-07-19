package nexj.core.testing.unit;

import nexj.core.meta.testing.unit.UnitTest;
import nexj.core.meta.testing.unit.UnitTestCase;

/**
 * Interface for logging progress while executing unit test cases.
 */
public interface UnitTestLogger
{
   /**
    * A set of unit tests has started execution.
    */
   public void begin();

   /**
    * A set of unit tests has ended execution
    * 
    * @param nUnitTestCount The number of unit tests run.
    * @param nTestCaseCount The number of test cases run.
    * @param nErrorCount The number of test cases that encountered errors.
    * @param nFailureCount The number of failed test cases
    */
   public void end(int nUnitTestCount, int nTestCaseCount, int nErrorCount, int nFailureCount);

   /**
    * A set of unit tests has encountered an error.
    * 
    * @param t The error
    */
   public void err(Throwable t);

   /**
    * A unit test has started execution.
    * 
    * @param test The unit test
    * @param sTestArguments String representation of arguments to the test; null if none.
    */
   public void begin(UnitTest test, String sTestArguments);

   /**
    * A unit test has completed execution.
    * 
    * @param test The unit test
    * @param sTestArguments String representation of arguments to the test; null if none.
    */
   public void end(UnitTest test, String sTestArguments);

   /**
    * A unit test has encountered an error.
    * 
    * @param test The unit test
    * @param t The error
    */
   public void err(UnitTest test, Throwable t);

   /**
    * A test case has started execution.
    * 
    * @param testCase The test case
    */
   public void begin(UnitTestCase testCase);

   /**
    * A test case has completed execution successfully.
    * 
    * @param testCase The test case
    */
   public void end(UnitTestCase testCase);

   /**
    * A test case has encountered an error.
    * 
    * @param testCase The test case
    * @param t The error
    */
   public void err(UnitTestCase testCase, Throwable t);

   /**
    * A test case has failed. The actual result is not the expected result.
    * 
    * @param testCase The test case
    * @param t The assertion failure
    */
   public void fail(UnitTestCase testCase, UnitTestAssertionException t);
}
