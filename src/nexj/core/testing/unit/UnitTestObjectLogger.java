package nexj.core.testing.unit;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import nexj.core.meta.testing.unit.UnitTest;
import nexj.core.meta.testing.unit.UnitTestCase;
import nexj.core.util.Logger;
import nexj.core.util.StringUtil;

/**
 * Abstract base class for logging message objects.
 */
public class UnitTestObjectLogger implements UnitTestLogger
{
   // constants
   
   /**
    * Constant indicating a unit test or test case is starting.
    */
   public final static byte START = 1;

   /**
    * Constant indicating a test case failed with a UnitTestAssertionException.
    */
   public final static byte FAIL = 2;

   /**
    * Constant indicating a test run, unit test, or test case is finished normally.
    */
   public final static byte END = 3;

   /**
    * Constant indicating a test run, unit test, or test case encountered an unexpected error.
    */
   public final static byte ERR = 4;

   // attributes

   /**
    * The start time of the test run.
    */
   private long m_lSuiteStartTime;

   /**
    * The start time of the current unit test.
    */
   private long m_lUnitStartTime;

   /**
    * The start time of the current test case.
    */
   private long m_lCaseStartTime;

   // associations

   /**
    * The current unit test.
    */
   protected UnitTest m_utest;

   /**
    * The class logger.
    */
   private final static Logger s_logger = Logger.getLogger(UnitTestLogger.class);

   // operations

   /**
    * Log a message object
    * @param msg The message to log
    */
   protected void log(LogMessage msg)
   {
      switch (msg.getStatus())
      {
         case START:
            if (!StringUtil.isEmpty(msg.getName()))
            {
               if (s_logger.isInfoEnabled())
               {
                  String sName = msg.getName();

                  if (msg instanceof UnitTestMsg)
                  {
                     String sArgs = ((UnitTestMsg)msg).getTestArguments();

                     sName = (sArgs == null) ? sName : (sName + sArgs);
                  }

                  s_logger.info("Starting " + msg.getTypeCaption() + " \"" +
                     sName + "\"", msg.getCause());
               }
            }

            break;

         case END:
            if (msg instanceof SuiteMsg)
            {
               SuiteMsg suiteMsg = (SuiteMsg)msg;

               if (s_logger.isInfoEnabled())
               {
                  s_logger.info("Completed " + suiteMsg.getTestCaseCount() + " test case(s) in " +
                     suiteMsg.getUnitTestCount() + " unit test(s) with " + suiteMsg.getFailureCount() +
                     " failure(s) and " + suiteMsg.getErrorCount() + " error(s)");

                  s_logger.info("Total time " + (suiteMsg.getDuration() / 1000.000) + " sec");
               }
            }
            else
            {
               if (s_logger.isInfoEnabled())
               {
                  String sName = msg.getName();

                  if (msg instanceof UnitTestMsg)
                  {
                     String sArgs = ((UnitTestMsg)msg).getTestArguments();

                     sName = (sArgs == null) ? sName : (sName + sArgs);
                  }

                  s_logger.info("Completed " + msg.getTypeCaption() + " \"" + sName + "\"");
               }
            }

            break;

         case FAIL:
            s_logger.error("Assertion failed in " + msg.getTypeCaption() + " \"" +
               msg.getName() + "\"", msg.getCause());

            break;

         case ERR:
            if (StringUtil.isEmpty(msg.getName()))
            {
               s_logger.error("Unexpected error", msg.getCause());
            }
            else
            {
               s_logger.error("Unexpected error in " + msg.getTypeCaption() + " \"" +
                  msg.getName() + "\"", msg.getCause());
            }

            break;
      }
   }

   /**
    * @see nexj.core.testing.unit.UnitTestLogger#begin()
    */
   public void begin()
   {
      m_lSuiteStartTime = System.currentTimeMillis();
   }

   /**
    * @see nexj.core.testing.unit.UnitTestLogger#begin(nexj.core.meta.testing.unit.UnitTest, java.lang.String)
    */
   public void begin(UnitTest test, String sTestArguments)
   {
      m_utest = test;
      m_lUnitStartTime = System.currentTimeMillis();
      log(new UnitTestMsg(test.getName(), START, null, -1, sTestArguments));
   }

   /**
    * @see nexj.core.testing.unit.UnitTestLogger#begin(nexj.core.meta.testing.unit.UnitTestCase)
    */
   public void begin(UnitTestCase testCase)
   {
      m_lCaseStartTime = System.currentTimeMillis();
      log(new UnitTestCaseMsg(testCase.getName(), m_utest.getName(), START, null, -1));
   }

   /**
    * @see nexj.core.testing.unit.UnitTestLogger#end(int, int, int, int)
    */
   public void end(int nTestCaseCount, int nUnitTestCount, int nErrorCount, int nFailureCount)
   {
      log(new SuiteMsg(END, null, System.currentTimeMillis() - m_lSuiteStartTime, nTestCaseCount, nUnitTestCount, nErrorCount,
         nFailureCount));
   }

   /**
    * @see nexj.core.testing.unit.UnitTestLogger#end(nexj.core.meta.testing.unit.UnitTest, java.lang.String)
    */
   public void end(UnitTest test, String sTestArguments)
   {
      m_utest = null;
      log(new UnitTestMsg(test.getName(), END, null, System.currentTimeMillis() - m_lUnitStartTime, sTestArguments));
   }

   /**
    * @see nexj.core.testing.unit.UnitTestLogger#end(nexj.core.meta.testing.unit.UnitTestCase)
    */
   public void end(UnitTestCase testCase)
   {
      log(new UnitTestCaseMsg(testCase.getName(), m_utest.getName(), END, null, System.currentTimeMillis() - m_lCaseStartTime));
      m_lCaseStartTime = 0;
   }

   /**
    * @see nexj.core.testing.unit.UnitTestLogger#err(java.lang.Throwable)
    */
   public void err(Throwable t)
   {
      log(new SuiteMsg(ERR, t, System.currentTimeMillis() - m_lSuiteStartTime, -1, -1, -1, -1));
   }

   /**
    * @see nexj.core.testing.unit.UnitTestLogger#err(nexj.core.meta.testing.unit.UnitTest, java.lang.Throwable)
    */
   public void err(UnitTest test, Throwable t)
   {
      log(new UnitTestMsg(test.getName(), ERR, t, System.currentTimeMillis() - m_lUnitStartTime));
   }

   /**
    * @see nexj.core.testing.unit.UnitTestLogger#err(nexj.core.meta.testing.unit.UnitTestCase, java.lang.Throwable)
    */
   public void err(UnitTestCase testCase, Throwable t)
   {
      log(new UnitTestCaseMsg(testCase.getName(), m_utest.getName(), ERR, t, (m_lCaseStartTime > 0) ? System.currentTimeMillis() - m_lCaseStartTime : 0));
      m_lCaseStartTime = 0;
   }

   /**
    * @see nexj.core.testing.unit.UnitTestLogger#fail(nexj.core.meta.testing.unit.UnitTestCase,
    *      nexj.core.testing.unit.UnitTestAssertionException)
    */
   public void fail(UnitTestCase testCase, UnitTestAssertionException t)
   {
      log(new UnitTestCaseMsg(testCase.getName(), m_utest.getName(), FAIL, t, System.currentTimeMillis() - m_lCaseStartTime));
      m_lCaseStartTime = 0;
   }

   /**
    * Base class for the log messages.
    */
   public abstract static class LogMessage implements Serializable
   {
      private final static long serialVersionUID = 1844328905624898850L;

      /**
       * The message status.
       */
      private final byte m_nStatus;

      /**
       * The duration of execution.
       */
      private final long m_lDuration;

      /**
       * The name of the item this message is about.
       */
      private final String m_sName;

      /**
       * The cause of this message.
       */
      private transient Throwable m_cause;

      /**
       * A list representing the causes of this message.
       */
      private List m_exceptionList;

      /**
       * Constructor for base LogMessage object
       * 
       * @param sName Name of the relevant element
       * @param nStatus Type of message
       * @param cause Optional cause of this message
       * @param lDuration Optional number of milliseconds to indicate a duration
       */
      public LogMessage(String sName, byte nStatus, Throwable cause, long lDuration)
      {
         m_nStatus = nStatus;
         m_lDuration = lDuration;
         m_sName = sName;
         
         setCause(cause);
      }

      /**
       * @return the number of exceptions in this message
       */
      public int getExceptionCount()
      {
         return m_exceptionList.size() / 3;
      }

      /**
       * @return the class name of the exception at the specified position
       */
      public String getExceptionClassName(int nOrdinal)
      {
         return (String)m_exceptionList.get(0 + nOrdinal * 3);
      }

      /**
       * @return the message of the exception at the specified position
       */
      public String getExceptionMessage(int nOrdinal)
      {
         return (String)m_exceptionList.get(1 + nOrdinal * 3);
      }

      /**
       * @return the stack trace for the exception at the specified position
       */
      public StackTraceElement[] getExceptionStackTrace(int nOrdinal)
      {
         return (StackTraceElement[])m_exceptionList.get(2 + nOrdinal * 3);
      }

      /**
       * @return the status
       */
      public byte getStatus()
      {
         return m_nStatus;
      }

      /**
       * @return the name
       */
      public String getName()
      {
         return m_sName;
      }

      /**
       * @return the cause
       */
      public Throwable getCause()
      {
         return m_cause;
      }

      /**
       * Stores the cause
       */
      public void setCause(Throwable cause)
      {
         m_cause = cause;
         m_exceptionList = new ArrayList();
         
         while (cause != null)
         {
            m_exceptionList.add(cause.getClass().getName());
            m_exceptionList.add(cause.getLocalizedMessage());
            m_exceptionList.add(cause.getStackTrace());
            
            cause = cause.getCause();
         }
      }

      /**
       * @return the lDuration
       */
      public long getDuration()
      {
         return m_lDuration;
      }

      /**
       * @return string representation of this message's status code
       */
      protected String getStatusString()
      {
         switch (m_nStatus)
         {
            case START:
               return "START";

            case END:
               return "END";

            case FAIL:
               return "FAIL";

            case ERR:
               return "ERR";

            default:
               return "UNKNOWN STATUS " + m_nStatus;
         }
      }

      /**
       * @return The object type caption.
       */
      public abstract String getTypeCaption();

      /**
       * @return The hierarchy level (0 is root).
       */
      public abstract int getLevel();

      public String toString()
      {
         String sException = (getExceptionCount() > 0) ? getExceptionClassName(0) : "";
         
         return getStatusString() + ',' + m_sName + ',' + m_lDuration + ',' + sException;
      }
   }

   /**
    * Class for test run messages.
    */
   public static class SuiteMsg extends LogMessage
   {
      private final static long serialVersionUID = 678407336910704736L;

      /**
       * Number of unit tests run
       */
      private int m_nUnitTestCount;

      /**
       * Number of test cases run
       */
      private int m_nTestCaseCount;

      /**
       * Number of test cases ended in error
       */
      private int m_nErrorCount;

      /**
       * Number of test cases failed
       */
      private int m_nFailureCount;

      /**
       * @param nStatus The type of message
       * @param cause Optional throwable if this message is ERR status
       * @param lDuration Optional number of milliseconds elapsed if this message indicates a test run has ended
       * @param nTestCaseCount The number of test cases run
       * @param nUnitTestCount The number of unit tests run
       * @param nErrorCount The number of test cases that ended in error
       * @param nFailureCount The number of test cases that failed
       */
      public SuiteMsg(byte nStatus, Throwable cause, long lDuration, int nTestCaseCount, int nUnitTestCount, int nErrorCount,
         int nFailureCount)
      {
         super("", nStatus, cause, lDuration);
         m_nTestCaseCount = nTestCaseCount;
         m_nUnitTestCount = nUnitTestCount;
         m_nErrorCount = nErrorCount;
         m_nFailureCount = nFailureCount;
      }

      /**
       * @see nexj.core.testing.unit.UnitTestObjectLogger.LogMessage#getTypeCaption()
       */
      public String getTypeCaption()
      {
         return "test run";
      }

      /**
       * @see nexj.core.testing.unit.UnitTestObjectLogger.LogMessage#getLevel()
       */
      public int getLevel()
      {
         return 0;
      }

      /**
       * @return the testCaseCount
       */
      public int getTestCaseCount()
      {
         return m_nTestCaseCount;
      }

      /**
       * @return the unitTestCount
       */
      public int getUnitTestCount()
      {
         return m_nUnitTestCount;
      }

      /**
       * @return the errorCount
       */
      public int getErrorCount()
      {
         return m_nErrorCount;
      }

      /**
       * @return the failureCount
       */
      public int getFailureCount()
      {
         return m_nFailureCount;
      }
   }

   /**
    * Class for messages about a unit test.
    */
   public static class UnitTestMsg extends LogMessage
   {
      // constants

      /**
       * The serialization version.
       */
      private final static long serialVersionUID = -4726284064117128181L;

      // attributes

      /**
       * String representation of the test arguments; null if no arguments.
       */
      protected String m_sTestArguments;

      // constructors

      /**
       * @param sName The name of the unit test
       * @param nStatus The type of message this is
       * @param cause Optional throwable if this message is ERR status
       * @param lDuration Optional number of milliseconds if this message indicates a unit test has ended
       */
      public UnitTestMsg(String sName, byte nStatus, Throwable cause, long lDuration)
      {
         super(sName, nStatus, cause, lDuration);
      }

      /**
       * @param sName The name of the unit test
       * @param nStatus The type of message this is
       * @param cause Optional throwable if this message is ERR status
       * @param lDuration Optional number of milliseconds if this message indicates a unit test has ended
       * @param sTestArguments String representation of the test arguments.
       */
      public UnitTestMsg(String sName, byte nStatus, Throwable cause, long lDuration, String sTestArguments)
      {
         super(sName, nStatus, cause, lDuration);
         m_sTestArguments = sTestArguments;
      }

      // operations

      /**
       * @see nexj.core.testing.unit.UnitTestObjectLogger.LogMessage#getTypeCaption()
       */
      public String getTypeCaption()
      {
         return "unit test";
      }

      /**
       * @see nexj.core.testing.unit.UnitTestObjectLogger.LogMessage#getLevel()
       */
      public int getLevel()
      {
         return 1;
      }

      /**
       * @return The string representation of the test arguments; null string if none.
       */
      public String getTestArguments()
      {
         return m_sTestArguments;
      }
   }

   /**
    * Class for messages about an individual test case.
    */
   public static class UnitTestCaseMsg extends LogMessage
   {
      private final static long serialVersionUID = -8287869342413016225L;

      /**
       * The name of the unit test.
       */
      private final String m_sUnitTest;

      /**
       * @param sName The name of the test case
       * @param sUnitTest The name of the unit test the test case belongs to
       * @param nStatus The type of message this is
       * @param cause Optional throwable if this message is a FAIL or ERR status
       * @param lDuration Optional number of milliseconds if this message indicates a test case has ended
       */
      public UnitTestCaseMsg(String sName, String sUnitTest, byte nStatus, Throwable cause, long lDuration)
      {
         super(sName, nStatus, cause, lDuration);
         m_sUnitTest = sUnitTest;
      }

      /**
       * @see nexj.core.testing.unit.UnitTestObjectLogger.LogMessage#getTypeCaption()
       */
      public String getTypeCaption()
      {
         return "test case";
      }

      /**
       * @see nexj.core.testing.unit.UnitTestObjectLogger.LogMessage#getLevel()
       */
      public int getLevel()
      {
         return 2;
      }

      /**
       * @return the name of the unitTest this test case message's test case belongs to
       */
      public String getUnitTest()
      {
         return m_sUnitTest;
      }

      public String toString()
      {
         String sException = (getExceptionCount() > 0) ? getExceptionClassName(0) : "";
         
         return getStatusString() + ',' + m_sUnitTest + '/' + getName() + ',' + getDuration() + ',' + sException;
      }
   }

}
