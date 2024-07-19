// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import junit.extensions.TestSetup;
import junit.framework.TestSuite;

/**
 * A test decorator that is able to request a SQLDataTest to lock resources for the duration of a
 * TestSuite and request release once the TestSuite completes.
 */
public class SQLDataTestLock extends TestSetup
{
   /**
    * The object that should be locked for the duration of the Test.
    */
   protected SQLDataTest m_suite;

   // constructors

   /**
    * Constructor.
    * @param testClass The class to wrap during testing, also used for locking.
    */
   public SQLDataTestLock(Class testClass)
   {
      super(new TestSuite(testClass));
      m_suite = (SQLDataTest)TestSuite.createTest(testClass, null);
   }

   // operations

   /**
    * @see junit.extensions.TestSetup#setUp()
    */
   public void setUp()
   {
      m_suite.lock();
   }

   /**
    * @see junit.extensions.TestSetup#tearDown()
    */
   public void tearDown() throws Exception
   {
      m_suite.unlock();
   }
}
