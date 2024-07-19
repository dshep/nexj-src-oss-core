// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import nexj.test.junit.TestSuite;

import junit.framework.Test;

/**
 * 
 */
public class AllTests
{

   public static Test suite()
   {
      TestSuite suite = new TestSuite("Test for nexj.core.scripting");
      
      suite.addTest("nexj.core.scripting.debugger.AllTests");
      //$JUnit-BEGIN$
      suite.addTestSuite(PCodeFunctionTest.class);
      suite.addTestSuite(SchemeTest.class);
      suite.addTestSuite(ExpressionTest.class);
      suite.addTestSuite(SchemeParserTest.class);
      suite.addTestSuite(ObjectTest.class);
      suite.addTestSuite(TypedObjectTest.class);
      suite.addTestSuite(TypedExceptionObjectTest.class);
      suite.addTestSuite(CallCCTest.class);
      //$JUnit-END$
      return suite;
   }
}
