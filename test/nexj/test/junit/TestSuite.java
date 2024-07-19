// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.test.junit;

import junit.framework.Test;

import nexj.core.util.ObjUtil;

/**
 * TestSuite that supports dynamic loading of test classes
 */
public class TestSuite extends junit.framework.TestSuite
{
   /**
    * Constructs a TestSuite.
    * @param sName The name of the test.
    */
   public TestSuite(String sName)
   {
      super(sName);
   }
   
   /**
    * Add a test.
    * @param sTestClassname The name of the class from which to obtain the test.
    */
   public boolean addTest(String sTestClassname)
   {
      try
      {
         addTest((Test)Class.forName(sTestClassname).getMethod("suite", null).invoke(null, null));
         
         return true;
      }
      catch (ClassNotFoundException e)
      {  
         // if the class isn't found, don't test it.
         return false;
      }
      catch (Throwable t)
      {
         ObjUtil.rethrow(t);
         
         return false;
      }
   }

   /**
    * Add a suite.
    * @param sTestClassname The name of the class to add.
    */
   public boolean addTestSuite(String sTestClassname)
   {
      try
      {
         addTestSuite(Class.forName(sTestClassname));
         
         return true;
      }
      catch (ClassNotFoundException e)
      {  
         // if the class isn't found, don't test it.
         return false;
      }
   }
}
