// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.test.junit;

import junit.framework.Test;
import nexj.core.persistence.sql.SQLDataTest;
import nexj.core.persistence.sql.SQLDataTestLock;
import nexj.core.util.ObjUtil;

/**
 * TestSuite that supports dynamic loading of database adapter and schema manager test classes
 */
public class SQLTestSuite extends TestSuite
{
   /**
    * Constructs an SQLTestSuite.
    * @param sName The name of the test.
    */
   public SQLTestSuite(String sName)
   {
      super(sName);
   }
   
   /**
    * Add SQL tests, if the adapter is enabled.
    * @param sAdapterTest The name of the AdapterTest class.
    * @param sSchemaManagerTest The name of the SchemaManagerTest class.
    */
   public void addSQLTests(String sAdapterTest, String sSchemaManagerTest)
   {
      try
      {
         Class clazz = Class.forName(sAdapterTest);
         Object obj = clazz.getConstructor(new Class[] {String.class}).newInstance(new Object[] {""});
      
         if (((Boolean)clazz.getMethod("isEnabled", null).invoke(obj, null)).booleanValue())
         {
            addTestSuite(clazz);
            
            if (sSchemaManagerTest != null)
            {
               addTestSuite(sSchemaManagerTest);
            }
         }
      }
      catch (ClassNotFoundException e)
      {  // if the class isn't found, don't test it.
      }
      catch (Throwable t)
      {
         ObjUtil.rethrow(t);
      }
   }

   /**
    * @see junit.framework.TestSuite#addTestSuite(java.lang.Class)
    */
   public void addTestSuite(Class testClass)
   {
      // wrap the TestSuite inside an object that knows how to acquire/release DB locks
      addTest((SQLDataTest.class.isAssignableFrom(testClass)) ?
              (Test)new SQLDataTestLock(testClass) : (Test)new junit.framework.TestSuite(testClass));
   }
}