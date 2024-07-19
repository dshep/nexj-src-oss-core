// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql;

import nexj.test.junit.TestSuite;

import junit.framework.Test;

public class AllTests
{

   public static Test suite()
   {
      TestSuite suite = new TestSuite("Test for nexj.core.meta.persistence.sql");
      
      if (!suite.addTestSuite("nexj.core.meta.persistence.sql.EnterpriseRelationalSchemaTest"))
      {
         suite.addTestSuite(RelationalSchemaTest.class);
      }
      
      //$JUnit-BEGIN$
      suite.addTestSuite(ColumnTest.class);
      suite.addTestSuite(IndexColumnTest.class);
      suite.addTestSuite(IndexTest.class);
      suite.addTestSuite(TableTest.class);
      suite.addTestSuite(RelationalMappingTest.class);
      suite.addTestSuite(RelationalClassMappingTest.class);
      suite.addTestSuite(RelationalPrimitiveMappingTest.class);
      suite.addTestSuite(SQLSubstReaderTest.class);
      suite.addTestSuite(SQLTemplateSubstReaderTest.class);
      //$JUnit-END$
      return suite;
   }
}
