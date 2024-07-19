// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import junit.framework.Test;

import nexj.test.junit.SQLTestSuite;

public class AllTests
{

   public static Test suite()
   {
      SQLTestSuite suite = new SQLTestSuite("Test for nexj.core.persistence.sql");

      suite.addTestSuite(SQLWriterConnectionTest.class);
      suite.addSQLTests("nexj.core.persistence.sql.SQLHookTest", null);
      suite.addSQLTests("nexj.core.persistence.sql.MSSQLAdapterTest", "nexj.core.persistence.sql.MSSQLSchemaManagerTest");
      suite.addSQLTests("nexj.core.persistence.sql.MySQLAdapterTest", "nexj.core.persistence.sql.MySQLSchemaManagerTest");
      suite.addSQLTests("nexj.core.persistence.sql.PostgreSQLAdapterTest", "nexj.core.persistence.sql.PostgreSQLSchemaManagerTest");
      suite.addSQLTests("nexj.core.persistence.sql.Oracle9iAdapterTest", "nexj.core.persistence.sql.Oracle9iSchemaManagerTest");
      suite.addSQLTests("nexj.core.persistence.sql.Oracle10gAdapterTest", "nexj.core.persistence.sql.Oracle10gSchemaManagerTest");
      suite.addSQLTests("nexj.core.persistence.sql.Oracle11gAdapterTest", "nexj.core.persistence.sql.Oracle11gSchemaManagerTest");
      suite.addSQLTests("nexj.core.persistence.sql.SybaseAdapterTest", "nexj.core.persistence.sql.SybaseSchemaManagerTest");
      suite.addSQLTests("nexj.core.persistence.sql.DB2v8AdapterTest", "nexj.core.persistence.sql.DB2v8SchemaManagerTest");
      suite.addSQLTests("nexj.core.persistence.sql.DB2AdapterTest", "nexj.core.persistence.sql.DB2SchemaManagerTest");
      suite.addSQLTests("nexj.core.persistence.sql.TeradataAdapterTest", "nexj.core.persistence.sql.TeradataSchemaManagerTest");

      //$JUnit-BEGIN$
      //$JUnit-END$
      return suite;
   }
}