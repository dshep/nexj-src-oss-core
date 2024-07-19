// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import nexj.core.meta.persistence.sql.RelationalDatabaseFragment;
import nexj.core.persistence.Query;

public class PostgreSQLAdapterTest extends SQLAdapterTest
{
   public PostgreSQLAdapterTest(String name)
   {
      super(name);
   }

   /**
    * @see nexj.core.persistence.sql.SQLDataTest#getAdapterName()
    */
   protected String getAdapterName()
   {
      return "PostgreSQL";
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapterTest#getUnicodeCheckGuard(boolean)
    */
   protected String getUnicodeCheckGuard(boolean bUnicode)
   {
      return "exists (select encoding from pg_database where datname = E'" +
         ((RelationalDatabaseFragment)m_database.getDefaultFragment()).getDatabase() +
         "' and pg_encoding_to_char(encoding) " + (bUnicode ? "=" : "!=") + " 'UTF8'";
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapterTest#isCompatibleVersion(java.lang.String)
    */
   protected boolean isCompatibleVersion(String sDBVersion)
   {
      return true;
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapterTest#testLockTimeoutException()
    */
   public void testLockTimeoutException()
   {
      // As of Postgresql 9.0, setting a lock_timeout is not supported
   }

   /**
    * PostgreSQL does not support repeatable read with exclusive lock,
    * this tests that only the non-null table is locked. i.e.
    * SELECT a.id, a.colA, b.colB FROM a LEFT JOIN b ON b.id = a.id FOR UPDATE OF A **lock only A
    * SELECT a.id, a.colA, b.colB FROM a RIGHT JOIN b ON b.id = a.id FOR UPDATE OF B **lock only B
    */
   public void testOuterJoinLock() throws Exception
   {
      Query.createRead(getMetadata().getMetaclass("Contact"),
         parse("(firstName)(addresses))"), null, null, -1, 0, true, Query.SEC_NONE, m_context);
      // fails if lock is applied to both tables
   }
}
