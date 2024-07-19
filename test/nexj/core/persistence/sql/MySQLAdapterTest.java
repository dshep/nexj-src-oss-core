// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import java.sql.SQLException;
import java.sql.Statement;

import nexj.core.persistence.PersistenceException;
import nexj.core.persistence.Query;
import nexj.core.persistence.QueryTimeoutException;
import nexj.core.scripting.Pair;

public class MySQLAdapterTest extends SQLAdapterTest
{
   public MySQLAdapterTest(String name)
   {
      super(name);
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapterTest#getAdapterName()
    */
   protected String getAdapterName()
   {
      return "MySQL";
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapterTest#getUnicodeCheckGuard(boolean)
    */
   protected String getUnicodeCheckGuard(boolean bUnicode)
   {
      // Hardcode known tested/valid condition to for checking Unicode DB state.
      return "exists(select character_set_name from information_schema.columns" +
             " where table_schema='test' and table_name='Version'" +
             " and column_name='namespace' and character_set_name" +
             ((bUnicode) ? "" : " is not null and character_set_name!") +
             "='utf8')";
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapterTest#testMatch()
    */
   public void testMatch() throws Exception
   {
      super.testMatch();

      Pair attributes = parse("(firstName (addresses city))");
      Query query = Query.createRead(getMetadata().getMetaclass("Contact"), attributes,
         parse("(and (match? (@ addresses city) '(\"Toronto\\\" \\\"Roma\") 0.0) (= classCode \"CON\"))"),
         null, -1, 0, false, Query.SEC_NONE, m_context);
      assertEquals(0, query.read().size()); // test " escape
   }

   /**
    * @see nexj.core.persistence.sql.SQLAdapterTest#testQueryTimeoutException()
    */
   public void testQueryTimeoutException() throws Exception
   {
      // MySQL will not terminate a query if it's waiting on a lock, but it will terminate a
      // running query.
      SQLConnection con = m_adapter.getConnection();
      Statement stmt = null;
      PersistenceException exception = null;

      try
      {
         stmt = con.getConnection().createStatement();
         stmt.setQueryTimeout(1); // test applications specified query timeout
         stmt.executeQuery("select sleep(60)").close();
         fail(); // QueryTimeoutException expected
      }
      catch (SQLException e)
      {
         exception = m_adapter.getException(e, null, 0, 0); // validate before closing connection
      }
      finally
      {
         try
         {
            if (stmt != null)
            {
               stmt.close();
            }
         }
         catch (Throwable t)
         {
         }

         con.decRef();
      }

      assertTrue(exception instanceof QueryTimeoutException); // QueryTimeoutException expected
   }
}