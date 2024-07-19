// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import java.sql.Connection;
import java.sql.SQLException;

import nexj.core.meta.persistence.sql.RelationalDatabase;
import nexj.core.meta.persistence.sql.RelationalDatabaseFragment;
import nexj.core.runtime.InvocationContext;

/**
 * Connection factory dispatching to another factory based
 * on the current invocation context fragment.
 */
public class SQLFragmentConnectionFactory implements SQLConnectionFactory
{
   // associations

   /**
    * The relational database.
    */
   protected RelationalDatabase m_database;

   // operations

   /**
    * Sets the relational database.
    * @param database The relational database to set.
    */
   public void setDatabase(RelationalDatabase database)
   {
      m_database = database;
   }

   /**
    * @return The relational database.
    */
   public RelationalDatabase getDatabase()
   {
      return m_database;
   }

   /**
    * @see nexj.core.persistence.sql.SQLConnectionFactory#getConnection(nexj.core.persistence.sql.SQLAdapter)
    */
   public Connection getConnection(SQLAdapter adapter) throws SQLException
   {
      InvocationContext context = (adapter == null) ? null : adapter.getInvocationContext();

      return ((SQLConnectionFactory)((RelationalDatabaseFragment)m_database
            .getFragment((context == null) ? null : context.getUnitOfWork().getFragmentName()))
            .getConnectionFactory().getInstance(context)).getConnection(adapter);
   }
}
