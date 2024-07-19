// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.SQLException;

import nexj.core.persistence.PersistenceException;
import nexj.core.util.Logger;

/**
 * Factory for creating an SQL adapter based on a JDBC connection.
 */
public class SQLSchemaManagerFactory
{
   // associations

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(SQLSchemaManagerFactory.class);

   // operations

   /**
    * Creates a SQLAdapter for the database identified by name and version.
    * @param sDatabase The database product name.
    * @param sVersion The database product version.
    * @return The SQLAdapter.
    */
   public static SQLAdapter createAdapter(String sDatabase, String sVersion)
   {
      if (sDatabase.equals("MySQL"))
      {
         return new MySQLAdapter();
      }
      else if (sDatabase.equals("PostgreSQL"))
      {
         return new PostgreSQLAdapter();
      }
      else
      {
         throw new PersistenceException("Unknown database product \"{0}\"",
            new Object[]{sDatabase});
      }
   }

   /**
    * Instantiates a schema manager for the database identified by the connection metadata.
    * @param connection The database connection.
    */
   public static SQLSchemaManager create(Connection connection) throws SQLException, PersistenceException
   {
      DatabaseMetaData metadata = connection.getMetaData();
      String sDatabase = metadata.getDatabaseProductName();
      String sVersion = metadata.getDatabaseProductVersion();

      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Database product \"" + sDatabase + "\", \"" + sVersion + "\"");
      }

      SQLSchemaManager manager = createAdapter(sDatabase, sVersion).createSchemaManager(null);

      manager.setConnection(connection);

      return manager;
   }
}
