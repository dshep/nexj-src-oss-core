// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import java.sql.Connection;
import java.sql.SQLException;

import nexj.core.meta.persistence.sql.RelationalDatabaseFragment;

/**
 * Interface for overriding SQL connection creation.
 */
public interface SQLConnectionInterceptor
{
   /**
    * Opens a connection for the given adapter and fragment combination.
    * @param adapter The adapter for which to open the connection.
    * @param fragment The fragment for which to open the conenction.
    * @return The opened connection.
    */
   Connection getConnection(SQLAdapter adapter, RelationalDatabaseFragment fragment)
      throws SQLException;
}
