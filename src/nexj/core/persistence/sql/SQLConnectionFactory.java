// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import java.sql.Connection;
import java.sql.SQLException;

/**
 * Interface for opening SQL connections.
 */
public interface SQLConnectionFactory
{
   /**
    * Opens a connection for the given query.
    * @param adapter The adapter for which to open the connection.
    * @return The connection.
    */
   Connection getConnection(SQLAdapter adapter) throws SQLException;
}
