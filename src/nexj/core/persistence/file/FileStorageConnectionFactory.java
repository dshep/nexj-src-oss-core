// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.file;

import nexj.core.meta.persistence.file.FileDataSource;
import nexj.core.rpc.file.FileConnection;

/**
 * A factory that creates FileConnection objects.
 */
public interface FileStorageConnectionFactory
{
   /**
    * Creates a new FileConnection object.
    * 
    * @param adapter The adapter for which the object is being created.
    * @return A new connection.
    */
   FileConnection getConnection(FileAdapter adapter);


   /**
    * Gets the data source for this factory.
    * 
    * @return The data source.
    */
   FileDataSource getDataSource();
}
