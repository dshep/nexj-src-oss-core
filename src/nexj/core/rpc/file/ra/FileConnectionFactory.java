// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.file.ra;

import javax.resource.ResourceException;
import javax.resource.spi.ConnectionManager;

import nexj.core.rpc.file.FileConnection;
import nexj.core.rpc.ra.GenericConnectionFactory;

/**
 * The factory for FileConnection objects.
 */
public class FileConnectionFactory extends GenericConnectionFactory implements nexj.core.rpc.file.FileConnectionFactory
{
   // constants

   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = 3131832327579310354L;


   // associations

   /**
    * The File Connection managed connection factory.
    */
   protected FileManagedConnectionFactory m_factory;


   // constructors

   /**
    * Creates a new file connection factory.
    * 
    * @param factory The managed connection factory.
    * @param manager The app server's connection manager.
    */
   protected FileConnectionFactory(FileManagedConnectionFactory factory, ConnectionManager manager)
   {
      super(manager);
      m_factory = factory;
   }


   // operations

   /**
    * Gets a FileConnection from the managed pool. Actually, this method
    * gets a FileManagedConnection from the managed pool, and gets a
    * FileConnection from the managed connection. Since FileConnections
    * are not reusable, the FileManagedConnection will create a new
    * FileConnection.
    * 
    * @see nexj.core.rpc.file.FileConnectionFactory#getConnection()
    */
   public FileConnection getConnection() throws ResourceException
   {
      return (FileConnection)m_manager.allocateConnection(m_factory, null);
   }
}
