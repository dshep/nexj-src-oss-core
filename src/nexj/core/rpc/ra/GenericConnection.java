// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.ra;

import javax.resource.ResourceException;
import javax.resource.spi.ConnectionEvent;

/**
 * Generic connection handle.
 */
public abstract class GenericConnection
{
   // associations

   /**
    * The managed connection.
    */
   protected GenericManagedConnection m_managedConnection;

   // operations

   /**
    * Associates the connection handles with a managed connection.
    * @param managedConnection The managed connection.
    */
   protected synchronized void setManagedConnection(GenericManagedConnection managedConnection) throws ResourceException
   {
      if (managedConnection != m_managedConnection)
      {
         if (m_managedConnection != null)
         {
            m_managedConnection.removeConnection(this);
            m_managedConnection = null;
         }

         if (managedConnection != null)
         {
            managedConnection.addConnection(this);
            m_managedConnection = managedConnection;
         }
      }
   }

   /**
    * Closes the connection handle.
    */
   protected synchronized void closeHandle() throws ResourceException
   {
      GenericManagedConnection managedConnection = m_managedConnection;

      setManagedConnection(null);

      if (managedConnection != null)
      {
         managedConnection.notify(ConnectionEvent.CONNECTION_CLOSED, this, null);
      }
   }
}
