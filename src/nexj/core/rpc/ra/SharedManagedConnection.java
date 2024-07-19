// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.ra;

import java.util.Iterator;
import java.util.Set;

import javax.resource.ResourceException;
import javax.resource.spi.ConnectionRequestInfo;
import javax.security.auth.Subject;

import nexj.core.util.HashHolder;


/**
 * Managed connection supporting request multiplexing.
 */
public abstract class SharedManagedConnection extends GenericManagedConnection
{
   // associations

   /**
    * The connection handle set.
    */
   protected Set m_handleSet = new HashHolder(1);

   // operations

   /**
    * @see javax.resource.spi.ManagedConnection#getConnection(javax.security.auth.Subject, javax.resource.spi.ConnectionRequestInfo)
    */
   public synchronized Object getConnection(Subject subject, ConnectionRequestInfo cri) throws ResourceException
   {
      GenericConnection handle = createConnection(subject, cri);

      handle.setManagedConnection(this);

      return handle;
   }

   /**
    * @see nexj.core.rpc.ra.GenericManagedConnection#addConnection(GenericConnection)
    */
   protected synchronized void addConnection(GenericConnection handle)
   {
      m_handleSet.add(handle);
   }

   /**
    * @see nexj.core.rpc.ra.GenericManagedConnection#removeConnection(GenericConnection)
    */
   protected synchronized void removeConnection(GenericConnection handle)
   {
      m_handleSet.remove(handle);
   }

   /**
    * @see nexj.core.rpc.ra.GenericManagedConnection#closeConnections()
    */
   protected synchronized void closeConnections() throws ResourceException
   {
      for (Iterator itr = m_handleSet.iterator(); itr.hasNext();)
      {
         GenericConnection handle = (GenericConnection)itr.next();

         if (handle != null)
         {
            handle.closeHandle();
            itr.remove();
         }
      }
   }
}
