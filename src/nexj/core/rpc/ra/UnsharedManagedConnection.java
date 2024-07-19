// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.ra;

import javax.resource.ResourceException;
import javax.resource.spi.ConnectionRequestInfo;
import javax.resource.spi.SharingViolationException;
import javax.security.auth.Subject;

/**
 * Managed connection with a single handle.
 */
public abstract class UnsharedManagedConnection extends GenericManagedConnection
{
   // associations

   /**
    * The connection handle.
    */
   protected GenericConnection m_handle;

   // operations

   /**
    * @see javax.resource.spi.ManagedConnection#getConnection(javax.security.auth.Subject, javax.resource.spi.ConnectionRequestInfo)
    */
   public synchronized Object getConnection(Subject subject, ConnectionRequestInfo cri) throws ResourceException
   {
      if (m_handle == null)
      {
         createConnection(subject, cri).setManagedConnection(this);
      }

      return m_handle;
   }

   /**
    * @see nexj.core.rpc.ra.GenericManagedConnection#addConnection(GenericConnection)
    */
   protected synchronized void addConnection(GenericConnection handle) throws SharingViolationException
   {
      if (m_handle != null)
      {
         throw new SharingViolationException();
      }

      m_handle = handle;
   }

   /**
    * @see nexj.core.rpc.ra.GenericManagedConnection#removeConnection(GenericConnection)
    */
   protected synchronized void removeConnection(GenericConnection handle)
   {
      if (handle == m_handle)
      {
         m_handle = null;
      }
   }

   /**
    * @see nexj.core.rpc.ra.GenericManagedConnection#closeConnections()
    */
   protected synchronized void closeConnections() throws ResourceException
   {
      if (m_handle != null)
      {
         GenericConnection handle = m_handle;

         m_handle = null;
         handle.closeHandle();
      }
   }
}
