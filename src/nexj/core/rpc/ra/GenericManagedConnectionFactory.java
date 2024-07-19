// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.ra;

import java.io.PrintWriter;
import java.util.Iterator;
import java.util.Set;

import javax.resource.ResourceException;
import javax.resource.spi.ConnectionManager;
import javax.resource.spi.ConnectionRequestInfo;
import javax.resource.spi.ManagedConnection;
import javax.resource.spi.ManagedConnectionFactory;
import javax.resource.spi.ResourceAdapter;
import javax.resource.spi.ResourceAdapterAssociation;
import javax.security.auth.Subject;

/**
 * Generic managed connection factory.
 */
public abstract class GenericManagedConnectionFactory implements ManagedConnectionFactory, ResourceAdapterAssociation
{
   //constants

   /**
    * Serialization version.
    */
   private final static long serialVersionUID = 2310129820579539034L;

   // associations

   /**
    * The log writer.
    */
   protected transient PrintWriter m_logWriter;

   /**
    * The resource adapter.
    */
   protected ResourceAdapter m_adapter;

   // operations

   /**
    * @return The default connection manager instance.
    */
   protected abstract ConnectionManager getDefaultConnectionManager();

   /**
    * @see javax.resource.spi.ManagedConnectionFactory#createConnectionFactory()
    */
   public Object createConnectionFactory() throws ResourceException
   {
      return createConnectionFactory(getDefaultConnectionManager());
   }

   /**
    * @see javax.resource.spi.ManagedConnectionFactory#matchManagedConnections(java.util.Set, javax.security.auth.Subject, javax.resource.spi.ConnectionRequestInfo)
    */
   public ManagedConnection matchManagedConnections(Set pool, Subject subject, ConnectionRequestInfo cri) throws ResourceException
   {
      for (Iterator itr = pool.iterator(); itr.hasNext();)
      {
         GenericManagedConnection connection = (GenericManagedConnection)itr.next();

         if (connection.matches(subject, (GenericConnectionRequestInfo)cri))
         {
            return connection;
         }
      }

      return null;
   }

   /**
    * @see javax.resource.spi.ManagedConnectionFactory#setLogWriter(java.io.PrintWriter)
    */
   public synchronized void setLogWriter(PrintWriter logWriter) throws ResourceException
   {
      m_logWriter = logWriter;
   }

   /**
    * @see javax.resource.spi.ManagedConnectionFactory#getLogWriter()
    */
   public synchronized PrintWriter getLogWriter() throws ResourceException
   {
      return m_logWriter;
   }

   /**
    * @see javax.resource.spi.ResourceAdapterAssociation#getResourceAdapter()
    */
   public ResourceAdapter getResourceAdapter()
   {
      return m_adapter;
   }

   /**
    * @see javax.resource.spi.ResourceAdapterAssociation#setResourceAdapter(javax.resource.spi.ResourceAdapter)
    */
   public void setResourceAdapter(ResourceAdapter adapter) throws ResourceException
   {
      m_adapter = adapter;
   }
}
