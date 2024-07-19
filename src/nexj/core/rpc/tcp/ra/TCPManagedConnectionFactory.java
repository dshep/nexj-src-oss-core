// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.tcp.ra;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import javax.resource.ResourceException;
import javax.resource.spi.ConnectionManager;
import javax.resource.spi.ConnectionRequestInfo;
import javax.resource.spi.ManagedConnection;
import javax.resource.spi.ValidatingManagedConnectionFactory;
import javax.security.auth.Subject;

import nexj.core.rpc.ra.GenericManagedConnectionFactory;

/**
 * TCP managed connection factory.
 */
public class TCPManagedConnectionFactory extends GenericManagedConnectionFactory implements ValidatingManagedConnectionFactory
{
   // attributes

   /**
    * Serialization version.
    */
   private final static long serialVersionUID = -1196051219296117289L;

   // operations

   /**
    * @see nexj.core.rpc.ra.GenericManagedConnectionFactory#getDefaultConnectionManager()
    */
   protected ConnectionManager getDefaultConnectionManager()
   {
      return TCPResourceAdapter.getDefaultConnectionManager();
   }

   /**
    * @see javax.resource.spi.ManagedConnectionFactory#createConnectionFactory(javax.resource.spi.ConnectionManager)
    */
   public Object createConnectionFactory(ConnectionManager manager) throws ResourceException
   {
      return new TCPConnectionFactory(this, manager);
   }

   /**
    * @see javax.resource.spi.ManagedConnectionFactory#createManagedConnection(javax.security.auth.Subject, javax.resource.spi.ConnectionRequestInfo)
    */
   public ManagedConnection createManagedConnection(Subject subject, ConnectionRequestInfo cri) throws ResourceException
   {
      return new TCPManagedConnection((TCPConnectionRequestInfo)cri);
   }

   /**
    * @see java.lang.Object#hashCode()
    */
   public int hashCode()
   {
      return 0x846780;
   }

   /**
    * @see java.lang.Object#equals(java.lang.Object)
    */
   public boolean equals(Object obj)
   {
      return obj instanceof TCPManagedConnectionFactory;
   }

   /**
    * @see javax.resource.spi.ValidatingManagedConnectionFactory#getInvalidConnections(java.util.Set)
    */
   public Set getInvalidConnections(Set connectionSet) throws ResourceException
   {
      Set invalidSet = new HashSet();

      for (Iterator itr = connectionSet.iterator(); itr.hasNext(); )
      {
         TCPManagedConnection con = (TCPManagedConnection)itr.next();

         if (!con.isValid())
         {
            invalidSet.add(con);
         }
      }

      return invalidSet;
   }
}
