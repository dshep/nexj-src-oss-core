// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.ra;

import java.io.Serializable;

import javax.naming.NamingException;
import javax.naming.Reference;
import javax.resource.Referenceable;
import javax.resource.spi.ConnectionManager;

/**
 * Generic connection factory.
 */
public abstract class GenericConnectionFactory implements Serializable, Referenceable
{
   // constants
   
   /**
    * Serialization version.
    */
   private final static long serialVersionUID = -5609808494841683989L;

   // associations

   /**
    * The connection manager.
    */
   protected ConnectionManager m_manager;

   /**
    * The naming reference.
    */
   protected Reference m_reference;

   // constructors

   /**
    * Constructs the factory.
    * @param manager The connection manager.
    */
   protected GenericConnectionFactory(ConnectionManager manager)
   {
      m_manager = manager;
   }

   // operations

   /**
    * @see javax.resource.Referenceable#setReference(javax.naming.Reference)
    */
   public synchronized void setReference(Reference reference)
   {
      m_reference = reference;
   }

   /**
    * @see javax.naming.Referenceable#getReference()
    */
   public synchronized Reference getReference() throws NamingException
   {
      return m_reference;
   }
}
