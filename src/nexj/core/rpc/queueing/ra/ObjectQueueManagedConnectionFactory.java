package nexj.core.rpc.queueing.ra;

import javax.resource.ResourceException;
import javax.resource.spi.ConnectionManager;
import javax.resource.spi.ConnectionRequestInfo;
import javax.resource.spi.ManagedConnection;
import javax.security.auth.Subject;

import nexj.core.rpc.ra.GenericManagedConnectionFactory;

/**
 * ManagedConnection factory for ObjectQueue
 */
public class ObjectQueueManagedConnectionFactory extends GenericManagedConnectionFactory
{
   // constants

   /**
    * the serial version uid.
    */
   private final static long serialVersionUID = 813457705044347524L;

   // attributes
   
   /**
    * The dispatcher port.
    */
   protected int m_nPort;
   
   // operations

   /**
    * @see nexj.core.rpc.ra.GenericManagedConnectionFactory#getDefaultConnectionManager()
    */
   protected ConnectionManager getDefaultConnectionManager()
   {
      return ObjectQueueResourceAdapter.getDefaultConnectionManager();
   }

   /**
    * @see javax.resource.spi.ManagedConnectionFactory#createConnectionFactory(javax.resource.spi.ConnectionManager)
    */
   public Object createConnectionFactory(ConnectionManager manager) throws ResourceException
   {
      return new ObjectQueueConnectionFactory(this, manager);
   }

   /**
    * @see javax.resource.spi.ManagedConnectionFactory#createManagedConnection(javax.security.auth.Subject, javax.resource.spi.ConnectionRequestInfo)
    */
   public ManagedConnection createManagedConnection(Subject arg0, ConnectionRequestInfo cri) throws ResourceException
   {
      return new ObjectQueueManagedConnection((ObjectQueueResourceAdapter)m_adapter, m_nPort);
   }

   /**
    * Sets the dispatcher port.
    * @param nPort The dispatcher port to set.
    */
   public void setPort(int nPort)
   {
      m_nPort = nPort;
   }

   /**
    * @return The dispatcher port.
    */
   public int getPort()
   {
      return m_nPort;
   }
}
