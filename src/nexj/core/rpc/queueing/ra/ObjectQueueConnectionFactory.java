package nexj.core.rpc.queueing.ra;

import javax.resource.ResourceException;
import javax.resource.spi.ConnectionManager;

import nexj.core.rpc.queueing.ObjectQueueConnection;
import nexj.core.rpc.ra.GenericConnectionFactory;

/**
 * The ObjectQueueConnection factory.
 */
public class ObjectQueueConnectionFactory extends GenericConnectionFactory implements
   nexj.core.rpc.queueing.ObjectQueueConnectionFactory
{
   // constants
   /**
    * The serial version uid.
    */
   private final static long serialVersionUID = 3985916953813809710L;

   // associations

   /**
    * The managed connection factory.
    */
   protected ObjectQueueManagedConnectionFactory m_factory;

   // constructors

   /**
    * Creates a new ObjectQueue connection factory.
    *
    * @param factory The managed connection factory.
    * @param manager The app server's connection manager.
    */
   protected ObjectQueueConnectionFactory(ObjectQueueManagedConnectionFactory factory, ConnectionManager manager)
   {
      super(manager);
      m_factory = factory;
   }

   // operations

   /**
    * @see nexj.core.rpc.queueing.ObjectQueueConnectionFactory#open()
    */
   public ObjectQueueConnection open() throws ResourceException
   {
      return (ObjectQueueConnection)m_manager.allocateConnection(m_factory, null);
   }
}
