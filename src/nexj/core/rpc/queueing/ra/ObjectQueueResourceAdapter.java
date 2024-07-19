package nexj.core.rpc.queueing.ra;

import javax.resource.ResourceException;
import javax.resource.spi.ActivationSpec;
import javax.resource.spi.ConnectionManager;
import javax.resource.spi.endpoint.MessageEndpointFactory;

import nexj.core.rpc.queueing.ra.DefaultObjectConsumerPool;
import nexj.core.rpc.ra.GenericConnectionManager;
import nexj.core.rpc.ra.GenericConsumerPool;
import nexj.core.rpc.ra.GenericResourceAdapter;
import nexj.core.util.HashTab;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;

/**
 * ObjectQueue Resource Adapter.
 */
public class ObjectQueueResourceAdapter extends GenericResourceAdapter
{
   // associations

   /**
    * The consumer pools indexed by port.
    */
   protected Lookup m_poolByPortMap = new HashTab();

   /**
    * The default connection manager.
    */
   protected static GenericConnectionManager s_defaultConnectionManager;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(ObjectQueueResourceAdapter.class);

   // constructors

   /**
    * Constructs the resource adapter.
    */
   public ObjectQueueResourceAdapter()
   {
      super(s_logger);
   }

   /**
    * @return The default connection manager.
    */
   public static synchronized ConnectionManager getDefaultConnectionManager()
   {
      if (s_defaultConnectionManager == null)
      {
         s_defaultConnectionManager = new GenericConnectionManager();
      }

      return s_defaultConnectionManager;
   }

   /**
    * @see nexj.core.rpc.ra.GenericResourceAdapter#stop()
    */
   public void stop()
   {
      synchronized (ObjectQueueResourceAdapter.class)
      {
         if (s_defaultConnectionManager != null)
         {
            s_defaultConnectionManager.clear();
            s_defaultConnectionManager = null;
         }
      }

      super.stop();
   }

   /**
    * @see nexj.core.rpc.ra.GenericResourceAdapter#createConsumerPool(javax.resource.spi.endpoint.MessageEndpointFactory, javax.resource.spi.ActivationSpec)
    */
   protected GenericConsumerPool createConsumerPool(MessageEndpointFactory factory, ActivationSpec cfg)
      throws ResourceException
   {
      return new DefaultObjectConsumerPool(this, factory, (ObjectConsumerConfig)cfg);
   }
   
   /**
    * Set the consumer pool associated with a port.
    * @param nPort the port for which to get the consumer pool.
    * @param pool the pool to add.
    */
   public void putConsumerPool(int nPort, ObjectConsumerPool pool) throws ResourceException
   {
      synchronized (m_poolByPortMap)
      {
         ObjectConsumerPool oldPool = (ObjectConsumerPool)m_poolByPortMap.put(new Integer(nPort), pool);
         
         if (oldPool != null)
         {
            throw new ResourceException("Multiple ObjectConsumerPools mapped to same listening port");
         }
      }
   }
   
   /**
    * Remove the consumer pool associated with a port.
    * @param nPort the port for which to get the consumer pool.
    */
   public void removeConsumerPool(int nPort)
   {
      synchronized (m_poolByPortMap)
      {
         m_poolByPortMap.remove(Integer.valueOf(nPort));
      }
   }

   /**
    * Retrieve the consumer pool associated with a port.
    * @param nPort the port for which to get the consumer pool.
    * @return the consumer pool.
    */
   public DefaultObjectConsumerPool getConsumerPool(int nPort)
   {
      synchronized (m_poolByPortMap)
      {
         return (DefaultObjectConsumerPool)m_poolByPortMap.get(Integer.valueOf(nPort));
      }
   }
}
