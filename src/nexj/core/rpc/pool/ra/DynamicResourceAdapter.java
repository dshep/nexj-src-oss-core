package nexj.core.rpc.pool.ra;

import java.util.Locale;

import javax.resource.ResourceException;
import javax.resource.spi.ActivationSpec;
import javax.resource.spi.BootstrapContext;
import javax.resource.spi.ResourceAdapter;
import javax.resource.spi.ResourceAdapterInternalException;
import javax.resource.spi.endpoint.MessageEndpointFactory;
import javax.transaction.TransactionManager;
import javax.transaction.xa.XAResource;

import nexj.core.rpc.pool.PoolManager;
import nexj.core.runtime.platform.TransactionManagerLocator;
import nexj.core.util.J2EEUtil;
import nexj.core.util.Logger;
import nexj.core.util.ObjUtil;
import nexj.core.util.SysUtil;

/**
 * Resource adapter for integrating the pools into a Java EE container.
 * It is a singleton (per EAR/JVM combination) and uses a singleton activation spec
 * corresponding to a generic MDB processing the requests from all consumer pools.
 * It recovers all XA resources from all resource and consumer pools.
 * @see nexj.core.util.pool
 */
public class DynamicResourceAdapter implements ResourceAdapter
{
   // associations

   /**
    * The container context.
    */
   protected BootstrapContext m_context;

   /**
    * Endpoint factory.
    */
   protected MessageEndpointFactory m_factory;

   /**
    * The class logger.
    */
   protected final Logger m_logger = Logger.getLogger(getClass());

   // operations

   /**
    * @see javax.resource.spi.ResourceAdapter#start(javax.resource.spi.BootstrapContext)
    */
   public void start(BootstrapContext context) throws ResourceAdapterInternalException
   {
      if (!J2EEUtil.isContained())
      {
         throw new ResourceAdapterInternalException("Unknown J2EE container");
      }

      m_context = context;
   }

   /**
    * @see javax.resource.spi.ResourceAdapter#stop()
    */
   public void stop()
   {
      m_context = null;
   }

   /**
    * @see javax.resource.spi.ResourceAdapter#endpointActivation(javax.resource.spi.endpoint.MessageEndpointFactory, javax.resource.spi.ActivationSpec)
    */
   public synchronized void endpointActivation(MessageEndpointFactory factory, ActivationSpec spec) throws ResourceException
   {
      try
      {
         if (m_factory != null)
         {
            String sContainer = J2EEUtil.getPlatformName();
            TransactionManager txManager = ((TransactionManagerLocator)Class.forName(
               SysUtil.PACKAGE + ".core.runtime.platform." + sContainer.toLowerCase(Locale.ENGLISH) + '.' +
               sContainer + "TransactionManagerLocator").newInstance()).getTransactionManager();
            boolean bTransactional = ((DynamicActivationSpec)spec).isTransactional();

            PoolManager.setConsumerAdapter(
               new DynamicConsumerAdapter(m_context, txManager,
                  (bTransactional) ? m_factory : factory,
                  (bTransactional) ? factory : m_factory));
            m_factory = null;
         }
         else
         {
            m_factory = factory;
         }
      }
      catch (Throwable t)
      {
         throw new ResourceException(ObjUtil.getMessage(t), t);
      }

      m_logger.debug("Started");
   }

   /**
    * @see javax.resource.spi.ResourceAdapter#endpointDeactivation(javax.resource.spi.endpoint.MessageEndpointFactory, javax.resource.spi.ActivationSpec)
    */
   public synchronized void endpointDeactivation(MessageEndpointFactory factory, ActivationSpec spec)
   {
      try
      {
         PoolManager.setConsumerAdapter(null);
         m_factory = null;
      }
      catch (Throwable t)
      {
         m_logger.error("Unable to stop the pool", t);
      }

      m_logger.debug("Stopped");
   }

   /**
    * @see javax.resource.spi.ResourceAdapter#getXAResources(javax.resource.spi.ActivationSpec[])
    */
   public XAResource[] getXAResources(ActivationSpec[] spec) throws ResourceException
   {
      // TODO: Return all XA resources from all resource and consumer pools
      return null;
   }
}
