package nexj.core.util.pool.resource;

/**
 * Resource pool.
 */
public interface ResourcePool
{
   /**
    * Gets a resource from the pool, creating a new one if needed.
    * @param config The resource configuration (connection parameters). Can be null.
    * It must be immutable during and after this method invocation.
    * @return The pooled resource.
    * @throws PoolBusyException if the pool cannot provide the exception
    * within the busy timeout.
    * @throws ResourceFactoryException if a new resource cannot be created.
    * @see ResourcePool#getBusyTimeout()
    */
   Resource get(Object config) throws PoolBusyException, ResourceFactoryException;

   /**
    * Processes a resource release.
    * Invoked by the resource when its reference count reaches 0.
    * @param resource The resource.
    * @see Resource#release()
    */
   void release(Resource resource);

   /**
    * Processes a resource disposal.
    * Typically invoked internally or from Resource.dispose().
    * @param resource The resource.
    * @see Resource#dispose()
    */
   void dispose(Resource resource);

   /**
    * Disposes of all the pooled resources.
    */
   void dispose();

   /**
    * Invoked periodically by a system timer to maintain the pool,
    * including idle connection removal.
    */
   void maintain();

   /**
    * @return The maximum active resource count. -1 means unlimited.
    * This is enforced by the resource pool.
    * @see ResourcePool#get(Object)
    */
   int getMaxSize();

   /**
    * @return The timeout in milliseconds, after which the pool get() method throws PoolBusyException.
    * -1 means unlimited.
    * @see ResourcePool#get(Object)
    */
   long getBusyTimeout();

   /**
    * @return The timeout in milliseconds, after which idle resources are disposed of.
    * -1 means unlimited.
    */
   long getIdleTimeout();

   /**
    * @return True to enlist XA resources in transactions. 
    */
   boolean isTransactional();
}
