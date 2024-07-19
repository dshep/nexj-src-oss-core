package nexj.core.util.pool.resource;

import javax.transaction.xa.XAResource;

/**
 * Pooled resource.
 */
public interface Resource
{
   /**
    * @return The associated XA resource, or null if none.
    */
   XAResource getXAResource();

   /**
    * Sets the resource pool (invoked by the resource pool itself).
    * @param pool The resource pool.
    */
   void setPool(ResourcePool pool);

   /**
    * Sets the time of last use in milliseconds since 1-Jan-1970 UTC.
    * @param lTime The time to set.
    */
   void setTime(long lTime);

   /**
    * @return The time of last use in milliseconds since 1-Jan-1970 UTC.
    */
   long getTime();

   /**
    * Increases the resource reference count.
    */
   void reference();

   /**
    * Returns the resource to the pool.
    * After this, this resource should not be accessed until it is obtained again from the pool. 
    */
   void release();

   /**
    * Resets any resource state so that it could be returned to the pool.
    */
   void reset();

   /**
    * Disposes of the resource, so that it cannot be used anymore.
    * This must not throw any exception.
    */
   void dispose();
}
