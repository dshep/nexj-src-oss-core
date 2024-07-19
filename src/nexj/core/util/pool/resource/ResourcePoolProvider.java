package nexj.core.util.pool.resource;


/**
 * Interface implemented by objects owning a resource pool.
 */
public interface ResourcePoolProvider
{
   /**
    * Creates a new instance of a resource pool specific to the object.
    * @return The resource pool.
    */
   ResourcePool createResourcePool();

   /**
    * Associates a resource pool with the object.
    * @param pool The pool to associate. Can be null.
    */
   void setResourcePool(ResourcePool pool);

   /**
    * @return The associated resource pool. Can be null.
    */
   ResourcePool getResourcePool();
}
