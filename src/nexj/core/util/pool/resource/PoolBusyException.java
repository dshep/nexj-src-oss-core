package nexj.core.util.pool.resource;

/**
 * Exception indicating that a resource pool cannot provide a resource within a given timeout. 
 */
public class PoolBusyException extends ResourceException
{
   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = -7683351517034013737L;

   /**
    * Constructs the exception.
    * @param lTimeout The pool busy timeout in milliseconds.
    * @param nMaxSize The maximum pool size.
    */
   public PoolBusyException(long lTimeout, int nMaxSize)
   {
      super("err.pool.resource.busy", new Object[]{new Long(lTimeout), new Integer(nMaxSize)});
   }
}
