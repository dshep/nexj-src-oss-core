package nexj.core.rpc.pool;

/**
 * Interface implemented by processors of requests delivered by a consumer pool.
 */
public interface Processor
{
   /**
    * Processes the request.
    * @param request The request object.
    */
   void process(Object request) throws Throwable;
}
