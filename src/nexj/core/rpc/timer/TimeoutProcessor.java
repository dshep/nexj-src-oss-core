package nexj.core.rpc.timer;

/**
 * Interface implemented by timeout receivers.
 */
public interface TimeoutProcessor
{
   /**
    * Invoked on timeout.
    * @return The next timeout in milliseconds since 1-Jan-1970 00:00:00 UTC,
    *    0 to use the configured settings, or Long.MAX_VALUE to cancel the timer. 
    */
   long timeout();
}
