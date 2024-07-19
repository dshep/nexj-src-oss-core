package nexj.core.rpc.queueing;

/**
 *
 */
public interface DispatcherMessage
{
   // constants

   // message types
   /**
    * The startNode id.
    */
   public final static int START_NODE = 1;

   /**
    * The dispatch id.
    */
   public final static int DISPATCH = 2;

   /**
    * The getMessage id.
    */
   public final static int GET_MESSAGE = 3;

   /**
    * The receive id.
    */
   public final static int RECEIVE = 4;

   /**
    * The wake id.
    */
   public final static int WAKE = 5;

   /**
    * The post id.
    */
   public final static int POST = 6;

   /**
    * The execute id.
    */
   public final static int EXECUTE = 7;

   /**
    * The revoke id.
    */
   public final static int REVOKE = 8;

   // failure causes
   /**
    * Indicates that an exception has been raised.
    */
   public final static int EXCEPTION = 0;

   /**
    * Indicates that the dispatcher has changed since the message was sent.
    */
   public final static int DISPATCHER_CHANGED = 1;

   /**
    * Indicates that the dispatcher component has been shut down.
    */
   public final static int SHUTDOWN = 2;

   // operations

   /**
    * @return The message type.
    */
   public int getType();

   /**
    * @return The message arguments.
    */
   public Object[] getArgArray();

   /**
    * Processes the result.
    * @param result The result to process.
    */
   public void respond(Object result);

   /**
    * Handle failure of message processing.
    * @param nCause the cause of the failure: one of EXCEPTION, DISPATCHER_CHANGED, SHUTDOWN.
    * @param t the exception.
    */
   public void error(int nCause, Throwable t);

   /**
    * @return The message dispatcher.
    */
   public String getDispatcherName();
}
