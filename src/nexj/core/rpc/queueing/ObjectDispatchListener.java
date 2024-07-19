package nexj.core.rpc.queueing;

/**
 * Interface implemented by ObjectQueueDispatcher to handle dispatching events.
 */
public interface ObjectDispatchListener
{
   // dispatch category flags
   /**
    * The "only" distribution category.
    */
   public final static int ONLY = 0;

   /**
    * The "all" distribution category.
    */
   public final static int ALL = 1;

   /**
    * The "except" distribution category.
    */
   public final static int EXCEPT = 2;

   /**
    * The "any" distribution category.
    */
   public final static int ANY = 3;

   /**
    * The "dispatcher" distribution category.
    */
   public final static int DISPATCHER = 4;

   // operations

   /**
    * onMessage, handles message of type messageType with arguments args.
    * @param message the message to process.
    */
   public void onMessage(DispatcherMessage message);
}
