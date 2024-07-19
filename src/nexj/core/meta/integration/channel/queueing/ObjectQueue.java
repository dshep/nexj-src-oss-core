package nexj.core.meta.integration.channel.queueing;

import nexj.core.meta.integration.Channel;

/**
 * A system messaging channel.
 */
public class ObjectQueue extends Channel
{
   // constructors

   /**
    * @param sName The name of the channel, which may be different from the name of the queue.
    */
   public ObjectQueue(String sName)
   {
      super(sName);
   }

   // operations

   /**
    * @see nexj.core.meta.integration.Channel#isSynchronous()
    */
   public boolean isSynchronous()
   {
      return false;
   }

   /**
    * @see nexj.core.meta.integration.Channel#isTransactional()
    */
   public boolean isTransactional()
   {
      return false;
   }

   /**
    * @see nexj.core.meta.integration.Channel#initSenderStatPath()
    */
   protected void initSenderStatPath()
   {
      // Not needed
   }

   /**
    * @see nexj.core.meta.integration.Channel#initReceiverStatPath()
    */
   protected void initReceiverStatPath()
   {
      // Not needed
   }
}
