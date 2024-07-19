package nexj.core.rpc.queueing.ra;

import nexj.core.rpc.queueing.DispatcherMessage;

/**
 * Implementation of SysDispatcherMessage that delegates to another instance of SysDispatcherMessage
 */
public abstract class DispatcherMessageDelegator implements DispatcherMessage
{

   /**
    * @return the delegate message.
    */
   public abstract DispatcherMessage getMessage();
   
   /**
    * @see nexj.core.rpc.queueing.DispatcherMessage#error(int, java.lang.Throwable)
    */
   public void error(int nCause, Throwable t)
   {
   }

   /**
    * @see nexj.core.rpc.queueing.DispatcherMessage#getArgArray()
    */
   public Object[] getArgArray()
   {
      return getMessage().getArgArray();
   }

   /**
    * @see nexj.core.rpc.queueing.DispatcherMessage#getDispatcherName()
    */
   public String getDispatcherName()
   {
      return getMessage().getDispatcherName();
   }

   /**
    * @see nexj.core.rpc.queueing.DispatcherMessage#getType()
    */
   public int getType()
   {
      return getMessage().getType();
   }

   /**
    * @see nexj.core.rpc.queueing.DispatcherMessage#respond(java.lang.Object)
    */
   public void respond(Object result)
   {
   }

}
