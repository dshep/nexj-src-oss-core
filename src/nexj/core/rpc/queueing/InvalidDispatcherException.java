package nexj.core.rpc.queueing;

import nexj.core.util.UncheckedException;

/**
 * Indicates that the dispatcher associated with a SysDispatcherMessage is not valid.
 */
public class InvalidDispatcherException extends UncheckedException
{
   /**
    * 
    */
   private final static long serialVersionUID = 6788563921419264404L;

   /**
    * @param sDispatcherId the invalid dispatcher id.
    */
   public InvalidDispatcherException(String sDispatcherId)
   {
      super("err.rpc.queueing.dispatcher", new Object[]{sDispatcherId});
   }
   
}
