package nexj.core.rpc.queueing;

import nexj.core.util.UncheckedException;

/**
 * Indicates that the dispatcher associated with a SysDispatcherMessage is not valid.
 */
public class ShutdownException extends UncheckedException
{
   /**
    * 
    */
   private final static long serialVersionUID = 6788563921419264404L;

   /**
    * construct a ShutdownException.
    */
   public ShutdownException()
   {
      super("err.rpc.queueing.shutdown");
   }
   
}
