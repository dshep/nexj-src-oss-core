package nexj.core.rpc.tcp.ra;

import nexj.core.rpc.RPCException;

/**
 * Exception thrown when a socket operation times out.
 */
public class TCPTimeoutException extends RPCException
{
   // constants
   
   /**
    * Serialization version.
    */
   private static final long serialVersionUID = -5076193424594746464L;

   // constructors

   /**
    * Construct a TCPTimeoutException.
    */
   public TCPTimeoutException()
   {
      super("err.rpc.tcp.timeout");
   }
   
   /**
    * Construct a TCPTimeoutException with a cause.
    */
   public TCPTimeoutException(Throwable cause)
   {
      super("err.rpc.tcp.timeout", cause);
   }
}
