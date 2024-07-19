package nexj.core.rpc.queueing.ra;

import java.io.IOException;

/**
 * Wraps a LocalObjectSocket with a server socket interface.  A socket written to the LocalObjectServerSocket can be read
 * back through accept.
 */
public class LocalObjectServerSocket extends LocalObjectSocket implements ObjectServerSocket
{
   /**
    * Create a LocalObjectServerSocket with a maximum payload buffer size.
    * @param nMaxArraySize The maximum payload buffer size.
    */
   public LocalObjectServerSocket(int nMaxArraySize)
   {
      super(nMaxArraySize);
   }

   /**
    * @see nexj.core.rpc.queueing.ra.ObjectServerSocket#accept(int)
    */
   public ObjectSocket accept(int nTimeout) throws IOException
   {
      return (ObjectSocket)read(nTimeout);
   }
}
