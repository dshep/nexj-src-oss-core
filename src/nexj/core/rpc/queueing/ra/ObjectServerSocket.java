package nexj.core.rpc.queueing.ra;

import java.io.IOException;

/**
 * Interface for a server socket.
 */
public interface ObjectServerSocket
{
   /**
    * Accept a new socket.
    * @param nTimeout the timeout in milliseconds.
    * @return the socket accepted.
    */
   public ObjectSocket accept(int nTimeout) throws IOException, ClassNotFoundException;
   
   /**
    * Close the socket.
    */
   public void close() throws IOException;

   /**
    * @return true if the socket is closed.
    */
   public boolean isClosed();
}
