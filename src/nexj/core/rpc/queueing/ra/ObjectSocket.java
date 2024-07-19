package nexj.core.rpc.queueing.ra;

import java.io.IOException;

/**
 * Interface for a socket.
 */
public interface ObjectSocket
{
   /**
    * Writes an object.  Blocks until the object is read.
    * @param obj the object to write.
    */
   public void write(Object obj) throws IOException;

   /**
    * Reads an object, blocking until one is available.
    * @return Object the object.
    */
   public Object read() throws IOException, ClassNotFoundException;

   /**
    * Close the socket.
    */
   public void close() throws IOException;
   
   /**
    * Invalidate the socket (mark the underlying channel as unusable).
    */
   public void invalidate() throws IOException;

   /**
    * @return true if the socket is closed.
    */
   public boolean isClosed();
}
