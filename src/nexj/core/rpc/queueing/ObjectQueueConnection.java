package nexj.core.rpc.queueing;

import java.util.List;

import nexj.core.rpc.TransferObject;

/**
 * Interface for a ObjectQueue connection.
 */
public interface ObjectQueueConnection
{
   /**
    * Notify the messaging engine of a new message.
    */
   public void wake();
   
   /**
    * Posts a command for delivery to one or several nodes.
    * @param tobj the serializable message to post.
    */
   public void post(TransferObject tobj);

   /**
    * Notify consumer pool of revoked listener nodes.
    * @param unavailableNodeList the unavailable nodes.
    */
   public void revoke(List unavailableNodeList);

   /**
    * Close the connection.
    */
   public void close();

   /**
    * Get whether consumer pool is started.
    * @return Whether consumer pool is started.
    */
   public boolean isStarted();
}