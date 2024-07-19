package nexj.core.rpc.queueing;

import javax.resource.ResourceException;

/**
 * Factory interface for ObjectQueue connections.
 */
public interface ObjectQueueConnectionFactory
{
   /**
    * Creates a ObjectQueueConnection.
    * @return a clean ObjectQueueConnection.
    */
   public ObjectQueueConnection open() throws ResourceException;
}