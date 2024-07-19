package nexj.core.rpc.queueing.ra;

import java.util.List;

import javax.resource.ResourceException;

import nexj.core.rpc.TransferObject;
import nexj.core.rpc.ra.GenericConnection;
import nexj.core.util.Logger;

/**
 * A ObjectQueueConnection serves to notify the message dispatcher of pending SysMessage table operations.
 */
public class ObjectQueueConnection extends GenericConnection implements nexj.core.rpc.queueing.ObjectQueueConnection
{
   // associations

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(ObjectQueueConnection.class);

   // operations

   /**
    * @see nexj.core.rpc.queueing.ObjectQueueConnection#wake()
    */
   public void wake()
   {
      ((ObjectQueueManagedConnection)m_managedConnection).wake();
   }

   /**
    * Posts a command for delivery to one or several nodes.
    * @param tobj the serializable message to post.
    * @param sNodeName the name of a node.
    */
   public void post(TransferObject tobj)
   {
      ((ObjectQueueManagedConnection)m_managedConnection).post(tobj);
   }

   /**
    * @see nexj.core.rpc.queueing.ObjectQueueConnection#close()
    */
   public void close()
   {
      closeHandle();
   }

   /**
    * Closes this connection
    */
   public void closeHandle()
   {
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Closing connection: " + this + " (ManagedConnection=" + this.m_managedConnection + ")");
      }

      try
      {
         super.closeHandle();
      }
      catch (ResourceException e)
      {
         throw new ObjectQueueConnectionException("err.rpc.queueing.close", e);
      }
   }

   /**
    * @see nexj.core.rpc.queueing.ObjectQueueConnection#revoke(java.util.List)
    */
   public void revoke(List unavailableNodeList)
   {
      ((ObjectQueueManagedConnection)m_managedConnection).revoke(unavailableNodeList);
   }

   /**
    * @see nexj.core.rpc.queueing.ObjectQueueConnection#isStarted()
    */
   public boolean isStarted()
   {
      return ((ObjectQueueManagedConnection)m_managedConnection).isStarted();
   }
}
