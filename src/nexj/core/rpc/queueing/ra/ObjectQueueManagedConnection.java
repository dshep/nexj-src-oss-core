package nexj.core.rpc.queueing.ra;

import java.util.List;

import javax.resource.ResourceException;
import javax.resource.spi.ConnectionRequestInfo;
import javax.security.auth.Subject;

import nexj.core.rpc.TransferObject;
import nexj.core.rpc.ra.GenericConnection;
import nexj.core.rpc.ra.GenericConnectionRequestInfo;
import nexj.core.rpc.ra.UnsharedManagedConnection;
import nexj.core.util.Logger;

/**
 * Managed connection for communicating with the ObjectQueue dispatcher.
 */
public class ObjectQueueManagedConnection extends UnsharedManagedConnection
{
   // attributes

   /**
    * The dispatcher port.
    */
   protected int m_nPort;

   // associations

   /**
    * The resource adapter.
    */
   protected ObjectQueueResourceAdapter m_adapter;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(ObjectQueueManagedConnection.class);

   // constructors

   /**
    * @param adapter the resource adapter.
    */
   public ObjectQueueManagedConnection(ObjectQueueResourceAdapter adapter, int nPort)
   {
      m_adapter = adapter;
      m_nPort = nPort;
   }

   // operations

   /**
    * @return The dispatcher port.
    */
   public int getPort()
   {
      return m_nPort;
   }

   /**
    * @see nexj.core.rpc.ra.GenericManagedConnection#createConnection(javax.security.auth.Subject, javax.resource.spi.ConnectionRequestInfo)
    */
   protected GenericConnection createConnection(Subject subject, ConnectionRequestInfo cri) throws ResourceException
   {
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Creating new ObjectQueueConnection on " + this);
      }

      return new ObjectQueueConnection();
   }

   /**
    * @see nexj.core.rpc.ra.GenericManagedConnection#matches(javax.security.auth.Subject, nexj.core.rpc.ra.GenericConnectionRequestInfo)
    */
   public boolean matches(Subject subject, GenericConnectionRequestInfo cri)
   {
      return true;
   }

   /**
    * Notify the messaging engine of a new message.
    */
   public void wake()
   {
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Sending ObjectQueueConnection wake on " + this);
      }

      if (m_adapter != null)
      {
         ObjectConsumerPool pool = m_adapter.getConsumerPool(m_nPort);

         if (pool != null)
         {
            pool.wake();
         }
      }
      // else: when not in a container, there is no dispatcher to wake up
   }
   
   /**
    * Posts a command for delivery to one or several nodes.
    * @param tobj the serializable message to post.
    */
   public void post(final TransferObject tobj)
   {
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Sending ObjectQueueConnection post on " + this + ": " + tobj);
      }

      if (m_adapter != null)
      {
         DefaultObjectConsumerPool pool = m_adapter.getConsumerPool(m_nPort);

         if (pool != null)
         {
            pool.post(tobj);
         }
      }
      // else: when not in a container, there is no dispatcher to post to
   }

   /**
    * @see nexj.core.rpc.queueing.ObjectQueueConnection#revoke(java.util.List)
    */
   public void revoke(List unavailableNodeList)
   {
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Sending ObjectQueueConnection revoke on " + this + ", port = " + m_nPort);
      }

      if (m_adapter != null)
      {
         m_adapter.getConsumerPool(m_nPort).revoke(unavailableNodeList);
      }
   }

   /**
    * Get whether consumer pool is started.
    * @return Whether consumer pool is started.
    */
   public boolean isStarted()
   {
      return m_adapter.getConsumerPool(m_nPort) != null;
   }
}
