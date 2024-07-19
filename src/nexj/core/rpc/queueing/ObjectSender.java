package nexj.core.rpc.queueing;

import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;

import nexj.core.integration.IntegrationException;
import nexj.core.integration.SecureSender;
import nexj.core.integration.io.ObjectOutput;
import nexj.core.meta.integration.Message;
import nexj.core.meta.integration.channel.queueing.ObjectQueue;
import nexj.core.monitoring.ThreadLocalCounter;
import nexj.core.monitoring.jmx.StatManager;
import nexj.core.monitoring.jmx.StatManagerAware;
import nexj.core.rpc.RPCException;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.runtime.UnitOfWork;
import nexj.core.util.Logger;
import nexj.core.util.ObjUtil;

/**
 * Sender for ObjectQueue connections.  The sender is responsible for two kinds of messages: messages
 * sent to the dispatcher by model instances, and transfer objects sent on particular channels.
 */
public class ObjectSender implements SecureSender, InvocationContextAware, StatManagerAware
{
   // attributes

   /**
    * Counter of messages sent since the creation of this component
    */
   protected ThreadLocalCounter m_sentCounter = new ThreadLocalCounter();

   // associations

   /**
    * The channel metadata object.
    */
   protected ObjectQueue m_channel;

   /**
    * The invocation context.
    */
   protected InvocationContext m_invocationContext;

   /**
    * The receiver of a message.
    */
   public final static String RECEIVER = "receiver";

   /**
    * The delay of message delivery, in milliseconds.
    */
   public final static String DELAY = "delay";

   /**
    * The state of a message.
    */
   public final static String STATE = "state";
   
   /**
    * The serialized message object, if it is a java object.
    */
   public final static String OBJECT = "object";

   /**
    * The priority of a message.
    */
   public final static String PRIORITY = "priority";

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(ObjectSender.class);

   // operations

   /**
    * Sets the channel metadata object.
    * @param channel The channel metadata object to set.
    */
   public void setChannel(ObjectQueue channel)
   {
      m_channel = channel;
   }

   /**
    * @return The channel metadata object.
    */
   public ObjectQueue getChannel()
   {
      return m_channel;
   }

   /**
    * @see nexj.core.integration.Sender#createOutput()
    */
   public ObjectOutput createOutput()
   {
      return new ObjectOutput();
   }

   /**
    * @see nexj.core.integration.Sender#getSentCount()
    */
   public long getSentCount()
   {
      return m_sentCounter.get();
   }

   /**
    * @see nexj.core.integration.Sender#prepare(nexj.core.rpc.TransferObject, nexj.core.rpc.TransferObject, nexj.core.meta.integration.Message)
    */
   public void prepare(TransferObject raw, TransferObject tobj, Message message) throws IntegrationException
   {
   }

   /**
    * @see nexj.core.integration.Sender#send(nexj.core.rpc.TransferObject)
    */
   public void send(TransferObject tobj) throws IntegrationException
   {
      send(Collections.singletonList(tobj));
   }

   /**
    * @see nexj.core.integration.Sender#send(java.util.Collection)
    */
   public void send(Collection col) throws IntegrationException
   {
      try
      {
         String sQueueName = m_channel.getName();

         if (!m_channel.isSendable())
         {
            throw new RPCException("err.rpc.notSender", new Object[]{m_channel.getName()});
         }

         if (s_logger.isDebugEnabled())
         {
            for (Iterator itr = col.iterator(); itr.hasNext();)
            {
               s_logger.debug("Sending a message on channel \"" + m_channel.getName() + "\"");
               s_logger.dump(itr.next());
            }
         }

         m_sentCounter.add(col.size());
         m_invocationContext.addRPCCount(1);

         UnitOfWork oldUOW = m_invocationContext.getUnitOfWork();
         UnitOfWork uow = m_invocationContext.createUnitOfWork();

         try
         {
            ((Instance)m_invocationContext.getMetadata().getMetaclass("SysQueue")
               .invoke("getQueue", new Object[]{sQueueName})).invoke("send", new Object[]{col});

            uow.commit(true);
         }
         catch (Throwable t)
         {
            uow.rollback();
            ObjUtil.rethrow(t);
         }
         finally
         {
            m_invocationContext.setUnitOfWork(oldUOW);
         }
      }
      catch (IntegrationException e)
      {
         throw e;
      }
      catch (Exception e)
      {
         throw new RPCException("err.rpc.msg", e);
      }
   }

   /**
    * @see nexj.core.runtime.InvocationContextAware#setInvocationContext(nexj.core.runtime.InvocationContext)
    */
   public void setInvocationContext(InvocationContext context)
   {
      m_invocationContext = context;
   }

   /**
    * @see nexj.core.monitoring.jmx.StatManagerAware#setStatManager(nexj.core.monitoring.jmx.StatManager)
    */
   public void setStatManager(StatManager statManager)
   {
   }
}
