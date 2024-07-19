// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.queueing;

import nexj.core.meta.Component;
import nexj.core.meta.Repository;
import nexj.core.rpc.IntegrationMDB;
import nexj.core.rpc.ServerException;
import nexj.core.util.Logger;

/**
 * Message driven bean for the ObjectQueueDispatcher.
 */
public class ObjectQueueServerMDB extends IntegrationMDB implements ObjectDispatchListener
{
   /**
    * serial version uid.
    */
   private static final long serialVersionUID = 5938898378655006861L;

   // associations

   /**
    * The MDB logger.
    */
   protected final static Logger s_logger = Logger.getLogger(ObjectQueueServerMDB.class);

   /**
    * @see nexj.core.rpc.ServerMDB#getLogger()
    */
   protected Logger getLogger()
   {
      return s_logger;
   }

   /**
    * @see nexj.core.rpc.ServerMDB#init()
    */
   protected void init() throws Exception
   {
      m_sChannelName = "SysObjectQueueDispatcher";
   }

   /**
    * @see nexj.core.rpc.queueing.ObjectDispatchListener#onMessage(nexj.core.rpc.queueing.DispatcherMessage)
    */
   public void onMessage(DispatcherMessage message)
   {
      ObjectDispatchListener receiver = null;

      try
      {
         Component receiverComponent = Repository.getMetadata().getChannel(m_sChannelName).getReceiver();

         if (receiverComponent == null)
         {
            return;
         }

         receiver = (ObjectDispatchListener)receiverComponent.getInstance(null);
      }
      catch (IllegalStateException e)
      {
         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Error in " + this, e);
         }
      }

      try
      {
         receiver.onMessage(message);
      }
      catch (Throwable e)
      {
         if (!(e instanceof ServerException) && s_logger.isDebugEnabled())
         {
            s_logger.debug("Error in " + this, e);
         }
      }
   }
}
