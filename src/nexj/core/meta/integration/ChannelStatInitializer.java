package nexj.core.meta.integration;

import java.util.Iterator;

import nexj.core.meta.Component;
import nexj.core.meta.Metadata;
import nexj.core.monitoring.jmx.StatManager;
import nexj.core.monitoring.jmx.StatManagerAware;

/**
 * Channel statistics initializer.
 */
public class ChannelStatInitializer implements StatManagerAware
{
   // operations

   /**
    * Set statistics manager.
    * @param statManager Statistics manager.
    */
   public void setStatManager(StatManager statManager)
   {
      Metadata metadata = statManager.getMetadata();

      for (Iterator itr = metadata.getChannelIterator(); itr.hasNext();)
      {
         Channel channel = (Channel)itr.next();
         Component sender = channel.getSender();
         Component receiver = channel.getReceiver();

         if (sender != null)
         {
            Object senderInstance = sender.getInstance(null);

            if (senderInstance instanceof StatManagerAware)
            {
               ((StatManagerAware)senderInstance).setStatManager(statManager);
            }
            else
            {
               String sSenderStatPath = channel.getSenderStatPath();

               if (sSenderStatPath != null)
               {
                  statManager.defineStatistic(sSenderStatPath, Channel.STAT_TOTAL_COUNT,
                     "counter", true, Channel.STAT_PERSIST_CLASS);
                  statManager.defineStatistic(sSenderStatPath, Channel.STAT_AVERAGE_SEND_TIME,
                     "mean", true, Channel.STAT_PERSIST_CLASS);
               }
            }
         }

         if (receiver != null)
         {
            Object receiverInstance = receiver.getInstance(null);

            if (receiverInstance instanceof StatManagerAware)
            {
               ((StatManagerAware)receiverInstance).setStatManager(statManager);
            }
            else
            {
               String sReceiverStatPath = channel.getReceiverStatPath();

               if (sReceiverStatPath != null)
               {
                  statManager.defineStatistic(sReceiverStatPath, Channel.STAT_TOTAL_COUNT,
                     "counter", true, Channel.STAT_PERSIST_CLASS);
                  statManager.defineStatistic(sReceiverStatPath, Channel.STAT_AVERAGE_RECEIVE_TIME,
                     "mean", true, Channel.STAT_PERSIST_CLASS);
               }
            }
         }
      }
   }
}
