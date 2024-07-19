// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration;

import java.util.Iterator;

import nexj.core.integration.io.DebugInput;
import nexj.core.integration.io.ObjectInput;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.MessageTable;
import nexj.core.meta.integration.service.Binding;
import nexj.core.monitoring.jmx.StatManager;
import nexj.core.monitoring.jmx.StatUtil;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.InvocationContext;
import nexj.core.util.Logger;
import nexj.core.util.ObjUtil;

/**
 * Generic receiver implementation.
 */
public abstract class Receiver
{
   // operations

   /**
    * Determines if a channel is bound to a service.
    * @param channel The channel.
    * @param context The invocation context.
    * @return True if the channel is bound.
    */
   public boolean isBound(Channel channel, InvocationContext context)
   {
      return channel.getBindingCount() != 0;
   }

   /**
    * Initializes the statistics.
    * @param channel The channel.
    * @param manager The statistics manager.
    */
   protected void initStat(Channel channel, StatManager manager)
   {
      String sReceiverStatPath = channel.getReceiverStatPath();

      if (sReceiverStatPath != null)
      {
         manager.defineStatistic(sReceiverStatPath, Channel.STAT_TOTAL_COUNT,
            "counter", true, Channel.STAT_PERSIST_CLASS);

         manager.defineStatistic(sReceiverStatPath, Channel.STAT_AVERAGE_RECEIVE_TIME,
            "mean", true, Channel.STAT_PERSIST_CLASS);
      }
   }

   /**
    * Starts collecting statistics.
    * @param channel The channel.
    * @param context The invocation context.
    * @return The start time in nanoseconds from a system-dependent offset. 
    */
   protected long startStat(Channel channel, InvocationContext context)
   {
      long lStartTime = System.nanoTime();

      StatUtil.incrCounter(context, channel.getReceiverStatPath(), Channel.STAT_TOTAL_COUNT, 1);

      return lStartTime;
   }

   /**
    * Ends collecting the statistics.
    * @param lStartTime The start time in nanoseconds from a system-dependent offset.
    * @param channel The channel.
    * @param context The invocation context.
    */
   protected void endStat(long lStartTime, Channel channel, InvocationContext context)
   {
      StatUtil.updateAverage(context, channel.getReceiverStatPath(), Channel.STAT_AVERAGE_RECEIVE_TIME,
         (double)(System.nanoTime() - lStartTime) / 1000000);
   }

   /**
    * Processes a message received on a specified channel.
    * @param tobj The message.
    * @param channel The channel, on which the message has been received.
    * @param context The invocation context.
    */
   public void receive(TransferObject tobj, Channel channel, InvocationContext context) throws IntegrationException
   {
      long lStartTime = startStat(channel, context);
      Logger logger = channel.getLogger();

      if (logger.isDumpEnabled())
      {
         logger.dump("Received: " + tobj);
      }

      context.getUnitOfWork().checkLicense();

      try
      {
         Object body = tobj.findValue(Sender.BODY);
         MessageTable table = channel.getMessageTable();
         Iterator itr = null;

         if (table != null && table.getFormat() != null)
         {
            Input input = (body instanceof Input) ? (Input)body : new ObjectInput(body);

            if (logger.isDebugEnabled())
            {
               input = new DebugInput(input);
            }

            try
            {
               tobj = ((MessageParser)table.getFormat().getParser().getInstance(context)).parse(input, table);
               itr = channel.getBindingIterator(table.getMessage(tobj.getClassName()));
            }
            catch (IntegrationException e)
            {
               if (input instanceof DebugInput && logger.isDebugEnabled())
               {
                  logger.debug("Failed to parse " + tobj, e);
                  logger.debug("Message prefix: " + ((DebugInput)input).getPrefix());
               }

               ObjUtil.rethrow(e);
            }
         }
         else
         {
            if (body instanceof Input)
            {
               tobj.setValue(Sender.BODY, ((Input)body).getObject());
            }

            itr = channel.getBindingIterator();
         }

         while (itr.hasNext())
         {
            context.getMachine().invoke(((Binding)itr.next()).getFunction(), tobj, (Object[])null);  
         }
      }
      finally
      {
         endStat(lStartTime, channel, context);
      }
   }
}
