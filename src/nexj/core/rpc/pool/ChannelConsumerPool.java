package nexj.core.rpc.pool;

import nexj.core.meta.integration.Channel;
import nexj.core.util.pool.consumer.ConsumerAdapter;
import nexj.core.util.pool.consumer.ConsumerConfig;

/**
 * Channel-specific consumer pool.
 */
public abstract class ChannelConsumerPool extends StatConsumerPool
{
   // constructors

   /**
    * Constructs the consumer pool.
    * @param The integration channel.
    * @param adapter The consumer adapter.
    */
   public ChannelConsumerPool(Channel channel, ConsumerAdapter adapter)
   {
      super((ConsumerConfig)channel, adapter, channel.getLogger(), channel.getEnabler());
   }

   // operations

   /**
    * @see nexj.core.rpc.pool.StatConsumerPool#getStatPath()
    */
   protected String getStatPath()
   {
      return ((Channel)m_config).getReceiverStatPath();
   }

   /**
    * @see nexj.core.rpc.pool.StatConsumerPool#getStatClassName()
    */
   protected String getStatClassName()
   {
      return Channel.STAT_PERSIST_CLASS;
   }

   /**
    * @see nexj.core.rpc.pool.StatConsumerPool#getConsumerName()
    */
   protected String getConsumerName()
   {
      return "Receiver";
   }

   /**
    * @see nexj.core.util.pool.consumer.GenericConsumerPool#isTransactional()
    */
   public boolean isTransactional()
   {
      return ((Channel)m_config).isTransactional();
   }
}
