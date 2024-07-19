package nexj.core.rpc.pool.ra;

import nexj.core.meta.integration.Channel;
import nexj.core.rpc.ServerMDB;
import nexj.core.rpc.pool.Processor;
import nexj.core.util.Logger;
import nexj.core.util.pool.consumer.ConsumerConfig;

/**
 * Generic request processor used by the dynamic resource adapter.
 */
public class DynamicMDB extends ServerMDB implements DynamicProcessor
{
   // constants

   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = 2574406846764659994L;

   // associations

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(DynamicMDB.class);

   // operations

   /**
    * @see nexj.core.rpc.ServerMDB#getLogger()
    */
   protected Logger getLogger()
   {
      return s_logger;
   }

   /**
    * @see nexj.core.rpc.pool.ra.DynamicProcessor#process(java.lang.Object, nexj.core.util.pool.consumer.ConsumerConfig)
    */
   public void process(Object request, ConsumerConfig config) throws Throwable
   {
      ((Processor)((Channel)config).getReceiver().getInstance(null)).process(request);
   }
}
