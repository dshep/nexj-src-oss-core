package nexj.core.rpc.udp;

import java.net.DatagramPacket;

import javax.transaction.xa.XAResource;

import nexj.core.meta.integration.channel.udp.UDPChannel;
import nexj.core.util.pool.consumer.GenericConsumer;
import nexj.core.util.pool.consumer.GenericConsumerPool;

/**
 * UDP consumer.
 */
public class UDPConsumer extends GenericConsumer
{
   // associations

   /**
    * The buffer for receiving the packets.
    */
   protected DatagramPacket m_packet;

   // constructors

   /**
    * Constructs the consumer.
    * @param The consumer pool. 
    */
   public UDPConsumer(GenericConsumerPool pool) throws Throwable
   {
      super(pool);
   }

   // operations

   /**
    * @see nexj.core.util.pool.consumer.Consumer#getXAResource()
    */
   public XAResource getXAResource()
   {
      return null;
   }

   /**
    * @return The datagram packet.
    */
   public DatagramPacket getPacket()
   {
      return m_packet;
   }

   /**
    * Starts the consumer.
    */
   public void start() throws Throwable
   {
      activate();
   }

   /**
    * @see nexj.core.util.pool.consumer.GenericConsumer#init()
    */
   protected void init() throws Throwable
   {
      int nCount = ((UDPChannel)m_config).getMaxPacketSize();

      m_packet = new DatagramPacket(new byte[nCount], nCount);
   }

   /**
    * @see nexj.core.util.pool.consumer.GenericConsumer#consume()
    */
   protected boolean consume() throws Throwable
   {
      consume(m_packet);

      return false;
   }

   /**
    * @see nexj.core.util.pool.consumer.Consumer#reset()
    */
   public void reset()
   {
   }

   /**
    * @see nexj.core.util.pool.consumer.GenericConsumer#drop()
    */
   protected void drop() throws Throwable
   {
   }
}
