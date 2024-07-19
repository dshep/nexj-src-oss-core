package nexj.core.rpc.udp;

import java.net.SocketAddress;

import nexj.core.meta.integration.channel.udp.UDPChannel;
import nexj.core.rpc.pool.ChannelResourcePool;
import nexj.core.util.pool.resource.Resource;

/**
 * UPD connection pool.
 */
public class UDPConnectionPool extends ChannelResourcePool
{
   // constructors

   /**
    * The UDP connection pool.
    */
   public UDPConnectionPool(UDPChannel channel)
   {
      super(channel);
   }

   // operations

   /**
    * @see nexj.core.util.pool.resource.ResourcePool#getMaxSize()
    */
   public int getMaxSize()
   {
      return ((UDPChannel)m_channel).getMaxSenders();
   }

   /**
    * @see nexj.core.util.pool.resource.GenericResourcePool#create(java.lang.Object)
    * @param config An instance of SocketAddress.
    */
   protected Resource create(Object config) throws Exception
   {
      return new UDPConnection((SocketAddress)config);
   }
}
