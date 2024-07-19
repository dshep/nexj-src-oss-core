package nexj.core.rpc.udp;

import nexj.core.meta.integration.channel.udp.UDPChannel;
import nexj.core.rpc.pool.PoolManager;

public class UDPAssumptionTest extends UDPTest
{
   /**
    * @see nexj.core.rpc.udp.UDPTest#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();

      UDPChannel channel = new UDPChannel(m_channel.getName());

      init(channel);
      channel.setPort(m_channel.getPort());

      PoolManager poolManager = new PoolManager();

      poolManager.addPoolProvider(channel);
      poolManager.setStatManager(m_statManager);
      poolManager.setReceiveEnabled(true);
      poolManager.assume(m_poolManager);
      m_poolManager.complete();
      m_poolManager = poolManager;
      m_channel = channel;
      m_sender = (UDPSender)channel.getSender().getInstance(null);
      m_receiver = (UDPReceiver)channel.getReceiver().getInstance(null);
      m_receiver.setStatManager(m_statManager);
   }
}
