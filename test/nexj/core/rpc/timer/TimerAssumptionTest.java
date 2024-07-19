package nexj.core.rpc.timer;

import nexj.core.meta.integration.channel.timer.Timer;
import nexj.core.rpc.pool.PoolManager;

public class TimerAssumptionTest extends TimerTest
{
   protected void setUp() throws Exception
   {
      super.setUp();

      Timer channel = new Timer(m_channel.getName());

      init(channel);

      PoolManager poolManager = new PoolManager();

      poolManager.addPoolProvider(channel);
      poolManager.setStatManager(m_statManager);
      poolManager.setReceiveEnabled(true);
      poolManager.assume(m_poolManager);
      m_poolManager.complete();
      m_poolManager = poolManager;
      m_channel = channel;
   }
}
