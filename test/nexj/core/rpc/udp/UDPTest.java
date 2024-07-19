package nexj.core.rpc.udp;

import java.util.Collections;

import junit.framework.TestCase;

import nexj.core.integration.Sender;
import nexj.core.meta.Component;
import nexj.core.meta.Repository;
import nexj.core.meta.integration.channel.udp.UDPChannel;
import nexj.core.monitoring.Counter;
import nexj.core.monitoring.jmx.StatManager;
import nexj.core.monitoring.jmx.StatUtil;
import nexj.core.rpc.TransferObject;
import nexj.core.rpc.pool.PoolManager;
import nexj.test.util.Wait;

/**
 * UDP sender/receiver/pool test.
 */
public class UDPTest extends TestCase
{
   protected UDPChannel m_channel;
   protected StatManager m_statManager;
   protected PoolManager m_poolManager;
   protected UDPSender m_sender;
   protected UDPReceiver m_receiver;

   protected void setUp() throws Exception
   {
      m_statManager = (StatManager)Repository.getMetadata().getComponent("System.StatManager").getInstance(null);
      m_channel = new UDPChannel("UDPTest");
      init(m_channel);
      m_poolManager = new PoolManager();
      m_poolManager.addPoolProvider(m_channel);
      m_poolManager.setStatManager(m_statManager);
      m_poolManager.setReceiveEnabled(true);
      m_sender = (UDPSender)m_channel.getSender().getInstance(null);
      m_receiver = (UDPReceiver)m_channel.getReceiver().getInstance(null);
      m_receiver.setStatManager(m_statManager);

      m_poolManager.startup();

      final UDPConsumerPool pool = (UDPConsumerPool)m_channel.getConsumerPool();

      if (new Wait()
      {
         protected boolean isOver()
         {
            return pool.isAvailable();
         }
      }.proceed(30000))
      {
         m_channel.setPort(pool.getSocket().getLocalPort());
         m_channel.setLocalPort(0);
      }
      else
      {
         m_poolManager.shutdown();
         fail("Unable to find a free UDP port");
      }
   }

   protected void tearDown() throws Exception
   {
      m_poolManager.shutdown();
   }

   /**
    * Initializes the channel.
    * @param udp The channel to initialize.
    */
   protected void init(UDPChannel udp)
   {
      udp.setType(Repository.getMetadata().getChannelType("UDP"));

      Component sender = new Component("UDPSender", UDPSender.class, Component.SINGLETON);

      sender.addPrimitivePropertyInitializer("channel", udp);
      udp.setSender(sender);
      udp.setSendable(true);

      Component receiver = new Component("UDPReceiver", UDPReceiver.class, Component.SINGLETON);

      receiver.addPrimitivePropertyInitializer("channel", udp);
      udp.setReceiver(receiver);
      udp.setReceivable(true);

      udp.setLocalHost("localhost");
      udp.setHost(udp.getLocalHost());
   }

   public void testReceive()
   {
      TransferObject tobj = new TransferObject(1);

      tobj.setValue(Sender.BODY, "Hi!");

      final Counter cnt = (Counter)StatUtil.findStat(Repository.getMetadata(),
         m_channel.getReceiverStatPath(), UDPChannel.STAT_TOTAL_COUNT);
      final long lStartCount = cnt.get();

      m_sender.send(tobj);
      m_sender.send(Collections.singletonList(tobj));

      assertTrue("Failed to receive the messages", new Wait()
      {
         protected boolean isOver()
         {
            return cnt.get() == lStartCount + 2;
         }
      }.proceed(3000)); 
   }
}
