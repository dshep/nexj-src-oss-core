package nexj.core.rpc.timer;

import junit.framework.TestCase;

import nexj.core.meta.Component;
import nexj.core.meta.Repository;
import nexj.core.meta.integration.ChannelType;
import nexj.core.meta.integration.channel.timer.Timeout;
import nexj.core.meta.integration.channel.timer.Timer;
import nexj.core.monitoring.jmx.StatManager;
import nexj.core.rpc.pool.PoolManager;
import nexj.core.rpc.pool.Processor;
import nexj.test.util.Wait;

/**
 * Timer consumer pool test.
 */
public class TimerTest extends TestCase
{
   protected Timer m_channel;
   protected StatManager m_statManager;
   protected PoolManager m_poolManager;

   /**
    * @see junit.framework.TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      m_statManager = (StatManager)Repository.getMetadata().getComponent("System.StatManager").getInstance(null);
      m_channel = new Timer("Timer");
      init(m_channel);
      m_poolManager = new PoolManager();
      m_poolManager.addPoolProvider(m_channel);
      m_poolManager.setStatManager(m_statManager);
      m_poolManager.setReceiveEnabled(true);
      m_poolManager.startup();

      final TimerConsumerPool pool = (TimerConsumerPool)m_channel.getConsumerPool();

      if (!new Wait()
      {
         protected boolean isOver()
         {
            return pool.isAvailable();
         }
      }.proceed(30000))
      {
         m_poolManager.shutdown();
         fail("Unable to start the timer");
      }
   }

   /**
    * @see junit.framework.TestCase#tearDown()
    */
   protected void tearDown() throws Exception
   {
      m_poolManager.shutdown();
   }

   /**
    * Initializes the channel.
    * @param timer The channel to initialize.
    */
   protected void init(Timer timer)
   {
      timer.setType(new ChannelType("Timer"));
      timer.setStealth(false);
      timer.setReceiver(new Component("TimerProcessor", TimerProcessor.class, Component.SINGLETON));
      timer.setReceivable(true);
      timer.addTimeout(new Timeout(5));
      timer.addTimeout(new Timeout(1, 3));
   }

   public void testTimeout()
   {
      final TimerProcessor processor = (TimerProcessor)m_channel.getReceiver().getInstance(null);

      assertTrue("Failed to get timer notifications", new Wait()
      {
         protected boolean isOver()
         {
            synchronized (processor)
            {
               return processor.periodicCount + processor.aperiodicCount >= 8;
            }
         }
      }.proceed(30000));

      synchronized (processor)
      {
         assertEquals(3, processor.periodicCount);
         assertEquals(5, processor.aperiodicCount);
      }
   }

   // inner classes

   /**
    * Test timer processor.
    */
   public static class TimerProcessor implements Processor
   {
      public int periodicCount;
      public int aperiodicCount;

      /**
       * @see nexj.core.rpc.pool.Processor#process(java.lang.Object)
       */
      public synchronized void process(Object request) throws Throwable
      {
         nexj.core.rpc.timer.Timeout timeout = (nexj.core.rpc.timer.Timeout)request;

         if (timeout.getPeriod() != Long.MAX_VALUE)
         {
            if (++periodicCount == 3)
            {
               timeout.setTime(Long.MAX_VALUE);
            }
         }
         else
         {
            if (++aperiodicCount != 5)
            {
               timeout.setTime(System.currentTimeMillis());
            }
         }
      }
   }
}
