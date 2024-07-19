package nexj.core.util.pool.consumer;

public class MonitoredGenericConsumerPoolTest extends GenericConsumerPoolTest
{
   protected int m_nActivationCount;
   protected int m_nDeactivationCount;
   protected int m_nOkCount;

   protected void setUp() throws Exception
   {
      m_nActivationCount = 0;
      m_nDeactivationCount = 0;
      m_nOkCount = 0;

      super.setUp();
   }

   protected TestConsumerPool create()
   {
      return new TestConsumerPool(new TestConsumerConfig(), m_adapter)
      {
         protected boolean isMonitored()
         {
            return true;
         }

         protected void monitorActivation(int nIdleCount, int nActiveCount, long lBusyTime)
         {
            synchronized (MonitoredGenericConsumerPoolTest.this)
            {
               ++m_nActivationCount;
            }
         }

         protected void monitorDeactivation(int nIdleCount, int nActiveCount, long lActiveTime)
         {
            if (lActiveTime >= 0)
            {
               synchronized (MonitoredGenericConsumerPoolTest.this)
               {
                  ++m_nDeactivationCount;
               }
            }
         }

         protected void monitorStatus(boolean bOk)
         {
            if (bOk)
            {
               synchronized (MonitoredGenericConsumerPoolTest.this)
               {
                  ++m_nOkCount;
               }
            }

            synchronized (this)
            {
               m_bStatusUpdated = true;
               notifyAll();
            }
         }
      };
   }

   /**
    * @see nexj.core.util.pool.consumer.GenericConsumerPoolTest#verify(int, int, boolean)
    */
   protected void verify(int nRequestCount, int nStopCount, boolean bEmpty)
   {
      super.verify(nRequestCount, nStopCount, bEmpty);

      synchronized (this)
      {
         assertEquals(nRequestCount, m_nActivationCount);
         assertEquals(nRequestCount, m_nDeactivationCount);
         assertEquals(Math.max(0, nStopCount - 1) + 1, m_nOkCount);
      }
   }
}
