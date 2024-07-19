package nexj.core.util.pool.consumer;

import javax.transaction.TransactionManager;
import javax.transaction.xa.XAResource;

import nexj.core.util.Deque;
import nexj.core.util.HashDeque;

import junit.framework.TestCase;

public class GenericConsumerPoolTest extends TestCase
{
   protected TestConsumerAdapter m_adapter;
   protected TestConsumerPool m_pool;

   protected void setUp() throws Exception
   {
      m_adapter = new TestConsumerAdapter();
      m_pool = create();
      m_pool.start();
   }

   protected void tearDown() throws Exception
   {
      m_pool.stop(true);
      m_pool = null;
      m_adapter = null;
   }

   /**
    * @return A new consumer pool instance.
    */
   protected TestConsumerPool create()
   {
      return new TestConsumerPool(new TestConsumerConfig(), m_adapter);
   }

   /**
    * Simulates requests against the pool.
    * @param nCount The number of requests to make.
    */
   protected void request(int nCount)
   {
      for (int i = 0; i < nCount; ++i)
      {
         m_pool.receive(new Integer(i));
      }
   }

   /**
    * Verifies the pool request counts.
    * @param nRequestCount The number of requests that have been made.
    * @param nStopCount The number of times the pool has been stopped.
    * @param bEmpty True to expect an empty pool.
    */
   protected void verify(int nRequestCount, int nStopCount, boolean bEmpty)
   {
      m_pool.waitWhileBusy();

      assertEquals(Math.max(0, nStopCount - 1) + 1, m_pool.connectCount);
      assertEquals(nStopCount + 1, m_pool.suspendCount);
      assertEquals(nStopCount + 1, m_pool.closeCount);

      assertEquals(nRequestCount, m_pool.consumeCount);
      assertEquals(nRequestCount, m_pool.resetCount);
      assertEquals(Math.min(nRequestCount, Math.max(0, nStopCount - 1) + ((bEmpty) ? 1 : 0)), m_pool.dropCount);
      assertEquals(Math.max(Math.min(nRequestCount, 1), nStopCount), m_pool.initCount);

      assertEquals(Math.max(Math.min(nRequestCount, 1), nStopCount), m_adapter.getInterceptorCount);
      assertEquals(Math.min(nRequestCount, Math.max(0, nStopCount - 1) + ((bEmpty) ? 1 : 0)), m_adapter.releaseInterceptorCount);
      assertEquals(nRequestCount, m_adapter.beginCount);
      assertEquals(nRequestCount, m_adapter.endCount);
      assertEquals(nRequestCount, m_adapter.processCount);
   }

   public void testConnect()
   {
      m_pool.waitUntilConnected();
      verify(0, 0, true);
   }

   public void testListen()
   {
      request(3);
      verify(3, 0, false);
   }

   public void testStop()
   {
      request(3);
      m_pool.waitWhileBusy();
      m_pool.stop(true);
      verify(3, 1, true);
   }

   public void testSetConfig()
   {
      request(3);
      m_pool.waitWhileBusy();
      m_pool.setConfig(new TestConsumerConfig(0, 15));

      assertEquals(0, m_pool.getMaxSize());
      assertEquals(15, m_pool.getConfig().getRetryDelay());

      verify(3, 1, true);

      m_pool.setConfig(new TestConsumerConfig(1, 10));

      assertEquals(1, m_pool.getMaxSize());
      assertEquals(10, m_pool.getConfig().getRetryDelay());

      request(3);
      verify(6, 2, false);
   }

   public void testMaintain()
   {
      request(3);
      m_pool.waitWhileBusy();
      assertEquals(1, m_pool.m_nConsumerCount);

      try
      {
         Thread.sleep(m_pool.getIdleTimeout() << 3);
      }
      catch (InterruptedException e)
      {
      }

      m_pool.maintain();
      verify(3, 0, true);
   }

   // inner classes

   public static class TestConsumerAdapter implements ConsumerAdapter
   {
      public int getInterceptorCount;
      public int releaseInterceptorCount;
      public int beginCount;
      public int processCount;
      public int endCount;

      public TransactionManager getTransactionManager()
      {
         return null;
      }

      public void run(Runnable runnable) throws Throwable
      {
         new Thread(runnable).start();
      }

      /**
       * @see nexj.core.util.pool.consumer.ConsumerAdapter#run(nexj.core.util.pool.consumer.Consumer)
       */
      public void run(Consumer consumer) throws Throwable
      {
         consumer.run();
      }

      /**
       * @see nexj.core.util.pool.consumer.ConsumerAdapter#getInterceptor(nexj.core.util.pool.consumer.Consumer)
       */
      public Object getInterceptor(Consumer consumer) throws Throwable
      {
         ++this.getInterceptorCount;

         return null;
      }

      /**
       * @see nexj.core.util.pool.consumer.ConsumerAdapter#begin(nexj.core.util.pool.consumer.Consumer, java.lang.Object)
       */
      public void begin(Consumer consumer, Object interceptor) throws Throwable
      {
         ++this.beginCount;
      }

      /**
       * @see nexj.core.util.pool.consumer.ConsumerAdapter#process(nexj.core.util.pool.consumer.Consumer, java.lang.Object, java.lang.Object)
       */
      public void process(Consumer consumer, Object interceptor, Object request) throws Throwable
      {
         assert request instanceof Integer; 

         ++this.processCount;
      }

      /**
       * @see nexj.core.util.pool.consumer.ConsumerAdapter#end(java.lang.Object)
       */
      public void end(Object interceptor) throws Throwable
      {
         ++this.endCount;
      }

      /**
       * @see nexj.core.util.pool.consumer.ConsumerAdapter#releaseInterceptor(java.lang.Object)
       */
      public void releaseInterceptor(Object interceptor) throws Throwable
      {
         ++this.releaseInterceptorCount;
      }

      /**
       * @see nexj.core.util.pool.consumer.ConsumerAdapter#dispose()
       */
      public void dispose()
      {
      }

      /**
       * @see nexj.core.util.pool.consumer.ConsumerAdapter#maintain()
       */
      public void maintain()
      {
      }
   }

   public static class TestConsumerConfig implements ConsumerConfig
   {
      protected int m_nMaxSize;
      protected long m_lRetryDelay;

      public TestConsumerConfig(int nMaxSize, long lRetryDelay)
      {
         m_nMaxSize = nMaxSize;
         m_lRetryDelay = lRetryDelay;
      }

      public TestConsumerConfig()
      {
         this(3, 10);
      }

      public int getMaxPoolSize()
      {
         return m_nMaxSize;
      }

      public long getRetryDelay()
      {
         return m_lRetryDelay;
      }

      public long getMaxRetryDelay()
      {
         return 100;
      }

      public boolean isTransactional()
      {
         return false;
      }

      public int getTransactionTimeout()
      {
         return 0;
      }

      public boolean isCompatible(ConsumerConfig config)
      {
         return true;
      }
   }

   public static class TestConsumer extends GenericConsumer
   {
      protected Object m_request;

      protected TestConsumer(GenericConsumerPool pool) throws Throwable
      {
         super(pool);
      }

      public XAResource getXAResource()
      {
         return null;
      }

      public void start(Object request) throws Throwable
      {
         m_request = request;
         activate();
      }

      /**
       * @see nexj.core.util.pool.consumer.GenericConsumer#init()
       */
      protected void init() throws Throwable
      {
         ++((TestConsumerPool)m_pool).initCount;
      }

      protected boolean consume() throws Throwable
      {
         ++((TestConsumerPool)m_pool).consumeCount;
         consume(m_request);

         return false;
      }

      public void reset()
      {
         m_request = null;
         ++((TestConsumerPool)m_pool).resetCount;
      }

      /**
       * @see nexj.core.util.pool.consumer.GenericConsumer#drop()
       */
      protected void drop() throws Throwable
      {
         m_request = null;
         ++((TestConsumerPool)m_pool).dropCount;
      }
   }

   public static class TestConsumerPool extends GenericConsumerPool
   {
      public int connectCount;
      public int suspendCount;
      public int closeCount;

      public int initCount;
      public int consumeCount;
      public int resetCount;
      public int dropCount;

      protected boolean m_bSuspended;
      protected boolean m_bStatusUpdated;
      protected Deque m_requestDeque; 

      protected TestConsumerPool(ConsumerConfig config, ConsumerAdapter adapter)
      {
         super(config, adapter);
      }

      public boolean isTransactional()
      {
         return false;
      }

      protected synchronized void connect() throws Throwable
      {
         m_bStatusUpdated = !isMonitored();

         if (m_requestDeque == null)
         {
            m_requestDeque = new HashDeque();
         }

         m_bSuspended = false;
         notifyAll();

         ++this.connectCount;
      }

      protected synchronized void suspend()
      {
         m_bSuspended = true;
         notifyAll();

         ++this.suspendCount;
      }

      protected synchronized void close()
      {
         m_requestDeque = null;
         m_bSuspended = false;
         notifyAll();

         ++this.closeCount;
      }

      protected long getIdleTimeout()
      {
         return 8;
      }

      protected void listen()
      {
         if (m_requestDeque != null && m_requestDeque.size() == 0 && !m_bSuspended)
         {
            try
            {
               wait();
            }
            catch (InterruptedException e)
            {
            }
         }

         if (m_requestDeque != null && m_requestDeque.size() != 0)
         {
            TestConsumer consumer = null;

            try
            {
               consumer = ((TestConsumer)getConsumer());
               consumer.start(m_requestDeque.removeFirst());
               consumer = null;
            }
            catch (Throwable t)
            {
               m_logger.error("Unexpected error in " + this, t);
            }
            finally
            {
               if (consumer != null)
               {
                  consumer.dispose();
               }
            }

            notifyAll();
         }
      }

      protected GenericConsumer createConsumer() throws Throwable
      {
         return new TestConsumer(this);
      }

      public synchronized void waitUntilConnected()
      {
         while ((m_requestDeque == null || !m_bStatusUpdated) && !m_bStop)
         {
            try
            {
               wait();
            }
            catch (InterruptedException e)
            {
            }
         }
      }

      public void waitWhileBusy()
      {
         synchronized (this)
         {
            while (m_requestDeque != null && m_requestDeque.size() != 0)
            {
               try
               {
                  wait();
               }
               catch (InterruptedException e)
               {
               }
            }
         }

         synchronized (m_idleConsumerList)
         {
            while (m_nConsumerCount != m_idleConsumerList.size())
            {
               try
               {
                  m_idleConsumerList.wait();
               }
               catch (InterruptedException e)
               {
               }
            }
         }
      }

      public synchronized void receive(Object request)
      {
         waitUntilConnected();

         if (m_requestDeque == null || m_bSuspended)
         {
            throw new IllegalStateException();
         }

         m_requestDeque.addLast(request);
         notifyAll();
      }
   }
}
