package nexj.core.util.pool.resource;

import junit.framework.TestCase;

public class GenericResourcePoolTest extends TestCase
{
   protected GenericResourcePool m_pool;
   protected TestResource m_res;

   protected void setUp() throws Exception
   {
      m_pool = create();
      m_res = (TestResource)m_pool.get("a");
   }

   protected void tearDown() throws Exception
   {
      m_pool = null;
      m_res = null;
   }

   /**
    * @return A new resource pool instance.
    */
   protected GenericResourcePool create()
   {
      return new TestResourcePool();
   }

   public void testGet()
   {
      assertEquals(0, m_pool.m_nIdleCount);
      assertEquals(1, m_pool.m_nResourceCount);

      TestResource res2 = (TestResource)m_pool.get("a");

      assertEquals(0, m_pool.m_nIdleCount);
      assertEquals(2, m_pool.m_nResourceCount);
      assertNotSame(res2, m_res);
      assertEquals("a", m_res.config);
      assertFalse(m_res.isDropped);
      assertFalse(m_res.isReset);

      long lStartTime = System.nanoTime();

      try
      {
         m_pool.get("a");
         fail("Expected PoolBusyException");
      }
      catch (PoolBusyException e)
      {
      }

      long lTime = System.nanoTime() - lStartTime;

      assertTrue(lTime >= 900000);

      assertEquals(0, m_pool.m_nIdleCount);
      assertEquals(2, m_pool.m_nResourceCount);

      res2.reference();
      res2.release();
      assertFalse(res2.isReset);
      assertEquals(0, m_pool.m_nIdleCount);
      assertEquals(2, m_pool.m_nResourceCount);

      res2.release();
      assertTrue(res2.isReset);
      assertEquals(1, m_pool.m_nIdleCount);
      assertEquals(2, m_pool.m_nResourceCount);

      assertSame(res2, m_pool.get("a"));
      assertEquals(0, m_pool.m_nIdleCount);
      assertEquals(2, m_pool.m_nResourceCount);
      assertFalse(res2.isReset);

      res2.release();
      assertTrue(res2.isReset);
      assertEquals(1, m_pool.m_nIdleCount);
      assertEquals(2, m_pool.m_nResourceCount);

      TestResource res3 = (TestResource)m_pool.get("x");

      assertNotSame(m_res, res3);
      assertNotSame(res2, res3);
      assertTrue(res2.isDropped);
      assertEquals(0, m_pool.m_nIdleCount);
      assertEquals(2, m_pool.m_nResourceCount);

      res3.release();

      assertEquals(1, m_pool.m_nIdleCount);
      assertEquals(2, m_pool.m_nResourceCount);
      assertTrue(res3.isReset);

      res2 = (TestResource)m_pool.get("x");
      assertNotSame(res3, res2);
      assertEquals(0, m_pool.m_nIdleCount);
      assertEquals(2, m_pool.m_nResourceCount);
      assertFalse(res2.isReset);

      res2.release();
      assertEquals(1, m_pool.m_nIdleCount);
      assertEquals(2, m_pool.m_nResourceCount);
      assertTrue(res2.isReset);

      res3 = (TestResource)m_pool.get("i");

      assertNotSame(m_res, res3);
      assertNotSame(res2, res3);
      assertTrue(res2.isDropped);
      assertEquals(0, m_pool.m_nIdleCount);
      assertEquals(2, m_pool.m_nResourceCount);

      res3.release();

      assertEquals(1, m_pool.m_nIdleCount);
      assertEquals(2, m_pool.m_nResourceCount);
      assertTrue(res3.isReset);

      res2 = (TestResource)m_pool.get("i");
      assertNotSame(res3, res2);
      assertEquals(0, m_pool.m_nIdleCount);
      assertEquals(2, m_pool.m_nResourceCount);
      assertFalse(res2.isReset);

      res2.release();
      assertEquals(1, m_pool.m_nIdleCount);
      assertEquals(2, m_pool.m_nResourceCount);
      assertTrue(res2.isReset);
   }

   public void testRelease()
   {
      m_pool.release(m_res);

      assertEquals(1, m_pool.m_nIdleCount);
      assertEquals(1, m_pool.m_nResourceCount);
      assertTrue(m_res.isReset);
      assertFalse(m_res.isDropped);
   }

   public void testDisposeResource()
   {
      m_pool.dispose(m_res);

      assertEquals(0, m_pool.m_nIdleCount);
      assertEquals(0, m_pool.m_nResourceCount);
      assertFalse(m_res.isReset);
      assertTrue(m_res.isDropped);
   }

   public void testDispose()
   {
      m_pool.dispose();

      assertEquals(0, m_pool.m_nIdleCount);
      assertEquals(0, m_pool.m_nResourceCount);
      assertFalse(m_res.isReset);
      assertTrue(m_res.isDropped);
   }

   public void testMaintain() throws Exception
   {
      m_pool.maintain();
      m_res.release();
      Thread.sleep(20);
      assertEquals(1, m_pool.m_nIdleCount);
      assertEquals(1, m_pool.m_nResourceCount);
      m_pool.maintain();
      assertEquals(0, m_pool.m_nIdleCount);
      assertEquals(0, m_pool.m_nResourceCount);
      assertTrue(m_res.isReset);
      assertTrue(m_res.isDropped);
   }

   public void testGetBusyTimeout()
   {
      assertEquals(1, m_pool.getBusyTimeout());
   }

   public void testGetIdleTimeout()
   {
      assertEquals(3, m_pool.getIdleTimeout());
   }

   public void testIsTransactional()
   {
      assertFalse(m_pool.isTransactional());
   }

   // inner classes

   public static class TestResource extends GenericResource
   {
      public Object config;
      public boolean isReset;
      public boolean isDropped;

      public TestResource(Object config)
      {
         this.config = config;
      }

      public void reset()
      {
         this.isReset = true;
      }

      protected void drop() throws Throwable
      {
         this.isDropped = true;
      }
   }

   public static class TestResourcePool extends GenericResourcePool
   {
      public int getMaxSize()
      {
         return 2;
      }

      public long getBusyTimeout()
      {
         return 1;
      }

      public long getIdleTimeout()
      {
         return 3;
      }

      protected boolean isValid(Resource resource, Object config) throws Exception
      {
         ((TestResource)resource).isReset = false;

         if ("x".equals(config))
         {
            throw new RuntimeException();
         }

         return !"i".equals(config);
      }

      protected Resource create(Object config) throws Exception
      {
         return new TestResource(config);
      }
   }
}
