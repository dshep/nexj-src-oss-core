package nexj.core.util.pool.resource;

public class MonitoredGenericResourcePoolTest extends GenericResourcePoolTest
{
   protected int m_nActivationCount;
   protected int m_nDeactivationCount;
   protected int m_nSuccessCount;

   /**
    * @see nexj.core.util.pool.resource.GenericResourcePoolTest#setUp()
    */
   protected void setUp() throws Exception
   {
      m_nActivationCount = 0;
      m_nDeactivationCount = 0;
      m_nSuccessCount = 0;

      super.setUp();
   }

   /**
    * @see nexj.core.util.pool.resource.GenericResourcePoolTest#create()
    */
   protected GenericResourcePool create()
   {
      return new TestResourcePool()
      {
         protected boolean isMonitored()
         {
            return true;
         }

         protected void monitorActivation(int nIdleCount, int nActiveCount, long lBusyTime, boolean bSuccess)
         {
            ++m_nActivationCount;

            if (bSuccess)
            {
               ++m_nSuccessCount;
            }
         }

         /**
          * @see nexj.core.util.pool.resource.GenericResourcePool#monitorDeactivation(int, int, long)
          */
         protected void monitorDeactivation(int nIdleCount, int nActiveCount, long lActiveTime)
         {
            ++m_nDeactivationCount;
         }
      };
   }

   /**
    * @see nexj.core.util.pool.resource.GenericResourcePoolTest#testGet()
    */
   public void testGet()
   {
      super.testGet();
      assertEquals(8, m_nActivationCount);
      assertEquals(6, m_nDeactivationCount);
      assertEquals(7, m_nSuccessCount);
   }
}
