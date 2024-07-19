// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;


/**
 * Test for WeakKeyHashTab
 */
public class WeakKeyHashTabTest extends HashTabTest
{
   /**
    * @see nexj.core.util.HashTab2DTest#init()
    */
   protected void init()
   {
      m_htab = new WeakKeyHashTab(1);
   }

   /**
    * This test is dependent on JVM garbage collection.
    * 
    * @throws InterruptedException
    */
   public void testReferenceCleanup() throws InterruptedException
   {
      m_htab.clear();

      assertEquals(0, m_htab.size());

      Object a = new Object();
      Object b = new Object();
      Object c = new Object();

      m_htab.put(a, "Value A");
      m_htab.put(b, "Value B");
      m_htab.put(c, "Value C");

      assertEquals(3, m_htab.size());

      System.gc();
      Thread.sleep(200);
      System.gc();

      assertEquals(3, m_htab.size());

      a = null;

      System.gc();
      Thread.sleep(200);
      System.gc();

      assertEquals(2, m_htab.size());

      int i = 0;

      for (Lookup.Iterator it = m_htab.iterator(); it.hasNext();)
      {
         assertNotNull(it.next());
         assertNotNull(it.getKey());
         assertNotNull(it.getValue());
         i++;
      }

      assertEquals(2, i);

      assertEquals(m_htab.get(b), "Value B");
      assertEquals(m_htab.get(c), "Value C");

      b = null;
      c = null;

      System.gc();
      Thread.sleep(200);
      System.gc();

      assertEquals(0, m_htab.size());

      for (Lookup.Iterator it = m_htab.iterator(); it.hasNext();)
      {
         assertFalse(true);
      }
   }
}
