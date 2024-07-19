// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.util.Collection;

import nexj.core.util.Lookup;
import nexj.core.util.MultiMap;

import junit.framework.TestCase;

public class MultiMapTest extends TestCase
{
   MultiMap m_map = new MultiMap();

   /**
    * @see junit.framework.TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();

      m_map.add("a", "a1");
      m_map.add("a", "a2");
      m_map.add("b", "b1");
   }

   public void testSize()
   {
      assertEquals(3, m_map.size());
   }

   public void testIterator()
   {
      int i = 0;

      for (Lookup.Iterator it = m_map.valueIterator(); it.hasNext(); i++)
      {
         it.next();

         Object value = it.getValue();
         Object key = it.getKey();

         assertSame(value, it.getValue());
         assertSame(key, it.getKey());

         if (value.equals("a1") || value.equals("a2"))
         {
            assertEquals("a", key);
         }
         else if (value.equals("b1"))
         {
            assertEquals("b", key);
         }
      }

      assertEquals(3, i);
   }

   public void testGet()
   {
      Collection list = m_map.get("a");

      assertNotNull(list);
      assertEquals(2, list.size());
      assertTrue(list.contains("a1"));
      assertTrue(list.contains("a2"));

      list = m_map.get("b");

      assertNotNull(list);
      assertEquals(1, list.size());
      assertTrue(list.contains("b1"));
   }
}
