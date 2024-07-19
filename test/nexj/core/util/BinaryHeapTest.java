// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.Iterator;

import junit.framework.TestCase;

public class BinaryHeapTest extends TestCase
{
   protected BinaryHeap m_heap; 

   /**
    * @see junit.framework.TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();
      
      m_heap = new BinaryHeap(2, ComparableComparator.INSTANCE);
      
      m_heap.add(new Integer(4));
      m_heap.add(new Integer(3));
      m_heap.add(new Integer(5));
      m_heap.add(new Integer(1));
      m_heap.add(new Integer(2));
      m_heap.add(new Integer(3));
   }

   public void testAdd()
   {
      m_heap.add(new Integer(0));
      
      assertEquals(7, m_heap.size());
      assertEquals(new Integer(0), m_heap.first());
      
      m_heap.add(new Integer(7));
      assertEquals(8, m_heap.size());
      assertEquals(new Integer(0), m_heap.first());
   }

   public void testClear()
   {
      m_heap.clear();
      assertEquals(0, m_heap.size());
      assertNull(m_heap.first());
   }

   public void testContains()
   {
      assertTrue(m_heap.contains(new Integer(3)));
      assertFalse(m_heap.contains(new Integer(7)));
   }

   public void testFirst()
   {
      assertEquals(new Integer(1), m_heap.first());
   }

   public void testRemove()
   {
      assertTrue(m_heap.remove(new Integer(3)));
      assertEquals(5, m_heap.size());
      assertEquals("[1, 2, 5, 4, 3]", m_heap.toString());
      assertFalse(m_heap.remove(new Integer(0)));
      assertEquals(5, m_heap.size());
      assertEquals("[1, 2, 5, 4, 3]", m_heap.toString());
   }

   public void testRemoveFirst()
   {
      assertEquals(new Integer(1), m_heap.removeFirst());
      assertEquals(5, m_heap.size());
      assertEquals("[2, 3, 3, 4, 5]", m_heap.toString());
      assertEquals(new Integer(2), m_heap.removeFirst());
      assertEquals(new Integer(3), m_heap.removeFirst());
      assertEquals(new Integer(3), m_heap.removeFirst());
      assertEquals(new Integer(4), m_heap.removeFirst());
      assertEquals(new Integer(5), m_heap.removeFirst());
      assertNull(m_heap.removeFirst());
   }

   public void testSize()
   {
      assertEquals(6, m_heap.size());
   }

   public void testIterator()
   {
      Iterator itr = m_heap.iterator();
      
      assertTrue(itr.hasNext());
      assertEquals(new Integer(1), itr.next());
      assertTrue(itr.hasNext());
      assertEquals(new Integer(2), itr.next());
      itr.remove();
      assertEquals(5, m_heap.size());
      assertEquals("[1, 3, 3, 4, 5]", m_heap.toString());
      assertTrue(itr.hasNext());
      assertEquals(new Integer(3), itr.next());
      assertTrue(itr.hasNext());
      assertEquals(new Integer(3), itr.next());
      assertTrue(itr.hasNext());
      assertEquals(new Integer(4), itr.next());
      assertTrue(itr.hasNext());
      assertEquals(new Integer(5), itr.next());
      itr.remove();
      assertFalse(itr.hasNext());
      assertEquals(4, m_heap.size());
      assertEquals("[1, 3, 3, 4]", m_heap.toString());
   }

   public void testClone()
   {
      BinaryHeap heap = (BinaryHeap)m_heap.clone();
      
      assertEquals(6, heap.size());
      assertEquals("[1, 2, 3, 4, 3, 5]", heap.toString());
      
      assertEquals(new Integer(1), heap.removeFirst());
      assertEquals(5, heap.size());
      assertEquals(6, m_heap.size());
      assertEquals("[1, 2, 3, 4, 3, 5]", m_heap.toString());
   }

   public void testToString()
   {
      assertEquals("[1, 2, 3, 4, 3, 5]", m_heap.toString());
   }
   
   public void testSerialize() throws IOException, ClassNotFoundException
   {
      ByteArrayOutputStream bostream = new ByteArrayOutputStream();
      ObjectOutputStream ostream = new ObjectOutputStream(bostream);

      ostream.writeObject(m_heap);
      
      ObjectInputStream istream = new ObjectInputStream(new ByteArrayInputStream(bostream.toByteArray()));
      
      BinaryHeap heap = (BinaryHeap)istream.readObject();

      assertEquals(m_heap.size(), heap.size());
      assertEquals("[1, 2, 3, 4, 3, 5]", heap.toString());
   }
   
   public void testHeap()
   {
      BinaryHeap heap = new BinaryHeap(ComparableComparator.INSTANCE);
      
      for (int nCount = 0; nCount < 100; ++nCount)
      {
         for (int i = 0, n = RandUtil.getSecureRandom().nextInt(1000) + 10; i < n; ++i)
         {
            heap.add(new Integer(RandUtil.getSecureRandom().nextInt()));
         }
         
         for (Iterator itr = heap.iterator(); itr.hasNext();)
         {
            itr.next();
            
            if (RandUtil.getSecureRandom().nextBoolean())
            {
               itr.remove();
            }
         }
         
         for (int i = 0, n = heap.size() >> 1; i < n; ++i)
         {
            heap.add(new Integer(RandUtil.getSecureRandom().nextInt()));
         }

         Integer old = (Integer)heap.removeFirst();

         while (heap.size() != 0)
         {
            Integer value = (Integer)heap.removeFirst();

            assertTrue(old.compareTo(value) <= 0);
            old = value;
         }
      }
   }
}
