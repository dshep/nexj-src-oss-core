// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.Iterator;

import junit.framework.TestCase;

public class HashDequeTest extends TestCase
{
   private HashDeque m_hd;

   public HashDequeTest(String name)
   {
      super(name);
   }

   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();
      
      m_hd = new HashDeque(1);
      
      m_hd.add("Value A");
      m_hd.add("Value B");
      m_hd.add("Value C");
   }

   /*
    * Test method for 'nexj.core.util.HashDeque.add(Object)'
    */
   public void testAdd()
   {
      assertFalse(m_hd.add("Value A"));
      assertEquals(3, m_hd.size());
      assertTrue(m_hd.add("Value D"));
      assertTrue(m_hd.contains("Value D"));
      assertEquals(4, m_hd.size());
      assertEquals("Value D", m_hd.last());
   }

   public void testAddFirst()
   {
      assertFalse(m_hd.addFirst("Value A"));
      assertTrue(m_hd.addFirst("Value D"));
      assertEquals("Value D", m_hd.first());
      assertFalse(m_hd.addFirst("Value A"));
      assertEquals("Value A", m_hd.first());
   }

   public void testAddLast()
   {
      assertFalse(m_hd.addLast("Value A"));
      assertTrue(m_hd.addLast("Value D"));
      assertEquals("Value D", m_hd.last());
      assertFalse(m_hd.addLast("Value A"));
      assertEquals("Value A", m_hd.last());
   }
   
   /*
    * Test method for 'nexj.core.util.HashDeque.contains(Object)'
    */
   public void testContains()
   {
      assertTrue(m_hd.contains("Value A"));
      assertFalse(m_hd.contains("Value b"));
      assertFalse(m_hd.contains(null));
   }

   /*
    * Test method for 'nexj.core.util.HashDeque.remove(Object)'
    */
   public void testRemove()
   {
      assertTrue(m_hd.remove("Value A"));
      assertEquals(2, m_hd.size());
      assertFalse(m_hd.contains("Value A"));
      assertEquals("Value B", m_hd.first());
      assertEquals("Value C", m_hd.last());
   }

   public void testRemoveFirst()
   {
      assertEquals("Value A", m_hd.removeFirst());
      assertEquals("Value B", m_hd.removeFirst());
      assertEquals("Value C", m_hd.removeFirst());
      assertNull(m_hd.removeFirst());
      assertEquals(0, m_hd.size());
   }
   
   public void testRemoveLast()
   {
      assertEquals("Value C", m_hd.removeLast());
      assertEquals("Value B", m_hd.removeLast());
      assertEquals("Value A", m_hd.removeLast());
      assertNull(m_hd.removeLast());
      assertEquals(0, m_hd.size());
   }

   /*
    * Test method for 'nexj.core.util.GenericHashDeque.clear()'
    */
   public void testClear()
   {
      m_hd.clear();
      
      assertEquals(0, m_hd.size());
      assertFalse(m_hd.contains("Value A"));
      assertFalse(m_hd.contains("Value B"));
      assertFalse(m_hd.contains("Value C"));
   }

   /*
    * Test method for 'nexj.core.util.GenericHashDeque.size()'
    */
   public void testSize()
   {
      assertEquals(3, m_hd.size());
   }

   /*
    * Test method for 'nexj.core.util.GenericHashDeque.iterator()'
    */
   public void testIterator()
   {
      Iterator itr = m_hd.iterator();
      
      assertTrue(itr.hasNext());
      assertEquals("Value A", itr.next());
      assertTrue(itr.hasNext());
      assertEquals("Value B", itr.next());
      assertTrue(itr.hasNext());
      assertEquals("Value C", itr.next());
      assertFalse(itr.hasNext());
   }

   /*
    * Test method for 'nexj.core.util.GenericHashDeque.clone()'
    */
   public void testClone()
   {
      HashDeque hd = (HashDeque)m_hd.clone();

      assertEquals(hd.size(), m_hd.size());
      assertEquals(hd.contains("Value A"), m_hd.contains("Value A"));
   }

   /*
    * Test method for 'nexj.core.util.GenericHashDeque.toString()'
    */
   public void testToString()
   {
      assertEquals("[\"Value A\", \"Value B\", \"Value C\"]", m_hd.toString());
   }

   public void testSerialize() throws IOException, ClassNotFoundException
   {
      ByteArrayOutputStream bostream = new ByteArrayOutputStream();
      ObjectOutputStream ostream = new ObjectOutputStream(bostream);

      ostream.writeObject(m_hd);

      ObjectInputStream istream = new ObjectInputStream(new ByteArrayInputStream(bostream.toByteArray()));
      HashDeque hd = (HashDeque)istream.readObject();

      assertEquals(m_hd.size(), hd.size());

      for (Iterator itr = m_hd.iterator(); itr.hasNext();)
      {
         assertTrue(hd.contains(itr.next()));
      }
   }
}
