// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.Iterator;

import junit.framework.TestCase;

public class HashHolderTest extends TestCase
{
   protected HashHolder m_hh;

   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();
      
      init();
      
      m_hh.add("Value A");
      m_hh.add("Value B");
      m_hh.add("Value C");
   }
   
   protected void init()
   {
      m_hh = new HashHolder(1);
   }

   /*
    * Test method for 'nexj.core.util.HashHolder.add(Object)'
    */
   public void testAdd()
   {
      assertFalse(m_hh.add("Value A"));
      assertEquals(3, m_hh.size());
      assertTrue(m_hh.add("Value D"));
      assertTrue(m_hh.contains("Value D"));
      assertEquals(4, m_hh.size());
   }

   /*
    * Test method for 'nexj.core.util.HashHolder.contains(Object)'
    */
   public void testContains()
   {
      assertTrue(m_hh.contains("Value A"));
      assertFalse(m_hh.contains("Value b"));
      assertFalse(m_hh.contains(null));
   }

   /*
    * Test method for 'nexj.core.util.HashHolder.remove(Object)'
    */
   public void testRemove()
   {
      assertTrue(m_hh.remove("Value A"));
      assertEquals(2, m_hh.size());
      assertFalse(m_hh.contains("Value A"));
   }

   /*
    * Test method for 'nexj.core.util.GenericHashHolder.clear()'
    */
   public void testClear()
   {
      m_hh.clear();
      
      assertEquals(0, m_hh.size());
      assertFalse(m_hh.contains("Value A"));
      assertFalse(m_hh.contains("Value B"));
      assertFalse(m_hh.contains("Value C"));
   }

   /*
    * Test method for 'nexj.core.util.GenericHashHolder.size()'
    */
   public void testSize()
   {
      assertEquals(3, m_hh.size());
   }

   /*
    * Test method for 'nexj.core.util.GenericHashHolder.iterator()'
    */
   public void testIterator()
   {
      Iterator itr = m_hh.iterator();
      
      assertTrue(itr.hasNext());
      assertEquals("Value C", itr.next());
      assertTrue(itr.hasNext());
      assertEquals("Value A", itr.next());
      assertTrue(itr.hasNext());
      assertEquals("Value B", itr.next());
      assertFalse(itr.hasNext());
   }

   /*
    * Test method for 'nexj.core.util.GenericHashHolder.clone()'
    */
   public void testClone()
   {
      HashHolder hh = (HashHolder)m_hh.clone();

      assertEquals(hh.size(), m_hh.size());
      assertEquals(hh.contains("Value A"), m_hh.contains("Value A"));
   }

   /*
    * Test method for 'nexj.core.util.GenericHashHolder.toString()'
    */
   public void testToString()
   {
      assertEquals("[\"Value C\", \"Value A\", \"Value B\"]", m_hh.toString());
   }

   public void testSerialize() throws IOException, ClassNotFoundException
   {
      ByteArrayOutputStream bostream = new ByteArrayOutputStream();
      ObjectOutputStream ostream = new ObjectOutputStream(bostream);

      ostream.writeObject(m_hh);

      ObjectInputStream istream = new ObjectInputStream(new ByteArrayInputStream(bostream.toByteArray()));
      HashHolder hh = (HashHolder)istream.readObject();

      assertEquals(m_hh.size(), hh.size());

      for (Iterator itr = m_hh.iterator(); itr.hasNext();)
      {
         assertTrue(hh.contains(itr.next()));
      }
   }
}
