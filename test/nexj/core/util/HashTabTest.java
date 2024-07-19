// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import junit.framework.TestCase;

public class HashTabTest extends TestCase
{
   protected Lookup m_htab;

   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();

      init();
      
      m_htab.put("Key A", "Value A");
      m_htab.put("Key B", "Value B");
      m_htab.put("Key C", "Value C");
   }

   protected void init()
   {
      m_htab = new HashTab(1);
   }
   
   public void testClear()
   {
      m_htab.clear();
      
      assertEquals(0, m_htab.size());
      
      assertNull(m_htab.get("Key A"));
      assertNull(m_htab.get("Key B"));
      assertNull(m_htab.get("Key C"));
   }

   public void testGet()
   {
      assertEquals("Value A", m_htab.get("Key A"));
      assertEquals("Value B", m_htab.get("Key B"));
      assertEquals("Value C", m_htab.get("Key C"));
      
      assertNull(m_htab.get("blah"));
   }

   public void testPut()
   {
      assertEquals("Value A", m_htab.put("Key A", "123"));
      assertEquals("123", m_htab.get("Key A"));
      assertEquals(3, m_htab.size());
      
      assertNull(m_htab.put("Key D", "Value D"));
      assertEquals("Value D", m_htab.get("Key D"));
      assertEquals(4, m_htab.size());
   }

   public void testRemove()
   {
      assertEquals("Value A", m_htab.remove("Key A"));
      assertEquals(2, m_htab.size());
      assertNull(m_htab.get("Key A"));
   }

   public void testSize()
   {
      assertEquals(3, m_htab.size());
   }

   /*
    * Test for String toString()
    */
   public void testToString()
   {
      assertEquals("{\"Key A\"=\"Value A\", \"Key B\"=\"Value B\", \"Key C\"=\"Value C\"}", m_htab.toString());
   }
   
   public void testContains()
   {
      assertTrue(m_htab.contains("Key A"));
      assertFalse(m_htab.contains("Key b"));
   }

   public void testIterator()
   {
      Lookup.Iterator itr = m_htab.iterator();
      
      itr.next();
      itr.next();
      itr.next();
      
      assertFalse(itr.hasNext());
   }

   /*
    * Test for Object clone()
    */
   public void testClone()
   {
      Lookup htab = (Lookup)m_htab.clone();

      assertEquals(htab.size(), m_htab.size());
      assertEquals(htab.get("Key A"), m_htab.get("Key A"));
      htab.put("Key B", "B");
      assertFalse(htab.get("Key B").equals(m_htab.get("Key B")));
   }
   
   public void testSerialize() throws IOException, ClassNotFoundException
   {
      ByteArrayOutputStream bostream = new ByteArrayOutputStream();
      ObjectOutputStream ostream = new ObjectOutputStream(bostream);

      ostream.writeObject(m_htab);
      
      ObjectInputStream istream = new ObjectInputStream(new ByteArrayInputStream(bostream.toByteArray()));
      
      Lookup htab = (Lookup)istream.readObject();

      assertEquals(m_htab.size(), htab.size());
      
      for (Lookup.Iterator itr = m_htab.iterator(); itr.hasNext();)
      {
         itr.next();
         assertEquals(itr.getValue(), htab.get(itr.getKey()));
      }
   }
}
