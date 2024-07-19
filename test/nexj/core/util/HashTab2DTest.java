// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import junit.framework.TestCase;

public class HashTab2DTest extends TestCase
{
   protected HashTab2D m_htab;

   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();

      init();

      m_htab.put("Key A", "Key 1", "Value A");
      m_htab.put("Key B", "Key 2", "Value B");
      m_htab.put("Key C", "Key 3", "Value C");
   }

   protected void init()
   {
      m_htab = new HashTab2D(1);
   }

   public void testPut()
   {
      assertEquals("Value A", m_htab.put("Key A", "Key 1", "123"));
      assertEquals("123", m_htab.get("Key A", "Key 1"));
      assertEquals(3, m_htab.size());
      
      assertNull(m_htab.put("Key D", "Key 4", "Value D"));
      assertEquals("Value D", m_htab.get("Key D", "Key 4"));
      assertEquals(4, m_htab.size());
   }

   public void testGet()
   {
      assertEquals("Value A", m_htab.get("Key A", "Key 1"));
      assertEquals("Value B", m_htab.get("Key B", "Key 2"));
      assertEquals("Value C", m_htab.get("Key C", "Key 3"));
      
      assertNull(m_htab.get("blah", "blah"));
   }

   public void testContains()
   {
      assertTrue(m_htab.contains("Key A", "Key 1"));
      assertFalse(m_htab.contains("Key b", "Key 1"));
   }

   public void testRemove()
   {
      assertEquals("Value A", m_htab.remove("Key A", "Key 1"));
      assertEquals(2, m_htab.size());
      assertNull(m_htab.get("Key A", "Key 1"));
   }

   public void testClear()
   {
      m_htab.clear();
      
      assertEquals(0, m_htab.size());
      
      assertNull(m_htab.get("Key A", "Key 1"));
      assertNull(m_htab.get("Key B", "Key 2"));
      assertNull(m_htab.get("Key C", "Key 3"));
   }

   public void testSize()
   {
      assertEquals(3, m_htab.size());
   }

   public void testValueIterator()
   {
      Lookup2D.Iterator itr = m_htab.valueIterator();
      
      itr.next();
      itr.next();
      itr.next();
      
      assertFalse(itr.hasNext());
   }

   /*
    * Class under test for Object clone()
    */
   public void testClone()
   {
      HashTab2D htab = (HashTab2D)m_htab.clone();

      assertEquals(htab.size(), m_htab.size());
      assertEquals(htab.get("Key A", "Key 1"), m_htab.get("Key A", "Key 1"));
      htab.put("Key B", "Key 2", "B");
      assertFalse(htab.get("Key B", "Key 2").equals(m_htab.get("Key B", "Key 2")));
   }

   /*
    * Class under test for String toString()
    */
   public void testToString()
   {
      assertEquals("{(\"Key A\",\"Key 1\")=\"Value A\", (\"Key B\",\"Key 2\")=\"Value B\", (\"Key C\",\"Key 3\")=\"Value C\"}", m_htab.toString());
   }

   public void testSerialize() throws IOException, ClassNotFoundException
   {
      ByteArrayOutputStream bostream = new ByteArrayOutputStream();
      ObjectOutputStream ostream = new ObjectOutputStream(bostream);

      ostream.writeObject(m_htab);

      ObjectInputStream istream = new ObjectInputStream(new ByteArrayInputStream(bostream.toByteArray()));

      HashTab2D htab = (HashTab2D)istream.readObject();

      assertEquals(m_htab.size(), htab.size());
      
      for (Lookup2D.Iterator itr = m_htab.valueIterator(); itr.hasNext();)
      {
         itr.next();
         assertEquals(itr.getValue(), htab.get(itr.getKey1(), itr.getKey2()));
      }
   }
}
