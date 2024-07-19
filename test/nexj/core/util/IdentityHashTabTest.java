// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import junit.framework.TestCase;

public class IdentityHashTabTest extends TestCase
{
   private IdentityHashTab m_htab = null;
   private String m_keyA = new String("A"); // Different identitites
   private String m_keyB = new String("A"); // Different identitites
   private String m_keyC = new String("A"); // Different identitites

   public IdentityHashTabTest(String arg0)
   {
      super(arg0);
   }

   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();
      
      m_htab = new IdentityHashTab(1);
      
      m_htab.put(m_keyA, "Value A");
      m_htab.put(m_keyB, "Value B");
      m_htab.put(m_keyC, "Value C");
   }

   public void testClear()
   {
      m_htab.clear();
      
      assertEquals(0, m_htab.size());
      
      assertNull(m_htab.get(m_keyA));
      assertNull(m_htab.get(m_keyB));
      assertNull(m_htab.get(m_keyC));
   }

   public void testGet()
   {
      assertEquals("Value A", m_htab.get(m_keyA));
      assertEquals("Value B", m_htab.get(m_keyB));
      assertEquals("Value C", m_htab.get(m_keyC));
      
      assertNull(m_htab.get("blah"));
   }

   public void testPut()
   {
      assertEquals("Value A", m_htab.put(m_keyA, "123"));
      assertEquals("123", m_htab.get(m_keyA));
      assertEquals(3, m_htab.size());
      
      assertNull(m_htab.put("Key D", "Value D"));
      assertEquals("Value D", m_htab.get("Key D"));
      assertEquals(4, m_htab.size());
   }

   public void testRemove()
   {
      assertEquals("Value A", m_htab.remove(m_keyA));
      assertEquals(2, m_htab.size());
      assertNull(m_htab.get(m_keyA));
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
      assertTrue(m_htab.toString().length() > 0);
   }

   public void testContains()
   {
      assertTrue(m_htab.contains(m_keyA));
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
      IdentityHashTab htab = (IdentityHashTab)m_htab.clone();

      assertEquals(htab.size(), m_htab.size());
      assertEquals(htab.get(m_keyA), m_htab.get(m_keyA));
      htab.put(m_keyB, "B");
      assertFalse(htab.get(m_keyB).equals(m_htab.get(m_keyB)));
   }
   
   public void testSerialize() throws IOException, ClassNotFoundException
   {
      ByteArrayOutputStream bostream = new ByteArrayOutputStream();
      ObjectOutputStream ostream = new ObjectOutputStream(bostream);

      ostream.writeObject(m_htab);
      
      ObjectInputStream istream = new ObjectInputStream(new ByteArrayInputStream(bostream.toByteArray()));
      
      IdentityHashTab htab = (IdentityHashTab)istream.readObject();

      assertEquals(m_htab.size(), htab.size());
      
      for (Lookup.Iterator itr = m_htab.iterator(); itr.hasNext();)
      {
         assertNull(htab.get(itr.next()));
      }
   }
}
