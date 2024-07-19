// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import junit.framework.TestCase;

public class LinkedHashTabTest extends TestCase
{
   private LinkedHashTab m_htab;

   public LinkedHashTabTest(String arg0)
   {
      super(arg0);
   }

   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();
      
      m_htab = new LinkedHashTab(1);
      
      m_htab.put("Key A", "Value A");
      m_htab.put("Key B", "Value B");
      m_htab.put("Key C", "Value C");
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
      assertEquals(8, m_htab.m_table.length);
      assertEquals(8, m_htab.m_links.length);
      assertEquals("Value A", m_htab.get("Key A"));
      assertEquals("Value B", m_htab.get("Key B"));
      assertEquals("Value C", m_htab.get("Key C"));
      
      assertNull(m_htab.get("blah"));
   }

   public void testPut()
   {
      assertEquals("Value A", m_htab.put("Key A", "123"));
      assertEquals("123", m_htab.get("Key A"));
      assertEquals("Key A", m_htab.firstKey());
      assertEquals("Key C", m_htab.lastKey());
      assertEquals(3, m_htab.size());
      
      assertNull(m_htab.put("Key D", "Value D"));
      assertEquals("Value D", m_htab.get("Key D"));
      assertEquals("Key A", m_htab.firstKey());
      assertEquals("Key D", m_htab.lastKey());
      assertEquals(4, m_htab.size());
      assertEquals(16, m_htab.m_table.length);
      assertEquals(16, m_htab.m_links.length);
   }

   public void testPutFirst()
   {
      assertEquals("Value C", m_htab.putFirst("Key C", "C"));
      assertEquals("Value A", m_htab.get("Key A"));
      assertEquals("Key C", m_htab.firstKey());
      assertEquals("Key B", m_htab.lastKey());
      assertEquals(3, m_htab.size());
      assertEquals("{\"Key C\"=\"C\", \"Key A\"=\"Value A\", \"Key B\"=\"Value B\"}", m_htab.toString());

      assertEquals("C", m_htab.putFirst("Key C", "C"));
      assertEquals("{\"Key C\"=\"C\", \"Key A\"=\"Value A\", \"Key B\"=\"Value B\"}", m_htab.toString());

      assertNull(m_htab.putFirst("Key D", "D"));
      assertEquals("Key D", m_htab.firstKey());
      assertEquals("Key B", m_htab.lastKey());
      assertEquals(4, m_htab.size());
      assertEquals("{\"Key D\"=\"D\", \"Key C\"=\"C\", \"Key A\"=\"Value A\", \"Key B\"=\"Value B\"}", m_htab.toString());
   }
   
   public void testPutLast()
   {
      assertEquals("Value A", m_htab.putLast("Key A", "A"));
      assertEquals("Value C", m_htab.get("Key C"));
      assertEquals("Key B", m_htab.firstKey());
      assertEquals("Key A", m_htab.lastKey());
      assertEquals(3, m_htab.size());
      assertEquals("{\"Key B\"=\"Value B\", \"Key C\"=\"Value C\", \"Key A\"=\"A\"}", m_htab.toString());

      assertEquals("A", m_htab.putLast("Key A", "A"));
      assertEquals("{\"Key B\"=\"Value B\", \"Key C\"=\"Value C\", \"Key A\"=\"A\"}", m_htab.toString());

      assertNull(m_htab.putLast("Key D", "D"));
      assertEquals("Key B", m_htab.firstKey());
      assertEquals("Key D", m_htab.lastKey());
      assertEquals(4, m_htab.size());
      assertEquals("{\"Key B\"=\"Value B\", \"Key C\"=\"Value C\", \"Key A\"=\"A\", \"Key D\"=\"D\"}", m_htab.toString());
   }
   
   public void testFirstKey()
   {
      assertEquals("Key A", m_htab.firstKey());
   }
   
   public void testFirstValue()
   {
      assertEquals("Value A", m_htab.firstValue());
   }
   
   public void testLastKey()
   {
      assertEquals("Key C", m_htab.lastKey());
   }

   public void testLastValue()
   {
      assertEquals("Value C", m_htab.lastValue());
   }

   public void testRemove()
   {
      assertEquals("Value A", m_htab.remove("Key A"));
      assertEquals("Key B", m_htab.firstKey());
      assertEquals("Key C", m_htab.lastKey());
      assertEquals(2, m_htab.size());
      assertNull(m_htab.get("Key A"));
      assertEquals(8, m_htab.m_table.length);
      assertEquals(8, m_htab.m_links.length);
   }

   public void testRemoveFirst()
   {
      assertEquals("Value A", m_htab.removeFirst());
      assertNull(m_htab.get("Key A"));
      assertEquals(2, m_htab.size());
      assertEquals("{\"Key B\"=\"Value B\", \"Key C\"=\"Value C\"}", m_htab.toString());
      assertEquals("Value B", m_htab.removeFirst());
      assertNull(m_htab.get("Key B"));
      assertEquals(1, m_htab.size());
      assertEquals("{\"Key C\"=\"Value C\"}", m_htab.toString());
      assertEquals("Value C", m_htab.removeFirst());
      assertNull(m_htab.get("Key C"));
      assertEquals(0, m_htab.size());
      assertEquals("{}", m_htab.toString());
      assertNull(m_htab.removeFirst());
      assertEquals(8, m_htab.m_table.length);
      assertEquals(8, m_htab.m_links.length);
   }

   public void testRemoveLast()
   {
      assertEquals("Value C", m_htab.removeLast());
      assertNull(m_htab.get("Key C"));
      assertEquals(2, m_htab.size());
      assertEquals("{\"Key A\"=\"Value A\", \"Key B\"=\"Value B\"}", m_htab.toString());
      assertEquals("Value B", m_htab.removeLast());
      assertNull(m_htab.get("Key B"));
      assertEquals(1, m_htab.size());
      assertEquals("{\"Key A\"=\"Value A\"}", m_htab.toString());
      assertEquals("Value A", m_htab.removeLast());
      assertNull(m_htab.get("Key A"));
      assertEquals(0, m_htab.size());
      assertEquals("{}", m_htab.toString());
      assertNull(m_htab.removeLast());
      assertEquals(8, m_htab.m_table.length);
      assertEquals(8, m_htab.m_links.length);
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
      
      assertEquals("Key A", itr.next());
      assertEquals("Key A", itr.getKey());
      assertEquals("Value A", itr.getValue());
      assertEquals("Key B", itr.next());
      assertEquals("Key B", itr.getKey());
      assertEquals("Value B", itr.getValue());
      assertEquals("Key C", itr.next());
      assertEquals("Key C", itr.getKey());
      assertEquals("Value C", itr.getValue());

      assertFalse(itr.hasNext());
      
      itr = m_htab.iterator();
      
      assertEquals("Key A", itr.next());
      itr.setValue("A");
      assertEquals("Key B", itr.next());
      itr.remove();
      assertEquals("Key C", itr.next());

      assertFalse(itr.hasNext());
      assertEquals(2, m_htab.size());
      assertEquals("{\"Key A\"=\"A\", \"Key C\"=\"Value C\"}", m_htab.toString());
   }

   public void testReverseIterator()
   {
      Lookup.Iterator itr = m_htab.reverseIterator();

      assertEquals("Key C", itr.next());
      assertEquals("Key C", itr.getKey());
      assertEquals("Value C", itr.getValue());
      assertEquals("Key B", itr.next());
      assertEquals("Key B", itr.getKey());
      assertEquals("Value B", itr.getValue());
      assertEquals("Key A", itr.next());
      assertEquals("Key A", itr.getKey());
      assertEquals("Value A", itr.getValue());

      assertFalse(itr.hasNext());

      itr = m_htab.reverseIterator();

      assertEquals("Key C", itr.next());
      itr.setValue("C");
      assertEquals("Key B", itr.next());
      itr.remove();
      assertEquals("Key A", itr.next());

      assertFalse(itr.hasNext());
      assertEquals(2, m_htab.size());
      assertEquals("{\"Key A\"=\"Value A\", \"Key C\"=\"C\"}", m_htab.toString());
   }

   /*
    * Test for Object clone()
    */
   public void testClone()
   {
      LinkedHashTab htab = (LinkedHashTab)m_htab.clone();

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
      
      LinkedHashTab htab = (LinkedHashTab)istream.readObject();

      assertEquals(m_htab.size(), htab.size());

      for (Lookup.Iterator itr = m_htab.iterator(); itr.hasNext();)
      {
         itr.next();
         assertEquals(itr.getValue(), htab.get(itr.getKey()));
      }
   }
}
