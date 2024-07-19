// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.ConcurrentModificationException;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

import junit.framework.TestCase;

import nexj.core.meta.Metaclass;
import nexj.core.meta.Metadata;
import nexj.core.meta.PrimitivePrivilege;
import nexj.core.meta.Privilege;
import nexj.core.meta.Repository;
import nexj.core.persistence.OID;
import nexj.core.scripting.Pair;
import nexj.core.util.Binary;
import nexj.core.util.DuplicateItemException;

public class InstanceArrayListTest extends TestCase
{
   protected Metadata m_metadata;
   protected InvocationContext m_context;
   protected InstanceArrayList m_list;
   protected Instance m_contact;
   protected Instance[] m_instanceArray;
   protected int m_nFlags = InstanceList.DEFAULT;

   protected void setUp() throws Exception
   {
      m_metadata = Repository.getMetadata();
      m_context = new InvocationContext(m_metadata);

      for (Iterator itr = m_metadata.getPrivilegeIterator(); itr.hasNext();)
      {
         Privilege p = (Privilege)itr.next();

         if (p instanceof PrimitivePrivilege)
         {
            m_context.getPrivilegeSet().add((PrimitivePrivilege)p);
         }
      }

      ThreadContextHolder.setContext(m_context);
      m_list = new InstanceArrayList(2);

      Metaclass contact = m_metadata.getMetaclass("Contact");

      m_contact = new Instance(contact, m_context);

      m_contact.setNew();
      m_contact.setValue("firstName", "Plato");
      m_list.setAssociation(m_contact, contact.getAttribute("addresses"), false);
      m_instanceArray = new Instance[2];

      Instance instance = new Instance(m_metadata.getMetaclass("Address"), m_context);

      instance.setNew();
      instance.setValue("city", "a");
      m_instanceArray[m_list.getCount()] = instance;
      m_list.add(instance, m_nFlags);

      instance = new Instance(m_metadata.getMetaclass("Address"), m_context);

      instance.setNew();
      instance.setValue("city", "b");
      m_instanceArray[m_list.getCount()] = instance;
      m_list.add(instance, m_nFlags);
   }

   protected void tearDown() throws Exception
   {
      m_context.complete(false);
      ThreadContextHolder.setContext(null);
      m_context = null;
      m_metadata = null;
      m_contact = null;
      m_instanceArray = null;
      m_list = null;
   }

   protected void assertEquals()
   {
      assertEquals(m_instanceArray.length, m_list.size());

      for (int i = 0; i < m_instanceArray.length; ++i)
      {
         Instance instance = m_instanceArray[i];

         assertTrue(m_list.contains(instance));
         assertEquals(i, m_list.indexOf(instance));
         assertSame(instance, m_list.get(i));
      }
   }
   
   public void testGetContainer()
   {
      assertEquals("Plato", m_list.getContainer().getValue("firstName"));
   }

   public void testGetAttribute()
   {
      assertEquals("addresses", m_list.getAttribute().getName());
   }

   public void testSetWeak()
   {
      m_list.setWeak(true);

      assertTrue(m_list.isWeak());
      assertEquals();

      m_list.setWeak(false);

      assertFalse(m_list.isWeak());
      assertEquals();
   }

   public void testIsWeak()
   {
      assertEquals((m_nFlags & InstanceList.WEAK) != 0, m_list.isWeak());
   }

   public void testSetLazy()
   {
      m_list.setLazy(false);
      assertFalse(m_list.isLazy());
   }

   public void testIsLazy()
   {
      assertFalse(m_list.isLazy());
   }

   public void testLoad()
   {
      m_list.load();
   }

   public void testAddIntInstanceInt()
   {
      Instance instance = new Instance(m_metadata.getMetaclass("Address"), m_context);
      
      instance.cache(new OID(new Object[]{new Binary(new byte[]{1})}));
      instance.setValue("city", "c");
      m_list.add(1, instance, m_nFlags);

      assertEquals(3, m_list.size());
      assertEquals("c", m_list.getInstance(1).getValue("city"));

      instance = new Instance(m_metadata.getMetaclass("Address"), m_context);
      
      instance.setNew();
      instance.setOID(new OID(new Object[]{new Binary(new byte[]{2})}));
      instance.setValue("city", "d");
      m_list.add(2, instance, m_nFlags);

      try
      {
         m_list.add(2, instance, m_nFlags);
         fail("Expected DuplicateItemException");
      }
      catch (DuplicateItemException e)
      {
      }

      m_list.add(3, instance, m_nFlags | InstanceList.REPLACE);

      assertEquals(4, m_list.size());
      assertEquals("c", m_list.getInstance(1).getValue("city"));
      assertEquals("d", m_list.getInstance(2).getValue("city"));
      assertEquals("b", m_list.getInstance(3).getValue("city"));
   }

   public void testAddInstanceInt()
   {
      Instance instance = new Instance(m_metadata.getMetaclass("Address"), m_context);
      
      instance.setNew();
      instance.setValue("city", "c");
      assertTrue(m_list.add(instance, m_nFlags));
      assertEquals(3, m_list.size());
      assertEquals("b", m_list.getInstance(1).getValue("city"));
      assertEquals("c", m_list.getInstance(2).getValue("city"));
      
      try
      {
         m_list.add(instance, m_nFlags);
         fail("Expected DuplicateItemException");
      }
      catch (DuplicateItemException e)
      {
      }

      m_list.add(instance, m_nFlags | InstanceList.REPLACE);
      assertEquals(3, m_list.size());
      assertEquals("b", m_list.getInstance(1).getValue("city"));
      assertEquals("c", m_list.getInstance(2).getValue("city"));
   }

   public void testGetInstance()
   {
      assertEquals("a", m_list.getInstance(0).getValue("city"));
      assertEquals("b", m_list.getInstance(1).getValue("city"));
   }

   public void testRemoveIntInt()
   {
      assertEquals("a", ((Instance)m_list.remove(0, InstanceList.DEFAULT)).getValue("city"));
      assertEquals(1, m_list.size());
      assertEquals("b", m_list.getInstance(0).getValue("city"));
   }

   public void testRemoveInstanceInt()
   {
      assertTrue(m_list.remove(m_list.getInstance(0), InstanceList.DEFAULT));
      assertEquals(1, m_list.size());
      assertEquals("b", m_list.getInstance(0).getValue("city"));
   }

   public void testGet()
   {
      assertEquals("b", ((Instance)m_list.get(1)).getValue("city"));
      
      try
      {
         m_list.get(-1);
         fail("Expected IndexOutOfBoundsException");
      }
      catch (IndexOutOfBoundsException e)
      {
      }
      
      try
      {
         m_list.get(2);
         fail("Expected IndexOutOfBoundsException");
      }
      catch (IndexOutOfBoundsException e)
      {
      }
   }

   public void testSize()
   {
      assertEquals(2, m_list.size());
   }
   
   public void testGetCount()
   {
      assertEquals(2, m_list.getCount());
   }

   public void testAddIntObject()
   {
      Instance instance = new Instance(m_metadata.getMetaclass("Address"), m_context);
      
      instance.setNew();
      instance.setValue("city", "c");
      m_list.add(0, instance);
      assertEquals(3, m_list.size());
      assertEquals("c", m_list.getInstance(0).getValue("city"));
      assertEquals("a", m_list.getInstance(1).getValue("city"));
      
      try
      {
         m_list.add(-1, instance);
         fail("Expected IndexOutOfBoundsException");
      }
      catch (IndexOutOfBoundsException e)
      {
      }

      try
      {
         m_list.add(4, instance);
         fail("Expected IndexOutOfBoundsException");
      }
      catch (IndexOutOfBoundsException e)
      {
      }
   }

   public void testAddObject()
   {
      Instance instance = new Instance(m_metadata.getMetaclass("Address"), m_context);
      
      instance.setNew();
      instance.setValue("city", "c");
      assertTrue(m_list.add(instance));
      assertEquals(3, m_list.size());
      assertEquals("b", m_list.getInstance(1).getValue("city"));
      assertEquals("c", m_list.getInstance(2).getValue("city"));
   }

   public void testRemoveInt()
   {
      assertEquals("b", ((Instance)m_list.remove(1, InstanceList.DEFAULT)).getValue("city"));
      assertEquals(1, m_list.size());
      assertEquals("a", m_list.getInstance(0).getValue("city"));
      
      try
      {
         m_list.remove(-1);
         fail("Expected IndexOutOfBoundsException");
      }
      catch (IndexOutOfBoundsException e)
      {
      }

      try
      {
         m_list.remove(1);
         fail("Expected IndexOutOfBoundsException");
      }
      catch (IndexOutOfBoundsException e)
      {
      }
   }

   public void testRemoveObject()
   {
      assertTrue(m_list.remove(m_list.getInstance(1)));
      assertEquals(1, m_list.size());
      assertEquals("a", m_list.getInstance(0).getValue("city"));
   }

   public void testContains()
   {
      assertTrue(m_list.contains(m_list.getInstance(1)));

      Instance instance = new Instance(m_metadata.getMetaclass("Address"), m_context);
      
      instance.setNew();
      instance.setValue("city", "c");

      assertFalse(m_list.contains(instance));
      assertFalse(m_list.contains(new Object()));
   }

   public void testIndexOf()
   {
      assertEquals(1, m_list.indexOf(m_list.get(1)));

      Instance instance = new Instance(m_metadata.getMetaclass("Address"), m_context);

      instance.setNew();
      instance.setValue("city", "c");

      assertEquals(-1, m_list.indexOf(instance));
   }

   public void testLastIndexOf()
   {
      assertEquals(1, m_list.lastIndexOf(m_list.get(1)));

      Instance instance = new Instance(m_metadata.getMetaclass("Address"), m_context);

      instance.setNew();
      instance.setValue("city", "c");

      assertEquals(-1, m_list.lastIndexOf(instance));

   }

   public void testClear()
   {
      m_list.clear();
      assertEquals(0, m_list.size());
      assertTrue(m_list.isEmpty());
      
      try
      {
         m_list.get(0);
         fail("Expected IndexOutOfBoundsException");
      }
      catch (IndexOutOfBoundsException e)
      {
      }
   }

   public void testIsEmpty()
   {
      assertFalse(m_list.isEmpty());
      m_list.remove(0);
      m_list.remove(0);
      assertTrue(m_list.isEmpty());
   }

   public void testToArray()
   {
      Instance[] array = (Instance[])m_list.toArray();

      assertEquals(2, array.length);
      assertEquals("a", array[0].getValue("city"));
      assertEquals("b", array[1].getValue("city"));
   }

   public void testToArrayObjectArray()
   {
      Instance[] array = new Instance[1];

      array = (Instance[])m_list.toArray(array);

      assertEquals(2, array.length);
      assertEquals("a", array[0].getValue("city"));
      assertEquals("b", array[1].getValue("city"));

      array = new Instance[3];
      array = (Instance[])m_list.toArray(array);
      assertEquals(3, array.length);
      assertEquals("a", array[0].getValue("city"));
      assertEquals("b", array[1].getValue("city"));
      assertNull(array[2]);
   }

   public void testAddAllIntCollection()
   {
      List list = new ArrayList();
      Instance instance = new Instance(m_metadata.getMetaclass("Address"), m_context);

      instance.setNew();
      instance.setValue("city", "c");
      list.add(instance);

      instance = new Instance(m_metadata.getMetaclass("Address"), m_context);
      instance.setNew();
      instance.setValue("city", "d");
      list.add(instance);
      
      assertTrue(m_list.addAll(1, list));
      assertEquals(4, m_list.size());
      assertEquals("a", m_list.getInstance(0).getValue("city"));
      assertEquals("c", m_list.getInstance(1).getValue("city"));
      assertEquals("d", m_list.getInstance(2).getValue("city"));
      assertEquals("b", m_list.getInstance(3).getValue("city"));
   }

   public void testAddAllCollection()
   {
      List list = new ArrayList();
      Instance instance = new Instance(m_metadata.getMetaclass("Address"), m_context);

      instance.setNew();
      instance.setValue("city", "c");
      list.add(instance);

      instance = new Instance(m_metadata.getMetaclass("Address"), m_context);
      instance.setNew();
      instance.setValue("city", "d");
      list.add(instance);
      
      assertTrue(m_list.addAll(list));
      assertEquals(4, m_list.size());
      assertEquals("a", m_list.getInstance(0).getValue("city"));
      assertEquals("b", m_list.getInstance(1).getValue("city"));
      assertEquals("c", m_list.getInstance(2).getValue("city"));
      assertEquals("d", m_list.getInstance(3).getValue("city"));
   }

   public void testContainsAll()
   {
      List list = new ArrayList();
      Instance instance = new Instance(m_metadata.getMetaclass("Address"), m_context);

      instance.setNew();
      instance.setValue("city", "c");
      list.add(instance);
      list.add(m_list.get(0));
      
      assertFalse(m_list.containsAll(list));
      list.remove(0);
      assertTrue(m_list.containsAll(list));
   }

   public void testRemoveAll()
   {
      List list = new ArrayList();

      list.add(m_list.get(0));
      assertTrue(m_list.removeAll(list));
      assertEquals(1, m_list.size());
      assertEquals("b", m_list.getInstance(0).getValue("city"));
   }

   public void testRetainAll()
   {
      List list = new ArrayList();

      list.add(m_list.get(0));
      assertTrue(m_list.retainAll(list));
      assertEquals(1, m_list.size());
      assertEquals("a", m_list.getInstance(0).getValue("city"));
   }

   public void testIterator()
   {
      Iterator itr = m_list.iterator();
      
      assertTrue(itr.hasNext());
      assertEquals("a", ((Instance)itr.next()).getValue("city"));
      itr.remove();
      assertTrue(itr.hasNext());
      assertEquals(1, m_list.size());
      assertEquals("b", m_list.getInstance(0).getValue("city"));
      
      Instance instance = new Instance(m_metadata.getMetaclass("Address"), m_context);

      instance.setNew();
      instance.setValue("city", "c");
      m_list.add(instance);
      
      assertTrue(itr.hasNext());
      assertEquals("b", ((Instance)itr.next()).getValue("city"));
      assertFalse(itr.hasNext());
      itr.remove();
      assertEquals(1, m_list.size());
      assertEquals("c", m_list.getInstance(0).getValue("city"));
   }

   public void testListIterator()
   {
      ListIterator itr = m_list.listIterator();
      
      assertEquals(-1, itr.previousIndex());
      assertEquals(0, itr.nextIndex());
      assertTrue(itr.hasNext());
      assertEquals("a", ((Instance)itr.next()).getValue("city"));
      assertEquals(0, itr.previousIndex());
      assertEquals(1, itr.nextIndex());
      assertTrue(itr.hasPrevious());
      itr.remove();
      assertEquals(-1, itr.previousIndex());
      assertEquals(0, itr.nextIndex());
      assertTrue(itr.hasNext());
      assertEquals(1, m_list.size());
      assertEquals("b", m_list.getInstance(0).getValue("city"));
      assertFalse(itr.hasPrevious());

      Instance instance = new Instance(m_metadata.getMetaclass("Address"), m_context);

      instance.setNew();
      instance.setValue("city", "c");
      m_list.add(instance);
      
      assertTrue(itr.hasNext());
      assertEquals("b", ((Instance)itr.next()).getValue("city"));
      assertFalse(itr.hasNext());
      assertTrue(itr.hasPrevious());
      assertEquals(0, itr.previousIndex());
      assertEquals(1, itr.nextIndex());
      assertEquals("b", ((Instance)itr.previous()).getValue("city"));
      assertTrue(itr.hasNext());
      assertFalse(itr.hasPrevious());
      assertEquals(-1, itr.previousIndex());
      assertEquals(0, itr.nextIndex());
      assertEquals(2, m_list.size());
      assertEquals("b", m_list.getInstance(0).getValue("city"));
      assertEquals("c", m_list.getInstance(1).getValue("city"));
      
      try
      {
         itr.add(instance);
         fail("Expected ConcurrentModificationException");
      }
      catch (ConcurrentModificationException e)
      {
      }
   }

   public void testListIteratorInt()
   {
      ListIterator itr = m_list.listIterator(1);
      
      assertTrue(itr.hasNext());
      assertEquals("b", ((Instance)itr.next()).getValue("city"));
      assertFalse(itr.hasNext());
      assertTrue(itr.hasPrevious());
      assertEquals(1, itr.previousIndex());
      assertEquals(2, itr.nextIndex());
   }

   public void testSubList()
   {
      List list = m_list.subList(1, 2);

      assertEquals(1, list.size());
      assertEquals("b", ((Instance)list.get(0)).getValue("city"));
      assertFalse(list.contains(m_list.get(0)));
      assertTrue(list.contains(m_list.get(1)));
      assertTrue(m_list.containsAll(list));
      assertFalse(list.containsAll(m_list));

      Instance instance = new Instance(m_metadata.getMetaclass("Address"), m_context);

      instance.setNew();
      instance.setValue("city", "c");
      
      list.add(0, instance);
      
      assertEquals(2, list.size());
      assertEquals(3, m_list.size());
      assertEquals("c", ((Instance)list.get(0)).getValue("city"));
      assertEquals("b", ((Instance)list.get(1)).getValue("city"));
      assertEquals("a", m_list.getInstance(0).getValue("city"));
      assertEquals("c", m_list.getInstance(1).getValue("city"));
      assertEquals("b", m_list.getInstance(2).getValue("city"));
      
      list.clear();
      assertEquals(0, list.size());
      assertEquals(1, m_list.size());
      assertEquals("a", m_list.getInstance(0).getValue("city"));
   }

   public void testSet()
   {
      try
      {
         m_list.set(0, null);
         fail("Expected UnsupportedOperationException");
      }
      catch (UnsupportedOperationException e)
      {
      }
   }

   public void testReverse()
   {
      m_list.reverse();
      
      assertEquals(2, m_list.size());
      assertEquals("b", m_list.getInstance(0).getValue("city"));
      assertEquals("a", m_list.getInstance(1).getValue("city"));
   }

   public void testList()
   {
      Pair pair = m_list.list();
      
      assertEquals(2, Pair.length(pair));
      assertEquals("a", ((Instance)pair.getHead()).getValue("city"));
      assertEquals("b", ((Instance)pair.getNext().getHead()).getValue("city"));
   }

   public void testSort()
   {
      m_list.sort(new Comparator()
      {
         public int compare(Object left, Object right)
         {
            return ((Comparable)((Instance)right).getValue("city")).compareTo(((Instance)left).getValue("city"));
         }
      });

      assertEquals("b", m_list.getInstance(0).getValue("city"));
      assertEquals("a", m_list.getInstance(1).getValue("city"));
      assertEquals(1, m_list.indexOf(m_instanceArray[0]));
      assertEquals(0, m_list.indexOf(m_instanceArray[1]));
   }

   public void testClone()
   {
      InstanceArrayList list = (InstanceArrayList)m_list.clone();
      
      assertNotSame(list, m_list);
      assertEquals(list, m_list);
      
      list.remove(0);
      
      assertEquals(1, list.size());
      assertEquals(2, m_list.size());
   }

   public void testHashCode()
   {
      assertTrue(m_list.hashCode() != 0);
      assertTrue(m_list.hashCode() == m_list.hashCode());
   }

   public void testEquals()
   {
      List list = new ArrayList();

      list.add(m_list.get(0));
      list.add(m_list.get(1));

      assertTrue(m_list.equals(m_list));
      assertFalse(m_list.equals(list));
      assertTrue(list.equals(m_list));
   }

   public void testToString()
   {
      assertEquals("[Instance<Address, null, NEW>(city=\"a\", contact=Instance<Contact, null, NEW>), " +
         "Instance<Address, null, NEW>(city=\"b\", contact=Instance<Contact, null, NEW>)]", m_list.toString());
   }
}
