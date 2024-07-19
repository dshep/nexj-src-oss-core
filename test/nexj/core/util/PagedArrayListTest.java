// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

import nexj.core.util.PagedArrayList.StreamTable;
import nexj.test.util.TempFileUtil;

import junit.framework.TestCase;


/**
 * Tests the functionality of the paging array list, which swaps array data
 * to disk to reduce heap space requirements.
 */
public class PagedArrayListTest extends TestCase
{
   // operations

   /**
    * Tests that the paged array list cleans up resources when it is disposed
    * and when an I/O error occurs.
    */
   public void testPagedArrayListCleanUp() throws Exception
   {
      TestPagedArrayList list = new TestPagedArrayList(10);

      for (int i = 0; i < 100; i++)
      {
         list.add(Integer.valueOf(i));
      }

      assertEquals(100, list.size());
      assertTrue(list.isStreamTableOpen());

      // Test that operations fail after it has been disposed
      list.dispose();
      assertFalse(list.isStreamTableOpen());

      try
      {
         list.get(0);
         fail();
      }
      catch (IllegalStateException ex)
      {
         assertEquals("Paged array list has been disposed", ex.getMessage());
      }

      try
      {
         list.size();
         fail();
      }
      catch (IllegalStateException ex)
      {
         assertEquals("Paged array list has been disposed", ex.getMessage());
      }

      list.dispose();


      // Test that the list is automatically disposed after an I/O error
      list = new TestPagedArrayList(10);

      for (int i = 0; i < 100; i++)
      {
         list.add(Integer.valueOf(i));
      }

      assertEquals(100, list.size());
      assertTrue(list.isStreamTableOpen());

      list.setIOError(new IOException("Simulated error"));

      try
      {
         assertTrue(list.isStreamTableOpen());
         list.get(0);
         fail();
      }
      catch (WrapperException ex)
      {
         assertEquals("Simulated error", ex.getMessage());
      }

      assertFalse(list.isStreamTableOpen());

      try
      {
         list.get(0);
         fail();
      }
      catch (IllegalStateException ex)
      {
         assertEquals("Paged array list has been disposed", ex.getMessage());
      }

      assertFalse(list.isStreamTableOpen());
   }

   /**
    * Tests that the paging array list can be serialized.
    */
   public void testPagedArrayListSerialization() throws Exception
   {
      doTestSerialization(new TestPagedArrayList(10));
   }

   /**
    * Tests the test of the paging array list by using a
    * java.util.ArrayList as the unit under test.
    */
   public void testArrayListSerialization() throws Exception
   {
      doTestSerialization(new ArrayList());
   }

   /**
    * Tests that the List may be serialized.
    * @param list The List implementation to test.
    */
   public void doTestSerialization(List list) throws Exception
   {
      ByteArrayOutputStream ostream;
      ObjectOutputStream oos;
      ByteArrayInputStream istream;
      ObjectInputStream ois;
      byte[] nSerializedArray;
      List deserialized;

      for (int i = 0; i < 100; i++)
      {
         list.add(Integer.valueOf(i));
      }

      ostream = new ByteArrayOutputStream();
      oos = new ObjectOutputStream(ostream);
      oos.writeObject(list);
      oos.close();
      nSerializedArray = ostream.toByteArray();

      istream = new ByteArrayInputStream(nSerializedArray);
      ois = new ObjectInputStream(istream);
      deserialized = (List)ois.readObject();
      assertEquals(-1, ois.read());

      assertEquals(100, deserialized.size());

      for (int i = 0; i < 100; i++)
      {
         assertEquals(i, ((Integer)deserialized.get(i)).intValue());
      }
   }

   /**
    * Tests the addAll() method of the paging array list.
    */
   public void testPagedArrayListAddAll() throws Exception
   {
      doTestAddAll(new TestPagedArrayList(10));
   }

   /**
    * Tests the test of the paging array list by using a
    * java.util.ArrayList as the unit under test.
    */
   public void testArrayListAddAll() throws Exception
   {
      doTestAddAll(new ArrayList());
   }

   /**
    * Tests the addAll() methods of a List implementation.
    * @param list The List implementation to test.
    */
   public void doTestAddAll(List list) throws Exception
   {
      for (int i = 0; i < 100; i++)
      {
         list.add(new TestElement("Original #" + i));
      }

      ArrayList emptyList, oneItemList, pageSizeList;

      emptyList = new ArrayList();
      oneItemList = new ArrayList();
      oneItemList.add(new TestElement("A single item"));
      pageSizeList = new ArrayList();

      for (int i = 0; i < 10; i++)
      {
         pageSizeList.add(new TestElement("Page size list #" + i));
      }

      // Test adding the empty list at various positions
      list.addAll(emptyList);
      list.addAll(0, emptyList);
      list.addAll(list.size(), emptyList);
      list.addAll(50, emptyList);

      assertEquals(100, list.size());

      for (int i = 0; i < 100; i++)
      {
         assertEquals("Original #" + i, ((TestElement)list.get(i)).getValue());
      }

      // Test adding the single item list at various positions
      list.addAll(oneItemList);
      list.addAll(0, oneItemList);
      list.addAll(list.size(), oneItemList);
      list.addAll(50, oneItemList);

      assertEquals(104, list.size());

      assertEquals("A single item", ((TestElement)list.get(0)).getValue());

      for (int i = 1; i <= 49; i++)
      {
         assertEquals("Original #" + (i - 1), ((TestElement)list.get(i)).getValue());
      }

      assertEquals("A single item", ((TestElement)list.get(50)).getValue());

      for (int i = 51; i <= 101; i++)
      {
         assertEquals("Original #" + (i - 2), ((TestElement)list.get(i)).getValue());
      }

      assertEquals("A single item", ((TestElement)list.get(102)).getValue());
      assertEquals("A single item", ((TestElement)list.get(103)).getValue());

      assertSame(list.get(0), list.get(50));
      assertSame(list.get(0), list.get(102));
      assertSame(list.get(0), list.get(103));


      // Test adding the list with a page of elements
      list.addAll(99, pageSizeList);

      assertEquals("Original #96", ((TestElement)list.get(98)).getValue());

      for (int i = 0; i < 9; i++)
      {
         assertEquals("Page size list #" + i, ((TestElement)list.get(99 + i)).getValue());
      }

      assertEquals("Original #97", ((TestElement)list.get(109)).getValue());
      assertEquals("Original #98", ((TestElement)list.get(110)).getValue());
      assertEquals("Original #99", ((TestElement)list.get(111)).getValue());
      assertEquals("A single item", ((TestElement)list.get(112)).getValue());
      assertEquals("A single item", ((TestElement)list.get(113)).getValue());
   }

   /**
    * Tests the iterator functionality of the paging array list. 
    */
   public void testPagedArrayListIterator() throws Exception
   {
      doTestIterator(new TestPagedArrayList(10));
   }

   /**
    * Tests the test of the paging array list by using a
    * java.util.ArrayList as the unit under test.
    */
   public void testArrayListIterator() throws Exception
   {
      doTestIterator(new ArrayList());
   }

   /**
    * Tests that a list can be iterated with the standard iterator
    * as well as the list iterator.
    * @param list The List implementation to test.
    */
   public void doTestIterator(List list) throws Exception
   {
      for (int i = 256; i >= 0; i--)
      {
         list.add(new TestElement(Integer.toHexString(i) + " buckets of bits on the bus."));
      }

      int k = 256;

      for (Iterator itr = list.iterator(); itr.hasNext(); )
      {
         TestElement item = (TestElement)itr.next();

         assertEquals(Integer.toHexString(k) + " buckets of bits on the bus.", item.getValue());

         if (k % 2 == 0)
         {
            itr.remove();
         }

         k--;
      }

      assertEquals(128, list.size());

      for (int i = 0; i <= 127; i++)
      {
         assertEquals(Integer.toHexString(256 - (i * 2 + 1)) + " buckets of bits on the bus.",
            ((TestElement)list.get(i)).getValue());
      }

      // Testing of the list iterator
      list.clear();

      for (int i = 0; i < 100; i++)
      {
         list.add(new TestElement("Original element index #" + i));
      }

      k = 0;

      ListIterator itr;

      for (itr = list.listIterator(); itr.hasNext(); )
      {
         assertEquals(k, itr.nextIndex());

         TestElement item = (TestElement)itr.next();

         assertEquals("Original element index #" + k, item.getValue());
         
         k++;
      }

      for (itr = list.listIterator(); itr.hasNext(); )
      {
         itr.add(new TestElement("Even element"));
         itr.next();
      }

      assertEquals(list.size(), itr.nextIndex());
      assertEquals(200, list.size());
      k = list.size() - 1;

      while (itr.hasPrevious())
      {
         assertEquals(k, itr.previousIndex());

         TestElement item = (TestElement)itr.previous();

         if (k % 2 == 0)
         {
            assertEquals("Even element", item.getValue());
         }
         else
         {
            assertEquals("Original element index #" + ((k + 1) / 2 - 1), item.getValue());
         }

         k--;
      }

      assertEquals(-1, itr.previousIndex());

      // Test of "set()"
      list.clear();

      for (int i = 0; i < 100; i++)
      {
         list.add(new TestElement("Original element index #" + i));
      }

      for (itr = list.listIterator(); itr.hasNext(); )
      {
         int nThisIndex = itr.nextIndex();

         itr.next();

         if (nThisIndex % 2 == 0)
         {
            itr.set(new TestElement("Even index #" + nThisIndex));
         }
         else
         {
            itr.set(new TestElement("Odd index #" + nThisIndex));
         }
      }

      assertEquals(100, list.size());
      k = list.size() - 1;
      itr = list.listIterator(100);

      while (itr.hasPrevious())
      {
         TestElement item = (TestElement)itr.previous();

         if (k % 2 == 0)
         {
            assertEquals("Even index #" + k, item.getValue());
         }
         else
         {
            assertEquals("Odd index #" + k, item.getValue());
         }

         k--;
      }
   }

   /**
    * Tests that a paging array list returns the same object instance every time
    * an element is accessed. (As long as the client maintains a reference to
    * the element).
    */
   public void testPagedArrayListInstanceCaching() throws Exception
   {
      doTestInstanceCaching(new TestPagedArrayList(10));
   }

   /**
    * Tests the test of the paging array list by using a
    * java.util.ArrayList as the unit under test.
    */
   public void testArrayListInstanceCaching() throws Exception
   {
      doTestInstanceCaching(new ArrayList());
   }

   /**
    * Tests that a list returns the same object instance every time an element
    * is accessed.
    * @param list The List implementation to test.
    */
   public void doTestInstanceCaching(List list) throws Exception
   {
      TestElement e1, e2, e3, e4, e5;

      for (int i = 0; i < 50; i++)
      {
         list.add(new TestElement("Element #" + i));
      }

      // Test that getting an element, forcing a page swap, and getting the element
      // a second time, yields the same object instance.
      e1 = (TestElement)list.get(0);
      e2 = (TestElement)list.get(49);
      e3 = (TestElement)list.get(0);

      assertEquals("Element #0", e1.getValue());
      assertEquals("Element #49", e2.getValue());
      assertEquals("Element #0", e3.getValue());
      assertEquals(e1, e3);
      assertSame(e1, e3);

      // Test that adding an element, forcing a page swap, and getting it again
      // yields the same object instance that was added.
      e1 = new TestElement("Added element");
      list.add(e1);
      list.get(0);
      e2 = (TestElement)list.get(list.size() - 1);

      assertEquals("Added element", e1.getValue());
      assertEquals("Added element", e2.getValue());
      assertEquals(e1, e2);
      assertSame(e1, e2);


      // Test that element removal works and that indices are shifted correctly
      list.remove(0);
      e1 = (TestElement)list.get(48);
      assertEquals("Element #49", e1.getValue());
      e1 = (TestElement)list.get(list.size() - 1);
      assertEquals(e1, e2);
      assertSame(e1, e2);

      e1 = (TestElement)list.get(0);
      assertNotSame(e1, e3);
      assertEquals("Element #0", e3.getValue());
      assertEquals("Element #1", e1.getValue());

      // Test that clearing the list works while external elements are held
      e1 = (TestElement)list.get(0);
      e2 = (TestElement)list.get(list.size() - 1);
      list.clear();

      assertEquals("Element #1", e1.getValue());
      assertEquals("Added element", e2.getValue());

      list.add(new TestElement("New Element #0"));
      e3 = (TestElement)list.get(0);
      assertEquals("New Element #0", e3.getValue());
      assertNotSame(e1, e3);

      // Test that an instance can be added in more than one position
      list.clear();

      for (int i = 0; i < 50; i++)
      {
         list.add(new TestElement("Element #" + i));
      }

      for (ListIterator itr = list.listIterator(); itr.hasNext(); )
      {
         TestElement item = (TestElement)itr.next();

         itr.add(item);
      }

      assertEquals(100, list.size());

      for (int i = 0; i < 100; i++)
      {
         assertEquals("Element #" + (i / 2), ((TestElement)list.get(i)).getValue());
      }

      System.gc();
      e1 = (TestElement)list.get(0);
      e2 = (TestElement)list.get(1);
      e3 = (TestElement)list.get(99);
      e4 = (TestElement)list.get(1);
      e5 = (TestElement)list.get(0);

      assertEquals("Element #0", e1.getValue());
      assertEquals("Element #0", e2.getValue());
      assertEquals("Element #49", e3.getValue());
      assertEquals("Element #0", e4.getValue());
      assertEquals("Element #0", e5.getValue());
      assertEquals(e1, e2);
      assertSame(e1, e2);
      assertEquals(e4, e5);
      assertSame(e4, e5);

      assertSame(e1, e4);  // This fails sometimes
      assertSame(e1, e5);
      assertSame(e2, e4);
      assertSame(e2, e5);
   }

   /**
    * Tests the sub list creation functionality of the paging array list.
    */
   public void testPagedArrayListSubList() throws Exception
   {
      doTestSubList(new TestPagedArrayList(10));
   }

   /**
    * Tests the sublist test of the paging array list by using a
    * java.util.ArrayList as the unit under test.
    */
   public void testArrayListSubList() throws Exception
   {
      doTestSubList(new ArrayList());
   }

   /**
    * Tests the sub list creation functionality of a List implementation.
    * @param list The List implementation to test.
    */
   public void doTestSubList(List list) throws Exception
   {
      // Add 10 pages of data
      for (int i = 0; i < 100; i++)
      {
         list.add(Integer.valueOf(i*2));
      }

      List list2 = list.subList(0, 10);

      assertEquals(10, list2.size());

      for (int i = 0; i < 10; i++)
      {
         assertEquals(i*2, ((Integer)list2.get(i)).intValue());
      }

      list2.set(0, Integer.valueOf(-100));
      assertEquals(-100, ((Integer)list2.get(0)).intValue());
      assertEquals(-100, ((Integer)list.get(0)).intValue());

      list.set(0, Integer.valueOf(-200));
      assertEquals(-200, ((Integer)list2.get(0)).intValue());
      assertEquals(-200, ((Integer)list.get(0)).intValue());

      // Inserting an element makes the sublist larger and the main list larger, too.
      list2.add(0, Integer.valueOf(-300));
      assertEquals(11, list2.size());
      assertEquals(101, list.size());
      assertEquals(-300, ((Integer)list2.get(0)).intValue());
      assertEquals(-300, ((Integer)list.get(0)).intValue());
      assertEquals(-200, ((Integer)list2.get(1)).intValue());
      assertEquals(-200, ((Integer)list.get(1)).intValue());

      for (int i = 2; i < 11; i++)
      {
         assertEquals((i - 1) * 2, ((Integer)list2.get(i)).intValue());
      }

      for (int i = 2; i < 101; i++)
      {
         assertEquals((i - 1) * 2, ((Integer)list.get(i)).intValue());
      }

      // Test that a sublist can be cleared to remove a range on the parent list
      list2.clear();

      assertEquals(90, list.size());

      for (int i = 0; i < 90; i++)
      {
         assertEquals((i + 10) * 2, ((Integer)list.get(i)).intValue());
      }


      // Test a sublist that is split over 2 pages of data
      list.clear();

      for (int i = 0; i < 20; i++)
      {
         list.add(Integer.valueOf(i));
      }

      list2 = list.subList(9, 11);
      assertEquals(2, list2.size());
      assertEquals(9, ((Integer)list2.get(0)).intValue());
      assertEquals(10, ((Integer)list2.get(1)).intValue());

      list2 = list.subList(9, 10);
      assertEquals(1, list2.size());
      assertEquals(9, ((Integer)list2.get(0)).intValue());

      list2 = list.subList(10, 11);
      assertEquals(1, list2.size());
      assertEquals(10, ((Integer)list2.get(0)).intValue());

      list2 = list.subList(9, 9);
      assertEquals(0, list2.size());
      list2.add(Integer.valueOf(100));
      assertEquals(1, list2.size());
      assertEquals(21, list.size());
      assertEquals(8, ((Integer)list.get(8)).intValue());
      assertEquals(100, ((Integer)list.get(9)).intValue());
      assertEquals(9, ((Integer)list.get(10)).intValue());
   }

   /**
    * Tests the paging array list itself.
    */
   public void testPagedArrayListInsertRemove() throws Exception
   {
      doInsertRemoveTest(new TestPagedArrayList(10));
   }

   /**
    * Tests the tests of the paging array list by using a
    * java.util.ArrayList as the unit under test.
    */
   public void testArrayListInsertRemove() throws Exception
   {
      doInsertRemoveTest(new ArrayList());
   }

   /**
    * Tests basic functionality of a List implementation.
    * @param list The List implementation to test.
    */
   public void doInsertRemoveTest(List list) throws Exception
   {
      assertEquals(0, list.size());
      assertTrue(list.isEmpty());

      // Add 10 pages worth of data
      for (int i = 0; i < 50; i++)
      {
         list.add(Integer.valueOf(i * 2 + 1));
      }

      for (int i = 0; i < 50; i++)
      {
         list.add(i * 2, Integer.valueOf(i * 2));
      }

      // Verify
      assertEquals(100, list.size());
      assertFalse(list.isEmpty());

      for (int i = 99; i >= 0; i--)
      {
         assertEquals(i, ((Integer)list.get(i)).intValue());
      }

      // Remove last element and verify
      list.remove(99);
      assertEquals(99, list.size());

      for (int i = 98; i >= 0; i--)
      {
         assertEquals(i, ((Integer)list.get(i)).intValue());
      }

      // Remove first element and verify
      list.remove(0);
      assertEquals(98, list.size());

      for (int i = 97; i >= 0; i--)
      {
         assertEquals(i + 1, ((Integer)list.get(i)).intValue());
      }

      // Set first element and verify
      assertEquals(Integer.valueOf(1), list.set(0, Integer.valueOf(-100)));

      for (int i = 97; i >= 1; i--)
      {
         assertEquals(i + 1, ((Integer)list.get(i)).intValue());
      }

      assertEquals(-100, ((Integer)list.get(0)).intValue());

      // Add element to end again
      list.add(list.size(), Integer.valueOf(-101));
      assertEquals(99, list.size());

      for (int i = 97; i >= 1; i--)
      {
         assertEquals(i + 1, ((Integer)list.get(i)).intValue());
      }

      assertEquals(-100, ((Integer)list.get(0)).intValue());
      assertEquals(-101, ((Integer)list.get(98)).intValue());

      // Insert element in middle
      list.add(25, Integer.valueOf(-102));
      assertEquals(100, list.size());

      for (int i = 98; i >= 26; i--)
      {
         assertEquals(i, ((Integer)list.get(i)).intValue());
      }

      for (int i = 24; i >= 1; i--)
      {
         assertEquals(i + 1, ((Integer)list.get(i)).intValue());
      }

      assertEquals(-100, ((Integer)list.get(0)).intValue());
      assertEquals(-101, ((Integer)list.get(99)).intValue());
      assertEquals(-102, ((Integer)list.get(25)).intValue());

      // Remove all elements, removing from beginning
      assertEquals(-100, ((Integer)list.remove(0)).intValue());

      for (int i = 1; i <= 24; i++)
      {
         assertEquals(i + 1, ((Integer)list.remove(0)).intValue());
      }

      assertEquals(-102, ((Integer)list.remove(0)).intValue());

      for (int i = 26; i <= 98; i++)
      {
         assertEquals(i, ((Integer)list.remove(0)).intValue());
      }

      assertEquals(-101, ((Integer)list.remove(0)).intValue());
      assertTrue(list.isEmpty());

      // Add 10 pages of data
      for (int i = 0; i < 100; i++)
      {
         list.add(Integer.valueOf(-i));
      }

      for (int i = 0; i < 100; i++)
      {
         assertEquals(-i, ((Integer)list.get(i)).intValue());
      }

      list.clear();
      assertEquals(0, list.size());
      assertTrue(list.isEmpty());

      try
      {
         list.remove(0);
         fail();
      }
      catch (IndexOutOfBoundsException ex)
      {
      }

      try
      {
         list.add(1, new Object());
         fail();
      }
      catch (IndexOutOfBoundsException ex)
      {
      }
   }

   /**
    * Tests that no paging is done until necessary. This ensures that
    * no disk accesses are performed when the number of elements in
    * the array is small (less than the page size).
    */
   public void testNoPagingUntilPageSizeExceeded() throws Exception
   {
      PagedArrayList list = new NoPagingPagedArrayList(5);

      list.add(Integer.valueOf(0));
      list.add(Integer.valueOf(1));
      list.add(Integer.valueOf(2));
      list.add(Integer.valueOf(3));
      list.add(Integer.valueOf(4));

      try
      {
         list.add(Integer.valueOf(5));
         fail();
      }
      catch (UnsupportedOperationException ex)
      {
         assertEquals("Paging not supported", ex.getMessage());
      }
   }

   /**
    * Tests that the paging array list throws out of bounds exceptions for
    * illegal indices.
    */
   public void testOutOfBounds() throws Exception
   {
      PagedArrayList list = new TestPagedArrayList(5);

      try
      {
         list.set(0, Integer.valueOf(0));
         fail();
      }
      catch (IndexOutOfBoundsException ex)
      {
      }
   }

   /**
    * Tests that an insert may be done to the last page of data.
    */
   public void testInsertsInLastPage() throws Exception
   {
      List list = new TestPagedArrayList(10);

      // Add one page of data
      for (int i = 0; i < 10; i++)
      {
         list.add(Integer.valueOf(i));
      }

      // Insert into this page
      list.add(9, Integer.valueOf(-100));

      // Verify that it was added
      assertEquals(11, list.size());

      for (int i = 0, nCheckNum = 0; i < list.size(); i++)
      {
         if (i == 9)
         {
            assertEquals(-100, ((Integer)list.get(i)).intValue());
         }
         else
         {
            assertEquals(nCheckNum, ((Integer)list.get(i)).intValue());
            nCheckNum++;
         }
      }
   }

   /**
    * Tests the toArray() methods on the paged array list.
    */
   public void testToArray() throws Exception
   {
      List list = new TestPagedArrayList(10);

      // Add one page of data
      for (int i = 0; i < 10; i++)
      {
         list.add(Integer.valueOf(i));
      }

      // Test the toArray() no-arg method
      Object[] array = list.toArray();

      assertTrue(array instanceof Object[]);
      assertTrue(!(array instanceof Integer[]));

      for (int i = 0; i < 10; i++)
      {
         assertTrue(array[i] instanceof Integer);
         assertEquals(array[i], list.get(i));
      }

      // Test the toArray() method with argument that is the correct size
      Integer[] integerArray = new Integer[10];

      array = list.toArray(integerArray);

      assertTrue(array instanceof Integer[]);
      assertTrue(array == integerArray);

      for (int i = 0; i < 10; i++)
      {
         assertTrue(array[i] instanceof Integer);
         assertEquals(array[i], list.get(i));
      }

      // Test the toArray() method with argument that is too small
      integerArray = new Integer[9];

      array = list.toArray(integerArray);

      assertTrue(array instanceof Integer[]);
      assertFalse(array == integerArray);

      for (int i = 0; i < 10; i++)
      {
         assertTrue(array[i] instanceof Integer);
         assertEquals(array[i], list.get(i));
      }

      // Test the toArray() method with argument that is bigger
      integerArray = new Integer[11];

      array = list.toArray(integerArray);

      assertTrue(array instanceof Integer[]);
      assertTrue(array == integerArray);

      for (int i = 0; i < 10; i++)
      {
         assertTrue(array[i] instanceof Integer);
         assertEquals(array[i], list.get(i));
      }

      assertNull(array[10]);
   }

   public void testListObjectCache() throws Exception
   {
      PagedArrayList.ListObjectCache cache = new PagedArrayList.ListObjectCache();

      cache.insert(100, new TestElement("A"));
      cache.set(50, new TestElement("B"));
      cache.set(0, new TestElement("C"));

      assertEquals("A", ((TestElement)cache.get(100)).getValue());
      assertEquals("B", ((TestElement)cache.get(50)).getValue());
      assertEquals("C", ((TestElement)cache.get(0)).getValue());

      cache.insert(0, new TestElement("X"));
      assertEquals("A", ((TestElement)cache.get(101)).getValue());
      assertEquals("B", ((TestElement)cache.get(51)).getValue());
      assertEquals("C", ((TestElement)cache.get(1)).getValue());
      assertEquals("X", ((TestElement)cache.get(0)).getValue());

      // Tests the expand code
      cache.insert(105, new TestElement("Y"));
      assertEquals("A", ((TestElement)cache.get(101)).getValue());
      assertEquals("B", ((TestElement)cache.get(51)).getValue());
      assertEquals("C", ((TestElement)cache.get(1)).getValue());
      assertEquals("X", ((TestElement)cache.get(0)).getValue());
      assertEquals("Y", ((TestElement)cache.get(105)).getValue());

      cache.remove(0);
      assertEquals("A", ((TestElement)cache.get(100)).getValue());
      assertEquals("B", ((TestElement)cache.get(50)).getValue());
      assertEquals("C", ((TestElement)cache.get(0)).getValue());
      assertEquals("Y", ((TestElement)cache.get(104)).getValue());

      // Tests the shrink code
      cache.remove(50);
      assertEquals("A", ((TestElement)cache.get(99)).getValue());
      assertEquals("C", ((TestElement)cache.get(0)).getValue());
      assertEquals("Y", ((TestElement)cache.get(103)).getValue());
   }

   /**
    * Tests that closing the file-backed stream table prevents further operations,
    * and also that it doesn't leak resources (disk space)
    */
   public void testFileBackedStreamTableClose() throws Exception
   {
      OutputStream o1;
      OutputStreamWriter w1;
      PagedArrayList.FileBackedStreamTable table = new PagedArrayList.FileBackedStreamTable(4);
      File file = TempFileUtil.makeTemporaryFile(this.getName());

      table.open(file);

      o1 = table.getOutputStream(1);
      w1 = new OutputStreamWriter(o1);

      w1.write("0000111");
      table.close();

      assertFalse(file.exists());

      // Test that the stream fails when the table has been closed
      try
      {
         w1.close();
         fail();
      }
      catch (IOException ex)
      {
         assertEquals("Stream closed", ex.getMessage());
      }


      // Test that the table does not give out new streams once closed
      try
      {
         table.getInputStream(1);
         fail();
      }
      catch (IOException ex)
      {
         assertEquals("Stream table closed", ex.getMessage());
      }

      try
      {
         table.getOutputStream(1);
         fail();
      }
      catch (IOException ex)
      {
         assertEquals("Stream table closed", ex.getMessage());
      }

      try
      {
         table.deleteStream(1);
         fail();
      }
      catch (IOException ex)
      {
         assertEquals("Stream table closed", ex.getMessage());
      }


      // Test that the stream continues failing when the table is reopened
      // Must open table and get the writer from there, then close table, then re-open
      // The reason is that previous writer has already been partially closed.
      table.open(file);
      o1 = table.getOutputStream(1);
      w1 = new OutputStreamWriter(o1);
      w1.write("2222333");
      assertTrue(file.exists());
      table.close();
      assertFalse(file.exists());
      table.open(file);

      try
      {
         w1.close();
         fail();
      }
      catch (IOException ex)
      {
         assertEquals("Stream closed", ex.getMessage());
      }


      // Test that streams fail when operated on after they are closed
      o1 = table.getOutputStream(1);
      o1.close();

      try
      {
         o1.write(0x01);
         fail();
      }
      catch (IOException ex)
      {
         assertEquals("Stream closed", ex.getMessage());
      }
   }

   /**
    * Tests that the FileBackedStreamTable returns free blocks at the end of the
    * file back to the OS.
    */
   public void testFileBackedStreamTableCompaction() throws Exception
   {
      PagedArrayList.FileBackedStreamTable table = new PagedArrayList.FileBackedStreamTable(4);

      try
      {
         File file = TempFileUtil.makeTemporaryFile(this.getName());

         table.open(file);

         assertEquals(0, file.length());

         OutputStream o1;
         OutputStreamWriter w1;

         // Current file contents: nothing
         o1 = table.getOutputStream(1);
         w1 = new OutputStreamWriter(o1);
         w1.write("00001111");
         w1.close();

         assertEquals(8, file.length());

         // Current file contents: 00001111
         o1 = table.getOutputStream(2);
         w1 = new OutputStreamWriter(o1);
         w1.write("22223333");
         w1.close();

         assertEquals(16, file.length());

         // Current file contents: 0000111122223333
         o1 = table.getOutputStream(1);
         w1 = new OutputStreamWriter(o1);
         w1.write("444");
         w1.close();

         assertEquals(16, file.length());

         // Current file contents: 444'''''22223333
         table.deleteStream(1);

         assertEquals(16, file.length());

         // Current file contents: ''''''''22223333
         o1 = table.getOutputStream(2);
         w1 = new OutputStreamWriter(o1);
         w1.write("555566667777");
         w1.close(); // compaction happens at this moment

         assertEquals(12, file.length());

         // Current file contents: 555566667777
         o1 = table.getOutputStream(2);
         w1 = new OutputStreamWriter(o1);
         w1.write("==");
         w1.close();

         assertEquals(4, file.length());

         // Current file contents: ==''
      }
      finally
      {
         table.close();
      }
   }

   /**
    * Does functional tests on the MemoryBackedStreamTable.
    */
   public void testMemoryBackedStreamTable() throws Exception
   {
      doTestStreamTable(new MemoryBackedStreamTable());
   }

   /**
    * Does functional tests on the FileBackedStreamTable.
    */
   public void testFileBackedStreamTable() throws Exception
   {
      PagedArrayList.FileBackedStreamTable table = new PagedArrayList.FileBackedStreamTable(16);

      table.open(TempFileUtil.makeTemporaryFile(this.getName()));
      doTestStreamTable(table);
      table.close();
   }

   /**
    * Does the functional tests, but using blocks of one byte.
    */
   public void testFileBackedStreamTableMinimalBlockSize() throws Exception
   {
      PagedArrayList.FileBackedStreamTable table = new PagedArrayList.FileBackedStreamTable(1);

      table.open(TempFileUtil.makeTemporaryFile(this.getName()));
      doTestStreamTable(table);
      table.close();
   }

   /**
    * Does the functional tests, but using a block size larger than any stream's data.
    */
   public void testFileBackedStreamTableLargeBlockSize() throws Exception
   {
      PagedArrayList.FileBackedStreamTable table = new PagedArrayList.FileBackedStreamTable(128);

      table.open(TempFileUtil.makeTemporaryFile(this.getName()));
      doTestStreamTable(table);
      table.close();
   }

   /**
    * Tests the functionality of a StreamTable implementation.
    * @param table The StreamTable implementation to test.
    */
   public void doTestStreamTable(StreamTable table) throws Exception
   {
      OutputStream o1, o2, o3;
      OutputStreamWriter w1, w2, w3;
      InputStream i1, i2, i3;
      InputStreamReader r1, r2, r3;
      BufferedReader br1, br2, br3;


      // Initially, there should be no streams
      assertNull(table.getInputStream(0));
      assertNull(table.getInputStream(Long.MAX_VALUE));
      assertNull(table.getInputStream(Long.MIN_VALUE));


      // Deletion should do nothing
      table.deleteStream(0);
      table.deleteStream(Long.MAX_VALUE);
      table.deleteStream(Long.MIN_VALUE);


      // Create the first stream
      o1 = table.getOutputStream(0);
      w1 = new OutputStreamWriter(o1);

      w1.write("This is the first stream!");
      w1.close();

      try
      {
         o1.write(0x30);
         fail();
      }
      catch (IOException ex)
      {
      }


      // Read the first stream
      i1 = table.getInputStream(0);
      r1 = new InputStreamReader(i1, "UTF-8");
      br1 = new BufferedReader(r1);

      assertEquals("This is the first stream!", br1.readLine());
      assertEquals(-1, br1.read());
      br1.close();
      assertEquals(-1, i1.read());


      // Create two streams at once
      o2 = table.getOutputStream(1);
      o3 = table.getOutputStream(2);
      w2 = new OutputStreamWriter(o2);
      w3 = new OutputStreamWriter(o3);

      w2.write("This is part A of the second stream.\n");
      w3.write("This is part A of the third stream.");
      w2.write("This is part B of the second stream.\n");
      w3.write("This is part B of the third stream.");
      w2.close();
      w3.close();

      // Read all three streams
      i1 = table.getInputStream(0);
      i2 = table.getInputStream(1);
      i3 = table.getInputStream(2);
      r1 = new InputStreamReader(i1, "UTF-8");
      r2 = new InputStreamReader(i2, "UTF-8");
      r3 = new InputStreamReader(i3, "UTF-8");
      br1 = new BufferedReader(r1);
      br2 = new BufferedReader(r2);
      br3 = new BufferedReader(r3);

      assertEquals("This is the first stream!", br1.readLine());
      assertEquals(-1, br1.read());
      br1.close();


      assertEquals("This is part A of the second stream.", br2.readLine());


      assertEquals("This is part A of the third stream.This is part B of the third stream.", br3.readLine());
      assertEquals(-1, br3.read());
      br3.close();


      assertEquals("This is part B of the second stream.", br2.readLine());
      assertEquals(-1, br2.read());
      br2.close();


      // Re-write the first stream
      o1 = table.getOutputStream(0);
      w1 = new OutputStreamWriter(o1);

      w1.write("This is the new data for the first stream.");
      w1.close();


      // Read all three streams
      i1 = table.getInputStream(0);
      i2 = table.getInputStream(1);
      i3 = table.getInputStream(2);
      r1 = new InputStreamReader(i1, "UTF-8");
      r2 = new InputStreamReader(i2, "UTF-8");
      r3 = new InputStreamReader(i3, "UTF-8");
      br1 = new BufferedReader(r1);
      br2 = new BufferedReader(r2);
      br3 = new BufferedReader(r3);

      assertEquals("This is the new data for the first stream.", br1.readLine());
      assertEquals(-1, br1.read());
      br1.close();


      assertEquals("This is part A of the second stream.", br2.readLine());
      assertEquals("This is part B of the second stream.", br2.readLine());
      assertEquals(-1, br2.read());
      br2.close();


      assertEquals("This is part A of the third stream.This is part B of the third stream.", br3.readLine());
      assertEquals(-1, br3.read());
      br3.close();


      // Delete the second stream
      table.deleteStream(1);


      // Read streams
      i1 = table.getInputStream(0);
      assertNull(table.getInputStream(1));
      i3 = table.getInputStream(2);
      r1 = new InputStreamReader(i1, "UTF-8");
      r3 = new InputStreamReader(i3, "UTF-8");
      br1 = new BufferedReader(r1);
      br3 = new BufferedReader(r3);

      assertEquals("This is the new data for the first stream.", br1.readLine());
      assertEquals(-1, br1.read());
      br1.close();


      assertEquals("This is part A of the third stream.This is part B of the third stream.", br3.readLine());
      assertEquals(-1, br3.read());
      br3.close();

      // Test a zero-length stream
      assertNull(table.getInputStream(10));
      o1 = table.getOutputStream(10);
      w1 = new OutputStreamWriter(o1);
      w1.close();
      i1 = table.getInputStream(10);
      r1 = new InputStreamReader(i1, "UTF-8");
      assertEquals(-1, r1.read());
      r1.close();
      table.deleteStream(10);
      assertNull(table.getInputStream(10));

      // Test writing a byte of value -1 (catches improper integer cast when reading)
      assertNull(table.getInputStream(11));
      o1 = table.getOutputStream(11);
      o1.write(0x7f);
      o1.write(0x80);
      o1.write(0xfe);
      o1.write(0xff);
      o1.close();
      i1 = table.getInputStream(11);
      assertEquals(0x7f, i1.read());
      assertEquals(0x80, i1.read());
      assertEquals(0xfe, i1.read());
      assertEquals(0xff, i1.read());
      assertEquals(-1, i1.read());
      i1.close();
      table.deleteStream(11);
      assertNull(table.getInputStream(11));
   }


   // inner classes

   /**
    * An immutable object containing a String, to use as a list element when
    * testing Lists.
    */
   protected static class TestElement implements Serializable
   {
      // constants

      /**
       * The serialization UID. 
       */
      private static final long serialVersionUID = 7420462270387036110L;


      // attributes

      /**
       * The String value held by this element.
       */
      protected String m_sValue;


      // constructors

      /**
       * Creates a new test element.
       * @param sValue The String to go in the element.
       */
      public TestElement(String sValue)
      {
         m_sValue = sValue;
      }


      // operations

      /**
       * @return This element's String.
       */
      public String getValue()
      {
         return m_sValue;
      }

      /**
       * @see java.lang.Object#equals(java.lang.Object)
       */
      public boolean equals(Object obj)
      {
         if (obj instanceof TestElement)
         {
            return ObjUtil.equal(m_sValue, ((TestElement)obj).m_sValue);
         }

         return false;
      }

      /**
       * @see java.lang.Object#hashCode()
       */
      public int hashCode()
      {
         return m_sValue.hashCode();
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         return "TestElement(\"" + m_sValue + "\")";
      }
   }

   /**
    * An immutable test element containing a String, in which the hash code
    * of this object is settable by the user.
    * 
    * WARNING! The equals method does comparison of the Strings in the
    * elements--it does not look at the hash codes. Therefore, it is possible
    * to violate the general contract of equals() by having two elements
    * that equate positively but have different hash codes. It is the
    * responsiblity of the caller to ensure that any two instances created
    * with equal strings will have the same hash code.
    */
   protected static class GivenHashTestElement extends TestElement
   {
      // constants

      /**
       * The serialization UID.
       */
      private static final long serialVersionUID = 2559353571613814425L;


      // attributes

      /**
       * The hash code for this instance.
       */
      protected int m_nHashCode;


      // constructors

      /**
       * Creates a new test element with a given hash code.
       * @param sValue The String to go in the element.
       */
      public GivenHashTestElement(String sValue, int nHashCode)
      {
         super(sValue);
         m_nHashCode = nHashCode;
      }


      // operations

      /**
       * @see nexj.core.util.PagedArrayListTest.TestElement#hashCode()
       */
      public int hashCode()
      {
         return m_nHashCode;
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         return "UniqueHashTestElement(\"" + m_sValue + "\", hash=0x" + Integer.toHexString(m_nHashCode) + ")";
      }
   }


   /**
    * A PagedArrayList that will not perform paging.
    */
   protected static class NoPagingPagedArrayList extends PagedArrayList
   {
      // constants

      /**
       * The serialization version.
       */
      private static final long serialVersionUID = 7189575655242516718L;


      // attributes

      /**
       * Flag to control whether operations that cause a page fault
       * will succeed (by swapping the page) or fail (with an exception).
       */
      protected boolean m_bPagingEnabled;

      /**
       * The number of page swap operations performed.
       */
      protected long m_lPageSwapCount;


      // constructors

      public NoPagingPagedArrayList(int nElementsPerPage)
      {
         super(nElementsPerPage);
      }


      // operations

      /**
       * Gets the number of pages used by the data in this list.
       * 
       * @return The number of pages in use.
       */
      public int getNumberOfPages()
      {
         return m_pageList.size();
      }

      /**
       * Resets the page swap count to zero.
       */
      public void resetPageSwapCount()
      {
         m_lPageSwapCount = 0;
      }

      /**
       * Gets the number of page swaps performed since resetPageSwapCount()
       * was last called.
       * 
       * @return The page swap count.
       */
      public long getPageSwapCount()
      {
         return m_lPageSwapCount;
      }

      /**
       * Sets the paging enabled flag to enable paging. Without paging enabled,
       * any page fault will throw an exception.
       * 
       * @param bPagingEnabled True to enable paging; false to throw an exception
       *                       on page fault.
       */
      public void setPagingEnabled(boolean bPagingEnabled)
      {
         m_bPagingEnabled = bPagingEnabled;
      }

      /**
       * @see nexj.core.util.PagedArrayList#createStreamTable()
       */
      protected StreamTable createStreamTable()
      {
         if (m_bPagingEnabled)
         {
            return super.createStreamTable();
         }
         else
         {
            throw new UnsupportedOperationException("Paging not supported");
         }
      }

      /**
       * @see nexj.core.util.PagedArrayList#deletePage(nexj.core.util.PagedArrayList.PageInfo)
       */
      protected void deletePage(PageInfo page)
      {
         if (m_bPagingEnabled)
         {
            super.deletePage(page);
         }
         else
         {
            throw new UnsupportedOperationException("Paging not supported");
         }
      }

      /**
       * @see nexj.core.util.PagedArrayList#swapPageOut(nexj.core.util.PagedArrayList.PageInfo)
       */
      protected void swapPageOut(PageInfo page)
      {
         if (m_bPagingEnabled)
         {
            super.swapPageOut(page);
         }
         else
         {
            throw new UnsupportedOperationException("Paging not supported");
         }
      }

      /**
       * @see nexj.core.util.PagedArrayList#swapTo(nexj.core.util.PagedArrayList.PageInfo)
       */
      protected void swapTo(PageInfo newPage)
      {
         if (m_bPagingEnabled || newPage == m_activePage)
         {
            super.swapTo(newPage);
            m_lPageSwapCount++;
         }
         else
         {
            throw new UnsupportedOperationException("Paging not supported");
         }
      }
   }

   /**
    * A PagedArrayList that uses a MemoryBackedStreamTable to store the
    * paged data. This narrows the scope and increases test performance.
    */
   protected static class TestPagedArrayList extends PagedArrayList
   {
      // constants

      /**
       * The serialization version.
       */
      private static final long serialVersionUID = 7250154337974170251L;


      // associations

      /**
       * A shadow copy of the stream table so that we can check its status
       * even after the super class has made its reference to the stream
       * table invalid.
       */
      protected transient MemoryBackedStreamTable m_streamTableShadow;


      // constructors

      public TestPagedArrayList(int nElementsPerPage)
      {
         super(nElementsPerPage);
      }


      // operations

      /**
       * @see nexj.core.util.PagedArrayList#createStreamTable()
       */
      protected StreamTable createStreamTable()
      {
         m_streamTableShadow = new MemoryBackedStreamTable();
         return m_streamTableShadow;
      }

      /**
       * Sets an error to throw on the next I/O operation on the
       * stream table.
       * 
       * @param ex The error to throw.
       */
      public void setIOError(IOException ex)
      {
         m_streamTableShadow.setIOError(ex);
      }

      /**
       * @return True if the stream table is open; false otherwise.
       */
      public boolean isStreamTableOpen()
      {
         return m_streamTableShadow.isOpen();
      }
   }

   /**
    * This is a stream table implementation that stores the streams on the heap.
    */
   public static class MemoryBackedStreamTable implements StreamTable
   {
      // attributes

      /**
       * The next free stream id.
       */
      protected long m_lFreeStreamId;


      // associations

      /**
       * The stream data lookup. Keyed on Long stream ids, values
       * are byte[].
       */
      protected Lookup m_streamLookup;

      /**
       * The error to throw when the next operation is performed.
       */
      protected IOException m_errorToThrow;


      // constructors

      /**
       * Creates an initializes an empty stream table.
       */
      public MemoryBackedStreamTable()
      {
         m_streamLookup = new HashTab();
      }


      // operations

      /**
       * Sets an error to throw on the next I/O operation on the
       * stream table.
       * 
       * @param ex The error to throw.
       */
      public void setIOError(IOException ex)
      {
         m_errorToThrow = ex;
      }

      /**
       * Simulates an I/O error by throwing an exception that was previously
       * set with setIOError(). Resets the member variable holding the exception
       * so that a subsequent call to simulateError() will not throw an error,
       * unless setIOError() is called again in the interim.
       * 
       * @throws IOException The I/O exception that was set with setIOError()
       */
      protected void simulateError() throws IOException
      {
         if (m_errorToThrow != null)
         {
            try
            {
               throw m_errorToThrow;
            }
            finally
            {
               m_errorToThrow = null;
            }
         }
      }

      /**
       * @see nexj.core.util.PagedArrayList.StreamTable#clear()
       */
      public void clear()
      {
         m_lFreeStreamId = 0;
         m_streamLookup.clear();
         m_errorToThrow = null;
      }

      /**
       * @see nexj.core.util.PagedArrayList.StreamTable#deleteStream(long)
       */
      public void deleteStream(long lStreamId) throws IOException
      {
         simulateError();
         m_streamLookup.remove(Long.valueOf(lStreamId));
      }

      /**
       * @see nexj.core.util.PagedArrayList.StreamTable#getInputStream(long)
       */
      public InputStream getInputStream(long lStreamId) throws IOException
      {
         simulateError();

         byte[] nStreamDataArray = (byte[])m_streamLookup.get(Long.valueOf(lStreamId));

         if (nStreamDataArray == null)
         {
            return null;
         }

         return new ByteArrayInputStream(nStreamDataArray);
      }

      /**
       * @see nexj.core.util.PagedArrayList.StreamTable#getOutputStream(long)
       */
      public OutputStream getOutputStream(long lStreamId) throws IOException
      {
         simulateError();

         if (lStreamId >= m_lFreeStreamId)
         {
            m_lFreeStreamId = lStreamId + 1;
         }

         return new StreamTableOutputStream(lStreamId);
      }

      /**
       * @see nexj.core.util.PagedArrayList.StreamTable#getFreeStreamId()
       */
      public long getFreeStreamId()
      {
         return m_lFreeStreamId;
      }

      /**
       * @see java.io.Closeable#close()
       */
      public void close() throws IOException
      {
         simulateError();
         m_streamLookup = null;
      }

      /**
       * @return True if this stream table is open; false otherwise.
       */
      public boolean isOpen()
      {
         return m_streamLookup != null;
      }


      // inner classes

      /**
       * The output stream implementation provided by the MemoryBackedStreamTable
       * for writing data into the table.
       */
      protected class StreamTableOutputStream extends OutputStream
      {
         // attributes

         /**
          * The id of the stream being written.
          */
         protected long m_lStreamId;


         // associations

         /**
          * The true destination to which the data are written.
          */
         ByteArrayOutputStream m_out;


         // constructors

         /**
          * Creates a new output stream for the given stream id.
          * @param lStreamId The id of the stream being written.
          */
         public StreamTableOutputStream(long lStreamId)
         {
            m_lStreamId = lStreamId;
            m_out = new ByteArrayOutputStream();
         }


         // operations

         /**
          * Checks that this stream table is open.
          * @throws IOException If not open.
          */
         protected void checkOpen() throws IOException
         {
            if (m_out == null)
            {
               throw new IOException("Stream closed");
            }
         }

         /**
          * @see java.io.OutputStream#write(int)
          */
         public void write(int b) throws IOException
         {
            simulateError();
            checkOpen();
            m_out.write(b);
         }

         /**
          * @see java.io.OutputStream#close()
          */
         public void close() throws IOException
         {
            simulateError();

            if (m_out != null)
            {
               byte[] nDataArray = m_out.toByteArray();
   
               m_streamLookup.put(Long.valueOf(m_lStreamId), nDataArray);
               m_out = null;
            }
         }

         /**
          * @see java.io.OutputStream#flush()
          */
         public void flush() throws IOException
         {
            simulateError();
            checkOpen();
         }

         /**
          * @see java.io.OutputStream#write(byte[], int, int)
          */
         public void write(byte[] b, int off, int len) throws IOException
         {
            simulateError();
            checkOpen();
            m_out.write(b, off, len);
         }

         /**
          * @see java.io.OutputStream#write(byte[])
          */
         public void write(byte[] b) throws IOException
         {
            simulateError();
            checkOpen();
            m_out.write(b);
         }
      }

   }
}
