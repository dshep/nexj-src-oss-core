// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.Arrays;
import java.util.Iterator;

import junit.framework.TestCase;

import nexj.core.persistence.OID;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.PropertyIterator;
import nexj.core.util.SysUtil;
import nexj.core.util.UncheckedException;

public class TransferObjectTest extends TestCase
{
   private TransferObject m_tobj = null;

   /**
    * Constructor for TransferObjectTest.
    * @param name
    */
   public TransferObjectTest(String name)
   {
      super(name);
   }

   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();
      
      m_tobj = new TransferObject("Contact", 2);
      m_tobj.setEventName("new");
      m_tobj.setOID(new OID(new Object[]{new Integer(123)}));
      m_tobj.setVersion((short)12345);
      m_tobj.setValue("firstName", "John");
      m_tobj.setValue("lastName", "Smith");
      m_tobj.setValue("middleName", null);
   }

   public void testSetClassName()
   {
      m_tobj.setClassName("abc");
      assertEquals("abc", m_tobj.getClassName());
   }

   public void testGetClassName()
   {
      assertEquals("Contact", m_tobj.getClassName());
   }

   public void testSetEventName()
   {
      m_tobj.setEventName("read");
      assertEquals("read", m_tobj.getEventName());
   }

   public void testGetEventName()
   {
      assertEquals("new", m_tobj.getEventName());
   }

   public void testSetValue()
   {
      m_tobj.setValue("a", "b");
      assertEquals(4, m_tobj.getValueCount());
      assertEquals("b", m_tobj.getValue("a"));
      
      m_tobj.setValue("middleName", "Peter");
      assertEquals(4, m_tobj.getValueCount());
      assertEquals("Peter", m_tobj.getValue("middleName"));
   }

   public void testGetValue()
   {
      assertEquals("John", m_tobj.getValue("firstName"));
      assertEquals("Smith", m_tobj.getValue("lastName"));
      assertNull(m_tobj.getValue("middleName"));
      
      try
      {
         m_tobj.getValue("m");
         fail("Expected UncheckedException");
      }
      catch (UncheckedException e)
      {
      }
   }
   
   public void testFindValue()
   {
      assertEquals("John", m_tobj.findValue("firstName", "a"));
      assertEquals("Smith", m_tobj.findValue("lastName", "a"));
      assertNull(m_tobj.findValue("middleName", "a"));
      assertEquals("a", m_tobj.findValue("q", "a"));
      assertEquals("John", m_tobj.findValue("firstName"));
      assertEquals("Smith", m_tobj.findValue("lastName"));
      assertNull(m_tobj.findValue("middleName"));
      assertNull(m_tobj.findValue("q"));
   }
   
   public void testHasValue()
   {
      assertTrue(m_tobj.hasValue("firstName"));
      assertTrue(m_tobj.hasValue("lastName"));
      assertFalse(m_tobj.hasValue("LastName"));
      assertFalse(m_tobj.hasValue("m"));
   }

   public void testRemoveValue()
   {
      assertTrue(m_tobj.removeValue("firstName"));
      assertEquals(2, m_tobj.getValueCount());
      assertFalse(m_tobj.hasValue("firstName"));
      assertFalse(m_tobj.removeValue("m"));
      assertEquals(2, m_tobj.getValueCount());
   }
   
   public void testRemoveAllValues()
   {
      m_tobj.removeAllValues();
      assertEquals(0, m_tobj.getValueCount());
      
      int nCount = 0;
      
      for (PropertyIterator itr = m_tobj.getIterator(); itr.hasNext();)
      {
         itr.next();
         ++nCount;
      }
      
      assertEquals(0, nCount);
   }

   public void testGetValueCount()
   {
      assertEquals(3, m_tobj.getValueCount());
   }

   public void testGetIterator()
   {
      Iterator itr = m_tobj.getIterator();
      
      assertNotNull(itr);
      
      itr.next();
      itr.next();
      itr.next();
      
      assertFalse(itr.hasNext());
   }

   public void testSetVersion()
   {
      m_tobj.setVersion((short)123);
      
      assertEquals(123L, m_tobj.getVersion());
   }

   public void testGetVersion()
   {
      assertEquals(12345L, m_tobj.getVersion());
   }

   public void testSetOID()
   {
      m_tobj.setOID(new OID(new Object[]{"a"}));
      assertEquals("a", m_tobj.getOID().getValue(0));
      m_tobj.setOID(null);
      assertNull(m_tobj.getOID());
   }

   public void testGetOID()
   {
      assertEquals(new Integer(123), m_tobj.getOID().getValue(0));
   }
   
   public void testExternalize() throws IOException, ClassNotFoundException
   {
      ByteArrayOutputStream bostream = new ByteArrayOutputStream();
      ObjectOutputStream ostream = new ObjectOutputStream(bostream);

      ostream.writeObject(m_tobj);
      
      ObjectInputStream istream = new ObjectInputStream(new ByteArrayInputStream(bostream.toByteArray()));
      
      TransferObject tobj = (TransferObject)istream.readObject();

      assertEquals(m_tobj.getClassName(), tobj.getClassName());
      assertEquals(m_tobj.getEventName(), tobj.getEventName());
      assertEquals(m_tobj.getVersion(), tobj.getVersion());
      assertEquals(m_tobj.getOID(), tobj.getOID());
      assertEquals(m_tobj.getValueCount(), tobj.getValueCount());
      
      for (PropertyIterator itr = m_tobj.getIterator(); itr.hasNext();)
      {
         itr.next();
         assertEquals(itr.getValue(), tobj.getValue(itr.getName()));
      }
   }

   /*
    * Test for Object clone()
    */
   public void testClone()
   {
      TransferObject tobj = (TransferObject)m_tobj.clone();

      assertEquals(tobj.getValueCount(), m_tobj.getValueCount());
      assertEquals(tobj.getValue("firstName"), m_tobj.getValue("firstName"));
      tobj.setValue("firstName", "a");
      assertFalse(tobj.getValue("firstName").equals(m_tobj.getValue("firstName")));
   }

   /*
    * Test for String toString()
    */
   public void testToString()
   {
      assertEquals("TO<Contact'new, 12345, OID:1:I3:123>(" + SysUtil.LINE_SEP + "   middleName=(),"  + SysUtil.LINE_SEP +
         "   lastName=\"Smith\"," + SysUtil.LINE_SEP + "   firstName=\"John\"" + SysUtil.LINE_SEP + ")", m_tobj.toString());

      m_tobj.setValue("self", m_tobj);
      m_tobj.setValue("selfArray", new Object[]{m_tobj, m_tobj});
      m_tobj.setValue("selfCol", Arrays.asList(new Object[]{m_tobj, m_tobj}));

      assertEquals("TO<Contact'new, 12345, OID:1:I3:123>(" + SysUtil.LINE_SEP + "   middleName=()," + SysUtil.LINE_SEP +
         "   lastName=\"Smith\"," + SysUtil.LINE_SEP + "   selfCol={" + SysUtil.LINE_SEP +
         "      REF:TO<Contact'new, 12345, OID:1:I3:123>," + SysUtil.LINE_SEP +
         "      REF:TO<Contact'new, 12345, OID:1:I3:123>" + SysUtil.LINE_SEP + "   }," + SysUtil.LINE_SEP +
         "   firstName=\"John\"," + SysUtil.LINE_SEP + "   self=REF:TO<Contact'new, 12345, OID:1:I3:123>," + SysUtil.LINE_SEP +
         "   selfArray=#(REF:TO<Contact'new, 12345, OID:1:I3:123> REF:TO<Contact'new, 12345, OID:1:I3:123>)" + SysUtil.LINE_SEP +
         ")", m_tobj.toString());

      TransferObject tobj = new TransferObject(1);
      Lookup map = new HashTab(1);

      map.put("tobj", tobj);
      tobj.setValue("map", map);

      assertEquals("TO<, @>(" + SysUtil.LINE_SEP + "   map={\"tobj\"=REF:TO<, @>}" + SysUtil.LINE_SEP + ")",
         tobj.toString().replaceAll("@\\d+", "@"));
   }
}
