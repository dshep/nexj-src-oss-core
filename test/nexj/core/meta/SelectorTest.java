// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import java.util.Iterator;

import junit.framework.TestCase;

public class SelectorTest extends TestCase
{
   private Selector m_firstName = null;
   private Selector m_read = null;
   private Selector m_test = null;

   /**
    * Constructor for SelectorTest.
    * @param name
    */
   public SelectorTest(String name)
   {
      super(name);
   }

   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();
      
      Metaclass contact = Repository.getMetadata().getMetaclass("Contact");
      
      m_firstName = contact.getSelector("firstName");
      m_read = contact.getSelector("read");
      
      m_test = Repository.getMetadata().getMetaclass("Principal").getSelector("test");
   }

   public void testGetMetaclass()
   {
      assertEquals("Contact", m_firstName.getMetaclass().getName());
      assertEquals("Contact", m_read.getMetaclass().getName());
   }

   public void testIsVarArg()
   {
      assertEquals(false, m_read.isVarArg());
      assertEquals(true, m_test.isVarArg());
   }

   public void testFindMember()
   {
      assertEquals("firstName", m_firstName.findMember(0).getName());
      assertEquals("firstName", m_firstName.findMember(1).getName());
      assertNull(m_firstName.findMember(2));
      assertNull(m_test.findMember(0));
      assertEquals("test", m_test.findMember(1).getName());
      assertEquals("test", m_test.findMember(2).getName());
      assertEquals("test", m_test.findMember(3).getName());
   }

   public void testGetMember()
   {
      assertEquals("read", m_read.getMember(6).getName());
      
      try
      {
         m_read.getMember(8).getName();
         fail("Expected MetadataLookupException");
      }
      catch (MetadataLookupException e)
      {
      }
   }

   public void testGetMaxArgCount()
   {
      assertEquals(1, m_firstName.getMaxArgCount());
      assertEquals(6, m_read.getMaxArgCount());
      assertEquals(1, m_test.getMaxArgCount());
   }

   public void testGetMemberIterator()
   {
      Iterator itr = m_firstName.getMemberIterator();
      
      assertNotNull(itr.next());
      assertNotNull(itr.next());
      assertFalse(itr.hasNext());
   }

   public void testGetName()
   {
      assertEquals("firstName", m_firstName.getName());
      assertEquals("read", m_read.getName());
   }

   /*
    * Test for String toString()
    */
   public void testToString()
   {
      assertEquals("Selector firstName", m_firstName.toString());
   }
}
