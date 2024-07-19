// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import java.util.Iterator;

import junit.framework.TestCase;

public class EventTest extends TestCase
{
   private Event m_principalTest;
   private Event m_userTest;
   private Event m_groupTest;
   private Event m_principalRefRead;
   private Event m_testAccess;

   /**
    * Constructor for EventTest.
    * @param name
    */
   public EventTest(String name)
   {
      super(name);
   }

   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();

      Metadata metadata = Repository.getMetadata();

      m_principalTest = (Event)metadata.getMetaclass("Principal").getSelector("test").getMember(2);
      m_userTest = (Event)metadata.getMetaclass("User").getSelector("test").getMember(2);
      m_groupTest = (Event)metadata.getMetaclass("Group").getSelector("test").getMember(2);
      m_principalRefRead = (Event)metadata.getMetaclass("PrincipalRef").getSelector("read").getMember(6);
      m_testAccess = (Event)metadata.getMetaclass("TestPointcut3").getSelector("testAccess").getMember(0);
   }

   public void testIsAttribute()
   {
      assertEquals(false, m_principalTest.isAttribute());
   }

   public void testIsVarArg()
   {
      assertEquals(true, m_principalTest.isVarArg());
   }

   /*
    * Test for Action getAction(String)
    */
   public void testGetActionString()
   {
      assertEquals("main", m_principalTest.getAction("main").getName());
      assertEquals("a", m_principalTest.getAction("a").getName());
      assertEquals("b", m_principalTest.getAction("b").getName());
      assertEquals("c", m_principalTest.getAction("c").getName());
      assertEquals("main", m_userTest.getAction("main").getName());
      assertEquals("a", m_userTest.getAction("a").getName());
      assertEquals("d", m_userTest.getAction("d").getName());
      assertEquals("a", m_groupTest.getAction("a").getName());
      assertEquals("e", m_groupTest.getAction("e").getName());
      
      try
      {
         m_principalTest.getAction("z");
         fail("Expected MetadataLookupException");
      }
      catch (MetadataLookupException e)
      {
      }
   }

   public void testFindAction()
   {
      assertEquals("a", m_principalTest.findAction("a").getName());
      assertNull(m_principalTest.findAction("z"));
   }

   /*
    * Test for Action getAction(int)
    */
   public void testGetActionint()
   {
      assertEquals("a", m_principalTest.getAction(0).getName());
      assertEquals("b", m_principalTest.getAction(1).getName());
      assertEquals("c", m_principalTest.getAction(2).getName());
      assertEquals("main", m_principalTest.getAction(3).getName());
      
      assertEquals("d", m_userTest.getAction(0).getName());
      assertEquals("a", m_userTest.getAction(1).getName());
      assertEquals("b", m_userTest.getAction(2).getName());
      assertEquals("c", m_userTest.getAction(3).getName());
      assertEquals("main", m_userTest.getAction(4).getName());

      assertEquals("a", m_groupTest.getAction(0).getName());
      assertEquals("b", m_groupTest.getAction(1).getName());
      assertEquals("e", m_groupTest.getAction(2).getName());
      assertEquals("c", m_groupTest.getAction(3).getName());
      assertEquals("main", m_groupTest.getAction(4).getName());
   }

   public void testGetActionCount()
   {
      assertEquals(4, m_principalTest.getActionCount());
      assertEquals(5, m_userTest.getActionCount());
      assertEquals(5, m_groupTest.getActionCount());
   }

   public void testGetActionIterator()
   {
      Iterator itr = m_principalTest.getActionIterator();
      itr.next();
      itr.next();
      itr.next();
      itr.next();
      assertEquals(false, itr.hasNext());
   }

   /*
    * Test for Argument getArgument(String)
    */
   public void testGetArgumentString()
   {
      assertEquals("x", m_principalTest.getArgument("x").getName());
      assertEquals("y", m_principalTest.getArgument("y").getName());

      try
      {
         m_principalTest.getArgument("a");
         fail("Expected MetadataLookupException");
      }
      catch (MetadataLookupException e)
      {
      }
   }

   /*
    * Test for Argument getArgument(int)
    */
   public void testGetArgumentint()
   {
      assertEquals("x", m_principalTest.getArgument(0).getName());
      assertEquals("y", m_principalTest.getArgument(1).getName());
   }

   public void testGetArgumentCount()
   {
      assertEquals(2, m_principalTest.getArgumentCount());
   }

   public void testGetArgumentIterator()
   {
      Iterator itr = m_principalTest.getArgumentIterator();
      itr.next();
      itr.next();
      assertFalse(itr.hasNext());
   }

   public void testGetSelector()
   {
      assertEquals("test", m_principalTest.getSelector().getName());
   }

   public void testIsStatic()
   {
      assertEquals(false, m_principalTest.isStatic());
   }

   public void testGetMetaclass()
   {
      assertEquals("Principal", m_principalTest.getMetaclass().getName());
      assertEquals("User", m_userTest.getMetaclass().getName());
      assertEquals("Group", m_groupTest.getMetaclass().getName());
   }

   public void testGetDeclarator()
   {
      assertEquals("Principal", m_principalTest.getDeclarator().getName());
      assertEquals("User", m_userTest.getDeclarator().getName());
   }
   
   public void testGetRootDeclarator()
   {
      assertEquals("Principal", m_principalTest.getRootDeclarator().getName());
      assertEquals("Principal", m_userTest.getRootDeclarator().getName());
   }

   public void testGetName()
   {
      assertEquals("test", m_principalTest.getName());
      assertEquals("test", m_userTest.getName());
   }

   public void testGetVisibility()
   {
      assertEquals(Metaclass.PROTECTED, m_userTest.getVisibility());
   }

   public void testGetPrivilege()
   {
      assertNull(m_userTest.getPrivilege());
      assertEquals("updateContact", Repository.getMetadata().getMetaclass("Contact").findEvent("update", 0).getPrivilege().getName());
      assertNotNull(m_principalRefRead.getPrivilege());
      assertEquals("readContact", m_principalRefRead.getPrivilege().getName());
      assertEquals("updateContact", m_testAccess.getPrivilege().getName());
   }

   public void testGetAccessAttribute()
   {
      assertNull(m_userTest.getAccessAttribute());
      assertEquals("readable", Repository.getMetadata().getMetaclass("Contact").findEvent("read", 6).getAccessAttribute().getName());
      assertEquals("readable", Repository.getMetadata().getMetaclass("Patient").findEvent("read", 6).getAccessAttribute().getName());
      assertNotNull(m_principalRefRead.getAccessAttribute());
      assertEquals("readable", m_principalRefRead.getAccessAttribute().getName());
      assertEquals("access", m_testAccess.getAccessAttribute().getName());
   }

   public void testGetTransactionMode()
   {
      assertEquals(Event.TX_SUPPORTED, m_principalTest.getTransactionMode());
      assertEquals(Event.TX_SUPPORTED, m_userTest.getTransactionMode());
      assertEquals(Event.TX_REQUIRED, Repository.getMetadata().getMetaclass("Contact").findEvent("update", 0).getTransactionMode());
      assertEquals(Event.TX_REQUIRED, Repository.getMetadata().getMetaclass("PrincipalRef").findEvent("ref2", 0).getTransactionMode());
      assertEquals(Event.TX_SUPPORTED, Repository.getMetadata().getMetaclass("PrincipalRef2").findEvent("ref2", 0).getTransactionMode());
      assertEquals(Event.TX_NONE, m_testAccess.getTransactionMode());
   }

   public void testIsAudited()
   {
      assertTrue(m_principalTest.isAudited());
      assertFalse(m_groupTest.isAudited());
      assertTrue(m_userTest.isAudited());
      assertFalse(m_principalRefRead.isAudited());
   }

   public void testGetAudited()
   {
      assertEquals(Boolean.TRUE, m_principalTest.getAudited());
      assertEquals(Boolean.FALSE, m_groupTest.getAudited());
      assertEquals(Boolean.TRUE, m_userTest.getAudited());
      assertNull(m_principalRefRead.getAudited());
   }

   public void testIsCompatibleWith()
   {
      Event derived = new Event(null);
      Event base = new Event(null);
      Argument user = new Argument("arg");
      Argument object = new Argument("arg");
      Argument untyped = new Argument("arg");

      object.setType(Repository.getMetadata().getMetaclass("Object"));
      user.setType(Repository.getMetadata().getMetaclass("User"));

      assertTrue(derived.isCompatibleWith(base));
      assertTrue(base.isCompatibleWith(derived));

      derived.setResult(object);
      assertTrue(derived.isCompatibleWith(base));
      assertTrue(base.isCompatibleWith(derived));

      base.setResult(user);
      assertFalse(derived.isCompatibleWith(base));
      assertTrue(base.isCompatibleWith(derived));

      base.setResult(null);
      derived.setResult(null);
      assertTrue(derived.isCompatibleWith(base));
      assertTrue(base.isCompatibleWith(derived));

      derived.addArgument(user);
      assertFalse(derived.isCompatibleWith(base));
      assertFalse(base.isCompatibleWith(derived));

      base.addArgument(object);
      assertTrue(derived.isCompatibleWith(base));
      assertFalse(base.isCompatibleWith(derived));

      base = new Event(null);
      base.addArgument(untyped);
      assertTrue(derived.isCompatibleWith(base));
      assertTrue(base.isCompatibleWith(derived));
   }

   public void testInheritArguments()
   {
      Type type = Repository.getMetadata().getMetaclass("User");
      Event derived = new Event(null);
      Event base = new Event(null);
      Argument user = new Argument("arg");
      Argument untyped = new Argument("arg");
      Argument result = new Argument("arg");

      result.setType(type);
      user.setType(type);

      base.addArgument(user);
      derived.setResult(result);
      derived.addArgument(untyped);
      derived.inheritEventTypes(base);

      assertEquals(type, base.getArgument(0).getType());
      assertEquals(type, derived.getArgument(0).getType());
      assertNull(base.getResult());
      assertEquals(type, derived.getResult().getType());

      base.inheritEventTypes(derived);
      assertEquals(type, base.getResult().getType());
      assertEquals(type, derived.getResult().getType());
   }

   /*
    * Test for String toString()
    */
   public void testToString()
   {
      assertEquals("Event Principal.test(x, y)", m_principalTest.toString());
      assertEquals("Event User.test(x, y)", m_userTest.toString());
   }
}
