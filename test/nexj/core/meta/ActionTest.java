// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import junit.framework.TestCase;

public class ActionTest extends TestCase
{
   private Action m_a = null;
   
   /**
    * Constructor for ActionTest.
    * @param name
    */
   public ActionTest(String name)
   {
      super(name);
   }

   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();
      
      m_a = ((Event)Repository.getMetadata().getMetaclass("Principal").getSelector("test").getMember(2)).getAction("a");
   }

   public void testGetType()
   {
      assertEquals(Action.BEFORE, m_a.getType());
   }

   public void testGetCondition()
   {
      assertEquals(Boolean.FALSE, m_a.getCondition());
   }

   public void testGetEvent()
   {
      assertEquals("test", m_a.getEvent().getName());
   }

   public void testGetNextAction()
   {
      assertEquals("b", m_a.getNextAction().getName());
   }

   public void testGetMethod()
   {
      assertEquals("loadMapping", m_a.getMethod().getName());
   }

   public void testGetName()
   {
      assertEquals("a", m_a.getName());
   }

   /*
    * Test for String toString()
    */
   public void testToString()
   {
      assertEquals("Action Principal.test.a <before> b", m_a.toString());
   }
}
