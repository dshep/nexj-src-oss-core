// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

import junit.framework.TestCase;

import nexj.core.meta.Repository;

public class StepTest extends TestCase
{
   protected Step m_step;
   
   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();
      m_step = Repository.getMetadata().getWorkflow("HolidayRequest_A").getFlowStep("approve by default if no action is taken ;-)");
   }

   public void testGetActivity()
   {
      assertNotNull(m_step.getActivity());
   }

   public void testGetFork()
   {
      assertEquals("escalationJoin", m_step.getFork().getName());
   }

   public void testGetNext()
   {
      assertEquals(m_step.getName(), m_step.getNext().getName());
   }

   public void testGetOrdinal()
   {
      assertEquals(8, m_step.getOrdinal());
   }

   public void testIsPersistent()
   {
      assertFalse(m_step.isPersistent());
   }

   public void testIsActive()
   {
      assertTrue(m_step.isActive(new State(m_step.getActivity().getFlow(), "8", null, null)));
   }

   public void testGetCleanupAssoc()
   {
      assertEquals("timers", m_step.getNext().getCleanupAssoc(0).getName());
   }

   public void testGetCleanupAssocCount()
   {
      assertEquals(0, m_step.getCleanupAssocCount());
   }

   public void testGetCleanupAssocIterator()
   {
      assertFalse(m_step.getCleanupAssocIterator().hasNext());
   }
}
