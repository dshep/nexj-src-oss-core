// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

import junit.framework.TestCase;

import nexj.core.meta.Repository;

public class StateTest extends TestCase
{
   protected Flow m_flow;
   protected State m_state;
   
   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();
      m_flow = Repository.getMetadata().getWorkflow("HolidayRequest_A");
      m_state = new State(m_flow, "0", null, null);
   }

   /*
    * Class under test for void State(Flow, String)
    */
   public void testStateWorkflowString()
   {
      assertEquals("", new State(m_flow, "", null, null).toString());
      assertEquals("3 5 9", new State(m_flow, "3 5 9", null, null).toString());
   }

   /*
    * Class under test for void State(Flow, boolean)
    */
   public void testStateWorkflowboolean()
   {
      assertEquals("", new State(m_flow, false).toString());
      assertEquals("0", new State(m_flow, true).toString());
   }

   public void testGetWorkflow()
   {
      assertEquals("HolidayRequest_A", m_state.getFlow().getName());
   }

   public void testSetDirty()
   {
      m_state.setDirty(false);
      assertFalse(m_state.isDirty());
      m_state.setDirty(true);
      assertTrue(m_state.isDirty());
   }

   public void testIsDirty()
   {
      assertFalse(m_state.isDirty());
   }

   public void testIsFinal()
   {
      assertFalse(m_state.isFinal());
   }

   public void testAddCleanupStep()
   {
      m_state.addCleanupStep(m_flow.getFlowStep(1));
      assertTrue(m_state.getCleanupStepCollection().contains(m_flow.getFlowStep(1)));
   }

   public void testClearCleanupSteps()
   {
      m_state.addCleanupStep(m_flow.getFlowStep(1));
      m_state.clearCleanupSteps();
      assertTrue(m_state.getCleanupStepCollection().isEmpty());
   }

   public void testGetCleanupStepCollection()
   {
      assertTrue(m_state.getCleanupStepCollection().isEmpty());
      m_state.addCleanupStep(m_flow.getFlowStep(1));
      assertFalse(m_state.getCleanupStepCollection().isEmpty());
   }

   public void testGetCleanupStepIterator()
   {
      assertFalse(m_state.getCleanupStepIterator().hasNext());
      m_state.addCleanupStep(m_flow.getFlowStep(1));
      assertTrue(m_state.getCleanupStepIterator().hasNext());
   }

   public void testAdd()
   {
      m_state.add(m_flow.getFlowStep(3));
      assertTrue(m_state.contains(m_flow.getFlowStep(3)));
      assertTrue(m_state.contains(m_flow.getFlowStep(0)));
   }

   /*
    * Class under test for boolean remove(Step, boolean)
    */
   public void testRemoveStepboolean()
   {
      assertTrue(m_state.remove(m_flow.getFlowStep(0), false));
      assertFalse(m_state.contains(m_flow.getFlowStep(0)));
   }

   /*
    * Class under test for boolean remove(Step)
    */
   public void testRemoveStep()
   {
      m_state.remove(m_flow.getFlowStep(0));
      assertFalse(m_state.contains(m_flow.getFlowStep(0)));
   }

   public void testRemoveConcurrent()
   {
      m_state.removeConcurrent(null);
      assertFalse(m_state.contains(m_flow.getFlowStep(0)));
   }

   /*
    * Class under test for boolean contains(Step)
    */
   public void testContainsStep()
   {
      assertTrue(m_state.contains(m_flow.getFlowStep(0)));
   }

   /*
    * Class under test for boolean contains(String)
    */
   public void testContainsString()
   {
      assertTrue(m_state.contains("Review by the manager"));
   }

   public void testContainsConcurrent()
   {
      assertTrue(m_state.containsConcurrent(null));
   }

   public void testGet()
   {
      assertEquals("Review by the manager", m_state.get(0).getName());
   }

   public void testSize()
   {
      assertEquals(1, m_state.size());
   }

   public void testGetStepNames()
   {
      assertEquals("(\"Review by the manager\")", m_state.getStepNames().toString());
   }

   public void testGetTransientStep()
   {
      assertEquals("Review by the manager", m_state.getTransientStep().getName());
   }

   public void testRun()
   {
      assertNotNull(m_state.run());
      assertEquals("1", m_state.toString());
   }

   public void testFromString()
   {
      m_state.fromString("3 5 9");
      assertEquals("3 5 9", m_state.toString());
   }

   /*
    * Class under test for String toString()
    */
   public void testToString()
   {
      assertEquals("0", m_state.toString());
   }
}
