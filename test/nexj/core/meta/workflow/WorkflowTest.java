// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

import java.util.Iterator;

import junit.framework.TestCase;

import nexj.core.meta.Event;
import nexj.core.meta.Repository;

public class WorkflowTest extends TestCase
{
   protected Workflow m_workflow;

   protected void setUp() throws Exception
   {
      super.setUp();
      m_workflow = Repository.getMetadata().getWorkflow("HolidayRequest_A");
   }

   public void testGetWorkflow()
   {
      assertSame(m_workflow, m_workflow.getFlow());
   }

   public void testGetParent()
   {
      assertNull(m_workflow.getParent());
   }

   public void testGetContainer()
   {
      assertNull(m_workflow.getContainer());
   }

   public void testGetFork()
   {
      assertNull(m_workflow.getFork());
   }

   public void testGetName()
   {
      assertEquals("HolidayRequest_A", m_workflow.getName());
   }

   public void testGetVersion()
   {
      assertEquals(0, m_workflow.getVersion());
   }

   public void testGetFullName()
   {
      assertEquals("HolidayRequest_A.0", m_workflow.getFullName());
   }

   public void testGetMetaclass()
   {
      assertEquals("HRRequest", m_workflow.getMetaclass().getName());
   }

   public void testGetAttributes()
   {
      assertNull(m_workflow.getAttributes());
   }

   public void testGetCaption()
   {
      assertEquals("Holiday Request", m_workflow.getCaption());
   }

   public void testGetAssociationCount()
   {
      assertEquals(0, m_workflow.getAssociationCount());
   }

   public void testGetAssociationIterator()
   {
      assertFalse(m_workflow.getAssociationIterator().hasNext());
   }

   public void testGetTarget()
   {
      assertEquals("HRRequest", m_workflow.getTarget().getName());
   }

   public void testGetEvent()
   {
      assertEquals("create", m_workflow.getEvent().getName());
      assertEquals(0, m_workflow.getEvent().getArgumentCount());
   }

   public void testGetCondition()
   {
      assertEquals(Boolean.TRUE, m_workflow.getCondition());
   }

   public void testGetWorkflowStepString()
   {
      assertEquals("branch on manager's decision", m_workflow.getFlowStep("branch on manager's decision").getName());
   }

   public void testGetWorkflowStepint()
   {
      assertEquals("Review by the manager", m_workflow.getFlowStep(0).getName());
   }

   public void testGetWorkflowStepCount()
   {
      assertEquals(19, m_workflow.getFlowStepCount());
   }

   public void testGetWorkflowStepIterator()
   {
      Iterator itr = m_workflow.getFlowStepIterator(); 
      
      for (int i = 0; i < m_workflow.getFlowStepCount(); ++i)
      {
         itr.next();
      }
      
      assertFalse(itr.hasNext());
   }

   public void testGetInitialState()
   {
      assertEquals("0", new State(m_workflow, true).toString());
   }

   public void testGetFunctions()
   {
      assertNotNull(m_workflow.getFunctions(null, (Event)Repository.getMetadata()
         .getMetaclass("HRRequest").getSelector("update").getMember(0)));
   }

   public void testToString()
   {
      assertEquals("Workflow HolidayRequest_A.0", m_workflow.toString());
   }
}
