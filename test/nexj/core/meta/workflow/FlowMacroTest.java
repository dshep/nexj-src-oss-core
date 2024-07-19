// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.workflow;

import java.util.Iterator;

import nexj.core.meta.Primitive;
import nexj.core.meta.Repository;
import nexj.core.util.Undefined;

import junit.framework.TestCase;

public class FlowMacroTest extends TestCase
{
   protected FlowMacro m_macro;
   
   protected void setUp() throws Exception
   {
      super.setUp();
      
      m_macro = Repository.getMetadata().getFlowMacro("Increment");
   }

   public void testGetArgument()
   {
      assertEquals("variable", m_macro.getArgument(0).getName());
      assertEquals(Undefined.VALUE, m_macro.getArgument(0).getDefault());
      assertEquals("delta", m_macro.getArgument(1).getName());
      assertEquals(Primitive.ONE_INTEGER, m_macro.getArgument(1).getDefault());
   }

   public void testGetArgumentCount()
   {
      assertEquals(2, m_macro.getArgumentCount());
   }

   public void testGetArgumentIterator()
   {
      Iterator itr = m_macro.getArgumentIterator();
      
      itr.next();
      itr.next();

      assertFalse(itr.hasNext());
   }

   public void testGetBody()
   {
      assertNull(m_macro.getBody());
   }

   public void testGetFunction()
   {
      assertNotNull(m_macro.getFunction());
   }

   public void testIsWorkflowCompatible()
   {
      assertTrue(m_macro.isWorkflowCompatible());
   }

   public void testIsServiceCompatible()
   {
      assertFalse(m_macro.isServiceCompatible());
   }

   public void testGetName()
   {
      assertEquals("Increment", m_macro.getName());
   }
}
