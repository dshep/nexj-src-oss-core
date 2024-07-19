// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.service;

import nexj.core.meta.Repository;

import junit.framework.TestCase;

public class ServiceTest extends TestCase
{
   protected Service m_jump;

   protected void setUp() throws Exception
   {
      super.setUp();
      m_jump = Repository.getMetadata().getService("Jump");
   }

   public void testLinking()
   {
      assertNull(m_jump.getFlowStep("1").getNext().getNext());
   }
}
