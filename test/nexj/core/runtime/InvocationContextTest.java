// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

import nexj.core.meta.Metaclass;
import nexj.core.meta.Primitive;
import nexj.core.persistence.Query;
import nexj.core.persistence.sql.SQLDataTest;
import nexj.core.rpc.Request;
import nexj.core.rpc.Server;
import nexj.core.rpc.TransferObject;

public class InvocationContextTest extends SQLDataTest
{
   public InvocationContextTest(String sName)
   {
      super(sName);
   }

   public void testSetLoggerUnitOfWork()
   {
      assertEquals(0, Query.createRead(getMetadata().getMetaclass("SysObjectLog"),
         null, null, null, -1, 0, false, Query.SEC_NONE, m_context).read().size());

      UnitOfWork uowSaved = m_context.setLoggerUnitOfWork();

      Instance audit = new Instance(getMetadata().getMetaclass("SysObjectLog"), m_context);

      audit.setNew();
      audit.setValue("class", "Test");
      audit.setValue("event", "N/A");
      audit.setValue("argCount", Primitive.ZERO_INTEGER);
      audit.setValue("user", m_context.getUser());

      m_context.setUnitOfWork(uowSaved);
      m_context.complete(false);
      m_context.initUnitOfWork();

      assertEquals(1, Query.createRead(getMetadata().getMetaclass("SysObjectLog"),
         null, parse("(= event \"N/A\")"), null, -1, 0, false, Query.SEC_NONE, m_context).read().size());
   }

   /**
    * This test is a pre-condition of other tests in this file.
    */
   public void testCanUnitTestsDetectImproperCommitOfATransaction()
   {
      Metaclass metaclass = m_context.getMetadata().getMetaclass("InvocationContextTest");
      UnitOfWork initialUOW = m_context.getUnitOfWork();

      assertFalse(initialUOW.isTransient());
      assertNull(initialUOW.getTransaction());
      metaclass.invoke("new");

      initialUOW.setTransient(true);  // force error if initialUOW is committed

      try
      {
         metaclass.invoke("testCommitTransaction");
         fail("Expected exception");
      }
      catch (WorkStateException ex)
      {
         assertEquals("err.runtime.transientCommit", ex.getErrorCode());
      }
   }

   /**
    * Precondition:
    *    testCanUnitTestsDetectImproperCommitOfATransaction must PASS for this test result to be valid.
    */
   public void testSuspendTransactionDoesNotCommitOtherTransaction()
   {
      Metaclass metaclass = m_context.getMetadata().getMetaclass("InvocationContextTest");
      UnitOfWork initialUOW = m_context.getUnitOfWork();

      assertEquals(0, initialUOW.getInstanceCount());
      assertFalse(initialUOW.isTransient());
      assertNull(initialUOW.getTransaction());
      metaclass.invoke("new");

      initialUOW.setTransient(true);  // force error if initialUOW is committed
      metaclass.invoke("testSuspendTransaction");

      UnitOfWork finalUOW = m_context.getUnitOfWork();

      assertSame(initialUOW, finalUOW);
   }

   /**
    * Precondition:
    *    testCanUnitTestsDetectImproperCommitOfATransaction must PASS for this test result to be valid.
    */
   public void testNoneTransactionModeDoesNotCommitOtherTransaction()
   {
      Metaclass metaclass = m_context.getMetadata().getMetaclass("InvocationContextTest");
      UnitOfWork initialUOW = m_context.getUnitOfWork();

      assertEquals(0, initialUOW.getInstanceCount());
      assertFalse(initialUOW.isTransient());
      assertNull(initialUOW.getTransaction());
      metaclass.invoke("new");

      initialUOW.setTransient(true);  // force error if initialUOW is committed
      metaclass.invoke("testTransactionModeNONE");

      UnitOfWork finalUOW = m_context.getUnitOfWork();

      assertSame(initialUOW, finalUOW);
   }

   /**
    * Test that begin-transaction creates a new, non-transient UnitOfWork when invoked
    * from a transient, transacted UnitOfWork. 
    * 
    * Precondition:
    *    testCanUnitTestsDetectImproperCommitOfATransaction must PASS for this test result to be valid.
    */
   public void testBeginTransactionWhenTransient()
   {
      Server server = (Server)m_context.getComponentInstance("Server.Generic");
      Request req = new Request();
      TransferObject tobj = new TransferObject();

      tobj.setClassName("InvocationContextTest");
      tobj.setEventName("create");

      req.addInvocation(tobj);
      req.setCommit(false);

      // This should complete without throwing the err.runtime.transientCommit exception
      server.invoke(req);
   }
}
