// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import junit.framework.TestCase;

import nexj.core.meta.Repository;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.ThreadContextHolder;

/**
 * Tests the Scheme compiler and p-code interpreter.
 */
public class SchemeTest extends TestCase
{
   private InvocationContext m_context;
   
   /**
    * Constructor for SchemeTest.
    * @param name
    */
   public SchemeTest(String name)
   {
      super(name);
   }

   /*
    * @see TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();
      m_context = new InvocationContext(Repository.getMetadata());
      m_context.setSecure(false);
      ThreadContextHolder.setContext(m_context);
   }

   /**
    * @see junit.framework.TestCase#tearDown()
    */
   protected void tearDown() throws Exception
   {
      m_context.complete(false);
      ThreadContextHolder.setContext(null);
      super.tearDown();
   }

   public void testScheme()
   {
      m_context.getMachine().getGlobalEnvironment().defineVariable("file-url", SchemeTest.class.getResource("testfile.txt").toString());
      Intrinsic.load(SchemeTest.class.getResource("schemetest.scm").toString(), m_context.getMachine());
   }
}
