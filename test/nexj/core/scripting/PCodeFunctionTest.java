// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import java.util.Set;

import nexj.core.meta.Repository;
import nexj.core.util.HashHolder;

import junit.framework.TestCase;

public class PCodeFunctionTest extends TestCase
{
   protected GlobalEnvironment m_env;
   protected PCodeFunction m_user;
   protected PCodeFunction m_sysGenerateEvent;

   protected void setUp() throws Exception
   {
      m_env = Repository.getMetadata().getGlobalEnvironment();
      m_user = (PCodeFunction)m_env.getVariable(Symbol.define("user"));
      m_sysGenerateEvent = (PCodeFunction)m_env.getVariable(Symbol.SYS_GENERATE_EVENT);
   }

   public void testComputeGlobals()
   {
      Set symbolSet = new HashHolder();

      m_sysGenerateEvent.computeGlobals(symbolSet, m_env);
      assertEquals(2, symbolSet.size());
      assertTrue(symbolSet.contains(Symbol.define("reverse!")));
      assertTrue(symbolSet.contains(Symbol.define("reverse2!")));

      symbolSet.clear();
      m_sysGenerateEvent.computeGlobals(symbolSet, null);
      assertEquals(2, symbolSet.size());
      assertTrue(symbolSet.contains(Symbol.define("reverse!")));
      assertTrue(symbolSet.contains(Symbol.define("reverse2!")));
   }

   public void testDisasm()
   {
      assertEquals(
         "; PCodeFunction:user\n" +
         "0\tchkf\t0\n" +
         "2\tpushc\t[0]\t; 'user\n" +
         "4\tcall:invocation-context\t0\n" +
         "6\tcall\t1\n" +
         "\n" +
         "0\tdefc\t'user\n" +
         "1\tdefc\t#(0 10 0 0  4 11 4 0  6 11 3 0) ; debug info\n" +
         "2\tdefc\t#(\"syslibrary:server\" \"user\")\n",
         m_user.disasm());

      assertTrue(m_sysGenerateEvent.disasm().length() > 100);
   }
}
