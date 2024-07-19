package nexj.core.scripting;

import java.io.StringReader;

import nexj.core.meta.Repository;
import nexj.core.runtime.InvocationContext;
import nexj.core.util.IdentityHashTab;
import nexj.core.util.Lookup;
import nexj.core.util.TextPositionReader;

import junit.framework.TestCase;

/**
 * Tests call-with-continuation.
 */
public class CallCCTest extends TestCase
{
   private GlobalEnvironment m_globalEnv = null;
   private SchemeParser m_parser = null;
   private Lookup m_posMap = null;
   private Compiler m_compiler = null;
   private InvocationContext m_context = null;
   private Machine m_machine = null;

   /**
    * Constructor for CallCCTest.
    * @param name
    */
   public CallCCTest(String name)
   {
      super(name);
   }

   /**
    * @see junit.framework.TestCase#setUp()
    */
   protected void setUp() throws Exception
   {
      super.setUp();

      m_posMap = new IdentityHashTab();
      m_globalEnv = new GlobalEnvironment();
      m_parser = new SchemeParser(m_globalEnv);
      m_compiler = new Compiler();
      m_context = new InvocationContext(Repository.getMetadata());
      m_machine = m_context.getMachine();
   }

   /**
    * @see junit.framework.TestCase#tearDown()
    */
   protected void tearDown() throws Exception
   {
      super.tearDown();

      m_posMap = null;
      m_globalEnv = null;
      m_parser = null;
      m_compiler = null;
      m_context = null;
      m_machine = null;
   }

   /**
    * Tests that the current stack values are retained after resizing the stack during a continuation invocation.
    */
   public void testCallCCStackResizing()
   {
      Object expr = m_parser.parse(new TextPositionReader(new StringReader("(define cont '())")), m_posMap);
      PCodeFunction func = m_compiler.compile(expr, m_posMap, m_machine, true);

      m_machine.invoke(func, (Pair)null);

      expr = m_parser.parse(new TextPositionReader(
         new StringReader("(try (call/cc (lambda (c) (set! cont c) 123)) (lambda (e) 'error) ())")), m_posMap);
      func = m_compiler.compile(expr, m_posMap, m_machine, true);
      m_machine.invoke(func, (Pair)null);

      expr = m_parser.parse(new TextPositionReader(new StringReader(
         "(cont \"retval\" 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118)"
         )), m_posMap);
      func = m_compiler.compile(expr, m_posMap, m_machine, true);

      Object value = m_machine.invoke(func, (Pair)null);

      assertEquals("retval", value);
   }
}
